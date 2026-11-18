// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_DISK_DRIVE_H_
#define OZ3_DEVICES_DISK_DRIVE_H_

#include <array>
#include <cstdint>
#include <memory>
#include <optional>
#include <string_view>
#include <variant>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "gb/base/context.h"
#include "gb/base/validated_context.h"
#include "oz3/core/core_types.h"
#include "oz3/core/date_time.h"
#include "oz3/core/device.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/lockable.h"
#include "oz3/core/port.h"
#include "oz3/devices/disk_drive_command.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_hardware.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_status.h"

namespace oz3 {

//==============================================================================
// DiskDrivePreset
//==============================================================================

// The drive's timing presets: rough approximations of the effective speeds of
// typical drives of the time, not of any specific drive.
enum class DiskDrivePreset {
  kFloppy360k,   // 5 1/4" 360 KB double-density floppy (1981).
  kFloppy720k,   // 3 1/2" 720 KB double-density floppy (1986).
  kFloppy1440k,  // 3 1/2" 1.44 MB high-density floppy (1987).
  kHardDisk10m,  // 10 MB hard disk, PC/XT class (1983).
  kHardDisk20m,  // 20 MB hard disk (1984).
  kHardDisk40m,  // 40 MB hard disk, AT class (1986).
};

// The read cache every preset has: 4 sectors, copied out at 1,000,000 words a
// second (a 1 MHz controller copying a word a cycle).
inline constexpr int kDiskDriveReadCacheSectors = 4;
inline constexpr int64_t kDiskDriveCacheWordsPerSec = 1'000'000;

namespace internal {

// Returns a preset with rates in Kiw (1024 words) a second, and the latency in
// milliseconds.
constexpr DiskHardwareSettings MakeDiskDrivePreset(int64_t read_kiw_per_sec,
                                                   int64_t write_kiw_per_sec,
                                                   int latency_ms) {
  return {.read_cache_sectors = kDiskDriveReadCacheSectors,
          .read_words_per_sec = read_kiw_per_sec * 1024,
          .write_words_per_sec = write_kiw_per_sec * 1024,
          .cache_words_per_sec = kDiskDriveCacheWordsPerSec,
          .latency = absl::Milliseconds(latency_ms)};
}

}  // namespace internal

// Returns the hardware settings of `preset`: its read and write rates and
// latency, with the read cache above.
constexpr DiskHardwareSettings GetDiskDrivePreset(DiskDrivePreset preset) {
  switch (preset) {
    case DiskDrivePreset::kFloppy360k:
      return internal::MakeDiskDrivePreset(8, 6, 150);
    case DiskDrivePreset::kFloppy720k:
      return internal::MakeDiskDrivePreset(10, 8, 120);
    case DiskDrivePreset::kFloppy1440k:
      return internal::MakeDiskDrivePreset(20, 16, 100);
    case DiskDrivePreset::kHardDisk10m:
      return internal::MakeDiskDrivePreset(40, 40, 85);
    case DiskDrivePreset::kHardDisk20m:
      return internal::MakeDiskDrivePreset(64, 64, 65);
    case DiskDrivePreset::kHardDisk40m:
      return internal::MakeDiskDrivePreset(128, 128, 40);
  }
  return {};
}

//==============================================================================
// DiskDriveSettings
//==============================================================================

// Everything a host sets about a disk drive but its name: its wiring, how many
// files it can have open, and its timing.
struct DiskDriveSettings {
  bool operator==(const DiskDriveSettings&) const = default;

  // The most slots, as handle FFh means every slot.
  static constexpr int kMaxSlots = 255;

  // The longest latency, which is far longer than any real drive's.
  static constexpr absl::Duration kMaxLatency = absl::Minutes(1);

  // The first of DiskDrive::kPorts consecutive processor ports, in this order:
  // the command port, the data port, and the control port.
  int port = 0;

  // The interrupt raised as each completion is posted.
  DeviceInterrupt interrupt;

  // How many files can be open at once, 1 to kMaxSlots.
  int slots = 4;

  // The read cache, rates, and latency, none of which may be negative, and
  // the latency at most kMaxLatency.
  DiskHardwareSettings hardware =
      GetDiskDrivePreset(DiskDrivePreset::kFloppy1440k);
};

//==============================================================================
// DiskDriveFactory
//==============================================================================

// A disk drive on its ports, which a Processor creates a DiskDrive from.
class DiskDriveFactory final : public DeviceFactory {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a factory for a drive named `name`. Returns an error if `settings`
  // are invalid. The ports must also be ones the processor has, which
  // ProcessorConfig::Validate checks.
  static absl::StatusOr<std::unique_ptr<DiskDriveFactory>> Create(
      std::string_view name, const DiskDriveSettings& settings);

  ~DiskDriveFactory() override = default;

  //----------------------------------------------------------------------------
  // DeviceFactory implementation
  //----------------------------------------------------------------------------

  const DeviceConfig& GetConfig() const override { return config_; }
  std::unique_ptr<Device> CreateDevice(gb::Context* context) const override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  DiskDriveFactory(std::string_view name, const DiskDriveSettings& settings);

  const DeviceConfig config_;
  const DiskDriveSettings settings_;
};

//==============================================================================
// DiskDrive
//==============================================================================

// A smart disk drive: it owns the file system on the disk image the host
// attaches, and a program (normally the OS) asks it for files by name through
// its three ports, as the manual's Disk Drive page describes.
//
// The OS writes a command word, with the command's code in its high byte and
// any handle in its low byte, and the command's arguments, to the command port,
// and reads a status and any results back. The drive checks a command without
// touching the disk (its accept), and a latent command then completes later,
// once its disk work is done, raising the drive's interrupt. The control port
// shows the drive's state, and resets it when the OS writes a request.
//
// The drive is busy while its disk is, for the time the disk's settings give
// the work, in the processor's cycles. Meanwhile it does nothing else, not even
// a reset, so attaching an image makes it busy reading the superblock.
class DiskDrive final : public Device {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // What the drive takes from the processor's context: the host's clock, if
  // it has one, which the drive stamps files with. Without one, it stamps no
  // date.
  using Contract = gb::ContextContract<kConstraintHostClock>;

  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The ports the drive is wired to.
  static constexpr int kPorts = 3;

  // The control port's control word: the phase (a DiskDrivePhase) in its low
  // nibble, whether an image is attached, and the current command's code in
  // its high byte, or 0 when idle.
  static constexpr uint16_t kPhaseMask = 0x000F;
  static constexpr uint16_t kDiskAttached = 0x0010;
  static constexpr int kCommandShift = 8;

  // The handle that means every slot, for CLOSE.
  static constexpr int kAllSlots = 0xFF;

  // The request bits the OS writes to the control port.
  static constexpr uint16_t kResetRequest = 0x0001;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Writes back everything the file system hasn't committed, stamped with the
  // clock, as at a clean shutdown. A failure leaves the disk as a crash there
  // would. So whatever the clock uses must outlive the processor.
  ~DiskDrive() override;

  //----------------------------------------------------------------------------
  // Host interface
  //
  // Called between Processor::Execute calls.
  //----------------------------------------------------------------------------

  // Attaches `image`, which must not be null, first detaching any image
  // attached, as DetachImage does. The file system `image` is on must outlive
  // the drive, which may write to it as it is destroyed. The drive reads the
  // superblock, and is busy for that time from its next run. A program finds
  // out what the disk holds, such as no file system, from its commands; a host
  // that wants to know first opens the image with DiskImage::Open.
  void AttachImage(std::unique_ptr<DiskImage> image);

  // Detaches and returns the image, or null if the drive is empty, discarding
  // what the file system hadn't committed, as pulling a floppy would. A command
  // whose disk work is done keeps its status, and one whose work is still to
  // run finds no disk.
  std::unique_ptr<DiskImage> DetachImage();

 protected:
  //----------------------------------------------------------------------------
  // ExecutionComponent implementation
  //----------------------------------------------------------------------------

  void Execute() override;

 private:
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The drive's ports, as numbered in its DeviceConfig, and each one's bit in
  // a mask of ports to lock.
  static constexpr int kCommandPort = 0;
  static constexpr int kDataPort = 1;
  static constexpr int kControlPort = 2;
  static constexpr uint8_t kCommandLock = 1 << kCommandPort;
  static constexpr uint8_t kDataLock = 1 << kDataPort;
  static constexpr uint8_t kControlLock = 1 << kControlPort;

  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // A command the OS has written: its command word, its definition (null for
  // an unknown code), the argument fields taken so far, and the arguments its
  // accept decoded.
  struct Command {
    // Returns the handle in the command word.
    int GetHandle() const { return word & 0xFF; }

    uint16_t word = 0;
    const DiskCommandDef* def = nullptr;
    std::vector<PortWords> arguments;

    // The arguments the accept decoded and found valid: a name, or FIND's
    // pattern; RENAME's new name; and FORMAT's or LABEL's label.
    std::optional<DiskFilename> name;
    std::optional<DiskFilename> new_name;
    std::optional<DiskLabel> label;
  };

  // An accept or a completion: its status, its results, which a command has
  // only after an OK status, and how many of its fields (the status, then the
  // results) are on the command port, the last of which the OS may not have
  // taken yet.
  struct CommandResponse {
    // Returns the number of fields: the status, then each result.
    int GetFieldCount() const { return 1 + static_cast<int>(results.size()); }

    DiskStatus status = DiskStatus::kOk;
    std::vector<PortWords> results;
    int fields_sent = 0;

    // Returns the phase the control port shows while sending the response.
    // Phase 3 means a field from the drive is on the command port, so until the
    // first is, the drive is still working on it.
    DiskDrivePhase GetPhase() const {
      return fields_sent > 0 ? DiskDrivePhase::kSending
                             : DiskDrivePhase::kWorking;
    }
  };

  // The drive's states. Each holds only what it works on, and returns the
  // phase and the command word the control port shows for it.
  struct IdleState {
    DiskDrivePhase GetPhase() const { return DiskDrivePhase::kIdle; }
    uint16_t GetCommandWord() const { return 0; }
  };
  struct ArgumentsState {
    DiskDrivePhase GetPhase() const { return DiskDrivePhase::kArguments; }
    uint16_t GetCommandWord() const { return command.word; }

    Command command;
  };
  struct SendingAcceptState {
    DiskDrivePhase GetPhase() const { return accept.GetPhase(); }
    uint16_t GetCommandWord() const { return command.word; }

    Command command;
    CommandResponse accept;
  };
  struct RunningWorkState {
    DiskDrivePhase GetPhase() const { return DiskDrivePhase::kWorking; }
    uint16_t GetCommandWord() const { return command.word; }

    Command command;
  };
  struct SendingCompletionState {
    DiskDrivePhase GetPhase() const { return completion.GetPhase(); }
    uint16_t GetCommandWord() const { return command.word; }

    Command command;
    CommandResponse completion;
  };

  // A READ's or WRITE's data phase, of the transfers its accept promised
  // (words, or dwords for the packed forms): what both directions keep.
  struct Transfer {
    Command command;
    int accepted = 0;  // The transfers the accept promised.

    // The transfers moved through the data port: put on it for a READ, though
    // the OS may not have taken the last yet, and taken from it for a WRITE.
    int moved = 0;
  };
  struct ReadingState {
    DiskDrivePhase GetPhase() const { return DiskDrivePhase::kReading; }
    uint16_t GetCommandWord() const { return transfer.command.word; }

    Transfer transfer;

    // The file's position when the read began.
    int start_position = 0;

    // The next transfer, read from the file while the OS takes the one on the
    // data port, until it goes on the port.
    std::optional<PortWords> next;
  };
  struct WritingState {
    DiskDrivePhase GetPhase() const { return DiskDrivePhase::kWriting; }
    uint16_t GetCommandWord() const { return transfer.command.word; }

    Transfer transfer;
  };

  // A data phase after a disk call failed. Its early completion's status goes
  // out first, and nothing moves on the data port until it has. The rest of
  // the transfers then move no data, at port speed.
  struct FailedTransfer {
    // Returns the phase the control port shows: phase 3 while the early
    // completion's status waits on the command port, and otherwise
    // `data_phase`.
    DiskDrivePhase GetPhase(DiskDrivePhase data_phase) const {
      return completion.has_value() && completion->fields_sent > 0
                 ? DiskDrivePhase::kSending
                 : data_phase;
    }

    Transfer transfer;

    // The early completion, until the OS has taken it.
    std::optional<CommandResponse> completion;
  };
  struct FailedReadingState {
    DiskDrivePhase GetPhase() const {
      return failure.GetPhase(DiskDrivePhase::kReading);
    }
    uint16_t GetCommandWord() const { return failure.transfer.command.word; }

    FailedTransfer failure;

    // The file's position when the read began.
    int start_position = 0;

    // The next transfer to put on the data port: the words read before the
    // failure, padded with zeros, and then only zeros.
    PortWords next;
  };
  struct FailedWritingState {
    DiskDrivePhase GetPhase() const {
      return failure.GetPhase(DiskDrivePhase::kWriting);
    }
    uint16_t GetCommandWord() const { return failure.transfer.command.word; }

    FailedTransfer failure;
  };

  using State =
      std::variant<IdleState, ArgumentsState, SendingAcceptState,
                   RunningWorkState, SendingCompletionState, ReadingState,
                   WritingState, FailedReadingState, FailedWritingState>;

  // Whether a run can go on, after a state's step or the work a run does
  // before one.
  enum class StateResult {
    // It did what it could, so the run goes on to the step of the drive's
    // state, which may be a new one.
    kContinue,

    // It can't go on until the OS reads or writes a port, or a lock it needs
    // is granted.
    kWait,
  };

  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The factory constructs the drive with its validated settings.
  friend class DiskDriveFactory;

  DiskDrive(DeviceConfig config, const DiskDriveSettings& settings,
            Contract contract);

  // Requests the locks of the ports in `ports`, a mask, that the drive doesn't
  // hold or hasn't requested. Returns whether it holds them all, which it keeps
  // until the run ends without waiting for one.
  bool LockPorts(uint8_t ports);

  // Returns `port`'s lock, which the drive must hold.
  const Lock& GetLock(int port) const;

  // Returns whether the drive has asked for a lock it hasn't been granted.
  bool IsWaitingForLock() const;

  // Releases every lock the drive holds or has requested.
  void ReleaseLocks();

  // If the disk is busy, as the host or the run's step made it, releases every
  // lock and advances the drive's cycles by that time, and returns true.
  bool ChargeBusyTime();

  // Returns the clock's date and time, or no date without a clock.
  DateTime GetNow();

  //----------------------------------------------------------------------------
  // Control port
  //----------------------------------------------------------------------------

  // Returns whether the OS has written a request to the control port that the
  // drive hasn't taken.
  bool HasControlRequest();

  // Takes any request on the control port, and clears it. A reset first
  // withdraws the words on the command and data ports, leaves a READ's
  // position just past the words the OS took, and goes idle. Returns
  // kWait if the drive is waiting for a port's lock, which leaves the request
  // pending.
  StateResult HandleAnyControlRequest();

  // Clears the request on the control port, and shows the control word in
  // place of the request's word. The drive must hold the port's lock.
  void ClearControlRequest();

  // Writes the control word to the control port if it has changed. Returns
  // kWait if the drive is waiting for the port's lock. No request may be
  // pending.
  StateResult UpdateControlWord();

  // Returns the control word.
  uint16_t GetControlWord() const;

  //----------------------------------------------------------------------------
  // States
  //
  // Each runs its state's step, which locks the ports it uses once it has
  // something to do. Steps are what move the drive from one state to the next,
  // apart from a reset. A step that moves on doesn't use its state afterward,
  // as the new state replaces it.
  //----------------------------------------------------------------------------

  StateResult ExecuteState(IdleState& state);
  StateResult ExecuteState(ArgumentsState& state);
  StateResult ExecuteState(SendingAcceptState& state);
  StateResult ExecuteState(RunningWorkState& state);
  StateResult ExecuteState(SendingCompletionState& state);
  StateResult ExecuteState(ReadingState& state);
  StateResult ExecuteState(WritingState& state);
  StateResult ExecuteState(FailedReadingState& state);
  StateResult ExecuteState(FailedWritingState& state);

  // Returns the state that follows `command`'s accept, once the OS has taken
  // it: its work, its data phase, or idle.
  [[nodiscard]] State GetStateAfterAccept(Command command,
                                          const CommandResponse& accept);

  //----------------------------------------------------------------------------
  // Protocol
  //----------------------------------------------------------------------------

  // Loads a field the OS wrote to the command port into `field`, clearing its
  // status, and returns kContinue. Returns kWait if there is none yet, or the
  // drive is waiting for the port's lock.
  StateResult ReadCommandPort(PortWords& field);

  // Accepts `command`, which has all its arguments, and returns the state that
  // sends its accept.
  [[nodiscard]] SendingAcceptState Accept(Command command);

  // Validates that `response` has `result_count` results if its status is OK,
  // and none otherwise, as an error ends a command's fields.
  static void ValidateResponse(const CommandResponse& response,
                               int result_count);

  // Returns whether the OS has taken every field of `response`.
  bool IsCommandResponseTaken(const CommandResponse& response);

  // Puts the next field of `response` on the command port, if it has one left
  // and the OS has taken the last one sent, and returns kContinue. Otherwise,
  // or if the drive is waiting for a port's lock, returns kWait.
  //
  // The first field goes out with phase 3 (see CommandResponse::GetPhase), so
  // it also needs the control port's lock. It goes over any word the OS wrote
  // while the drive wasn't taking words, which the drive never read.
  StateResult SendNextField(CommandResponse& response);

  // Sends the next field of `completion` as SendNextField does, raising the
  // interrupt with its status.
  StateResult SendCompletionField(CommandResponse& completion);

  //----------------------------------------------------------------------------
  // Transfers
  //
  // A data phase moves a transfer at a time, with a disk call of a word or two
  // for each, so a sector's disk work comes at the transfer that starts it.
  //----------------------------------------------------------------------------

  // Returns the transfer of `command`, given its accept.
  static Transfer MakeTransfer(Command command, const CommandResponse& accept);

  // Returns the failure of `transfer`, with `status` as its early completion.
  static FailedTransfer FailTransfer(Transfer transfer, DiskStatus status);

  // Returns the state that ends `transfer`'s data phase, with its completion of
  // `status`.
  [[nodiscard]] static State EndTransfer(Transfer transfer, DiskStatus status);

  // Returns the state that ends `failure`'s data phase: idle once the OS has
  // taken its early completion, and until then, waiting for the OS to.
  [[nodiscard]] static State EndFailedTransfer(FailedTransfer failure);

  // Reads `transfer`'s next transfer from the file into `words`, padded with
  // zeros past the end of the file, and returns the read's status. If it
  // fails, `words` holds what was read before the failure.
  DiskStatus ReadNextTransfer(const Transfer& transfer, PortWords& words);

  // Writes `words`, a transfer the OS wrote, to the file, and returns the
  // write's status. If it fails, it flushes what the drive has taken first.
  DiskStatus WriteTransfer(const Transfer& transfer, PortWords words);

  // Seeks a READ's handle back past the words read ahead of the OS, as a reset
  // ends it with the position just past the words the OS took. The read began
  // at `start_position`, and `data_untaken` is whether the data port's flag
  // was still set when the reset withdrew its word.
  void AbortRead(const Transfer& transfer, int start_position,
                 bool data_untaken);

  //----------------------------------------------------------------------------
  // Commands
  //
  // A command's accept checks it without the disk, and its work does its disk
  // work. Each returns its response, with results only for an OK status.
  //----------------------------------------------------------------------------

  // An accept may decode `command`'s arguments into it, for its work.
  CommandResponse AcceptCommand(Command* command);
  CommandResponse RunCommand(const Command& command);

  // Returns the words of the name, pattern, or label argument in `command`'s
  // kDiskNameFields fields from `first_field`.
  static DiskFilename::Words GetNameWords(const Command& command,
                                          int first_field = 0);

  // Appends `words`, an even number of them, to `fields` as fields of two.
  static void AppendWordFields(absl::Span<const uint16_t> words,
                               std::vector<PortWords>* fields);

  // Returns what a command that opens a file in a new slot fails with at
  // accept, given its disk's status: `disk_status` if it isn't kOk, and then
  // too many open.
  DiskStatus GetNewSlotStatus(DiskStatus disk_status) const;

  // Returns the response to a command that opens `file`, with `status`.
  static CommandResponse MakeOpenResponse(DiskStatus status, DiskFile file);

  CommandResponse AcceptOpen(Command* command);
  CommandResponse RunOpen(const Command& command);
  CommandResponse AcceptOpenRoot();
  CommandResponse RunOpenRoot();
  CommandResponse AcceptCreate(Command* command);
  CommandResponse RunCreate(const Command& command);
  CommandResponse AcceptClose(const Command& command);
  CommandResponse RunClose(const Command& command);
  CommandResponse AcceptFlush(const Command& command);
  CommandResponse RunFlush(const Command& command);
  CommandResponse AcceptSeek(const Command& command);
  CommandResponse AcceptInfo(const Command& command);
  CommandResponse AcceptRead(const Command& command);
  CommandResponse AcceptWrite(const Command& command);
  CommandResponse AcceptTruncate(const Command& command);
  CommandResponse RunTruncate(const Command& command);
  CommandResponse AcceptFind(Command* command);
  CommandResponse RunFind(const Command& command);
  CommandResponse AcceptDelete(Command* command);
  CommandResponse RunDelete(const Command& command);
  CommandResponse AcceptRename(Command* command);
  CommandResponse RunRename(const Command& command);
  CommandResponse AcceptAttrib(Command* command);
  CommandResponse RunAttrib(const Command& command);
  CommandResponse AcceptMedia();
  CommandResponse AcceptVolume();
  CommandResponse RunVolume();
  CommandResponse AcceptFormat(Command* command);
  CommandResponse RunFormat(const Command& command);
  CommandResponse AcceptLabel(Command* command);
  CommandResponse RunLabel(const Command& command);

  //----------------------------------------------------------------------------
  // State
  //----------------------------------------------------------------------------

  DiskFileSystem file_system_;
  gb::ValidatedContext context_;

  // Each port's lock, while it is requested.
  std::array<std::unique_ptr<Lock>, kPorts> locks_;

  State state_;

  // The control word last written to the control port.
  uint16_t control_word_shown_ = 0;
};

}  // namespace oz3

#endif  // OZ3_DEVICES_DISK_DRIVE_H_
