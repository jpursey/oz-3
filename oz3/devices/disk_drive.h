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
#include <utility>
#include <variant>
#include <vector>

#include "absl/functional/any_invocable.h"
#include "absl/status/statusor.h"
#include "absl/time/civil_time.h"
#include "absl/time/time.h"
#include "oz3/core/core_types.h"
#include "oz3/core/date_time.h"
#include "oz3/core/device.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/lockable.h"
#include "oz3/core/port.h"
#include "oz3/devices/disk_drive_command.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_hardware.h"
#include "oz3/disk/disk_image.h"
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
  std::unique_ptr<Device> CreateDevice() const override;

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
// its three ports, as the wiki's Disk Drive page describes.
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

  // Returns the host's date and time, as the timer's clock does, which the
  // drive stamps files with.
  using Clock = absl::AnyInvocable<absl::CivilSecond()>;

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

  // The request bits the OS writes to the control port.
  static constexpr uint16_t kResetRequest = 0x0001;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Writes back everything the file system hasn't committed, stamped with the
  // clock, as at a clean shutdown. A failure leaves the disk as a crash there
  // would. So whatever the clock uses must outlive the drive.
  ~DiskDrive() override;

  //----------------------------------------------------------------------------
  // Host interface
  //
  // Called between Processor::Execute calls.
  //----------------------------------------------------------------------------

  // Sets the clock the drive stamps files with, replacing any it had, or none
  // for the drive to stamp no date.
  void SetClock(Clock clock) { clock_ = std::move(clock); }

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
  // an unknown code), and the argument fields taken so far.
  struct Command {
    // Returns the handle in the command word.
    int GetHandle() const { return word & 0xFF; }

    uint16_t word = 0;
    const DiskCommandDef* def = nullptr;
    std::vector<PortWords> arguments;
  };

  // An accept or a completion: its status, its results, which a command has
  // only after an OK status, and how many of its fields (the status, then the
  // results) are on the command port, the last of which the OS may not have
  // taken yet.
  struct CommandResponse {
    DiskStatus status = DiskStatus::kOk;
    std::vector<PortWords> results;
    int fields_sent = 0;
  };

  // The drive's states. Each holds only what it works on, and names the phase
  // the control port shows for it and the ports its step needs locked.
  struct IdleState {
    static constexpr DiskDrivePhase kPhase = DiskDrivePhase::kIdle;
    static constexpr uint8_t kLocks = kCommandLock;
  };
  struct ArgumentsState {
    static constexpr DiskDrivePhase kPhase = DiskDrivePhase::kArguments;
    static constexpr uint8_t kLocks = kCommandLock;
    Command command;
  };
  struct SendingAcceptState {
    static constexpr DiskDrivePhase kPhase = DiskDrivePhase::kSending;
    static constexpr uint8_t kLocks = kCommandLock;
    Command command;
    CommandResponse accept;
  };
  struct RunningWorkState {
    static constexpr DiskDrivePhase kPhase = DiskDrivePhase::kWorking;
    static constexpr uint8_t kLocks = 0;
    Command command;
  };
  struct SendingCompletionState {
    static constexpr DiskDrivePhase kPhase = DiskDrivePhase::kSending;
    static constexpr uint8_t kLocks = kCommandLock;
    Command command;
    CommandResponse completion;
  };

  // Withdrawing the words on the command and data ports. The reset request
  // stays pending on the control port until this is done, and the drive idle.
  struct ResettingState {
    static constexpr DiskDrivePhase kPhase = DiskDrivePhase::kIdle;
    static constexpr uint8_t kLocks = kCommandLock | kDataLock | kControlLock;
  };

  using State =
      std::variant<IdleState, ArgumentsState, SendingAcceptState,
                   RunningWorkState, SendingCompletionState, ResettingState>;

  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The factory constructs the drive with its validated settings.
  friend class DiskDriveFactory;

  DiskDrive(DeviceConfig config, const DiskDriveSettings& settings);

  // Requests the locks of the ports in `ports`, a mask, that the drive doesn't
  // hold or hasn't requested. Returns whether it holds them all, which it keeps
  // until the run ends without waiting for one.
  bool LockPorts(uint8_t ports);

  // Returns `port`'s lock, which the drive must hold.
  const Lock& GetLock(int port) const;

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

  // Takes the request on the control port: a reset starts resetting, which
  // leaves the request pending until it is done, and any other request is
  // cleared. Returns false if the drive is waiting for the port's lock.
  bool TakeControlRequest();

  // Clears the request on the control port, which then shows the request's
  // word until the control word is updated. The drive must hold the port's
  // lock.
  void ClearControlRequest();

  // Writes the control word to the control port if it has changed, unless a
  // request is pending. The drive must hold the port's lock if it has.
  void UpdateControlWord();

  // Returns whether the control port shows the current control word.
  bool IsControlWordShown() const {
    return GetControlWord() == control_word_shown_;
  }

  // Returns the control word, and its phase.
  uint16_t GetControlWord() const;
  DiskDrivePhase GetPhase() const;

  // Returns the ports the current state's step needs locked, and the control
  // port too if its control word isn't shown.
  uint8_t GetLocksNeeded() const;

  //----------------------------------------------------------------------------
  // States
  //
  // Each runs its state's step, holding the ports in the state's kLocks, and
  // returns true if it moved to another state, whose step then runs in the
  // same run. A step that moves on doesn't use its state afterward, as the new
  // state replaces it.
  //----------------------------------------------------------------------------

  bool ExecuteState(IdleState& state);
  bool ExecuteState(ArgumentsState& state);
  bool ExecuteState(SendingAcceptState& state);
  bool ExecuteState(RunningWorkState& state);
  bool ExecuteState(SendingCompletionState& state);
  bool ExecuteState(ResettingState& state);

  //----------------------------------------------------------------------------
  // Protocol
  //----------------------------------------------------------------------------

  // Loads a field the OS wrote to the command port, clearing its status, or
  // returns nullopt if there is none yet.
  std::optional<PortWords> ReadCommandPort();

  // Accepts `command`, which has all its arguments, and starts sending the
  // accept.
  void Accept(Command command);

  // Returns whether the OS has taken every field of `response`.
  bool IsCommandResponseTaken(const CommandResponse& response);

  // Puts the next field of `response` on the command port, if it has one left
  // and the OS has taken the last one sent. The first goes over any word the
  // OS wrote while the drive wasn't taking words, which the drive never read.
  // Returns whether it put one.
  bool SendNextField(CommandResponse& response);

  //----------------------------------------------------------------------------
  // Commands
  //
  // A command's accept checks it without the disk, and its work does its disk
  // work. Each returns its response, with results only for an OK status.
  //----------------------------------------------------------------------------

  CommandResponse AcceptCommand(const Command& command);
  CommandResponse RunCommand(const Command& command);

  CommandResponse AcceptOpenRoot();
  CommandResponse RunOpenRoot();
  CommandResponse AcceptClose(const Command& command);
  CommandResponse RunClose(const Command& command);
  CommandResponse AcceptMedia();

  //----------------------------------------------------------------------------
  // State
  //----------------------------------------------------------------------------

  DiskFileSystem file_system_;
  Clock clock_;

  // Each port's lock, while it is requested.
  std::array<std::unique_ptr<Lock>, kPorts> locks_;

  State state_;

  // The control word last written to the control port, or the request word
  // it shows once the drive has cleared a request.
  uint16_t control_word_shown_ = 0;
};

}  // namespace oz3

#endif  // OZ3_DEVICES_DISK_DRIVE_H_
