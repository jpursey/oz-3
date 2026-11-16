// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_DISK_DRIVE_H_
#define OZ3_DEVICES_DISK_DRIVE_H_

#include <array>
#include <cstdint>
#include <deque>
#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/functional/any_invocable.h"
#include "absl/status/statusor.h"
#include "absl/time/civil_time.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "oz3/core/core_types.h"
#include "oz3/core/date_time.h"
#include "oz3/core/device.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/lockable.h"
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

namespace internal {

// Returns a preset with rates in Kiw (1024 words) a second, and the latency in
// milliseconds.
constexpr DiskHardwareSettings MakeDiskDrivePreset(int64_t read_kiw_per_sec,
                                                   int64_t write_kiw_per_sec,
                                                   int latency_ms) {
  return {.read_cache_sectors = 4,
          .read_words_per_sec = read_kiw_per_sec * 1024,
          .write_words_per_sec = write_kiw_per_sec * 1024,
          .cache_words_per_sec = 1'000'000,
          .latency = absl::Milliseconds(latency_ms)};
}

}  // namespace internal

// Returns the hardware settings of `preset`: its read and write rates and
// latency, with the drive's default read cache of 4 sectors, and a cache rate
// of 1,000,000 words a second (a 1 MHz controller copying a word a cycle).
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

  // The first of DiskDrive::kPorts consecutive processor ports: the command
  // port, the data port, and then the control port.
  int port = 0;

  // The interrupt raised as each completion is posted.
  DeviceInterrupt interrupt;

  // How many files can be open at once, 1 to kMaxSlots.
  int slots = 4;

  // The read cache, rates, and latency, none of which may be negative.
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
// The drive does a command's disk work all at once, and charges the time the
// disk was busy for it in cycles. Disk work waits for the work before it, such
// as reading the superblock of an image just attached.
//
// The drive runs the cycle after a core reads or writes one of its ports, and
// otherwise sleeps until its disk work is done.
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

  // The control port's state word: the phase (a DiskDrivePhase) in its low
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
  // attached, as DetachImage does. The superblock is read as disk work, which
  // later disk work waits for. Returns kOk for a formatted disk, kUnformatted
  // for one with no file system, or kIoError, which fails the image, as
  // DiskFileSystem::AttachImage does. The image stays attached for all three.
  DiskStatus AttachImage(std::unique_ptr<DiskImage> image);

  // Detaches and returns the image, or null if the drive is empty, discarding
  // what the file system hadn't committed, as pulling a floppy would. A
  // command in progress on it completes with kNoDisk.
  std::unique_ptr<DiskImage> DetachImage();

 protected:
  //----------------------------------------------------------------------------
  // ExecutionComponent implementation
  //----------------------------------------------------------------------------

  void Execute() override;

 private:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // A field to send on the command port.
  struct Field {
    DiskPortWidth width = DiskPortWidth::kWord;
    std::array<uint16_t, 2> words = {0, 0};
    bool completion = false;  // A completion's status, for the interrupt.
  };

  // A latent command's completion, posted at `cycle`.
  struct Completion {
    DiskStatus status = DiskStatus::kOk;
    std::vector<uint16_t> results;
    Cycles cycle = 0;
  };

  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The factory constructs the drive with its validated settings.
  friend class DiskDriveFactory;

  DiskDrive(DeviceConfig config, const DiskDriveSettings& settings);

  // Returns `port`'s lock if it has been granted, requesting it if it hasn't
  // been, or null if it hasn't been granted yet, so the drive runs again next
  // cycle.
  Lock* GetLock(int port);

  // Returns the clock's date and time, or no date without a clock.
  DateTime GetNow();

  // Charges the busy time the file system has collected to the disk's
  // timeline, from the end of the work before it or now, whichever is later,
  // and returns the cycle the disk is done. Called after every file system
  // call that may touch the disk.
  Cycles ChargeBusyTime();

  //----------------------------------------------------------------------------
  // Ports
  //----------------------------------------------------------------------------

  // Acts on any request waiting on the control port. Returns false if the
  // drive is waiting for the locks it needs, so nothing else runs this cycle.
  bool ExecuteRequest();

  // Discards the command in progress and withdraws every word waiting on the
  // command and data ports, holding their locks.
  void Reset(const Lock& command_lock, const Lock& data_lock);

  // Moves a latent command's completion to the command port if it is due.
  void ExecuteCompletion();

  // Takes a word for the drive from the command port, and sends the next
  // field once the OS has taken the last.
  void ExecuteCommandPort();
  void SendFields();

  // Writes the state word to the control port if it has changed.
  void UpdateState();

  // Returns the state word, as the control port shows it.
  uint16_t GetStateWord() const;
  DiskDrivePhase GetPhase() const;

  //----------------------------------------------------------------------------
  // Protocol
  //----------------------------------------------------------------------------

  // Returns whether the drive is waiting for a command word or argument.
  bool IsReceiving() const;

  // Starts the command `command_word` names, accepting it if it takes no
  // arguments.
  void StartCommand(uint16_t command_word);

  // Adds an argument field, accepting the command once it is the last.
  void AddArgument(DiskPortWidth width, const std::array<uint16_t, 2>& words);

  // Accepts the command, sending its accept status and results, and then for a
  // latent command does its work.
  void Accept();

  // Queues `status` to send, and if it is kOk, `results`, as the fields
  // `widths` gives, which must hold exactly as many words.
  void Send(DiskStatus status, absl::Span<const DiskPortWidth> widths,
            absl::Span<const uint16_t> results, bool completion);

  // Ends the command once it has nothing left to send or do, or at once,
  // discarding what is left of it.
  void EndCommandIfDone();
  void EndCommand();

  //----------------------------------------------------------------------------
  // Commands
  //
  // A command's accept checks it without the disk, setting `results` on kOk,
  // and running it does its disk work, setting `results` on kOk.
  //----------------------------------------------------------------------------

  // Returns the handle in the command word.
  int GetHandle() const { return command_word_ & 0xFF; }

  DiskStatus AcceptCommand(std::vector<uint16_t>* results);
  DiskStatus RunCommand(std::vector<uint16_t>* results);

  DiskStatus AcceptOpenRoot();
  DiskStatus RunOpenRoot(std::vector<uint16_t>* results);
  DiskStatus AcceptClose();
  DiskStatus RunClose();
  DiskStatus AcceptMedia(std::vector<uint16_t>* results);

  //----------------------------------------------------------------------------
  // State
  //----------------------------------------------------------------------------

  DiskFileSystem file_system_;
  Clock clock_;

  // Each port's lock, while it is requested, and whether a lock this run
  // needed isn't granted yet.
  std::array<std::unique_ptr<Lock>, kPorts> locks_;
  bool waiting_ = false;

  // The command in progress, if active_: its command word and definition
  // (null for an unknown code), and its argument words so far.
  bool active_ = false;
  uint16_t command_word_ = 0;
  const DiskCommandDef* def_ = nullptr;
  std::vector<uint16_t> arguments_;
  int arguments_received_ = 0;

  // The fields to send on the command port, the first already there if
  // front_stored_.
  std::deque<Field> outgoing_;
  bool front_stored_ = false;

  // A latent command's completion, until it is posted.
  std::optional<Completion> completion_;

  // The cycle the disk finishes the work it has been given.
  Cycles disk_free_cycle_ = 0;

  // The state word last written to the control port.
  uint16_t state_shown_ = 0;
};

}  // namespace oz3

#endif  // OZ3_DEVICES_DISK_DRIVE_H_
