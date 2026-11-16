// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/disk_drive.h"

#include <algorithm>
#include <string>
#include <utility>

#include "absl/log/check.h"
#include "absl/log/log.h"
#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "oz3/core/port.h"
#include "oz3/disk/disk_media.h"

namespace oz3 {

namespace {

// The drive's ports, as numbered in its DeviceConfig.
constexpr int kCommandPort = 0;
constexpr int kDataPort = 1;
constexpr int kControlPort = 2;

// Returns the cycles `busy` takes at `clock_hz`, rounded up.
Cycles GetBusyCycles(absl::Duration busy, int64_t clock_hz) {
  return absl::ToInt64Seconds(absl::Ceil(busy * clock_hz, absl::Seconds(1)));
}

// Loads a field from `port` with `lock`, clearing its status. The port's
// address is back at word 0 after it.
std::array<uint16_t, 2> LoadField(Port& port, const Lock& lock,
                                  DiskPortWidth width) {
  std::array<uint16_t, 2> words = {0, 0};
  if (width == DiskPortWidth::kDword) {
    port.LoadWord(lock, Port::A, words[0]);
    port.LoadWord(lock, Port::A | Port::S, words[1]);
  } else {
    port.LoadWord(lock, Port::S, words[0]);
  }
  return words;
}

// Stores a field to `port` with `lock`, setting its status. The port's address
// is back at word 0 after it.
void StoreField(Port& port, const Lock& lock, DiskPortWidth width,
                const std::array<uint16_t, 2>& words) {
  if (width == DiskPortWidth::kDword) {
    port.StoreWord(lock, Port::A, words[0]);
    port.StoreWord(lock, Port::A | Port::S, words[1]);
  } else {
    port.StoreWord(lock, Port::S, words[0]);
  }
}

}  // namespace

//==============================================================================
// DiskDriveFactory
//==============================================================================

absl::StatusOr<std::unique_ptr<DiskDriveFactory>> DiskDriveFactory::Create(
    std::string_view name, const DiskDriveSettings& settings) {
  if (settings.slots < 1 || settings.slots > DiskDriveSettings::kMaxSlots) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Disk drive ", name, " has ", settings.slots,
        " slots, but it must have 1 to ", DiskDriveSettings::kMaxSlots));
  }
  const DiskHardwareSettings& hardware = settings.hardware;
  if (hardware.read_cache_sectors < 0 || hardware.read_words_per_sec < 0 ||
      hardware.write_words_per_sec < 0 || hardware.cache_words_per_sec < 0 ||
      hardware.latency < absl::ZeroDuration()) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Disk drive ", name, " has a negative read cache, rate, or latency"));
  }
  return absl::WrapUnique(new DiskDriveFactory(name, settings));
}

DiskDriveFactory::DiskDriveFactory(std::string_view name,
                                   const DiskDriveSettings& settings)
    : config_({.name = std::string(name),
               .ports = {settings.port, settings.port + 1, settings.port + 2},
               .interrupts = {settings.interrupt}}),
      settings_(settings) {}

std::unique_ptr<Device> DiskDriveFactory::CreateDevice() const {
  return absl::WrapUnique(new DiskDrive(config_, settings_));
}

//==============================================================================
// DiskDrive
//==============================================================================

DiskDrive::DiskDrive(DeviceConfig config, const DiskDriveSettings& settings)
    : Device(std::move(config)),
      file_system_(settings.hardware, settings.slots) {}

DiskDrive::~DiskDrive() {
  // There is nothing to tell of a failure, which leaves the disk as a crash
  // would.
  file_system_.Flush(GetNow());
}

DiskStatus DiskDrive::AttachImage(std::unique_ptr<DiskImage> image) {
  CHECK(image != nullptr);
  DetachImage();
  const DiskStatus status = file_system_.AttachImage(std::move(image));
  Wake();
  return status;
}

std::unique_ptr<DiskImage> DiskDrive::DetachImage() {
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  if (image == nullptr) {
    return nullptr;
  }

  // The disk's work goes with it.
  file_system_.CollectBusyTime();
  disk_free_cycle_ = 0;
  if (completion_.has_value()) {
    completion_ = Completion{.status = DiskStatus::kNoDisk};
  }
  Wake();
  return image;
}

void DiskDrive::Execute() {
  waiting_ = false;

  // Picks up disk work the host started, such as reading a superblock.
  ChargeBusyTime();

  if (ExecuteRequest()) {
    ExecuteCompletion();
    ExecuteCommandPort();
    UpdateState();
  }

  // Locks are kept while the drive waits for one, so it gathers them all.
  if (waiting_) {
    AdvanceCycles(1);
    return;
  }
  for (std::unique_ptr<Lock>& lock : locks_) {
    lock.reset();
  }
  if (completion_.has_value()) {
    Sleep(std::max<Cycles>(completion_->cycle - GetCycles(), 1));
    return;
  }
  Sleep();
}

Lock* DiskDrive::GetLock(int port) {
  std::unique_ptr<Lock>& lock = locks_[port];
  if (lock == nullptr) {
    lock = LockPort(port);
  }
  if (!lock->IsLocked()) {
    waiting_ = true;
    return nullptr;
  }
  return lock.get();
}

DateTime DiskDrive::GetNow() {
  return clock_ != nullptr ? DateTime::FromCivil(clock_()) : DateTime();
}

Cycles DiskDrive::ChargeBusyTime() {
  const absl::Duration busy = file_system_.CollectBusyTime();
  disk_free_cycle_ = std::max(disk_free_cycle_, GetCycles());
  if (busy > absl::ZeroDuration()) {
    disk_free_cycle_ += GetBusyCycles(busy, GetClockHz());
  }
  return disk_free_cycle_;
}

//------------------------------------------------------------------------------
// Ports
//------------------------------------------------------------------------------

bool DiskDrive::ExecuteRequest() {
  // A flag only changes under the port's lock, so a clear one needs no lock.
  Port& control = GetPort(kControlPort);
  if (control.GetStatus() == 0) {
    return true;
  }

  // Each lock is requested, so the drive waits for them all at once.
  Lock* control_lock = GetLock(kControlPort);
  Lock* command_lock = GetLock(kCommandPort);
  Lock* data_lock = GetLock(kDataPort);
  if (control_lock == nullptr || command_lock == nullptr ||
      data_lock == nullptr) {
    return false;
  }
  uint16_t request = 0;
  control.LoadWord(*control_lock, Port::S, request);
  if ((request & kResetRequest) != 0) {
    Reset(*command_lock, *data_lock);
  }

  // The state word is restored with the flag cleared, in one step.
  state_shown_ = GetStateWord();
  control.StoreWord(*control_lock, 0, state_shown_);
  return true;
}

void DiskDrive::Reset(const Lock& command_lock, const Lock& data_lock) {
  uint16_t word = 0;
  GetPort(kCommandPort).LoadWord(command_lock, Port::S, word);
  GetPort(kDataPort).LoadWord(data_lock, Port::S, word);
  EndCommand();
}

void DiskDrive::ExecuteCompletion() {
  if (!completion_.has_value() || completion_->cycle > GetCycles()) {
    return;
  }
  Send(completion_->status, def_->completion_results, completion_->results,
       /*completion=*/true);
  completion_.reset();
}

void DiskDrive::ExecuteCommandPort() {
  Port& port = GetPort(kCommandPort);
  if (IsReceiving() && port.GetStatus() != 0) {
    Lock* lock = GetLock(kCommandPort);
    if (lock == nullptr) {
      return;
    }
    if (!active_) {
      StartCommand(LoadField(port, *lock, DiskPortWidth::kWord)[0]);
    } else {
      const DiskPortWidth width = def_->arguments[arguments_received_];
      AddArgument(width, LoadField(port, *lock, width));
    }
  }
  SendFields();
  EndCommandIfDone();
}

void DiskDrive::SendFields() {
  Port& port = GetPort(kCommandPort);
  if (outgoing_.empty()) {
    return;
  }
  if (front_stored_) {
    // The field waits until the OS takes it.
    if (port.GetStatus() != 0) {
      return;
    }
    outgoing_.pop_front();
    front_stored_ = false;
    if (outgoing_.empty()) {
      return;
    }
  }

  // The first field after a quiet phase goes over any word the OS wrote then,
  // which the drive never read.
  Lock* lock = GetLock(kCommandPort);
  if (lock == nullptr) {
    return;
  }
  const Field& field = outgoing_.front();
  StoreField(port, *lock, field.width, field.words);
  front_stored_ = true;
  if (field.completion) {
    RaiseInterrupt(0);
  }
}

void DiskDrive::UpdateState() {
  const uint16_t state = GetStateWord();
  if (state == state_shown_) {
    return;
  }
  Lock* lock = GetLock(kControlPort);
  if (lock == nullptr) {
    return;
  }

  // A request that came while the drive waited for the lock is acted on next
  // cycle, as the state word mustn't go over it.
  Port& control = GetPort(kControlPort);
  if (control.GetStatus() != 0) {
    waiting_ = true;
    return;
  }
  control.StoreWord(*lock, 0, state);
  state_shown_ = state;
}

uint16_t DiskDrive::GetStateWord() const {
  uint16_t state = static_cast<uint16_t>(GetPhase());
  if (file_system_.HasImage()) {
    state |= kDiskAttached;
  }
  if (active_) {
    state |= command_word_ & 0xFF00;
  }
  return state;
}

DiskDrivePhase DiskDrive::GetPhase() const {
  if (!active_) {
    return DiskDrivePhase::kIdle;
  }
  if (IsReceiving()) {
    return DiskDrivePhase::kArguments;
  }
  if (!outgoing_.empty()) {
    return DiskDrivePhase::kSending;
  }
  if (completion_.has_value()) {
    return DiskDrivePhase::kWorking;
  }
  return DiskDrivePhase::kIdle;
}

//------------------------------------------------------------------------------
// Protocol
//------------------------------------------------------------------------------

bool DiskDrive::IsReceiving() const {
  return !active_ ||
         (def_ != nullptr &&
          arguments_received_ < static_cast<int>(def_->arguments.size()));
}

void DiskDrive::StartCommand(uint16_t command_word) {
  active_ = true;
  command_word_ = command_word;
  def_ = GetDiskCommandDef(static_cast<uint8_t>(command_word >> 8));
  arguments_.clear();
  arguments_received_ = 0;
  if (def_ == nullptr) {
    Send(DiskStatus::kBadCommand, {}, {}, /*completion=*/false);
    return;
  }
  if (def_->arguments.empty()) {
    Accept();
  }
}

void DiskDrive::AddArgument(DiskPortWidth width,
                            const std::array<uint16_t, 2>& words) {
  arguments_.push_back(words[0]);
  if (width == DiskPortWidth::kDword) {
    arguments_.push_back(words[1]);
  }
  ++arguments_received_;
  if (arguments_received_ == static_cast<int>(def_->arguments.size())) {
    Accept();
  }
}

void DiskDrive::Accept() {
  std::vector<uint16_t> results;
  const DiskStatus status = AcceptCommand(&results);
  Send(status, def_->accept_results, results, /*completion=*/false);
  if (status != DiskStatus::kOk || def_->kind != DiskCommandKind::kLatent) {
    return;
  }

  // The work is done at once, and completes when the disk would have finished
  // it, after any work before it.
  Completion completion;
  completion.status = RunCommand(&completion.results);
  completion.cycle = ChargeBusyTime();
  completion_ = std::move(completion);
}

void DiskDrive::Send(DiskStatus status, absl::Span<const DiskPortWidth> widths,
                     absl::Span<const uint16_t> results, bool completion) {
  outgoing_.push_back(
      {.words = {static_cast<uint16_t>(status), 0}, .completion = completion});
  if (status != DiskStatus::kOk) {
    return;
  }
  int index = 0;
  for (const DiskPortWidth width : widths) {
    Field field = {.width = width};
    field.words[0] = results[index++];
    if (width == DiskPortWidth::kDword) {
      field.words[1] = results[index++];
    }
    outgoing_.push_back(field);
  }
  CHECK_EQ(index, static_cast<int>(results.size()));
}

void DiskDrive::EndCommandIfDone() {
  if (active_ && !IsReceiving() && outgoing_.empty() &&
      !completion_.has_value()) {
    EndCommand();
  }
}

void DiskDrive::EndCommand() {
  // A command's other state is only read while it is active, and starting
  // the next sets it.
  active_ = false;
  outgoing_.clear();
  front_stored_ = false;
  completion_.reset();
}

//------------------------------------------------------------------------------
// Commands
//------------------------------------------------------------------------------

DiskStatus DiskDrive::AcceptCommand(std::vector<uint16_t>* results) {
  switch (def_->command) {
    case DiskDriveCommand::kOpenRoot:
      return AcceptOpenRoot();
    case DiskDriveCommand::kClose:
      return AcceptClose();
    case DiskDriveCommand::kMedia:
      return AcceptMedia(results);
    default:
      return DiskStatus::kBadCommand;
  }
}

DiskStatus DiskDrive::RunCommand(std::vector<uint16_t>* results) {
  switch (def_->command) {
    case DiskDriveCommand::kOpenRoot:
      return RunOpenRoot(results);
    case DiskDriveCommand::kClose:
      return RunClose();
    default:
      break;
  }

  // Only latent commands run, and each one a drive accepts has its work.
  LOG(FATAL) << "Command " << static_cast<int>(def_->command) << " has no work";
  return DiskStatus::kBadCommand;
}

DiskStatus DiskDrive::AcceptOpenRoot() {
  if (DiskStatus status = file_system_.GetImageStatus();
      status != DiskStatus::kOk) {
    return status;
  }
  if (!file_system_.HasFreeSlot()) {
    return DiskStatus::kTooManyOpen;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskDrive::RunOpenRoot(std::vector<uint16_t>* results) {
  DiskFile file = 0;
  const DiskStatus status = file_system_.OpenRoot(&file);
  if (status == DiskStatus::kOk) {
    *results = {static_cast<uint16_t>(file)};
  }
  return status;
}

DiskStatus DiskDrive::AcceptClose() {
  // A stale slot, or one on a failed image, still closes, and its completion
  // says what became of its writes.
  DiskFileInfo info;
  if (file_system_.GetFileInfo(GetHandle(), &info) == DiskStatus::kBadHandle) {
    return DiskStatus::kBadHandle;
  }
  return DiskStatus::kOk;
}

DiskStatus DiskDrive::RunClose() {
  return file_system_.Close(GetHandle(), GetNow());
}

DiskStatus DiskDrive::AcceptMedia(std::vector<uint16_t>* results) {
  if (DiskStatus status = file_system_.GetImageStatus();
      status != DiskStatus::kOk) {
    return status;
  }
  const DiskMedia* media = file_system_.GetMedia();
  const uint32_t sector_count = static_cast<uint32_t>(media->GetSectorCount());
  *results = {static_cast<uint16_t>(sector_count),
              static_cast<uint16_t>(sector_count >> 16), media->GetFlags()};
  return DiskStatus::kOk;
}

}  // namespace oz3
