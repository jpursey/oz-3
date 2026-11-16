// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/disk_drive.h"

#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "absl/log/log.h"
#include "absl/memory/memory.h"
#include "absl/numeric/int128.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "oz3/disk/disk_media.h"

namespace oz3 {

namespace {

// The drive's ports, as numbered in its DeviceConfig.
constexpr int kCommandPort = 0;
constexpr int kDataPort = 1;
constexpr int kControlPort = 2;

// The mode the drive moves a field with: both words, ending at word 0 again,
// with the status cleared by a load and set by a store.
constexpr uint16_t kFieldMode = Port::S | Port::A;

// Returns the cycles `busy` takes at `clock_hz`: the busy time rounded up to
// whole nanoseconds, and then to whole cycles.
Cycles GetBusyCycles(absl::Duration busy, int64_t clock_hz) {
  constexpr int64_t kNanosPerSec = 1'000'000'000;
  const absl::int128 nanos =
      absl::ToInt64Nanoseconds(absl::Ceil(busy, absl::Nanoseconds(1)));
  return static_cast<Cycles>((nanos * clock_hz + kNanosPerSec - 1) /
                             kNanosPerSec);
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
  if (hardware.latency > DiskDriveSettings::kMaxLatency) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Disk drive ", name, " has a latency of ", hardware.latency,
        ", but it must be at most ", DiskDriveSettings::kMaxLatency));
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
//
// The drive is a state machine (State), and each run does what its state is
// waiting for, if it is there, after taking any request off the control port.
// Each state shows a phase on the control port, which the drive updates after
// the port changes it describes, so the phase can lag the ports, but never run
// ahead of them.
//
// A run that does disk work ends by advancing the drive's cycles by the time
// the disk is busy with it. The drive can't run again until those cycles have
// passed, so it does nothing in the meantime, and a core's wake only takes
// effect after them. Disk work the host started, such as an attach's superblock
// read, is charged at the start of the drive's next run, which then does
// nothing else.
//
// Otherwise, the drive runs the cycle after a core reads or writes one of its
// ports, which wakes it, and sleeps between them.
//==============================================================================

DiskDrive::DiskDrive(DeviceConfig config, const DiskDriveSettings& settings)
    : Device(std::move(config)),
      file_system_(settings.hardware, settings.slots) {}

DiskDrive::~DiskDrive() {
  // There is nothing to tell of a failure, which leaves the disk as a crash
  // would.
  file_system_.Flush(GetNow());
}

void DiskDrive::AttachImage(std::unique_ptr<DiskImage> image) {
  CHECK(image != nullptr);
  DetachImage();

  // A program finds out what the disk holds from its commands.
  file_system_.AttachImage(std::move(image));
  Wake();
}

std::unique_ptr<DiskImage> DiskDrive::DetachImage() {
  std::unique_ptr<DiskImage> image = file_system_.DetachImage();
  if (image != nullptr) {
    Wake();
  }
  return image;
}

void DiskDrive::Execute() {
  if (ChargeBusyTime()) {
    return;
  }

  waiting_ = false;
  if (ExecuteControlPort()) {
    switch (state_) {
      case State::kIdle:
        ExecuteIdle();
        break;
      case State::kArguments:
        ExecuteArguments();
        break;
      case State::kSendingAccept:
      case State::kSendingCompletion:
        ExecuteSending();
        break;
      case State::kWorking:
        ExecuteWorking();
        break;
    }
    UpdateState();
  }

  if (ChargeBusyTime()) {
    return;
  }

  // Locks are kept while the drive waits for one, so it gathers them all.
  if (waiting_) {
    AdvanceCycles(1);
    return;
  }
  ReleaseLocks();

  // A completion goes on the command port the run after its work, even work
  // that took no time.
  if (state_ == State::kWorking) {
    AdvanceCycles(1);
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

void DiskDrive::ReleaseLocks() {
  for (std::unique_ptr<Lock>& lock : locks_) {
    lock.reset();
  }
}

DateTime DiskDrive::GetNow() {
  return clock_ != nullptr ? DateTime::FromCivil(clock_()) : DateTime();
}

bool DiskDrive::ChargeBusyTime() {
  const absl::Duration busy = file_system_.CollectBusyTime();
  if (busy <= absl::ZeroDuration()) {
    return false;
  }
  ReleaseLocks();
  AdvanceCycles(GetBusyCycles(busy, GetClockHz()));
  return true;
}

//------------------------------------------------------------------------------
// Control port
//------------------------------------------------------------------------------

bool DiskDrive::ExecuteControlPort() {
  // A flag only changes under the port's lock, so a clear one needs no lock.
  Port& control = GetPort(kControlPort);
  if (control.GetStatus() == 0) {
    return true;
  }

  // Only a reset needs the other ports, whose locks are requested together, so
  // the drive waits for them all at once.
  Lock* control_lock = GetLock(kControlPort);
  if (control_lock == nullptr) {
    return false;
  }
  if ((control.GetValue(0) & kResetRequest) != 0) {
    Lock* command_lock = GetLock(kCommandPort);
    Lock* data_lock = GetLock(kDataPort);
    if (command_lock == nullptr || data_lock == nullptr) {
      return false;
    }
    Reset(*command_lock, *data_lock);
  }
  uint16_t request = 0;
  control.LoadWord(*control_lock, Port::S, request);

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

void DiskDrive::UpdateState() {
  const uint16_t state = GetStateWord();
  if (state == state_shown_) {
    return;
  }
  Lock* lock = GetLock(kControlPort);
  if (lock == nullptr) {
    return;
  }

  // No request is waiting, as ExecuteControlPort took any there was this run.
  GetPort(kControlPort).StoreWord(*lock, 0, state);
  state_shown_ = state;
}

uint16_t DiskDrive::GetStateWord() const {
  uint16_t state = static_cast<uint16_t>(GetPhase());
  if (file_system_.HasImage()) {
    state |= kDiskAttached;
  }
  if (state_ != State::kIdle) {
    state |= command_word_ & 0xFF00;
  }
  return state;
}

DiskDrivePhase DiskDrive::GetPhase() const {
  switch (state_) {
    case State::kIdle:
      return DiskDrivePhase::kIdle;
    case State::kArguments:
      return DiskDrivePhase::kArguments;
    case State::kSendingAccept:
    case State::kSendingCompletion:
      return DiskDrivePhase::kSending;
    case State::kWorking:
      return DiskDrivePhase::kWorking;
  }
  return DiskDrivePhase::kIdle;
}

//------------------------------------------------------------------------------
// States
//------------------------------------------------------------------------------

void DiskDrive::ExecuteIdle() {
  const std::optional<PortWords> field = TakeField();
  if (!field.has_value()) {
    return;
  }
  command_word_ = field->GetWord(0);
  def_ = GetDiskCommandDef(static_cast<uint8_t>(command_word_ >> 8));
  arguments_.clear();
  if (def_ == nullptr) {
    accept_status_ = DiskStatus::kBadCommand;
    SetFields(accept_status_, {});
    state_ = State::kSendingAccept;
    ExecuteSending();
    return;
  }
  if (def_->arguments == 0) {
    Accept();
    return;
  }
  state_ = State::kArguments;
}

void DiskDrive::ExecuteArguments() {
  const std::optional<PortWords> field = TakeField();
  if (!field.has_value()) {
    return;
  }
  arguments_.push_back(*field);
  if (static_cast<int>(arguments_.size()) == def_->arguments) {
    Accept();
  }
}

void DiskDrive::ExecuteSending() {
  // A field waits until the OS takes it, which a clear flag shows without the
  // lock. The next goes in under the lock the drive gets once it has.
  Port& port = GetPort(kCommandPort);
  if (fields_sent_ > 0 && port.GetStatus() != 0) {
    return;
  }
  if (fields_sent_ == static_cast<int>(fields_.size())) {
    FinishSending();
    return;
  }
  Lock* lock = GetLock(kCommandPort);
  if (lock == nullptr) {
    return;
  }

  // A step's first field goes over any word the OS wrote while the drive
  // wasn't taking words, which the drive never read.
  fields_[fields_sent_++].Store(port, *lock, kFieldMode);
  if (fields_sent_ == 1 && state_ == State::kSendingCompletion) {
    RaiseInterrupt(0);
  }
}

void DiskDrive::ExecuteWorking() {
  state_ = State::kSendingCompletion;
  ExecuteSending();
}

//------------------------------------------------------------------------------
// Protocol
//------------------------------------------------------------------------------

std::optional<PortWords> DiskDrive::TakeField() {
  // A flag only changes under the port's lock, so a clear one needs no lock.
  Port& port = GetPort(kCommandPort);
  if (port.GetStatus() == 0) {
    return std::nullopt;
  }
  Lock* lock = GetLock(kCommandPort);
  if (lock == nullptr) {
    return std::nullopt;
  }
  PortWords field;
  field.Load(port, *lock, kFieldMode);
  return field;
}

void DiskDrive::Accept() {
  std::vector<PortWords> results;
  accept_status_ = AcceptCommand(&results);
  DCHECK(accept_status_ != DiskStatus::kOk ||
         static_cast<int>(results.size()) == def_->accept_results);
  SetFields(accept_status_, std::move(results));
  state_ = State::kSendingAccept;
  ExecuteSending();
}

void DiskDrive::SetFields(DiskStatus status, std::vector<PortWords> results) {
  fields_ = {PortWords(static_cast<uint16_t>(status))};
  if (status == DiskStatus::kOk) {
    fields_.insert(fields_.end(), results.begin(), results.end());
  }
  fields_sent_ = 0;
}

void DiskDrive::FinishSending() {
  if (state_ != State::kSendingAccept || accept_status_ != DiskStatus::kOk ||
      def_->kind != DiskCommandKind::kLatent) {
    EndCommand();
    return;
  }

  // A latent command's work runs once the OS has its accept, and the disk is
  // busy with it from the end of this run.
  std::vector<PortWords> results;
  const DiskStatus status = RunCommand(&results);
  DCHECK(status != DiskStatus::kOk ||
         static_cast<int>(results.size()) == def_->completion_results);
  SetFields(status, std::move(results));
  state_ = State::kWorking;
}

void DiskDrive::EndCommand() {
  // A command's other state is only read outside kIdle, and starting the next
  // sets it.
  state_ = State::kIdle;
  fields_.clear();
  fields_sent_ = 0;
}

//------------------------------------------------------------------------------
// Commands
//------------------------------------------------------------------------------

DiskStatus DiskDrive::AcceptCommand(std::vector<PortWords>* results) {
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

DiskStatus DiskDrive::RunCommand(std::vector<PortWords>* results) {
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

DiskStatus DiskDrive::RunOpenRoot(std::vector<PortWords>* results) {
  DiskFile file = 0;
  const DiskStatus status = file_system_.OpenRoot(&file);
  if (status == DiskStatus::kOk) {
    *results = {PortWords(static_cast<uint16_t>(file))};
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

DiskStatus DiskDrive::AcceptMedia(std::vector<PortWords>* results) {
  if (DiskStatus status = file_system_.GetImageStatus();
      status != DiskStatus::kOk) {
    return status;
  }
  const DiskMedia* media = file_system_.GetMedia();
  *results = {
      PortWords::FromDword(static_cast<uint32_t>(media->GetSectorCount())),
      PortWords(media->GetFlags())};
  return DiskStatus::kOk;
}

}  // namespace oz3
