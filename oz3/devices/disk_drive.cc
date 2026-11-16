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

// The mode the drive moves a field with: the status cleared by a load, and set
// by a store.
constexpr uint16_t kFieldMode = Port::S;

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
// A step that moves to another state lets that state's step run in the same
// run, until a step waits for the OS or the disk.
//
// Each step first gathers the locks of the ports it uses, and the control
// port's when the control word has changed, and shows the control word for its
// state. So each step runs with its phase shown, and the OS, which can't read
// the control port while the drive holds its lock, never sees a phase run
// ahead of the ports.
//
// A run that does disk work ends by releasing its locks and advancing the
// drive's cycles by the time the disk is busy with it. The drive can't run
// again until those cycles have passed, so it does nothing in the meantime, and
// a core's wake only takes effect after them. Disk work the host started, such
// as an attach's superblock read, is charged at the start of the drive's next
// run, which then does nothing else.
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
  if (HasControlRequest() && !TakeControlRequest()) {
    AdvanceCycles(1);
    return;
  }
  bool moved = true;
  while (moved) {
    if (!LockPorts(GetLocksNeeded())) {
      AdvanceCycles(1);
      return;
    }
    UpdateControlWord();
    moved =
        std::visit([this](auto& state) { return ExecuteState(state); }, state_);
    if (ChargeBusyTime()) {
      return;
    }
  }
  ReleaseLocks();
  Sleep();
}

bool DiskDrive::LockPorts(uint8_t ports) {
  // Every lock is requested, so the drive waits for them all at once.
  bool locked = true;
  for (int port = 0; port < kPorts; ++port) {
    if ((ports & (1 << port)) == 0) {
      continue;
    }
    std::unique_ptr<Lock>& lock = locks_[port];
    if (lock == nullptr) {
      lock = LockPort(port);
    }
    if (!lock->IsLocked()) {
      locked = false;
    }
  }
  return locked;
}

const Lock& DiskDrive::GetLock(int port) const {
  const Lock* lock = locks_[port].get();
  DCHECK(lock != nullptr && lock->IsLocked());
  return *lock;
}

void DiskDrive::ReleaseLocks() {
  for (std::unique_ptr<Lock>& lock : locks_) {
    lock.reset();
  }
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

DateTime DiskDrive::GetNow() {
  return clock_ != nullptr ? DateTime::FromCivil(clock_()) : DateTime();
}

//------------------------------------------------------------------------------
// Control port
//------------------------------------------------------------------------------

bool DiskDrive::HasControlRequest() {
  // A flag only changes under the port's lock, so reading one needs no lock.
  return GetPort(kControlPort).GetStatus() != 0;
}

bool DiskDrive::TakeControlRequest() {
  if (!LockPorts(kControlLock)) {
    return false;
  }
  if ((GetPort(kControlPort).GetValue(0) & kResetRequest) != 0) {
    state_ = ResettingState();
    return true;
  }
  ClearControlRequest();
  return true;
}

void DiskDrive::ClearControlRequest() {
  uint16_t request = 0;
  GetPort(kControlPort).LoadWord(GetLock(kControlPort), Port::S, request);
  control_word_shown_ = request;
}

void DiskDrive::UpdateControlWord() {
  // The port shows a pending request's word until the drive clears it.
  Port& control = GetPort(kControlPort);
  if (IsControlWordShown() || control.GetStatus() != 0) {
    return;
  }
  const uint16_t control_word = GetControlWord();
  control.StoreWord(GetLock(kControlPort), 0, control_word);
  control_word_shown_ = control_word;
}

uint16_t DiskDrive::GetControlWord() const {
  uint16_t control_word = static_cast<uint16_t>(GetPhase());
  if (file_system_.HasImage()) {
    control_word |= kDiskAttached;
  }
  const uint16_t command_word = std::visit(
      [](const auto& state) -> uint16_t {
        if constexpr (requires { state.command; }) {
          return state.command.word;
        } else {
          return 0;
        }
      },
      state_);
  return control_word | (command_word & (0xFF << kCommandShift));
}

DiskDrivePhase DiskDrive::GetPhase() const {
  return std::visit([](const auto& state) { return state.kPhase; }, state_);
}

uint8_t DiskDrive::GetLocksNeeded() const {
  const uint8_t locks =
      std::visit([](const auto& state) { return state.kLocks; }, state_);
  return IsControlWordShown() ? locks : locks | kControlLock;
}

//------------------------------------------------------------------------------
// States
//------------------------------------------------------------------------------

bool DiskDrive::ExecuteState(IdleState&) {
  const std::optional<PortWords> field = ReadCommandPort();
  if (!field.has_value()) {
    return false;
  }
  const uint16_t word = field->GetWord(0);
  Command command = {
      .word = word,
      .def = GetDiskCommandDef(static_cast<uint8_t>(word >> kCommandShift))};

  // An unknown code fails at once, after the command word alone.
  if (command.def == nullptr || command.def->arguments == 0) {
    Accept(std::move(command));
  } else {
    state_ = ArgumentsState{.command = std::move(command)};
  }
  return true;
}

bool DiskDrive::ExecuteState(ArgumentsState& state) {
  const std::optional<PortWords> field = ReadCommandPort();
  if (!field.has_value()) {
    return false;
  }
  std::vector<PortWords>& arguments = state.command.arguments;
  arguments.push_back(*field);
  if (static_cast<int>(arguments.size()) < state.command.def->arguments) {
    return false;
  }
  Accept(std::move(state.command));
  return true;
}

bool DiskDrive::ExecuteState(SendingAcceptState& state) {
  if (!IsCommandResponseTaken(state.accept)) {
    SendNextField(state.accept);
    return false;
  }

  // A latent command's work runs once the OS has its accept.
  if (state.accept.status != DiskStatus::kOk ||
      state.command.def->kind != DiskCommandKind::kLatent) {
    state_ = IdleState();
  } else {
    state_ = RunningWorkState{.command = std::move(state.command)};
  }
  return true;
}

bool DiskDrive::ExecuteState(RunningWorkState& state) {
  CommandResponse completion = RunCommand(state.command);
  DCHECK(completion.status != DiskStatus::kOk ||
         static_cast<int>(completion.results.size()) ==
             state.command.def->completion_results);
  state_ = SendingCompletionState{.command = std::move(state.command),
                                  .completion = std::move(completion)};
  return true;
}

bool DiskDrive::ExecuteState(SendingCompletionState& state) {
  if (IsCommandResponseTaken(state.completion)) {
    state_ = IdleState();
    return true;
  }
  if (SendNextField(state.completion) && state.completion.fields_sent == 1) {
    RaiseInterrupt(0);
  }
  return false;
}

bool DiskDrive::ExecuteState(ResettingState&) {
  // A reset discards what the state it replaced held.
  uint16_t word = 0;
  GetPort(kCommandPort).LoadWord(GetLock(kCommandPort), Port::S, word);
  GetPort(kDataPort).LoadWord(GetLock(kDataPort), Port::S, word);
  ClearControlRequest();
  state_ = IdleState();
  return true;
}

//------------------------------------------------------------------------------
// Protocol
//------------------------------------------------------------------------------

std::optional<PortWords> DiskDrive::ReadCommandPort() {
  Port& port = GetPort(kCommandPort);
  if (port.GetStatus() == 0) {
    return std::nullopt;
  }
  PortWords field;
  field.Load(port, GetLock(kCommandPort), kFieldMode);
  return field;
}

void DiskDrive::Accept(Command command) {
  CommandResponse accept = AcceptCommand(command);
  DCHECK(accept.status != DiskStatus::kOk ||
         static_cast<int>(accept.results.size()) ==
             (command.def != nullptr ? command.def->accept_results : 0));
  state_ = SendingAcceptState{.command = std::move(command),
                              .accept = std::move(accept)};
}

bool DiskDrive::IsCommandResponseTaken(const CommandResponse& response) {
  return response.fields_sent ==
             1 + static_cast<int>(response.results.size()) &&
         GetPort(kCommandPort).GetStatus() == 0;
}

bool DiskDrive::SendNextField(CommandResponse& response) {
  Port& port = GetPort(kCommandPort);
  const int field_count = 1 + static_cast<int>(response.results.size());
  if (response.fields_sent == field_count ||
      (response.fields_sent > 0 && port.GetStatus() != 0)) {
    return false;
  }
  const PortWords field =
      response.fields_sent == 0
          ? PortWords::FromWord(static_cast<uint16_t>(response.status))
          : response.results[response.fields_sent - 1];
  field.Store(port, GetLock(kCommandPort), kFieldMode);
  ++response.fields_sent;
  return true;
}

//------------------------------------------------------------------------------
// Commands
//------------------------------------------------------------------------------

DiskDrive::CommandResponse DiskDrive::AcceptCommand(const Command& command) {
  if (command.def == nullptr) {
    return {.status = DiskStatus::kBadCommand};
  }
  switch (command.def->command) {
    case DiskDriveCommand::kOpenRoot:
      return AcceptOpenRoot();
    case DiskDriveCommand::kClose:
      return AcceptClose(command);
    case DiskDriveCommand::kMedia:
      return AcceptMedia();
    default:
      return {.status = DiskStatus::kBadCommand};
  }
}

DiskDrive::CommandResponse DiskDrive::RunCommand(const Command& command) {
  switch (command.def->command) {
    case DiskDriveCommand::kOpenRoot:
      return RunOpenRoot();
    case DiskDriveCommand::kClose:
      return RunClose(command);
    default:
      break;
  }

  // Only latent commands run, and each one a drive accepts has its work.
  LOG(FATAL) << "Command " << static_cast<int>(command.def->command)
             << " has no work";
  return {.status = DiskStatus::kBadCommand};
}

DiskDrive::CommandResponse DiskDrive::AcceptOpenRoot() {
  if (DiskStatus status = file_system_.GetImageStatus();
      status != DiskStatus::kOk) {
    return {.status = status};
  }
  if (!file_system_.HasFreeSlot()) {
    return {.status = DiskStatus::kTooManyOpen};
  }
  return {};
}

DiskDrive::CommandResponse DiskDrive::RunOpenRoot() {
  DiskFile file = 0;
  if (DiskStatus status = file_system_.OpenRoot(&file);
      status != DiskStatus::kOk) {
    return {.status = status};
  }
  return {.results = {PortWords::FromWord(static_cast<uint16_t>(file))}};
}

DiskDrive::CommandResponse DiskDrive::AcceptClose(const Command& command) {
  // A stale slot, or one on a failed image, still closes, and its completion
  // says what became of its writes.
  DiskFileInfo info;
  if (file_system_.GetFileInfo(command.GetHandle(), &info) ==
      DiskStatus::kBadHandle) {
    return {.status = DiskStatus::kBadHandle};
  }
  return {};
}

DiskDrive::CommandResponse DiskDrive::RunClose(const Command& command) {
  return {.status = file_system_.Close(command.GetHandle(), GetNow())};
}

DiskDrive::CommandResponse DiskDrive::AcceptMedia() {
  if (DiskStatus status = file_system_.GetImageStatus();
      status != DiskStatus::kOk) {
    return {.status = status};
  }
  const DiskMedia* media = file_system_.GetMedia();
  return {.results = {PortWords::FromDword(
                          static_cast<uint32_t>(media->GetSectorCount())),
                      PortWords::FromWord(media->GetFlags())}};
}

}  // namespace oz3
