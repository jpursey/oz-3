// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/disk_drive.h"

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

// The fields of a name argument, each holding two of its words.
constexpr int kNameFields = DiskFilename::kWordCount / 2;

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
// Before each step, the drive shows its state's control word, locking the
// control port if the word has changed. A step locks the ports it uses once
// their flags show it has something to do, and a run that waits for a lock
// keeps the ones it holds. So each step runs with its phase shown, and the OS,
// which can't read the control port while the drive holds its lock, never sees
// a phase run ahead of the ports.
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
  if (HandleAnyControlRequest() == StateResult::kContinue) {
    while (UpdateControlWord() == StateResult::kContinue) {
      const StateResult result = std::visit(
          [this](auto& state) { return ExecuteState(state); }, state_);
      if (ChargeBusyTime()) {
        return;
      }
      if (result == StateResult::kWait) {
        break;
      }
    }
  }

  // A run that waits takes a cycle. While a lock it asked for isn't granted,
  // it keeps its locks and runs again the next cycle. Otherwise it waits for
  // the OS, and sleeps until a port access wakes it.
  AdvanceCycles(1);
  if (IsWaitingForLock()) {
    return;
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

bool DiskDrive::IsWaitingForLock() const {
  for (const std::unique_ptr<Lock>& lock : locks_) {
    if (lock != nullptr && !lock->IsLocked()) {
      return true;
    }
  }
  return false;
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

DiskDrive::StateResult DiskDrive::HandleAnyControlRequest() {
  if (!HasControlRequest()) {
    return StateResult::kContinue;
  }
  if (!LockPorts(kControlLock)) {
    return StateResult::kWait;
  }
  if ((GetPort(kControlPort).GetValue(0) & kResetRequest) != 0) {
    if (!LockPorts(kCommandLock | kDataLock)) {
      return StateResult::kWait;
    }

    // A reset discards what the state held.
    uint16_t word = 0;
    GetPort(kCommandPort).LoadWord(GetLock(kCommandPort), Port::S, word);
    GetPort(kDataPort).LoadWord(GetLock(kDataPort), Port::S, word);
    state_ = IdleState();
  }
  ClearControlRequest();
  return StateResult::kContinue;
}

void DiskDrive::ClearControlRequest() {
  Port& control = GetPort(kControlPort);
  const Lock& lock = GetLock(kControlPort);
  uint16_t request = 0;
  control.LoadWord(lock, Port::S, request);
  control_word_shown_ = GetControlWord();
  control.StoreWord(lock, 0, control_word_shown_);
}

DiskDrive::StateResult DiskDrive::UpdateControlWord() {
  // A request's word would be lost, but a run takes any request before this.
  DCHECK(!HasControlRequest());
  const uint16_t control_word = GetControlWord();
  if (control_word == control_word_shown_) {
    return StateResult::kContinue;
  }
  if (!LockPorts(kControlLock)) {
    return StateResult::kWait;
  }
  GetPort(kControlPort).StoreWord(GetLock(kControlPort), 0, control_word);
  control_word_shown_ = control_word;
  return StateResult::kContinue;
}

uint16_t DiskDrive::GetControlWord() const {
  const DiskDrivePhase phase =
      std::visit([](const auto& state) { return state.GetPhase(); }, state_);
  const uint16_t command_word = std::visit(
      [](const auto& state) { return state.GetCommandWord(); }, state_);
  uint16_t control_word =
      static_cast<uint16_t>(phase) | (command_word & (0xFF << kCommandShift));
  if (file_system_.HasImage()) {
    control_word |= kDiskAttached;
  }
  return control_word;
}

//------------------------------------------------------------------------------
// States
//------------------------------------------------------------------------------

DiskDrive::StateResult DiskDrive::ExecuteState(IdleState&) {
  PortWords field;
  if (ReadCommandPort(field) == StateResult::kWait) {
    return StateResult::kWait;
  }
  const uint16_t word = field.GetWord(0);
  Command command = {
      .word = word,
      .def = GetDiskCommandDef(static_cast<uint8_t>(word >> kCommandShift))};

  // A command with no arguments is accepted now, as is an unknown code, which
  // fails after the command word alone.
  if (command.def == nullptr || command.def->arguments == 0) {
    state_ = Accept(std::move(command));
  } else {
    command.arguments.reserve(command.def->arguments);
    state_ = ArgumentsState{.command = std::move(command)};
  }
  return StateResult::kContinue;
}

DiskDrive::StateResult DiskDrive::ExecuteState(ArgumentsState& state) {
  PortWords field;
  if (ReadCommandPort(field) == StateResult::kWait) {
    return StateResult::kWait;
  }
  std::vector<PortWords>& arguments = state.command.arguments;
  arguments.push_back(field);
  if (static_cast<int>(arguments.size()) < state.command.def->arguments) {
    return StateResult::kWait;
  }
  state_ = Accept(std::move(state.command));
  return StateResult::kContinue;
}

DiskDrive::StateResult DiskDrive::ExecuteState(SendingAcceptState& state) {
  if (!IsCommandResponseTaken(state.accept)) {
    return SendNextField(state.accept);
  }

  // A latent command's work runs once the OS has its accept.
  if (state.accept.status != DiskStatus::kOk ||
      state.command.def->kind != DiskCommandKind::kLatent) {
    state_ = IdleState();
  } else {
    state_ = RunningWorkState{.command = std::move(state.command)};
  }
  return StateResult::kContinue;
}

DiskDrive::StateResult DiskDrive::ExecuteState(RunningWorkState& state) {
  CommandResponse completion = RunCommand(state.command);
  ValidateResponse(completion, state.command.def->completion_results);
  state_ = SendingCompletionState{.command = std::move(state.command),
                                  .completion = std::move(completion)};
  return StateResult::kContinue;
}

DiskDrive::StateResult DiskDrive::ExecuteState(SendingCompletionState& state) {
  CommandResponse& completion = state.completion;
  if (IsCommandResponseTaken(completion)) {
    state_ = IdleState();
    return StateResult::kContinue;
  }

  // The interrupt goes out with the status, and phase 3.
  const StateResult result = SendNextField(completion);
  if (result == StateResult::kContinue && completion.fields_sent == 1) {
    RaiseInterrupt(0);
  }
  return result;
}

//------------------------------------------------------------------------------
// Protocol
//------------------------------------------------------------------------------

DiskDrive::StateResult DiskDrive::ReadCommandPort(PortWords& field) {
  // The flag stays set until the drive loads the field.
  Port& port = GetPort(kCommandPort);
  if (port.GetStatus() == 0 || !LockPorts(kCommandLock)) {
    return StateResult::kWait;
  }
  field.Load(port, GetLock(kCommandPort), kFieldMode);
  return StateResult::kContinue;
}

DiskDrive::SendingAcceptState DiskDrive::Accept(Command command) {
  CommandResponse accept = AcceptCommand(command);
  ValidateResponse(accept,
                   command.def != nullptr ? command.def->accept_results : 0);
  return {.command = std::move(command), .accept = std::move(accept)};
}

void DiskDrive::ValidateResponse(const CommandResponse& response,
                                 int result_count) {
  DCHECK_EQ(static_cast<int>(response.results.size()),
            response.status == DiskStatus::kOk ? result_count : 0);
}

bool DiskDrive::IsCommandResponseTaken(const CommandResponse& response) {
  return response.fields_sent == response.GetFieldCount() &&
         GetPort(kCommandPort).GetStatus() == 0;
}

DiskDrive::StateResult DiskDrive::SendNextField(CommandResponse& response) {
  Port& port = GetPort(kCommandPort);
  if (response.fields_sent == response.GetFieldCount() ||
      (response.fields_sent > 0 && port.GetStatus() != 0)) {
    return StateResult::kWait;
  }
  const bool first = response.fields_sent == 0;
  if (!LockPorts(first ? kCommandLock | kControlLock : kCommandLock)) {
    return StateResult::kWait;
  }
  const PortWords field =
      first ? PortWords::FromWord(static_cast<uint16_t>(response.status))
            : response.results[response.fields_sent - 1];
  field.Store(port, GetLock(kCommandPort), kFieldMode);
  ++response.fields_sent;
  if (first) {
    UpdateControlWord();
  }
  return StateResult::kContinue;
}

//------------------------------------------------------------------------------
// Commands
//------------------------------------------------------------------------------

DiskDrive::CommandResponse DiskDrive::AcceptCommand(const Command& command) {
  if (command.def == nullptr) {
    return {.status = DiskStatus::kBadCommand};
  }
  switch (command.def->command) {
    case DiskDriveCommand::kOpen:
      return AcceptOpen(command);
    case DiskDriveCommand::kOpenRoot:
      return AcceptOpenRoot();
    case DiskDriveCommand::kCreate:
      return AcceptCreate(command);
    case DiskDriveCommand::kClose:
      return AcceptClose(command);
    case DiskDriveCommand::kFlush:
      return AcceptFlush(command);
    case DiskDriveCommand::kSeek:
      return AcceptSeek(command);
    case DiskDriveCommand::kInfo:
      return AcceptInfo(command);
    case DiskDriveCommand::kMedia:
      return AcceptMedia();
    default:
      return {.status = DiskStatus::kBadCommand};
  }
}

DiskDrive::CommandResponse DiskDrive::RunCommand(const Command& command) {
  switch (command.def->command) {
    case DiskDriveCommand::kOpen:
      return RunOpen(command);
    case DiskDriveCommand::kOpenRoot:
      return RunOpenRoot();
    case DiskDriveCommand::kCreate:
      return RunCreate(command);
    case DiskDriveCommand::kClose:
      return RunClose(command);
    case DiskDriveCommand::kFlush:
      return RunFlush(command);
    default:
      break;
  }

  // Only latent commands run, and each one a drive accepts has its work.
  LOG(FATAL) << "Command " << static_cast<int>(command.def->command)
             << " has no work";
  return {.status = DiskStatus::kBadCommand};
}

std::optional<DiskFilename> DiskDrive::GetNameArgument(const Command& command,
                                                       int first) {
  DiskFilename::Words words;
  for (int i = 0; i < kNameFields; ++i) {
    const PortWords& field = command.arguments[first + i];
    words[i * 2] = field.GetWord(0);
    words[i * 2 + 1] = field.GetWord(1);
  }
  return DiskFilename::FromWords(words);
}

DiskStatus DiskDrive::GetNewSlotStatus(bool writes) const {
  if (DiskStatus status = file_system_.GetImageStatus();
      status != DiskStatus::kOk) {
    return status;
  }
  if (writes && file_system_.GetMedia()->IsWriteProtected()) {
    return DiskStatus::kWriteProtected;
  }
  if (!file_system_.HasFreeSlot()) {
    return DiskStatus::kTooManyOpen;
  }
  return DiskStatus::kOk;
}

DiskDrive::CommandResponse DiskDrive::MakeOpenResponse(DiskStatus status,
                                                       DiskFile file) {
  if (status != DiskStatus::kOk) {
    return {.status = status};
  }
  return {.status = DiskStatus::kOk,
          .results = {PortWords::FromWord(static_cast<uint16_t>(file))}};
}

DiskDrive::CommandResponse DiskDrive::AcceptOpen(const Command& command) {
  if (!GetNameArgument(command).has_value()) {
    return {.status = DiskStatus::kBadName};
  }
  return {.status = GetNewSlotStatus(/*writes=*/false)};
}

DiskDrive::CommandResponse DiskDrive::RunOpen(const Command& command) {
  // The accept checked the name.
  const std::optional<DiskFilename> name = GetNameArgument(command);
  DCHECK(name.has_value());
  DiskFile file = 0;
  const DiskStatus status = file_system_.Open(*name, &file);
  return MakeOpenResponse(status, file);
}

DiskDrive::CommandResponse DiskDrive::AcceptOpenRoot() {
  return {.status = GetNewSlotStatus(/*writes=*/false)};
}

DiskDrive::CommandResponse DiskDrive::RunOpenRoot() {
  DiskFile file = 0;
  const DiskStatus status = file_system_.OpenRoot(&file);
  return MakeOpenResponse(status, file);
}

DiskDrive::CommandResponse DiskDrive::AcceptCreate(const Command& command) {
  if (!GetNameArgument(command).has_value()) {
    return {.status = DiskStatus::kBadName};
  }
  return {.status = GetNewSlotStatus(/*writes=*/true)};
}

DiskDrive::CommandResponse DiskDrive::RunCreate(const Command& command) {
  // The accept checked the name.
  const std::optional<DiskFilename> name = GetNameArgument(command);
  DCHECK(name.has_value());
  DiskFile file = 0;
  const DiskStatus status = file_system_.Create(*name, GetNow(), &file);
  return MakeOpenResponse(status, file);
}

DiskDrive::CommandResponse DiskDrive::AcceptClose(const Command& command) {
  if (command.GetHandle() == kAllSlots) {
    return {.status = DiskStatus::kOk};
  }

  // A stale slot, or one on a failed image, still closes, and its completion
  // says what became of its writes.
  DiskFileInfo info;
  if (file_system_.GetFileInfo(command.GetHandle(), &info) ==
      DiskStatus::kBadHandle) {
    return {.status = DiskStatus::kBadHandle};
  }
  return {.status = DiskStatus::kOk};
}

DiskDrive::CommandResponse DiskDrive::RunClose(const Command& command) {
  const DateTime now = GetNow();
  if (command.GetHandle() != kAllSlots) {
    return {.status = file_system_.Close(command.GetHandle(), now)};
  }

  // Every slot is closed, even after a failure, and a free slot is a bad
  // handle, which closes nothing.
  DiskStatus result = DiskStatus::kOk;
  for (DiskFile file = 0; file < file_system_.GetMaxOpenFiles(); ++file) {
    const DiskStatus status = file_system_.Close(file, now);
    if (result == DiskStatus::kOk && status != DiskStatus::kBadHandle) {
      result = status;
    }
  }
  return {.status = result};
}

DiskDrive::CommandResponse DiskDrive::AcceptFlush(const Command& command) {
  // A directory's handle flushes too, writing nothing.
  DiskFileInfo info;
  return {.status = file_system_.GetFileInfo(command.GetHandle(), &info)};
}

DiskDrive::CommandResponse DiskDrive::RunFlush(const Command& command) {
  return {.status = file_system_.Flush(command.GetHandle(), GetNow())};
}

DiskDrive::CommandResponse DiskDrive::AcceptSeek(const Command& command) {
  DiskFileInfo info;
  if (DiskStatus status = file_system_.GetFileInfo(command.GetHandle(), &info);
      status != DiskStatus::kOk) {
    return {.status = status};
  }

  // A size fits in an int, so a position that doesn't is past it.
  const uint32_t position = command.arguments[0].GetDword();
  if (position > static_cast<uint32_t>(info.size)) {
    return {.status = DiskStatus::kPastEnd};
  }
  return {.status = file_system_.Seek(command.GetHandle(),
                                      static_cast<int>(position))};
}

DiskDrive::CommandResponse DiskDrive::AcceptInfo(const Command& command) {
  DiskFileInfo info;
  if (DiskStatus status = file_system_.GetFileInfo(command.GetHandle(), &info);
      status != DiskStatus::kOk) {
    return {.status = status};
  }
  return {
      .status = DiskStatus::kOk,
      .results = {PortWords::FromDword(static_cast<uint32_t>(info.position)),
                  PortWords::FromDword(static_cast<uint32_t>(info.size))}};
}

DiskDrive::CommandResponse DiskDrive::AcceptMedia() {
  if (DiskStatus status = file_system_.GetImageStatus();
      status != DiskStatus::kOk) {
    return {.status = status};
  }
  const DiskMedia* media = file_system_.GetMedia();
  return {.status = DiskStatus::kOk,
          .results = {PortWords::FromDword(
                          static_cast<uint32_t>(media->GetSectorCount())),
                      PortWords::FromWord(media->GetFlags())}};
}

}  // namespace oz3
