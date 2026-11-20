// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/power_control.h"

#include <string>
#include <utility>

#include "absl/memory/memory.h"
#include "oz3/core/port.h"

namespace oz3 {

//==============================================================================
// PowerControlFactory
//==============================================================================

PowerControlFactory::PowerControlFactory(std::string_view name,
                                         const PowerControlSettings& settings)
    : config_({.name = std::string(name), .ports = {settings.port}}) {}

std::unique_ptr<Device> PowerControlFactory::CreateDevice(
    gb::ValidatedContext& context) const {
  PowerControl::Contract contract(context);
  if (!contract.IsValid()) {
    return nullptr;
  }
  return absl::WrapUnique(new PowerControl(config_, std::move(contract)));
}

//==============================================================================
// PowerControl
//==============================================================================

PowerControl::PowerControl(DeviceConfig config, Contract contract)
    : Device(std::move(config)), context_(std::move(contract)) {}

void PowerControl::Execute() {
  if (lock_ == nullptr) {
    lock_ = LockPort(0);
  }
  if (!lock_->IsLocked()) {
    AdvanceCycles(1);
    return;
  }
  Port& port = GetPort(0);
  PortWords words;
  if (words.Load(port, *lock_, Port::T | Port::S) != 0) {
    // Both words are cleared once taken, so a command written later as a word
    // (which leaves word 1 as it is) doesn't take this exit code.
    PortWords().Store(port, *lock_, 0);
    Record(words.GetWord(0), words.GetWord(1));
  }
  lock_.reset();
  Sleep();
}

void PowerControl::Record(uint16_t command, uint16_t exit_code) {
  if (command != static_cast<uint16_t>(PowerCommand::kShutDown) &&
      command != static_cast<uint16_t>(PowerCommand::kRestart)) {
    return;
  }
  if (context_.Exists<PowerRequest>()) {
    return;
  }
  const PowerCommand power_command = static_cast<PowerCommand>(command);
  context_.SetValue<PowerRequest>(PowerRequest{
      .command = power_command,
      .exit_code = power_command == PowerCommand::kShutDown
                       ? static_cast<int16_t>(exit_code)
                       : 0,
  });
}

}  // namespace oz3
