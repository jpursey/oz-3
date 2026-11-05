// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/keyboard.h"

#include <string>
#include <utility>

#include "absl/memory/memory.h"
#include "oz3/core/port.h"

namespace oz3 {

//==============================================================================
// KeyboardFactory
//==============================================================================

KeyboardFactory::KeyboardFactory(std::string_view name,
                                 const KeyboardSettings& settings)
    : config_({.name = std::string(name),
               .ports = {settings.port},
               .interrupts = {settings.interrupt}}) {}

std::unique_ptr<Device> KeyboardFactory::CreateDevice() const {
  return absl::WrapUnique(new Keyboard(config_));
}

//==============================================================================
// Keyboard
//==============================================================================

Keyboard::Keyboard(DeviceConfig config)
    : Device(std::move(config)), keys_(kQueueSize, 0) {}

bool Keyboard::PressKey(uint16_t key) {
  if (static_cast<int>(keys_.size()) == kQueueSize) {
    return false;
  }
  keys_.push(key);
  Wake();
  return true;
}

void Keyboard::Execute() {
  if (keys_.empty()) {
    Sleep();
    return;
  }
  if (lock_ == nullptr) {
    lock_ = LockPort(0);
  }
  if (!lock_->IsLocked()) {
    AdvanceCycles(1);
    return;
  }

  // The key event goes in only if the status is clear, so one not yet read
  // stays until a core reads it.
  const bool stored =
      GetPort(0).StoreWord(*lock_, Port::T | Port::S, keys_.front()) == 0;
  lock_.reset();
  if (stored) {
    keys_.pop();
    RaiseInterrupt(0);
  }
  Sleep();
}

}  // namespace oz3
