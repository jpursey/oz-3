// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/keyboard.h"

#include <string>
#include <utility>

#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "oz3/core/port.h"

namespace oz3 {

namespace {

// Returns the wiring of a keyboard named `name` with `settings`: the typed
// keys' port first, and then the key state's ports.
DeviceConfig GetKeyboardConfig(std::string_view name,
                               const KeyboardSettings& settings) {
  DeviceConfig config = {.name = std::string(name)};
  if (settings.typed_port.has_value()) {
    config.ports.push_back(*settings.typed_port);
    config.interrupts.push_back(settings.typed_interrupt);
  }
  if (settings.state_port.has_value()) {
    for (int i = 0; i < Keyboard::kKeyStatePorts; ++i) {
      config.ports.push_back(*settings.state_port + i);
    }
  }
  return config;
}

}  // namespace

//==============================================================================
// KeyboardFactory
//==============================================================================

absl::StatusOr<std::unique_ptr<KeyboardFactory>> KeyboardFactory::Create(
    std::string_view name, const KeyboardSettings& settings) {
  if (!settings.typed_port.has_value() && !settings.state_port.has_value()) {
    return absl::InvalidArgumentError(
        absl::StrCat("Keyboard ", name,
                     " has no port: it must have typed keys, key state, "
                     "or both"));
  }
  if (settings.typed_port.has_value() && settings.state_port.has_value() &&
      *settings.typed_port >= *settings.state_port &&
      *settings.typed_port < *settings.state_port + Keyboard::kKeyStatePorts) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Keyboard ", name, " has its typed keys on port ", *settings.typed_port,
        ", but that is one of its key state ports (", *settings.state_port,
        " to ", *settings.state_port + Keyboard::kKeyStatePorts - 1, ")"));
  }
  return absl::WrapUnique(new KeyboardFactory(name, settings));
}

KeyboardFactory::KeyboardFactory(std::string_view name,
                                 const KeyboardSettings& settings)
    : config_(GetKeyboardConfig(name, settings)), settings_(settings) {}

std::unique_ptr<Device> KeyboardFactory::CreateDevice() const {
  return absl::WrapUnique(new Keyboard(config_, settings_));
}

//==============================================================================
// Keyboard
//==============================================================================

Keyboard::Keyboard(DeviceConfig config, const KeyboardSettings& settings)
    : Device(std::move(config)),
      typed_keys_(settings.typed_port.has_value()),
      key_state_port_(!settings.state_port.has_value() ? -1
                      : typed_keys_                    ? 1
                                                       : 0),
      keys_(kQueueSize, 0) {}

bool Keyboard::TypeKey(uint16_t key) {
  if (!typed_keys_ || static_cast<int>(keys_.size()) == kQueueSize) {
    return false;
  }
  keys_.push(key);
  Wake();
  return true;
}

void Keyboard::SetKeyState(int key, bool held) {
  if (key_state_port_ < 0 || key < 0 || key >= kKeyStateKeys) {
    return;
  }
  KeyStatePort& state = key_state_[key / 16];
  const uint16_t bit = static_cast<uint16_t>(1 << (key % 16));
  if (held == ((state.held & bit) != 0)) {
    return;
  }
  if (held) {
    state.held |= bit;
    state.pressed |= bit;
  } else {
    state.held &= ~bit;
  }
  state.changed = true;
  Wake();
}

void Keyboard::Execute() {
  // Both run each time, so the keyboard waits for every lock it needs at once.
  const bool typed_keys_waiting = ExecuteTypedKeys();
  const bool key_state_waiting = ExecuteKeyState();
  if (typed_keys_waiting || key_state_waiting) {
    AdvanceCycles(1);
    return;
  }
  Sleep();
}

bool Keyboard::ExecuteTypedKeys() {
  if (keys_.empty()) {
    return false;
  }
  if (typed_keys_lock_ == nullptr) {
    typed_keys_lock_ = LockPort(0);
  }
  if (!typed_keys_lock_->IsLocked()) {
    return true;
  }

  // The key event goes in only if the status is clear, so one not yet read
  // stays until a core reads it.
  const bool stored = GetPort(0).StoreWord(*typed_keys_lock_, Port::T | Port::S,
                                           keys_.front()) == 0;
  typed_keys_lock_.reset();
  if (stored) {
    keys_.pop();
    RaiseInterrupt(0);
  }
  return false;
}

bool Keyboard::ExecuteKeyState() {
  if (key_state_port_ < 0) {
    return false;
  }
  bool waiting = false;
  for (int i = 0; i < kKeyStatePorts; ++i) {
    KeyStatePort& state = key_state_[i];
    const int port_index = key_state_port_ + i;
    if (state.lock == nullptr) {
      if (!state.changed && state.written == 0) {
        continue;
      }

      // Pressed keys written to the port are taken once a read clears its
      // status. The status only changes under the port's lock, so a port
      // with its status set needs no lock to know they weren't. Once the
      // lock is requested, the port is written when it is granted, so the
      // lock is always released.
      if (!state.changed && GetPort(port_index).GetStatus() != 0) {
        continue;
      }
      state.lock = LockPort(port_index);
    }
    if (!state.lock->IsLocked()) {
      waiting = true;
      continue;
    }

    // Only the pressed keys that were in the port are taken, so a key pressed
    // since stays pressed.
    Port& port = GetPort(port_index);
    if (port.GetStatus() == 0) {
      state.pressed &= ~state.written;
    }
    port.StoreWord(*state.lock, Port::A, state.held);
    port.StoreWord(*state.lock, state.pressed != 0 ? Port::S : 0,
                   state.pressed);
    state.written = state.pressed;
    state.changed = false;
    state.lock.reset();
  }
  return waiting;
}

}  // namespace oz3
