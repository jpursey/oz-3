// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_KEYBOARD_H_
#define OZ3_DEVICES_KEYBOARD_H_

#include <array>
#include <cstdint>
#include <memory>
#include <optional>
#include <string_view>

#include "absl/status/statusor.h"
#include "gb/container/queue.h"
#include "oz3/core/device.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/lockable.h"

namespace oz3 {

//==============================================================================
// Keys
//==============================================================================

// A typed key event is a word: the low byte is the key, and the high byte the
// modifier keys held with it. Characters are their ASCII codes (20h to 7Eh),
// and Ctrl with A to Z is 01h to 1Ah, as in ASCII. The keyboard doesn't
// interpret key events: the host makes them, with these constants.
//
// In key state, each key has a code, which is its bit: the typed key's code
// for the key without Shift on a US keyboard, so letters are their lower case
// codes, and kKeyUp is the Up key. The modifier keys have codes of their own,
// left and right. Codes are less than Keyboard::kKeyStateKeys.

// Keys with ASCII codes.
inline constexpr uint16_t kKeyBackspace = 0x08;
inline constexpr uint16_t kKeyTab = 0x09;
inline constexpr uint16_t kKeyEnter = 0x0D;
inline constexpr uint16_t kKeyEscape = 0x1B;
inline constexpr uint16_t kKeyDelete = 0x7F;

// Keys without ASCII codes.
inline constexpr uint16_t kKeyUp = 0x80;
inline constexpr uint16_t kKeyDown = 0x81;
inline constexpr uint16_t kKeyLeft = 0x82;
inline constexpr uint16_t kKeyRight = 0x83;
inline constexpr uint16_t kKeyHome = 0x84;
inline constexpr uint16_t kKeyEnd = 0x85;
inline constexpr uint16_t kKeyPageUp = 0x86;
inline constexpr uint16_t kKeyPageDown = 0x87;
inline constexpr uint16_t kKeyInsert = 0x88;
inline constexpr uint16_t kKeyF1 = 0x90;
inline constexpr uint16_t kKeyF2 = 0x91;
inline constexpr uint16_t kKeyF3 = 0x92;
inline constexpr uint16_t kKeyF4 = 0x93;
inline constexpr uint16_t kKeyF5 = 0x94;
inline constexpr uint16_t kKeyF6 = 0x95;
inline constexpr uint16_t kKeyF7 = 0x96;
inline constexpr uint16_t kKeyF8 = 0x97;
inline constexpr uint16_t kKeyF9 = 0x98;
inline constexpr uint16_t kKeyF10 = 0x99;
inline constexpr uint16_t kKeyF11 = 0x9A;
inline constexpr uint16_t kKeyF12 = 0x9B;

// The modifier keys' codes, which are only in key state: a typed key event
// holds the modifiers as bits (kKeyShift, kKeyCtrl, and kKeyAlt).
inline constexpr uint16_t kKeyLeftShift = 0x89;
inline constexpr uint16_t kKeyRightShift = 0x8A;
inline constexpr uint16_t kKeyLeftCtrl = 0x8B;
inline constexpr uint16_t kKeyRightCtrl = 0x8C;
inline constexpr uint16_t kKeyLeftAlt = 0x8D;
inline constexpr uint16_t kKeyRightAlt = 0x8E;

// The modifier bits of a typed key event, which are combined with a key.
inline constexpr uint16_t kKeyShift = 0x0100;
inline constexpr uint16_t kKeyCtrl = 0x0200;
inline constexpr uint16_t kKeyAlt = 0x0400;

//==============================================================================
// KeyboardSettings
//==============================================================================

// Everything a host sets about a keyboard but its name: its wiring, which has
// typed keys, key state, or both.
struct KeyboardSettings {
  bool operator==(const KeyboardSettings&) const = default;

  // The first of Keyboard::kKeyStatePorts consecutive processor ports of key
  // state, or none for a keyboard without key state.
  std::optional<int> state_port;

  // The processor port of the typed keys, or none for a keyboard without
  // typed keys.
  std::optional<int> typed_port = 0;

  // The interrupt raised when a typed key event is put in the typed keys'
  // port, which a keyboard without typed keys doesn't have.
  DeviceInterrupt typed_interrupt;
};

//==============================================================================
// KeyboardFactory
//==============================================================================

// A keyboard on its ports, which a Processor creates a Keyboard from.
class KeyboardFactory final : public DeviceFactory {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a factory for a keyboard named `name`. Returns an error if
  // `settings` has neither typed keys nor key state, or the typed keys' port
  // is one of the key state's. The ports must also be ones the processor has,
  // which ProcessorConfig::Validate checks.
  static absl::StatusOr<std::unique_ptr<KeyboardFactory>> Create(
      std::string_view name, const KeyboardSettings& settings);

  ~KeyboardFactory() override = default;

  //----------------------------------------------------------------------------
  // DeviceFactory implementation
  //----------------------------------------------------------------------------

  const DeviceConfig& GetConfig() const override { return config_; }
  std::unique_ptr<Device> CreateDevice() const override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  KeyboardFactory(std::string_view name, const KeyboardSettings& settings);

  const DeviceConfig config_;
  const KeyboardSettings settings_;
};

//==============================================================================
// Keyboard
//==============================================================================

// A keyboard gives a machine the keys the host presses, as typed keys, key
// state, or both.
//
// Typed keys are one key event at a time in a port, with an interrupt as each
// arrives. The host types keys with TypeKey(), which queues their key events.
// Whenever the port's status is clear and a key event is waiting, the keyboard
// writes the oldest to port word 0, sets the status, and raises its interrupt.
// So each key event raises the interrupt once, when it becomes readable, and a
// handler that reads one with INS is interrupted again for the next. The
// keyboard puts the next key event in the port the cycle after a core reads
// one, which clears the status. A core that writes to the port sets its
// status, which the keyboard takes as a key event not yet read, so it waits
// until the status is cleared.
//
// Key state is kKeyStatePorts ports, each holding 16 keys, with bit i of port
// n for the key with code 16n + i. Word 0 is the keys held, and word 1 the
// keys pressed since the port was last read, with the status set while word 1
// isn't 0. The host sets keys held and released with SetKeyState(). Reading a
// port clears its status, which takes its pressed keys: the keyboard then
// clears them from word 1, keeping any pressed since. The keyboard writes a
// port only when one of its keys changes, or its pressed keys are taken, and
// a core's write to it stays until then.
//
// The keyboard takes no cycles of its own, and waits only while a core holds
// a port it writes.
class Keyboard final : public Device {
 public:
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The most typed key events waiting for the port.
  static constexpr int kQueueSize = 16;

  // The ports of key state, and the keys they hold.
  static constexpr int kKeyStatePorts = 10;
  static constexpr int kKeyStateKeys = kKeyStatePorts * 16;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  ~Keyboard() override = default;

  //----------------------------------------------------------------------------
  // Host interface
  //
  // Called between Processor::Execute calls.
  //----------------------------------------------------------------------------

  // Queues the typed key event `key`. Returns false if kQueueSize key events
  // are already waiting, which drops it, as old keyboards did, or if the
  // keyboard has no typed keys.
  bool TypeKey(uint16_t key);

  // Sets whether the key with code `key` is held. A key held that wasn't is
  // pressed too, so holding a key again doesn't press it again. Does nothing
  // if the keyboard has no key state, or `key` isn't in the range
  // [0, kKeyStateKeys).
  void SetKeyState(int key, bool held);

 protected:
  //----------------------------------------------------------------------------
  // ExecutionComponent implementation
  //----------------------------------------------------------------------------

  void Execute() override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The factory constructs the keyboard.
  friend class KeyboardFactory;

  // What the keyboard keeps for a port of key state.
  struct KeyStatePort {
    uint16_t held = 0;           // The keys held.
    uint16_t pressed = 0;        // The keys pressed and not yet taken.
    uint16_t written = 0;        // The pressed keys last written to word 1.
    bool changed = false;        // Whether a key changed since it was written.
    std::unique_ptr<Lock> lock;  // The port's lock, while it is requested.
  };

  // Constructs a keyboard wired as `config`, which is its factory's config for
  // `settings`.
  Keyboard(DeviceConfig config, const KeyboardSettings& settings);

  // Puts the next typed key event in its port if it can. Returns whether the
  // keyboard is waiting for the port's lock.
  bool ExecuteTypedKeys();

  // Writes the key state's ports that need it. Returns whether the keyboard is
  // waiting for a port's lock.
  bool ExecuteKeyState();

  // Whether the keyboard has typed keys, whose port is the device's port 0,
  // and the device's port index of the first key state port, or -1 for none.
  const bool typed_keys_;
  const int key_state_port_;

  // Typed key events waiting for the port, oldest first, in a queue that
  // doesn't grow.
  gb::Queue<uint16_t> keys_;
  std::unique_ptr<Lock> typed_keys_lock_;  // While it is requested.

  std::array<KeyStatePort, kKeyStatePorts> key_state_;
};

}  // namespace oz3

#endif  // OZ3_DEVICES_KEYBOARD_H_
