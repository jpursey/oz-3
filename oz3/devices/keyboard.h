// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_KEYBOARD_H_
#define OZ3_DEVICES_KEYBOARD_H_

#include <cstdint>
#include <memory>
#include <string_view>

#include "gb/container/queue.h"
#include "oz3/core/device.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/lockable.h"

namespace oz3 {

//==============================================================================
// Key events
//==============================================================================

// A key event is a word: the low byte is the key, and the high byte the
// modifier keys held with it. Characters are their ASCII codes (20h to 7Eh),
// and Ctrl with A to Z is 01h to 1Ah, as in ASCII. The keyboard doesn't
// interpret key events: the host makes them, with these constants.

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

// Modifier keys, which are combined with a key.
inline constexpr uint16_t kKeyShift = 0x0100;
inline constexpr uint16_t kKeyCtrl = 0x0200;
inline constexpr uint16_t kKeyAlt = 0x0400;

//==============================================================================
// KeyboardSettings
//==============================================================================

// Everything a host sets about a keyboard but its name: its wiring.
struct KeyboardSettings {
  bool operator==(const KeyboardSettings&) const = default;

  // The processor port the keyboard is on.
  int port = 0;

  // The interrupt raised when a key event is put in the port.
  DeviceInterrupt interrupt;
};

//==============================================================================
// KeyboardFactory
//==============================================================================

// A keyboard on one port, which a Processor creates a Keyboard from.
class KeyboardFactory final : public DeviceFactory {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a factory for a keyboard named `name`. Its settings are only
  // wiring, which ProcessorConfig::Validate checks.
  KeyboardFactory(std::string_view name, const KeyboardSettings& settings);
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

  const DeviceConfig config_;
};

//==============================================================================
// Keyboard
//==============================================================================

// A keyboard gives a machine the keys the host presses, one key event at a
// time in its port, with an interrupt as each arrives.
//
// The host presses keys with PressKey(), which queues their key events.
// Whenever the port's status is clear and a key event is waiting, the keyboard
// writes the oldest to port word 0, sets the status, and raises its interrupt.
// So each key event raises the interrupt once, when it becomes readable, and a
// handler that reads one with INS is interrupted again for the next. The
// keyboard puts the next key event in the port the cycle after a core reads
// one, which clears the status.
//
// A core that writes to the port sets its status, which the keyboard takes as
// a key event not yet read, so it waits until the status is cleared.
class Keyboard final : public Device {
 public:
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The most key events waiting for the port.
  static constexpr int kQueueSize = 16;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  ~Keyboard() override = default;

  //----------------------------------------------------------------------------
  // Host interface
  //----------------------------------------------------------------------------

  // Queues the key event `key`. Returns false if kQueueSize key events are
  // already waiting, which drops it, as old keyboards did. Called between
  // Processor::Execute calls.
  bool PressKey(uint16_t key);

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

  explicit Keyboard(DeviceConfig config);

  // Key events waiting for the port, oldest first, in a queue that doesn't
  // grow.
  gb::Queue<uint16_t> keys_;
  std::unique_ptr<Lock> lock_;  // The port's lock, while it is requested.
};

}  // namespace oz3

#endif  // OZ3_DEVICES_KEYBOARD_H_
