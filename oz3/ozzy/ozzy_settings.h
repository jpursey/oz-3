// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_OZZY_SETTINGS_H_
#define OZ3_OZZY_OZZY_SETTINGS_H_

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "absl/status/statusor.h"
#include "oz3/core/core_types.h"
#include "oz3/core/processor_config.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/screen.h"
#include "oz3/devices/teletype.h"

namespace oz3 {

//==============================================================================
// The boot ROM
//==============================================================================

// Returns the ROM pages of `bank`, with bit n for page n: the boot ROM's page,
// which is page 0 of bank 0, and no others.
constexpr uint16_t GetBootRomPages(int bank) {
  return bank == 0 ? 0x0001 : 0x0000;
}

//==============================================================================
// Device wiring
//==============================================================================

// The wiring of each console device, which is the same in every Ozzy, so
// programs know it: the screen's page and interrupt, the keyboard's port and
// interrupt, and the teletype's port. Interrupt cores have bit n for core n.
// The keyboard's interrupt is shared, so each key is handled by one core, while
// the screen's refresh is news for every core with a handler.
inline constexpr int kOzzyScreenBank = 0;
inline constexpr int kOzzyScreenPage = 15;
inline constexpr int kOzzyScreenInterrupt = 6;
inline constexpr uint8_t kOzzyScreenInterruptCores = kAllCores;
inline constexpr int kOzzyKeyboardPort = 1;
inline constexpr int kOzzyKeyboardInterrupt = 1;
inline constexpr uint8_t kOzzyKeyboardInterruptCores = kAllCores;
inline constexpr int kOzzyTeletypePort = 2;

//==============================================================================
// Device settings
//==============================================================================

// The settings of an Ozzy's screen that a host sets: all of ScreenSettings
// but its wiring, with the same defaults.
struct OzzyScreenSettings {
  bool operator==(const OzzyScreenSettings&) const = default;

  int columns = ScreenSettings().columns;
  int rows = ScreenSettings().rows;
  int refresh_hz = ScreenSettings().refresh_hz;
  int slow_blink_hz = ScreenSettings().slow_blink_hz;
  int fast_blink_hz = ScreenSettings().fast_blink_hz;
  std::optional<std::vector<uint16_t>> characters;
};

// The settings of an Ozzy's teletype that a host sets: all of
// TeletypeSettings but its wiring, with the same defaults.
struct OzzyTeletypeSettings {
  bool operator==(const OzzyTeletypeSettings&) const = default;

  int baud = TeletypeSettings().baud;
  int buffer_size = TeletypeSettings().buffer_size;
};

// Returns the settings of an Ozzy's device: the host's, and the wiring.
ScreenSettings GetOzzyScreenSettings(const OzzyScreenSettings& screen);
KeyboardSettings GetOzzyKeyboardSettings();
TeletypeSettings GetOzzyTeletypeSettings(const OzzyTeletypeSettings& teletype);

//==============================================================================
// OzzySettings
//==============================================================================

// Everything a host sets about an Ozzy: the processor, its memory, and which
// console devices it has, with their own settings. The defaults are the
// default Ozzy: one core at 1 MHz, one bank, a screen, and a keyboard.
struct OzzySettings {
  // The fastest clock rate.
  static constexpr int64_t kMaxClockHz = 100'000'000;

  // The fewest ports, which keeps ports 0 to 15 for devices.
  static constexpr int kMinPorts = 16;

  bool operator==(const OzzySettings&) const = default;

  // The clock rate, from 1 to kMaxClockHz.
  int64_t clock_hz = ProcessorConfig::kDefaultClockHz;

  // The cores, from 1 to kMaxCores.
  int cores = 1;

  // The ports, from kMinPorts to kMaxPorts.
  int ports = 32;

  // The banks, from 1 to kMaxMemoryBanks of them, each given as its RAM pages,
  // with bit n for page n. Page 0 of bank 0 is always the boot ROM, so bank 0
  // never has bit 0 set. At least one bank has RAM, for the boot ROM's stack.
  std::vector<uint16_t> banks = {0x7FFE};

  // The console devices, which an Ozzy has at most one of each of. With a
  // screen, its page isn't RAM.
  std::optional<OzzyScreenSettings> screen = OzzyScreenSettings();
  bool keyboard = true;
  std::optional<OzzyTeletypeSettings> teletype;
};

//==============================================================================
// Validating and reading settings
//==============================================================================

// Returns an error naming the first setting that breaks a rule of
// OzzySettings, such as `screen: page 15 of bank 0 is RAM`. What the
// processor and devices check themselves (such as a screen's size) is left to
// them.
absl::Status ValidateOzzySettings(const OzzySettings& settings);

// Reads settings from the text of a config file (a `.ozzy` file): Game Bits'
// text config format, rootless and with comments. Anything the text leaves
// out keeps its default, as does any setting a device leaves out. Without
// `devices`, the Ozzy has the default screen and keyboard.
//
// Returns an error naming where the first problem in the text is, such as
// `devices[1].port: unknown key`: an unknown key, a value of the wrong type, a
// value that doesn't fit its setting, or a second device of a type. Then the
// settings must be valid (see ValidateOzzySettings), which names the setting
// rather than the text.
absl::StatusOr<OzzySettings> ReadOzzySettings(std::string text);

}  // namespace oz3

#endif  // OZ3_OZZY_OZZY_SETTINGS_H_
