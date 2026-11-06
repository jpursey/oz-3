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
// Device defaults
//==============================================================================

// Returns Ozzy's settings for each console device, which a device in a config
// file starts from:
// - The screen: page 15 of bank 0, interrupt 6 on every core, 80 by 24, and
//   the screen's own rates.
// - The keyboard: port 1, interrupt 1 on core 0.
// - The teletype: port 2, at baud 0 (as fast as cores write).
ScreenSettings GetOzzyScreenSettings();
KeyboardSettings GetOzzyKeyboardSettings();
TeletypeSettings GetOzzyTeletypeSettings();

//==============================================================================
// OzzySettings
//==============================================================================

// Everything a host sets about an Ozzy: the processor, its memory, and its
// console devices. The defaults are the default Ozzy: one core at 1 MHz, one
// bank, a screen, and a keyboard.
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
  // never has bit 0 set.
  std::vector<uint16_t> banks = {0x7FFE};

  // The console devices, which an Ozzy has at most one of each of. A device's
  // pages are in a bank the Ozzy has, and are neither RAM nor the boot ROM.
  std::optional<ScreenSettings> screen = GetOzzyScreenSettings();
  std::optional<KeyboardSettings> keyboard = GetOzzyKeyboardSettings();
  std::optional<TeletypeSettings> teletype;
};

//==============================================================================
// Validating and reading settings
//==============================================================================

// Returns an error naming the first setting that breaks a rule of
// OzzySettings, such as `screen: page 3 of bank 0 is RAM`. What the
// processor and devices check themselves (such as a screen's size, or a
// device's port) is left to them.
absl::Status ValidateOzzySettings(const OzzySettings& settings);

// Reads settings from the text of a config file (a `.ozzy` file): Game Bits'
// text config format, rootless and with comments. Anything the text leaves
// out keeps its default, as does any setting a device leaves out (see
// GetOzzyScreenSettings). Without `devices`, the Ozzy has the default screen
// and keyboard.
//
// Returns an error naming where the first problem in the text is, such as
// `devices[1].interrupt_cores: the Ozzy has no core 2`: an unknown key, a value
// of the wrong type, a value that doesn't fit its setting, a second device of a
// type, or a core the Ozzy doesn't have. Then the settings must be valid (see
// ValidateOzzySettings), which names the setting rather than the text.
absl::StatusOr<OzzySettings> ReadOzzySettings(std::string text);

}  // namespace oz3

#endif  // OZ3_OZZY_OZZY_SETTINGS_H_
