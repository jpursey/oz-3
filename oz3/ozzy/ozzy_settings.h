// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_OZZY_SETTINGS_H_
#define OZ3_OZZY_OZZY_SETTINGS_H_

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/statusor.h"
#include "oz3/core/core_types.h"
#include "oz3/core/processor_config.h"
#include "oz3/devices/disk_drive.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/screen.h"
#include "oz3/devices/teletype.h"
#include "oz3/devices/timer.h"
#include "oz3/disk/disk_hardware.h"

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

// The wiring of each device, which is the same in every Ozzy, so programs know
// it: the screen's page and interrupt, the keyboard's typed keys' port and
// interrupt and its first key state port, the teletype's port, the timer's
// first port and interrupt, and the drives'. Interrupt cores have bit n for
// core n. The keyboard's interrupt is shared, so each key is handled by one
// core, while the screen's refresh and the timer's tick are news for every core
// with a handler. A drive's completion is for core 0 alone, the OS's, which
// issues the drives' commands, as a completion can be read only once.
inline constexpr int kOzzyScreenBank = 0;
inline constexpr int kOzzyScreenPage = 15;
inline constexpr int kOzzyScreenInterrupt = 6;
inline constexpr uint8_t kOzzyScreenInterruptCores = kAllCores;
inline constexpr int kOzzyKeyboardPort = 1;
inline constexpr int kOzzyKeyboardInterrupt = 1;
inline constexpr uint8_t kOzzyKeyboardInterruptCores = kAllCores;
inline constexpr int kOzzyKeyboardKeyStatePort = 6;
inline constexpr int kOzzyTeletypePort = 2;
inline constexpr int kOzzyTimerPort = 3;
inline constexpr int kOzzyTimerInterrupt = 0;
inline constexpr uint8_t kOzzyTimerInterruptCores = kAllCores;

// The drives, A: to D:, numbered from 0 for A:. Drive n's ports start at
// kOzzyDrivePort + n * DiskDrive::kPorts, and its interrupt is
// kOzzyDriveInterrupt + n.
inline constexpr int kOzzyDriveCount = 4;
inline constexpr int kOzzyDrivePort = 16;
inline constexpr int kOzzyDriveInterrupt = 2;
inline constexpr uint8_t kOzzyDriveInterruptCores = 0x01;  // Core 0.

// Returns the letter of drive `drive`: 'A' for drive 0.
constexpr char GetOzzyDriveLetter(int drive) {
  return static_cast<char>('A' + drive);
}

//==============================================================================
// Device settings
//==============================================================================

// The settings of an Ozzy's screen that a host sets: all of ScreenSettings
// but its wiring, with the same defaults. A config file sets all but
// draw_pixels, which depends on how the host shows the screen rather than on
// the machine.
struct OzzyScreenSettings {
  bool operator==(const OzzyScreenSettings&) const = default;

  int columns = ScreenSettings().columns;
  int rows = ScreenSettings().rows;
  int refresh_hz = ScreenSettings().refresh_hz;
  int slow_blink_hz = ScreenSettings().slow_blink_hz;
  int fast_blink_hz = ScreenSettings().fast_blink_hz;
  std::optional<std::vector<uint16_t>> characters;
  bool draw_pixels = ScreenSettings().draw_pixels;
};

// The settings of an Ozzy's teletype that a host sets: all of
// TeletypeSettings but its wiring, with the same defaults.
struct OzzyTeletypeSettings {
  bool operator==(const OzzyTeletypeSettings&) const = default;

  int baud = TeletypeSettings().baud;
  int buffer_size = TeletypeSettings().buffer_size;
};

// The settings of an Ozzy's timer that a host sets: all of TimerSettings but
// its wiring, with the same defaults.
struct OzzyTimerSettings {
  bool operator==(const OzzyTimerSettings&) const = default;

  int tick_hz = TimerSettings().tick_hz;
};

// The settings of an Ozzy's drive that a host sets: all of DiskDriveSettings
// but its wiring, with the same defaults, and the disk in it at power on.
struct OzzyDriveSettings {
  bool operator==(const OzzyDriveSettings&) const = default;

  int slots = DiskDriveSettings().slots;
  DiskHardwareSettings hardware = DiskDriveSettings().hardware;

  // The path of the disk image in the drive at power on, in the file system
  // Ozzy::Create is given, or empty for no disk.
  std::string image;
};

// Returns the settings of an Ozzy's device: the host's, and the wiring.
ScreenSettings GetOzzyScreenSettings(const OzzyScreenSettings& screen);
KeyboardSettings GetOzzyKeyboardSettings();
TeletypeSettings GetOzzyTeletypeSettings(const OzzyTeletypeSettings& teletype);
TimerSettings GetOzzyTimerSettings(const OzzyTimerSettings& timer);

// Returns the settings of drive `drive` (0 for A:) of an Ozzy, which must be
// 0 to kOzzyDriveCount - 1.
DiskDriveSettings GetOzzyDriveSettings(int drive,
                                       const OzzyDriveSettings& settings);

//==============================================================================
// OzzySettings
//==============================================================================

// Everything a host sets about an Ozzy: the processor, its memory, and which
// devices it has, with their own settings. The defaults are the default Ozzy:
// one core at 1 MHz, one bank, a screen, a keyboard, and a timer, with no
// drives.
struct OzzySettings {
  // The fastest clock rate.
  static constexpr int64_t kMaxClockHz = 100'000'000;

  // The fewest ports, which keeps ports 0 to 31 for devices.
  static constexpr int kMinPorts = 32;

  // The fastest a timer ticks, as the machine table holds its rate in a word.
  static constexpr int kMaxTimerTickHz = 0xFFFF;

  bool operator==(const OzzySettings&) const = default;

  // The clock rate, from 1 to kMaxClockHz.
  int64_t clock_hz = ProcessorConfig::kDefaultClockHz;

  // The cores, from 1 to kMaxCores.
  int cores = 1;

  // The ports, from kMinPorts to kMaxPorts.
  int ports = 48;

  // The banks, from 1 to kMaxMemoryBanks of them, each given as its RAM pages,
  // with bit n for page n. Page 0 of bank 0 is always the boot ROM, so bank 0
  // never has bit 0 set. At least one bank has RAM, for the boot ROM's stack.
  std::vector<uint16_t> banks = {0x7FFE};

  // The devices, which an Ozzy has at most one of each of. With a screen, its
  // page isn't RAM.
  std::optional<OzzyScreenSettings> screen = OzzyScreenSettings();
  bool keyboard = true;
  std::optional<OzzyTeletypeSettings> teletype;
  std::optional<OzzyTimerSettings> timer = OzzyTimerSettings();

  // The drives, lettered in order from A:, at most kOzzyDriveCount of them, no
  // two with the same image. A drive with no image keeps its place, so a disk
  // in B: has an empty A: before it.
  std::vector<OzzyDriveSettings> drives;
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
// `devices`, the Ozzy has the default screen, keyboard, and timer, and without
// `drives`, no drives. Each drive names its preset, as a drive's kind is the
// first thing to say about it.
//
// `path` is the config file's path, which errors begin with, if it isn't
// empty. Drives' image paths are normalized, and relative ones are made from
// its folder, so a config and its disks move together. Without a folder (an
// empty path, or a file name alone), they are kept relative.
//
// Returns an error naming where the first problem in the text is, by its line
// and column and its setting, such as `hello.ozzy(12:5): devices[1].port:
// unknown key`: a syntax error, an unknown key, a value of the wrong type, a
// value that doesn't fit its setting, a second device of a type, a drive
// with no preset, or an image that isn't a path. Then the settings must be
// valid (see ValidateOzzySettings), which names the setting but not its place
// in the text: `hello.ozzy: screen: page 15 of bank 0 is RAM`.
absl::StatusOr<OzzySettings> ReadOzzySettings(std::string text,
                                              std::string_view path = {});

}  // namespace oz3

#endif  // OZ3_OZZY_OZZY_SETTINGS_H_
