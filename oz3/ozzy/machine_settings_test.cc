// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/machine_settings.h"

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/processor_config.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/screen.h"
#include "oz3/devices/teletype.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;

// Returns the error message from reading `text`, or "ok" if it reads.
std::string ReadError(std::string_view text) {
  absl::StatusOr<MachineSettings> settings =
      ReadMachineSettings(std::string(text));
  if (settings.ok()) {
    return "ok";
  }
  EXPECT_EQ(settings.status().code(), absl::StatusCode::kInvalidArgument);
  return std::string(settings.status().message());
}

//==============================================================================
// Defaults
//==============================================================================

TEST(MachineSettingsTest, DeviceDefaults) {
  const ScreenSettings screen = GetOzzyScreenSettings();
  const ScreenSettings device_screen;
  EXPECT_EQ(screen.bank, 0);
  EXPECT_EQ(screen.page, 15);
  EXPECT_EQ(screen.interrupt.interrupt, 6);
  EXPECT_EQ(screen.interrupt.cores, kAllCores);
  EXPECT_EQ(screen.columns, 80);
  EXPECT_EQ(screen.rows, 24);
  EXPECT_EQ(screen.refresh_hz, device_screen.refresh_hz);
  EXPECT_EQ(screen.slow_blink_hz, device_screen.slow_blink_hz);
  EXPECT_EQ(screen.fast_blink_hz, device_screen.fast_blink_hz);
  EXPECT_FALSE(screen.characters.has_value());

  const KeyboardSettings keyboard = GetOzzyKeyboardSettings();
  EXPECT_EQ(keyboard.port, 1);
  EXPECT_EQ(keyboard.interrupt.interrupt, 1);
  EXPECT_EQ(keyboard.interrupt.cores, 0x01);

  const TeletypeSettings teletype = GetOzzyTeletypeSettings();
  const TeletypeSettings device_teletype;
  EXPECT_EQ(teletype.port, 2);
  EXPECT_EQ(teletype.baud, 0);
  EXPECT_EQ(teletype.buffer_size, device_teletype.buffer_size);
}

TEST(MachineSettingsTest, DefaultOzzy) {
  const MachineSettings settings;
  EXPECT_EQ(settings.clock_hz, ProcessorConfig::kDefaultClockHz);
  EXPECT_EQ(settings.cores, 1);
  EXPECT_EQ(settings.ports, 32);
  EXPECT_THAT(settings.banks, ElementsAre(0x7FFE));
  EXPECT_EQ(settings.screen, GetOzzyScreenSettings());
  EXPECT_EQ(settings.keyboard, GetOzzyKeyboardSettings());
  EXPECT_FALSE(settings.teletype.has_value());
}

TEST(MachineSettingsTest, EmptyConfigIsDefaultOzzy) {
  for (std::string_view text : {"", "  // Nothing but a comment\n"}) {
    absl::StatusOr<MachineSettings> settings =
        ReadMachineSettings(std::string(text));
    ASSERT_TRUE(settings.ok()) << settings.status();
    EXPECT_EQ(*settings, MachineSettings());
  }
}

//==============================================================================
// The machine
//==============================================================================

TEST(MachineSettingsTest, ReadsMachine) {
  absl::StatusOr<MachineSettings> settings = ReadMachineSettings(R"(
    clock_hz: 4000000,
    cores: 4,
    ports: 64,
    banks: [
      { ram: 0x7FFE },  // Bank 0: pages 1 to 14
      { ram: 0xFFFF },  // Bank 1: every page
      {}                // Bank 2: no RAM
    ]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(settings->clock_hz, 4'000'000);
  EXPECT_EQ(settings->cores, 4);
  EXPECT_EQ(settings->ports, 64);
  EXPECT_THAT(settings->banks, ElementsAre(0x7FFE, 0xFFFF, 0));
}

TEST(MachineSettingsTest, MachineRanges) {
  EXPECT_EQ(ReadError("clock_hz: 1"), "ok");
  EXPECT_EQ(ReadError("clock_hz: 100000000"), "ok");
  EXPECT_EQ(ReadError("clock_hz: 0"), "clock_hz: must be 1 to 100000000");
  EXPECT_EQ(ReadError("clock_hz: 100000001"),
            "clock_hz: must be 1 to 100000000");
  EXPECT_EQ(ReadError("cores: 1"), "ok");
  EXPECT_EQ(ReadError("cores: 8"), "ok");
  EXPECT_EQ(ReadError("cores: 0"), "cores: must be 1 to 8");
  EXPECT_EQ(ReadError("cores: 9"), "cores: must be 1 to 8");
  EXPECT_EQ(ReadError("ports: 16"), "ok");
  EXPECT_EQ(ReadError("ports: 256"), "ok");
  EXPECT_EQ(ReadError("ports: 15"), "ports: must be 16 to 256");
  EXPECT_EQ(ReadError("ports: 257"), "ports: must be 16 to 256");
}

TEST(MachineSettingsTest, BankRanges) {
  EXPECT_EQ(ReadError("banks: [{ ram: 0 }]"), "ok");
  EXPECT_EQ(ReadError("banks: [{ ram: 0 }, { ram: 0xFFFF }]"), "ok");
  EXPECT_EQ(ReadError("banks: []"), "banks: must have 1 to 16 banks");
  EXPECT_EQ(ReadError("banks: [{}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},"
                      " {}, {}, {}, {}]"),
            "ok");
  EXPECT_EQ(ReadError("banks: [{}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},"
                      " {}, {}, {}, {}, {}]"),
            "banks: must have 1 to 16 banks");
  EXPECT_EQ(ReadError("banks: [{}, { ram: -1 }]"),
            "banks[1].ram: must be 0 to 65535");
  EXPECT_EQ(ReadError("banks: [{}, { ram: 0x10000 }]"),
            "banks[1].ram: must be 0 to 65535");
}

TEST(MachineSettingsTest, BootRomPageIsNotRam) {
  EXPECT_EQ(ReadError("banks: [{ ram: 0xFFFF }]"),
            "banks[0].ram: page 0 of bank 0 is the boot ROM");
  EXPECT_EQ(ReadError("banks: [{ ram: 0x0001 }]"),
            "banks[0].ram: page 0 of bank 0 is the boot ROM");
  EXPECT_EQ(ReadError("banks: [{}, { ram: 0xFFFF }]"), "ok");
}

TEST(MachineSettingsTest, MachineErrors) {
  EXPECT_EQ(ReadError("core: 2"), "core: unknown key");
  EXPECT_EQ(ReadError("cores: 2, zebra: 1, apple: 1"), "apple: unknown key");
  EXPECT_EQ(ReadError("cores: 2.0"), "cores: must be an int");
  EXPECT_EQ(ReadError("clock_hz: \"fast\""), "clock_hz: must be an int");
  EXPECT_EQ(ReadError("ports: true"), "ports: must be an int");
  EXPECT_EQ(ReadError("banks: { ram: 0x7FFE }"), "banks: must be an array");
  EXPECT_EQ(ReadError("banks: [0x7FFE]"), "banks[0]: must be a map");
  EXPECT_EQ(ReadError("banks: [{ rom: 1 }]"), "banks[0].rom: unknown key");
  EXPECT_EQ(ReadError("banks: [{ ram: [1, 2] }]"),
            "banks[0].ram: must be an int");
  EXPECT_EQ(ReadError("devices: {}"), "devices: must be an array");
  EXPECT_EQ(ReadError("devices: [\"screen\"]"), "devices[0]: must be a map");
}

TEST(MachineSettingsTest, SyntaxError) {
  absl::StatusOr<MachineSettings> settings = ReadMachineSettings("cores: [");
  EXPECT_FALSE(settings.ok());
}

//==============================================================================
// Devices
//==============================================================================

TEST(MachineSettingsTest, NoDevices) {
  absl::StatusOr<MachineSettings> settings = ReadMachineSettings("devices: []");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_FALSE(settings->screen.has_value());
  EXPECT_FALSE(settings->keyboard.has_value());
  EXPECT_FALSE(settings->teletype.has_value());
}

TEST(MachineSettingsTest, DevicesTakeDefaults) {
  absl::StatusOr<MachineSettings> settings = ReadMachineSettings(R"(
    devices: [{ type: "teletype" }, { type: "screen" }, { type: "keyboard" }]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(settings->screen, GetOzzyScreenSettings());
  EXPECT_EQ(settings->keyboard, GetOzzyKeyboardSettings());
  EXPECT_EQ(settings->teletype, GetOzzyTeletypeSettings());
}

TEST(MachineSettingsTest, OnlyListedDevices) {
  absl::StatusOr<MachineSettings> settings =
      ReadMachineSettings("devices: [{ type: 'teletype' }]");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_FALSE(settings->screen.has_value());
  EXPECT_FALSE(settings->keyboard.has_value());
  EXPECT_TRUE(settings->teletype.has_value());
}

TEST(MachineSettingsTest, ReadsScreen) {
  absl::StatusOr<MachineSettings> settings = ReadMachineSettings(R"(
    cores: 4,
    banks: [{}, { ram: 0x00FF }],
    devices: [{
      type: "screen", bank: 1, page: 9, interrupt: 7, interrupt_cores: 2,
      columns: 40, rows: 12, refresh_hz: 60, slow_blink_hz: 2,
      fast_blink_hz: 8, characters: [0, 0x1234, 0xFFFF]
    }]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  ASSERT_TRUE(settings->screen.has_value());
  const ScreenSettings& screen = *settings->screen;
  EXPECT_EQ(screen.bank, 1);
  EXPECT_EQ(screen.page, 9);
  EXPECT_EQ(screen.interrupt.interrupt, 7);
  EXPECT_EQ(screen.interrupt.cores, 0x04);
  EXPECT_EQ(screen.columns, 40);
  EXPECT_EQ(screen.rows, 12);
  EXPECT_EQ(screen.refresh_hz, 60);
  EXPECT_EQ(screen.slow_blink_hz, 2);
  EXPECT_EQ(screen.fast_blink_hz, 8);
  ASSERT_TRUE(screen.characters.has_value());
  EXPECT_THAT(*screen.characters, ElementsAre(0, 0x1234, 0xFFFF));
}

TEST(MachineSettingsTest, ReadsKeyboard) {
  absl::StatusOr<MachineSettings> settings = ReadMachineSettings(R"(
    cores: 2,
    devices: [{ type: "keyboard", port: 5, interrupt: 3, interrupt_cores: 1 }]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  ASSERT_TRUE(settings->keyboard.has_value());
  EXPECT_EQ(settings->keyboard->port, 5);
  EXPECT_EQ(settings->keyboard->interrupt.interrupt, 3);
  EXPECT_EQ(settings->keyboard->interrupt.cores, 0x02);
}

TEST(MachineSettingsTest, ReadsTeletype) {
  absl::StatusOr<MachineSettings> settings = ReadMachineSettings(R"(
    devices: [{ type: "teletype", port: 7, baud: 300, buffer_size: 64 }]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  ASSERT_TRUE(settings->teletype.has_value());
  EXPECT_EQ(settings->teletype->port, 7);
  EXPECT_EQ(settings->teletype->baud, 300);
  EXPECT_EQ(settings->teletype->buffer_size, 64);
}

TEST(MachineSettingsTest, DeviceSettingsCheckedByDevices) {
  // The processor and the devices check these themselves, so reading only
  // checks they fit in an int.
  absl::StatusOr<MachineSettings> settings = ReadMachineSettings(R"(
    devices: [
      { type: "screen", interrupt: 99, columns: 1000, rows: -1 },
      { type: "teletype", port: 1000, baud: -5, buffer_size: 0 }
    ]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(settings->screen->interrupt.interrupt, 99);
  EXPECT_EQ(settings->screen->columns, 1000);
  EXPECT_EQ(settings->screen->rows, -1);
  EXPECT_EQ(settings->teletype->port, 1000);
  EXPECT_EQ(settings->teletype->baud, -5);
  EXPECT_EQ(settings->teletype->buffer_size, 0);
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', columns: 0x80000000 }]"),
            "devices[0].columns: must be -2147483648 to 2147483647");
}

//==============================================================================
// Interrupt cores
//==============================================================================

// Returns the keyboard's interrupt cores read from `interrupt_cores` in an Ozzy
// with 4 cores, or none if reading fails.
std::optional<uint8_t> ReadInterruptCores(std::string_view interrupt_cores) {
  absl::StatusOr<MachineSettings> settings = ReadMachineSettings(
      absl::StrCat("cores: 4, devices: [{ type: 'keyboard', interrupt_cores: ",
                   interrupt_cores, " }]"));
  if (!settings.ok()) {
    return std::nullopt;
  }
  return settings->keyboard->interrupt.cores;
}

TEST(MachineSettingsTest, InterruptCores) {
  EXPECT_EQ(ReadInterruptCores("0"), 0x01);
  EXPECT_EQ(ReadInterruptCores("3"), 0x08);
  EXPECT_EQ(ReadInterruptCores("[0, 2]"), 0x05);
  EXPECT_EQ(ReadInterruptCores("[3, 1, 3]"), 0x0A);
  EXPECT_EQ(ReadInterruptCores("[]"), 0x00);
  EXPECT_EQ(ReadInterruptCores("'all'"), kAllCores);
}

TEST(MachineSettingsTest, InterruptCoresErrors) {
  EXPECT_EQ(ReadError("cores: 2, devices: [{ type: 'keyboard', "
                      "interrupt_cores: 2 }]"),
            "devices[0].interrupt_cores: the Ozzy has no core 2");
  EXPECT_EQ(ReadError("devices: [{ type: 'keyboard', interrupt_cores: -1 }]"),
            "devices[0].interrupt_cores: the Ozzy has no core -1");
  EXPECT_EQ(ReadError("cores: 2, devices: [{ type: 'screen', "
                      "interrupt_cores: [0, 1, 2] }]"),
            "devices[0].interrupt_cores[2]: the Ozzy has no core 2");
  EXPECT_EQ(ReadError("devices: [{ type: 'keyboard', interrupt_cores: "
                      "['all'] }]"),
            "devices[0].interrupt_cores[0]: must be an int");
  EXPECT_EQ(ReadError("devices: [{ type: 'keyboard', interrupt_cores: "
                      "'none' }]"),
            "devices[0].interrupt_cores: must be a core, an array of cores, "
            "or \"all\"");
  EXPECT_EQ(ReadError("devices: [{ type: 'keyboard', interrupt_cores: 1.0 }]"),
            "devices[0].interrupt_cores: must be a core, an array of cores, "
            "or \"all\"");
}

//==============================================================================
// Device errors
//==============================================================================

TEST(MachineSettingsTest, DeviceTypeErrors) {
  EXPECT_EQ(ReadError("devices: [{ port: 1 }]"), "devices[0]: missing type");
  EXPECT_EQ(ReadError("devices: [{ type: 1 }]"),
            "devices[0].type: must be a string");
  EXPECT_EQ(ReadError("devices: [{ type: 'disk' }]"),
            "devices[0].type: unknown type \"disk\" (must be \"screen\", "
            "\"keyboard\", or \"teletype\")");
}

TEST(MachineSettingsTest, DeviceUnknownKeys) {
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', colums: 40 }]"),
            "devices[0].colums: unknown key");
  EXPECT_EQ(ReadError("devices: [{ type: 'keyboard', page: 3 }]"),
            "devices[0].page: unknown key");
  EXPECT_EQ(ReadError("devices: [{ type: 'teletype', interrupt: 3 }]"),
            "devices[0].interrupt: unknown key");
}

TEST(MachineSettingsTest, DeviceWrongTypes) {
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', rows: '24' }]"),
            "devices[0].rows: must be an int");
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', characters: 0 }]"),
            "devices[0].characters: must be an array");
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', characters: [0, -1] }]"),
            "devices[0].characters[1]: must be 0 to 65535");
  EXPECT_EQ(ReadError("devices: [{ type: 'keyboard', port: 1.5 }]"),
            "devices[0].port: must be an int");
  EXPECT_EQ(ReadError("devices: [{ type: 'teletype', baud: 'fast' }]"),
            "devices[0].baud: must be an int");
}

TEST(MachineSettingsTest, OneDeviceOfEachType) {
  EXPECT_EQ(ReadError("devices: [{ type: 'teletype' }, { type: 'screen' }, "
                      "{ type: 'screen', page: 14 }]"),
            "devices[2]: an Ozzy has only one screen");
  EXPECT_EQ(ReadError("devices: [{ type: 'keyboard' }, "
                      "{ type: 'keyboard', port: 3 }]"),
            "devices[1]: an Ozzy has only one keyboard");
  EXPECT_EQ(ReadError("devices: [{ type: 'teletype' }, "
                      "{ type: 'teletype', port: 3 }]"),
            "devices[1]: an Ozzy has only one teletype");
}

TEST(MachineSettingsTest, ScreenPage) {
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', page: 16 }]"),
            "devices[0].page: must be 0 to 15");
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', bank: 1 }]"),
            "devices[0].bank: the Ozzy has no bank 1");
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', bank: -1 }]"),
            "devices[0].bank: the Ozzy has no bank -1");
  EXPECT_EQ(ReadError("banks: [{}, {}], devices: [{ type: 'screen', bank: 1, "
                      "page: 0 }]"),
            "ok");
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', page: 0 }]"),
            "devices[0]: the screen's page 0 of bank 0 is the boot ROM");
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', page: 3 }]"),
            "devices[0]: the screen's page 3 of bank 0 is RAM");
  EXPECT_EQ(ReadError("banks: [{}, { ram: 0x8000 }], devices: [{ type: "
                      "'screen', bank: 1 }]"),
            "devices[0]: the screen's page 15 of bank 1 is RAM");
}

TEST(MachineSettingsTest, DefaultScreenPage) {
  EXPECT_EQ(ReadError("banks: [{ ram: 0xFFFE }]"),
            "devices: the screen's page 15 of bank 0 is RAM");
  EXPECT_EQ(ReadError("banks: [{ ram: 0xFFFE }], devices: []"), "ok");
  EXPECT_EQ(ReadError("banks: [{ ram: 0xFFFE }], devices: [{ type: "
                      "'screen', page: 0 }]"),
            "devices[0]: the screen's page 0 of bank 0 is the boot ROM");
}

}  // namespace
}  // namespace oz3
