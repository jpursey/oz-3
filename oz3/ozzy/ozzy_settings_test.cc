// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/ozzy_settings.h"

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/configs/console_config.h"
#include "oz3/configs/teletype_config.h"
#include "oz3/core/core_types.h"
#include "oz3/core/processor_config.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/screen.h"
#include "oz3/devices/teletype.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::EndsWith;
using ::testing::StartsWith;

// Returns the error message from reading `text`, or "ok" if it reads.
std::string ReadError(std::string_view text) {
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(std::string(text));
  if (settings.ok()) {
    return "ok";
  }
  EXPECT_EQ(settings.status().code(), absl::StatusCode::kInvalidArgument);
  return std::string(settings.status().message());
}

//==============================================================================
// Device settings
//==============================================================================

TEST(OzzySettingsTest, ScreenSettings) {
  // The defaults are the screen's own, with the Ozzy's wiring.
  ScreenSettings expected;
  expected.bank = 0;
  expected.page = 15;
  expected.interrupt = {.interrupt = 6, .cores = kAllCores};
  EXPECT_EQ(GetOzzyScreenSettings(OzzyScreenSettings()), expected);

  OzzyScreenSettings screen = {.columns = 40,
                               .rows = 12,
                               .refresh_hz = 60,
                               .slow_blink_hz = 2,
                               .fast_blink_hz = 8,
                               .characters = std::vector<uint16_t>{1, 2},
                               .draw_pixels = false};
  expected.columns = 40;
  expected.rows = 12;
  expected.refresh_hz = 60;
  expected.slow_blink_hz = 2;
  expected.fast_blink_hz = 8;
  expected.characters = std::vector<uint16_t>{1, 2};
  expected.draw_pixels = false;
  EXPECT_EQ(GetOzzyScreenSettings(screen), expected);
}

TEST(OzzySettingsTest, KeyboardSettings) {
  // Key state is on ports 6 to 15, the last of the ports kept for devices.
  const KeyboardSettings keyboard = GetOzzyKeyboardSettings();
  EXPECT_EQ(keyboard.state_port, 6);
  EXPECT_EQ(keyboard.typed_port, 1);
  EXPECT_EQ(keyboard.typed_interrupt.interrupt, 1);
  EXPECT_EQ(keyboard.typed_interrupt.cores, kAllCores);
  EXPECT_TRUE(keyboard.typed_interrupt.shared);
}

TEST(OzzySettingsTest, TeletypeSettings) {
  // The defaults are the teletype's own, with the Ozzy's wiring.
  TeletypeSettings expected;
  expected.port = 2;
  EXPECT_EQ(GetOzzyTeletypeSettings(OzzyTeletypeSettings()), expected);

  expected.baud = 300;
  expected.buffer_size = 64;
  EXPECT_EQ(GetOzzyTeletypeSettings({.baud = 300, .buffer_size = 64}),
            expected);
}

//==============================================================================
// Defaults
//==============================================================================

TEST(OzzySettingsTest, DefaultOzzy) {
  const OzzySettings settings;
  EXPECT_EQ(settings.clock_hz, ProcessorConfig::kDefaultClockHz);
  EXPECT_EQ(settings.cores, 1);
  EXPECT_EQ(settings.ports, 32);
  EXPECT_THAT(settings.banks, ElementsAre(0x7FFE));
  EXPECT_EQ(settings.screen, OzzyScreenSettings());
  EXPECT_TRUE(settings.keyboard);
  EXPECT_FALSE(settings.teletype.has_value());
}

TEST(OzzySettingsTest, EmptyConfigIsDefaultOzzy) {
  for (std::string_view text : {"", "  // Nothing but a comment\n"}) {
    absl::StatusOr<OzzySettings> settings = ReadOzzySettings(std::string(text));
    ASSERT_TRUE(settings.ok()) << settings.status();
    EXPECT_EQ(*settings, OzzySettings());
  }
}

//==============================================================================
// The processor and banks
//==============================================================================

TEST(OzzySettingsTest, ReadsMachine) {
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(R"(
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

TEST(OzzySettingsTest, CommasAreOptional) {
  // One entry a line, without commas.
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(R"(
    cores: 4
    banks: [
      { ram: 0x7FFE }
      { ram: 0xFFFF }
    ]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(settings->cores, 4);
  EXPECT_THAT(settings->banks, ElementsAre(0x7FFE, 0xFFFF));

  // A comma after the last entry.
  settings = ReadOzzySettings(R"(
    cores: 4,
    banks: [
      { ram: 0x7FFE },
      { ram: 0xFFFF },
    ],
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(settings->cores, 4);
  EXPECT_THAT(settings->banks, ElementsAre(0x7FFE, 0xFFFF));
}

TEST(OzzySettingsTest, ReadSettingsAreValidated) {
  // ValidateOzzySettings is tested below; this only shows reading uses it.
  EXPECT_EQ(ReadError("cores: 9"), "cores: must be 1 to 8");
  EXPECT_EQ(ReadError("banks: []"), "banks: must have 1 to 16 banks");
  EXPECT_EQ(ReadError("banks: [{ ram: 0xFFFF }]"),
            "banks[0]: RAM can't be on the boot ROM's page");
}

TEST(OzzySettingsTest, ValuesMustFit) {
  EXPECT_EQ(ReadError("banks: [{}, { ram: -1 }]"),
            "(1:20): banks[1].ram: must be 0 to 65535");
  EXPECT_EQ(ReadError("banks: [{}, { ram: 0x10000 }]"),
            "(1:20): banks[1].ram: must be 0 to 65535");
  EXPECT_EQ(ReadError("cores: 0x80000000"),
            "(1:8): cores: must be -2147483648 to 2147483647");
}

TEST(OzzySettingsTest, MachineErrors) {
  EXPECT_EQ(ReadError("core: 2"), "(1:7): core: unknown key");
  EXPECT_EQ(ReadError("cores: 2, zebra: 1, apple: 1"),
            "(1:28): apple: unknown key");
  EXPECT_EQ(ReadError("cores: 2.0"), "(1:8): cores: must be an int");
  EXPECT_EQ(ReadError("clock_hz: \"fast\""),
            "(1:11): clock_hz: must be an int");
  EXPECT_EQ(ReadError("ports: true"), "(1:8): ports: must be an int");
  EXPECT_EQ(ReadError("banks: { ram: 0x7FFE }"),
            "(1:8): banks: must be an array");
  EXPECT_EQ(ReadError("banks: [0x7FFE]"), "(1:9): banks[0]: must be a map");
  EXPECT_EQ(ReadError("banks: [{ rom: 1 }]"),
            "(1:16): banks[0].rom: unknown key");
  EXPECT_EQ(ReadError("banks: [{ ram: [1, 2] }]"),
            "(1:16): banks[0].ram: must be an int");
  EXPECT_EQ(ReadError("devices: {}"), "(1:10): devices: must be an array");
  EXPECT_EQ(ReadError("devices: [\"screen\"]"),
            "(1:11): devices[0]: must be a map");
}

TEST(OzzySettingsTest, SyntaxError) {
  EXPECT_THAT(ReadError("cores: ["), StartsWith("(1:9): "));
}

//==============================================================================
// Error locations
//==============================================================================

TEST(OzzySettingsTest, ErrorsNameTheFile) {
  // Each kind of error: in the text, in its syntax, and in the settings.
  constexpr std::string_view kText = R"(
    cores: 2,
    devices: [
      { type: "screen", colums: 40 }
    ]
  )";
  absl::StatusOr<OzzySettings> settings =
      ReadOzzySettings(std::string(kText), "hello.ozzy");
  EXPECT_EQ(settings.status(),
            absl::InvalidArgumentError(
                "hello.ozzy(4:33): devices[0].colums: unknown key"));
  settings = ReadOzzySettings("cores: [", "hello.ozzy");
  EXPECT_EQ(settings.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(settings.status().message(), StartsWith("hello.ozzy(1:9): "));
  settings = ReadOzzySettings("cores: 9", "hello.ozzy");
  EXPECT_EQ(settings.status(),
            absl::InvalidArgumentError("hello.ozzy: cores: must be 1 to 8"));
}

//==============================================================================
// Devices
//==============================================================================

TEST(OzzySettingsTest, NoDevices) {
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings("devices: []");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_FALSE(settings->screen.has_value());
  EXPECT_FALSE(settings->keyboard);
  EXPECT_FALSE(settings->teletype.has_value());
}

TEST(OzzySettingsTest, DevicesTakeDefaults) {
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(R"(
    devices: [{ type: "teletype" }, { type: "screen" }, { type: "keyboard" }]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(settings->screen, OzzyScreenSettings());
  EXPECT_TRUE(settings->keyboard);
  EXPECT_EQ(settings->teletype, OzzyTeletypeSettings());
}

TEST(OzzySettingsTest, OnlyListedDevices) {
  absl::StatusOr<OzzySettings> settings =
      ReadOzzySettings("devices: [{ type: 'teletype' }]");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_FALSE(settings->screen.has_value());
  EXPECT_FALSE(settings->keyboard);
  EXPECT_TRUE(settings->teletype.has_value());
}

TEST(OzzySettingsTest, ReadsScreen) {
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(R"(
    devices: [{
      type: "screen", columns: 40, rows: 12, refresh_hz: 60, slow_blink_hz: 2,
      fast_blink_hz: 8, characters: [0, 0x1234, 0xFFFF]
    }]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  ASSERT_TRUE(settings->screen.has_value());
  const OzzyScreenSettings& screen = *settings->screen;
  EXPECT_EQ(screen.columns, 40);
  EXPECT_EQ(screen.rows, 12);
  EXPECT_EQ(screen.refresh_hz, 60);
  EXPECT_EQ(screen.slow_blink_hz, 2);
  EXPECT_EQ(screen.fast_blink_hz, 8);
  ASSERT_TRUE(screen.characters.has_value());
  EXPECT_THAT(*screen.characters, ElementsAre(0, 0x1234, 0xFFFF));
}

TEST(OzzySettingsTest, ReadsTeletype) {
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(R"(
    devices: [{ type: "teletype", baud: 300, buffer_size: 64 }]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  ASSERT_TRUE(settings->teletype.has_value());
  EXPECT_EQ(settings->teletype->baud, 300);
  EXPECT_EQ(settings->teletype->buffer_size, 64);
}

TEST(OzzySettingsTest, DeviceSettingsCheckedByDevices) {
  // The devices check these themselves, so reading only checks they fit in an
  // int.
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(R"(
    devices: [
      { type: "screen", columns: 1000, rows: -1 },
      { type: "teletype", baud: -5, buffer_size: 0 }
    ]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(settings->screen->columns, 1000);
  EXPECT_EQ(settings->screen->rows, -1);
  EXPECT_EQ(settings->teletype->baud, -5);
  EXPECT_EQ(settings->teletype->buffer_size, 0);
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', columns: 0x80000000 }]"),
            "(1:38): devices[0].columns: must be -2147483648 to 2147483647");
}

TEST(OzzySettingsTest, WiringIsFixed) {
  // A device's wiring is the Ozzy's, so none of it is a key.
  for (std::string_view key :
       {"bank", "page", "interrupt", "interrupt_cores", "port"}) {
    for (std::string_view type : {"screen", "keyboard", "teletype"}) {
      EXPECT_THAT(
          ReadError(
              absl::StrCat("devices: [{ type: '", type, "', ", key, ": 1 }]")),
          EndsWith(absl::StrCat("): devices[0].", key, ": unknown key")))
          << type;
    }
  }
}

//==============================================================================
// Device errors
//==============================================================================

TEST(OzzySettingsTest, DeviceTypeErrors) {
  EXPECT_EQ(ReadError("devices: [{ port: 1 }]"),
            "(1:11): devices[0]: missing type");
  EXPECT_EQ(ReadError("devices: [{ type: 1 }]"),
            "(1:19): devices[0].type: must be \"screen\", \"keyboard\", or "
            "\"teletype\"");
  EXPECT_EQ(ReadError("devices: [{ type: 'disk' }]"),
            "(1:19): devices[0].type: must be \"screen\", \"keyboard\", or "
            "\"teletype\"");
}

TEST(OzzySettingsTest, DeviceUnknownKeys) {
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', colums: 40 }]"),
            "(1:37): devices[0].colums: unknown key");
  EXPECT_EQ(ReadError("devices: [{ type: 'keyboard', page: 3 }]"),
            "(1:37): devices[0].page: unknown key");
  EXPECT_EQ(ReadError("devices: [{ type: 'teletype', interrupt: 3 }]"),
            "(1:42): devices[0].interrupt: unknown key");
}

TEST(OzzySettingsTest, DeviceWrongTypes) {
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', rows: '24' }]"),
            "(1:35): devices[0].rows: must be an int");
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', characters: 0 }]"),
            "(1:41): devices[0].characters: must be an array");
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', characters: [0, -1] }]"),
            "(1:45): devices[0].characters[1]: must be 0 to 65535");
  EXPECT_EQ(ReadError("devices: [{ type: 'teletype', baud: 'fast' }]"),
            "(1:37): devices[0].baud: must be an int");
}

TEST(OzzySettingsTest, OneDeviceOfEachType) {
  EXPECT_EQ(ReadError("devices: [{ type: 'teletype' }, { type: 'screen' }, "
                      "{ type: 'screen', columns: 40 }]"),
            "(1:53): devices[2]: an Ozzy has only one screen");
  EXPECT_EQ(ReadError("devices: [{ type: 'keyboard' }, { type: 'keyboard' }]"),
            "(1:33): devices[1]: an Ozzy has only one keyboard");
  EXPECT_EQ(ReadError("devices: [{ type: 'teletype' }, "
                      "{ type: 'teletype', baud: 300 }]"),
            "(1:33): devices[1]: an Ozzy has only one teletype");
}

TEST(OzzySettingsTest, ScreenPage) {
  // The screen is always page 15 of bank 0, which must not be RAM.
  EXPECT_EQ(ReadError("banks: [{ ram: 0xFFFE }]"),
            "screen: page 15 of bank 0 is RAM");
  EXPECT_EQ(ReadError("banks: [{ ram: 0xFFFE }], devices: [{ type: "
                      "'keyboard' }, { type: 'teletype' }]"),
            "ok");
  EXPECT_EQ(ReadError("banks: [{ ram: 0x7FFE }, { ram: 0xFFFF }]"), "ok");
}

//==============================================================================
// Validating settings built in code
//==============================================================================

// Returns the error message from validating `settings`, or "ok" if they are
// valid.
std::string ValidateError(const OzzySettings& settings) {
  absl::Status status = ValidateOzzySettings(settings);
  if (status.ok()) {
    return "ok";
  }
  EXPECT_EQ(status.code(), absl::StatusCode::kInvalidArgument);
  return std::string(status.message());
}

TEST(OzzySettingsTest, ValidateDefaultOzzy) {
  EXPECT_EQ(ValidateError(OzzySettings()), "ok");
}

TEST(OzzySettingsTest, ValidateClockHz) {
  OzzySettings settings;
  settings.clock_hz = 1;
  EXPECT_EQ(ValidateError(settings), "ok");
  settings.clock_hz = OzzySettings::kMaxClockHz;
  EXPECT_EQ(ValidateError(settings), "ok");
  settings.clock_hz = 0;
  EXPECT_EQ(ValidateError(settings), "clock_hz: must be 1 to 100000000");
  settings.clock_hz = OzzySettings::kMaxClockHz + 1;
  EXPECT_EQ(ValidateError(settings), "clock_hz: must be 1 to 100000000");
}

TEST(OzzySettingsTest, ValidateCoresAndPorts) {
  OzzySettings settings;
  settings.cores = 8;
  settings.ports = 16;
  EXPECT_EQ(ValidateError(settings), "ok");
  settings.ports = 256;
  EXPECT_EQ(ValidateError(settings), "ok");
  settings.ports = 15;
  EXPECT_EQ(ValidateError(settings), "ports: must be 16 to 256");
  settings.ports = 257;
  EXPECT_EQ(ValidateError(settings), "ports: must be 16 to 256");
  settings.ports = 32;
  settings.cores = 0;
  EXPECT_EQ(ValidateError(settings), "cores: must be 1 to 8");
  settings.cores = 9;
  EXPECT_EQ(ValidateError(settings), "cores: must be 1 to 8");
}

TEST(OzzySettingsTest, ValidateBanks) {
  OzzySettings settings;
  settings.banks.assign(16, 0);
  EXPECT_EQ(ValidateError(settings),
            "banks: the Ozzy has no RAM, which the boot ROM needs for its "
            "stack");
  settings.banks[15] = 0x8000;
  EXPECT_EQ(ValidateError(settings), "ok");
  settings.banks.assign(17, 0);
  EXPECT_EQ(ValidateError(settings), "banks: must have 1 to 16 banks");
  settings.banks.clear();
  EXPECT_EQ(ValidateError(settings), "banks: must have 1 to 16 banks");
  settings.banks = {0x0001};
  EXPECT_EQ(ValidateError(settings),
            "banks[0]: RAM can't be on the boot ROM's page");
  settings.banks = {0x7FFE, 0xFFFF};
  EXPECT_EQ(ValidateError(settings), "ok");
}

TEST(OzzySettingsTest, ValidateScreenPage) {
  OzzySettings settings;
  settings.banks = {0xFFFE};
  EXPECT_EQ(ValidateError(settings), "screen: page 15 of bank 0 is RAM");
  settings.screen.reset();
  EXPECT_EQ(ValidateError(settings), "ok");
}

//==============================================================================
// The configs in configs/
//==============================================================================

TEST(OzzySettingsTest, ConsoleConfigIsTheDefaultOzzy) {
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(
      std::string(kConsoleConfig.GetText()), "configs/console.ozzy");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(*settings, OzzySettings());
}

TEST(OzzySettingsTest, TeletypeConfig) {
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(
      std::string(kTeletypeConfig.GetText()), "configs/teletype.ozzy");
  ASSERT_TRUE(settings.ok()) << settings.status();
  OzzySettings expected;
  expected.banks = {0xFFFE};
  expected.screen.reset();
  expected.teletype = OzzyTeletypeSettings();
  EXPECT_EQ(*settings, expected);
}

}  // namespace
}  // namespace oz3
