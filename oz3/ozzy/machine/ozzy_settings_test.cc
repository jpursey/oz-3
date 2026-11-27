// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/machine/ozzy_settings.h"

#include <cstdint>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/time/time.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/configs/console_config.h"
#include "oz3/configs/teletype_config.h"
#include "oz3/core/core_types.h"
#include "oz3/core/processor_config.h"
#include "oz3/devices/disk_drive/disk_drive.h"
#include "oz3/devices/keyboard/keyboard.h"
#include "oz3/devices/screen/screen.h"
#include "oz3/devices/teletype/teletype.h"
#include "oz3/devices/timer/timer.h"
#include "oz3/disk/disk_hardware.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::EndsWith;
using ::testing::IsEmpty;
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

TEST(OzzySettingsTest, TimerSettings) {
  // The defaults are the timer's own, with the Ozzy's wiring: its interrupt is
  // on every core, and not shared.
  TimerSettings expected;
  expected.port = 3;
  expected.interrupt = {.interrupt = 0, .cores = kAllCores};
  EXPECT_EQ(GetOzzyTimerSettings(OzzyTimerSettings()), expected);

  expected.tick_hz = 100;
  EXPECT_EQ(GetOzzyTimerSettings({.tick_hz = 100}), expected);
}

TEST(OzzySettingsTest, DriveSettings) {
  // Each drive has 3 ports from 16, and an interrupt from 2, on core 0 alone.
  const OzzyDriveSettings drive = {
      .slots = 9,
      .hardware = GetDiskDrivePreset(DiskDrivePreset::kHardDisk20m),
      .image = "a.ozd"};
  for (int i = 0; i < kOzzyDriveCount; ++i) {
    const DiskDriveSettings expected = {
        .port = 16 + 3 * i,
        .interrupt = {.interrupt = 2 + i, .cores = 0x01},
        .slots = 9,
        .hardware = GetDiskDrivePreset(DiskDrivePreset::kHardDisk20m)};
    EXPECT_EQ(GetOzzyDriveSettings(i, drive), expected) << i;
  }

  // The defaults are the drive's.
  const DiskDriveSettings settings = GetOzzyDriveSettings(0, {});
  EXPECT_EQ(settings.slots, DiskDriveSettings().slots);
  EXPECT_EQ(settings.hardware, DiskDriveSettings().hardware);
  EXPECT_EQ(GetOzzyDriveLetter(0), 'A');
  EXPECT_EQ(GetOzzyDriveLetter(3), 'D');
}

//==============================================================================
// Defaults
//==============================================================================

TEST(OzzySettingsTest, DefaultOzzy) {
  const OzzySettings settings;
  EXPECT_EQ(settings.clock_hz, ProcessorConfig::kDefaultClockHz);
  EXPECT_EQ(settings.cores, 1);
  EXPECT_EQ(settings.ports, 48);
  EXPECT_THAT(settings.banks, ElementsAre(0x7FFE));
  EXPECT_EQ(settings.screen, OzzyScreenSettings());
  EXPECT_TRUE(settings.keyboard);
  EXPECT_FALSE(settings.teletype.has_value());
  EXPECT_EQ(settings.timer, OzzyTimerSettings());
  EXPECT_THAT(settings.drives, IsEmpty());
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
  EXPECT_FALSE(settings->timer.has_value());
}

TEST(OzzySettingsTest, DevicesTakeDefaults) {
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(R"(
    devices: [
      { type: "teletype" }, { type: "screen" }, { type: "keyboard" },
      { type: "timer" }
    ]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(settings->screen, OzzyScreenSettings());
  EXPECT_TRUE(settings->keyboard);
  EXPECT_EQ(settings->teletype, OzzyTeletypeSettings());
  EXPECT_EQ(settings->timer, OzzyTimerSettings());
}

TEST(OzzySettingsTest, OnlyListedDevices) {
  absl::StatusOr<OzzySettings> settings =
      ReadOzzySettings("devices: [{ type: 'teletype' }]");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_FALSE(settings->screen.has_value());
  EXPECT_FALSE(settings->keyboard);
  EXPECT_TRUE(settings->teletype.has_value());
  EXPECT_FALSE(settings->timer.has_value());
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

TEST(OzzySettingsTest, ReadsTimer) {
  absl::StatusOr<OzzySettings> settings =
      ReadOzzySettings("devices: [{ type: 'timer', tick_hz: 100 }]");
  ASSERT_TRUE(settings.ok()) << settings.status();
  ASSERT_TRUE(settings->timer.has_value());
  EXPECT_EQ(settings->timer->tick_hz, 100);
}

TEST(OzzySettingsTest, DeviceSettingsCheckedByDevices) {
  // The devices check these themselves, so reading only checks they fit in an
  // int.
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(R"(
    devices: [
      { type: "screen", columns: 1000, rows: -1 },
      { type: "teletype", baud: -5, buffer_size: 0 },
      { type: "timer", tick_hz: 0 }
    ]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(settings->screen->columns, 1000);
  EXPECT_EQ(settings->screen->rows, -1);
  EXPECT_EQ(settings->teletype->baud, -5);
  EXPECT_EQ(settings->teletype->buffer_size, 0);
  EXPECT_EQ(settings->timer->tick_hz, 0);
  EXPECT_EQ(ReadError("devices: [{ type: 'screen', columns: 0x80000000 }]"),
            "(1:38): devices[0].columns: must be -2147483648 to 2147483647");
}

TEST(OzzySettingsTest, WiringIsFixed) {
  // A device's wiring is the Ozzy's, so none of it is a key.
  for (std::string_view key :
       {"bank", "page", "interrupt", "interrupt_cores", "port"}) {
    for (std::string_view type : {"screen", "keyboard", "teletype", "timer"}) {
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
            "(1:19): devices[0].type: must be \"screen\", \"keyboard\", "
            "\"teletype\", or \"timer\"");
  EXPECT_EQ(ReadError("devices: [{ type: 'disk' }]"),
            "(1:19): devices[0].type: must be \"screen\", \"keyboard\", "
            "\"teletype\", or \"timer\"");
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
  EXPECT_EQ(ReadError("devices: [{ type: 'timer', tick_hz: 60.5 }]"),
            "(1:37): devices[0].tick_hz: must be an int");
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
  EXPECT_EQ(ReadError("devices: [{ type: 'timer' }, { type: 'timer' }]"),
            "(1:30): devices[1]: an Ozzy has only one timer");
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
// Drives
//==============================================================================

TEST(OzzySettingsTest, NoDrives) {
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings("drives: []");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_THAT(settings->drives, IsEmpty());
}

TEST(OzzySettingsTest, ReadsDrives) {
  // Drives are lettered in order, so the first is A:.
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(R"(
    drives: [
      { preset: "floppy-720k" },
      {
        preset: "hd-20m",
        read_cache_sectors: 8,
        read_words_per_sec: 1000,
        write_words_per_sec: 2000,
        cache_words_per_sec: 3000,
        latency_ms: 30,
        slots: 16,
        image: "disks/work.ozd"
      }
    ]
  )");
  ASSERT_TRUE(settings.ok()) << settings.status();
  ASSERT_EQ(settings->drives.size(), 2);
  EXPECT_EQ(settings->drives[0],
            (OzzyDriveSettings{
                .hardware = GetDiskDrivePreset(DiskDrivePreset::kFloppy720k)}));
  EXPECT_EQ(settings->drives[1],
            (OzzyDriveSettings{.slots = 16,
                               .hardware = {.read_cache_sectors = 8,
                                            .read_words_per_sec = 1000,
                                            .write_words_per_sec = 2000,
                                            .cache_words_per_sec = 3000,
                                            .latency = absl::Milliseconds(30)},
                               .image = "disks/work.ozd"}));
}

TEST(OzzySettingsTest, DrivePresets) {
  const std::pair<std::string_view, DiskDrivePreset> kPresets[] = {
      {"floppy-360k", DiskDrivePreset::kFloppy360k},
      {"floppy-720k", DiskDrivePreset::kFloppy720k},
      {"floppy-1440k", DiskDrivePreset::kFloppy1440k},
      {"hd-10m", DiskDrivePreset::kHardDisk10m},
      {"hd-20m", DiskDrivePreset::kHardDisk20m},
      {"hd-40m", DiskDrivePreset::kHardDisk40m},
  };
  for (const auto& [name, preset] : kPresets) {
    absl::StatusOr<OzzySettings> settings =
        ReadOzzySettings(absl::StrCat("drives: [{ preset: '", name, "' }]"));
    ASSERT_TRUE(settings.ok()) << settings.status();
    ASSERT_EQ(settings->drives.size(), 1);
    EXPECT_EQ(settings->drives[0].hardware, GetDiskDrivePreset(preset)) << name;
  }
}

TEST(OzzySettingsTest, DriveSettingsChangeThePresetInAnyOrder) {
  // A map has no order, so a setting before the preset still changes it.
  absl::StatusOr<OzzySettings> settings = ReadOzzySettings(
      "drives: [{ latency_ms: 30, preset: 'hd-20m', read_cache_sectors: 0 }]");
  ASSERT_TRUE(settings.ok()) << settings.status();
  ASSERT_EQ(settings->drives.size(), 1);
  DiskHardwareSettings expected =
      GetDiskDrivePreset(DiskDrivePreset::kHardDisk20m);
  expected.latency = absl::Milliseconds(30);
  expected.read_cache_sectors = 0;
  EXPECT_EQ(settings->drives[0].hardware, expected);
}

TEST(OzzySettingsTest, ImagesAreFromTheConfigsFolder) {
  constexpr std::string_view kText = R"(
    drives: [
      { preset: "floppy-1440k", image: "disks/a.ozd" },
      { preset: "floppy-1440k", image: "/abs/b.ozd" },
      { preset: "floppy-1440k", image: "../other\\c.ozd" },
      { preset: "floppy-1440k", image: "c:/d.ozd" },
    ]
  )";
  absl::StatusOr<OzzySettings> settings =
      ReadOzzySettings(std::string(kText), "configs/work.ozzy");
  ASSERT_TRUE(settings.ok()) << settings.status();
  ASSERT_EQ(settings->drives.size(), 4);
  EXPECT_EQ(settings->drives[0].image, "configs/disks/a.ozd");
  EXPECT_EQ(settings->drives[1].image, "/abs/b.ozd");
  EXPECT_EQ(settings->drives[2].image, "other/c.ozd");
  EXPECT_EQ(settings->drives[3].image, "c:/d.ozd");

  // In an absolute folder, with a protocol (as a Windows drive is).
  settings = ReadOzzySettings(std::string(kText), "C:\\ozzy\\work.ozzy");
  ASSERT_TRUE(settings.ok()) << settings.status();
  EXPECT_EQ(settings->drives[0].image, "c:/ozzy/disks/a.ozd");
  EXPECT_EQ(settings->drives[1].image, "/abs/b.ozd");
  EXPECT_EQ(settings->drives[2].image, "c:/other/c.ozd");
  EXPECT_EQ(settings->drives[3].image, "c:/d.ozd");
}

TEST(OzzySettingsTest, ImagesWithNoFolderStayRelative) {
  constexpr std::string_view kText =
      "drives: [{ preset: 'floppy-1440k', image: 'disks/./a.ozd' }]";
  for (std::string_view path : {"", "work.ozzy"}) {
    absl::StatusOr<OzzySettings> settings =
        ReadOzzySettings(std::string(kText), path);
    ASSERT_TRUE(settings.ok()) << settings.status();
    ASSERT_EQ(settings->drives.size(), 1);
    EXPECT_EQ(settings->drives[0].image, "disks/a.ozd") << path;
  }
}

TEST(OzzySettingsTest, DriveErrors) {
  EXPECT_THAT(ReadError("drives: [{ preset: 'floppy-1440k' }, {}]"),
              EndsWith("drives[1]: missing preset"));
  EXPECT_THAT(ReadError("drives: [{ preset: 'floppy-1.44m' }]"),
              EndsWith("drives[0].preset: must be \"floppy-360k\", "
                       "\"floppy-720k\", \"floppy-1440k\", \"hd-10m\", "
                       "\"hd-20m\", or \"hd-40m\""));
  EXPECT_THAT(ReadError("drives: [{ preset: 'hd-10m', letter: 'A' }]"),
              EndsWith("drives[0].letter: unknown key"));
  EXPECT_THAT(ReadError("drives: [{ preset: 'hd-10m', image: '' }]"),
              EndsWith("drives[0].image: must be a path"));
  EXPECT_THAT(ReadError("drives: [{ preset: 'hd-10m', image: ':a.ozd' }]"),
              EndsWith("drives[0].image: isn't a valid path: :a.ozd"));
  EXPECT_THAT(ReadError("drives: [{ preset: 'hd-10m', image: 5 }]"),
              EndsWith("drives[0].image: must be a string"));
  EXPECT_THAT(ReadError("drives: [{ preset: 'hd-10m', latency_ms: 'a' }]"),
              EndsWith("drives[0].latency_ms: must be an int"));
  EXPECT_THAT(ReadError("drives: [{ preset: 'hd-10m', slots: 1.5 }]"),
              EndsWith("drives[0].slots: must be an int"));
  EXPECT_THAT(ReadError("drives: ['floppy-1440k']"),
              EndsWith("drives[0]: must be a map"));
  EXPECT_THAT(ReadError("drives: { preset: 'hd-10m' }"),
              EndsWith("drives: must be an array"));
}

TEST(OzzySettingsTest, AtMostFourDrives) {
  constexpr std::string_view kDrive = "{ preset: 'floppy-1440k' },";
  EXPECT_EQ(
      ReadError(absl::StrCat("drives: [", kDrive, kDrive, kDrive, kDrive, "]")),
      "ok");
  EXPECT_THAT(ReadError(absl::StrCat("drives: [", kDrive, kDrive, kDrive,
                                     kDrive, kDrive, "]")),
              EndsWith("drives: must have 0 to 4 values"));
}

TEST(OzzySettingsTest, ReadDrivesAreValidated) {
  // One image in two drives is the Ozzy's own rule, so it has no line.
  EXPECT_EQ(ReadError(R"(drives: [
                           { preset: 'hd-10m', image: 'a.ozd' },
                           { preset: 'hd-10m', image: './a.ozd' }
                         ])"),
            "drive B: image a.ozd is also drive A's");

  // The drive checks its own settings' ranges, as the Ozzy is created.
  EXPECT_EQ(ReadError("drives: [{ preset: 'hd-10m', slots: 0 }]"), "ok");
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
  // Without the timer, whose default rate is faster than the slowest clock.
  OzzySettings settings;
  settings.timer.reset();
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
  settings.ports = 32;
  EXPECT_EQ(ValidateError(settings), "ok");
  settings.ports = 256;
  EXPECT_EQ(ValidateError(settings), "ok");
  settings.ports = 31;
  EXPECT_EQ(ValidateError(settings), "ports: must be 32 to 256");
  settings.ports = 257;
  EXPECT_EQ(ValidateError(settings), "ports: must be 32 to 256");
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
            "banks[0]: page 1 must be RAM, for the boot ROM's stack");
  settings.banks[15] = 0xFFFF;
  EXPECT_EQ(ValidateError(settings),
            "banks[0]: page 1 must be RAM, for the boot ROM's stack");
  settings.banks[0] = 0x0004;
  EXPECT_EQ(ValidateError(settings),
            "banks[0]: page 1 must be RAM, for the boot ROM's stack");
  settings.banks[0] = 0x0002;
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

TEST(OzzySettingsTest, ValidateTimerTickHz) {
  // The machine table holds the rate in a word, and the timer ticks at most
  // once a cycle. The timer checks the rest.
  OzzySettings settings;
  settings.timer->tick_hz = 65535;
  EXPECT_EQ(ValidateError(settings), "ok");
  settings.timer->tick_hz = 65536;
  EXPECT_EQ(ValidateError(settings), "timer: tick_hz must be at most 65535");
  settings.timer.reset();
  EXPECT_EQ(ValidateError(settings), "ok");
  EXPECT_EQ(ReadError("devices: [{ type: 'timer', tick_hz: 100000 }]"),
            "timer: tick_hz must be at most 65535");
}

TEST(OzzySettingsTest, ValidateDriveImages) {
  OzzySettings settings;
  settings.drives = {
      {.image = "disks/a.ozd"}, {}, {.image = "disks/c.ozd"}, {}};
  EXPECT_EQ(ValidateError(settings), "ok");

  // Paths are compared normalized.
  settings.drives[3].image = "disks/./a.ozd";
  EXPECT_EQ(ValidateError(settings),
            "drive D: image disks/./a.ozd is also drive A's");
  settings.drives[3].image = "disks\\c.ozd";
  EXPECT_EQ(ValidateError(settings),
            "drive D: image disks\\c.ozd is also drive C's");
  settings.drives[1].image = "disks/c.ozd";
  EXPECT_EQ(ValidateError(settings),
            "drive C: image disks/c.ozd is also drive B's");
}

TEST(OzzySettingsTest, ValidateDriveCount) {
  OzzySettings settings;
  for (int count : {0, 1, kOzzyDriveCount}) {
    settings.drives.resize(count);
    EXPECT_EQ(ValidateError(settings), "ok") << count;
  }
  settings.drives.resize(kOzzyDriveCount + 1);
  EXPECT_EQ(ValidateError(settings), "drives: must have at most 4 drives");
}

TEST(OzzySettingsTest, ValidateTimerTickHzAgainstClockHz) {
  OzzySettings settings;
  settings.clock_hz = 60;
  settings.timer->tick_hz = 60;
  EXPECT_EQ(ValidateError(settings), "ok");
  settings.timer->tick_hz = 61;
  EXPECT_EQ(ValidateError(settings),
            "timer: tick_hz must be at most clock_hz (60)");
  settings.timer.reset();
  EXPECT_EQ(ValidateError(settings), "ok");

  // The default timer's 60 ticks a second is too fast for a 10 Hz Ozzy.
  EXPECT_EQ(ReadError("clock_hz: 10"),
            "timer: tick_hz must be at most clock_hz (10)");
  EXPECT_EQ(
      ReadError("clock_hz: 10, devices: [{ type: 'timer', tick_hz: 10 }]"),
      "ok");
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
  expected.timer = OzzyTimerSettings();
  EXPECT_EQ(*settings, expected);
}

}  // namespace
}  // namespace oz3
