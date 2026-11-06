// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/boot_rom.h"

#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/time/time.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/devices/screen.h"
#include "oz3/ozzy/machine_table.h"
#include "oz3/ozzy/ozzy.h"
#include "oz3/ozzy/ozzy_settings.h"
#include "oz3/tools/program_map.h"

namespace oz3 {
namespace {

using ::testing::IsEmpty;

//==============================================================================
// Helpers
//==============================================================================

// Where a core's stack is: its bank, and its base register.
struct StackPlace {
  int bank = 0;
  uint16_t base = 0;
};

// Returns whether core 0 has halted.
bool IsHalted(Ozzy& ozzy) {
  return ozzy.GetProcessor()->GetCore(0)->GetState() == CpuCore::State::kIdle;
}

// Returns an Ozzy for `settings` that has run until the boot ROM halts, and for
// its screen to refresh since, or null if it can't be created.
std::unique_ptr<Ozzy> BootOzzy(const OzzySettings& settings) {
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(settings);
  EXPECT_TRUE(ozzy.ok()) << ozzy.status();
  if (!ozzy.ok()) {
    return nullptr;
  }
  Ozzy& booted = **ozzy;

  // In steps of a millisecond, for up to a minute, so a slow clock has time
  // and a fast one doesn't run far past the halt.
  for (int step = 0; step < 60'000 && !IsHalted(booted); ++step) {
    booted.Run(absl::Milliseconds(1));
  }
  EXPECT_TRUE(IsHalted(booted)) << "The boot ROM didn't halt";

  // Long enough for the screen to refresh, 30 times a second, and the teletype
  // to print the character its port holds.
  booted.Run(absl::Milliseconds(34));
  return std::move(*ozzy);
}

// Returns where core 0's stack is.
StackPlace GetStackPlace(Ozzy& ozzy) {
  CpuCore::Registers registers;
  ozzy.GetProcessor()->GetCore(0)->GetRegisters(registers);
  return {.bank = CpuCore::Banks::FromWord(registers[CpuCore::MB]).stack,
          .base = registers[CpuCore::BS]};
}

// Returns the text of row `row` of `picture`, which must be plain characters.
std::string GetRowText(const ScreenPicture& picture, int row) {
  std::string text;
  for (int column = 0; column < picture.columns; ++column) {
    const uint16_t cell = picture.cells[row * picture.columns + column];
    EXPECT_EQ(cell >> 8, 0) << "Row " << row << ", column " << column;
    text.push_back(static_cast<char>(cell & 0xFF));
  }
  return text;
}

// Returns `text` padded with spaces to `columns`.
std::string PadRow(std::string_view text, int columns) {
  std::string row(text);
  row.resize(columns, ' ');
  return row;
}

// Expects the screen to show `rows` of text, and the cursor at the start of
// the third row, below the banner and a blank line, as it always is once the
// boot ROM is done.
void ExpectScreen(Ozzy& ozzy, const std::vector<std::string_view>& rows) {
  ASSERT_NE(ozzy.GetScreen(), nullptr);
  const ScreenPicture& picture = ozzy.GetScreen()->GetPicture();
  ASSERT_EQ(static_cast<int>(rows.size()), picture.rows);
  for (int row = 0; row < picture.rows; ++row) {
    EXPECT_EQ(GetRowText(picture, row), PadRow(rows[row], picture.columns))
        << "Row " << row;
  }
  EXPECT_EQ(picture.cursor_column, 0);
  EXPECT_EQ(picture.cursor_row, 2);
  EXPECT_EQ(picture.cursor_mode, ScreenCursorMode::kSlowBlink);
  EXPECT_EQ(picture.cursor_style, ScreenCursorStyle::kUnderline);
}

// Returns the rows of the default 80 by 24 screen, with `banner` on the first.
std::vector<std::string_view> GetBannerRows(std::string_view banner) {
  std::vector<std::string_view> rows(24, "");
  rows[0] = banner;
  return rows;
}

//==============================================================================
// The source
//==============================================================================

TEST(BootRomTest, SourceKnowsTheMachineTable) {
  // The source's own copy of the table's layout matches machine_table.h.
  std::map<std::string_view, uint32_t> expected = {
      {"TABLE", kMachineTableAddress},
      {"TABLE_CLOCK_HZ", kMachineTableAddress + kMachineHeaderClockHzLow},
      {"TABLE_CORES", kMachineTableAddress + kMachineHeaderCores},
      {"TABLE_BANKS", kMachineTableAddress + kMachineHeaderBanks},
      {"TABLE_DEVICES", kMachineTableAddress + kMachineHeaderDevices},
      {"TABLE_BANK_ENTRIES", kMachineTableAddress + kMachineTableBanks},
      {"TABLE_DEVICE_ENTRIES", kMachineTableAddress + kMachineTableDevices},
      {"BANK_RAM", kMachineBankRam},
      {"BANK_SIZE", kMachineBankSize},
      {"DEVICE_TYPE", kMachineDeviceType},
      {"DEVICE_PORT", kMachineDevicePort},
      {"DEVICE_BANK", kMachineDeviceBank},
      {"DEVICE_PAGE", kMachineDevicePage},
      {"DEVICE_COLUMNS", kMachineDeviceData},
      {"DEVICE_ROWS", kMachineDeviceData + 1},
      {"DEVICE_SIZE", kMachineDeviceSize},
      {"SCREEN", static_cast<uint32_t>(MachineDeviceType::kScreen)},
      {"TELETYPE", static_cast<uint32_t>(MachineDeviceType::kTeletype)},
  };
  for (const ProgramMap::Constant& constant :
       GetBootRom().GetMap().GetConstants()) {
    if (auto it = expected.find(constant.name); it != expected.end()) {
      EXPECT_EQ(constant.dword_value, it->second) << it->first;
      expected.erase(it);
    }
  }
  EXPECT_THAT(expected, IsEmpty());
}

//==============================================================================
// The banner on the screen
//==============================================================================

TEST(BootRomTest, DefaultOzzy) {
  std::unique_ptr<Ozzy> ozzy = BootOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, GetBannerRows("OZZY 0.1  1 MHz  1 core  56K RAM"));
  EXPECT_TRUE(IsHalted(*ozzy));
}

TEST(BootRomTest, Cores) {
  OzzySettings settings;
  settings.cores = 2;
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, GetBannerRows("OZZY 0.1  1 MHz  2 cores  56K RAM"));

  // The other cores are left idle.
  EXPECT_EQ(ozzy->GetProcessor()->GetCore(1)->GetState(),
            CpuCore::State::kIdle);

  settings.cores = kMaxCores;
  ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, GetBannerRows("OZZY 0.1  1 MHz  8 cores  56K RAM"));
}

TEST(BootRomTest, LeastRam) {
  OzzySettings settings;
  settings.banks = {0x0002};
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, GetBannerRows("OZZY 0.1  1 MHz  1 core  4K RAM"));
}

TEST(BootRomTest, MostRam) {
  // Every page but the boot ROM's and the screen's.
  OzzySettings settings;
  settings.banks.assign(kMaxMemoryBanks, 0xFFFF);
  settings.banks[0] = 0x7FFE;
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, GetBannerRows("OZZY 0.1  1 MHz  1 core  1016K RAM"));
}

TEST(BootRomTest, ClockRate) {
  // The clock rate is in the largest unit it is a whole number of.
  OzzySettings settings;
  settings.clock_hz = 4'770'000;
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, GetBannerRows("OZZY 0.1  4770 kHz  1 core  56K RAM"));

  settings.clock_hz = 32'768;
  ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, GetBannerRows("OZZY 0.1  32768 Hz  1 core  56K RAM"));
}

TEST(BootRomTest, FastestClockRates) {
  // The most digits are a dword.
  OzzySettings settings;
  settings.screen.reset();
  settings.teletype = OzzyTeletypeSettings();
  settings.clock_hz = OzzySettings::kMaxClockHz;
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(),
            "OZZY 0.1  100 MHz  1 core  56K RAM\r\n\r\n");

  settings.clock_hz = OzzySettings::kMaxClockHz - 1;
  ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(),
            "OZZY 0.1  99999999 Hz  1 core  56K RAM\r\n\r\n");
}

TEST(BootRomTest, SmallScreenWrapsAndScrolls) {
  // The banner wraps onto four rows, so it scrolls once as it wraps, and twice
  // more for the new lines after it, leaving only its last row.
  OzzySettings settings;
  settings.screen->columns = 10;
  settings.screen->rows = 3;
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, {"AM", "", ""});
}

TEST(BootRomTest, BannerFillsTheRow) {
  // A banner as wide as the screen is followed by one blank row, as on a
  // wider screen.
  OzzySettings settings;
  settings.screen->columns = 32;
  settings.screen->rows = 4;
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, {"OZZY 0.1  1 MHz  1 core  56K RAM", "", "", ""});
}

//==============================================================================
// The boot ROM's stack
//==============================================================================

TEST(BootRomTest, StackInFirstRamPage) {
  // The stack grows down from the end of its page.
  std::unique_ptr<Ozzy> ozzy = BootOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  StackPlace stack = GetStackPlace(*ozzy);
  EXPECT_EQ(stack.bank, 0);
  EXPECT_EQ(stack.base, 2 * kMemoryBankPageSize);

  OzzySettings settings;
  settings.banks = {0x0000, 0x0F00};
  ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  stack = GetStackPlace(*ozzy);
  EXPECT_EQ(stack.bank, 1);
  EXPECT_EQ(stack.base, 9 * kMemoryBankPageSize);
  ExpectScreen(*ozzy, GetBannerRows("OZZY 0.1  1 MHz  1 core  16K RAM"));
}

//==============================================================================
// The banner on the teletype
//==============================================================================

TEST(BootRomTest, Teletype) {
  // At 9600 baud, each character takes 1041 cycles, which the boot ROM waits
  // for.
  for (int baud : {0, 9600}) {
    SCOPED_TRACE(baud);
    OzzySettings settings;
    settings.screen.reset();
    settings.teletype = OzzyTeletypeSettings();
    settings.teletype->baud = baud;
    std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
    ASSERT_NE(ozzy, nullptr);
    EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(),
              "OZZY 0.1  1 MHz  1 core  56K RAM\r\n\r\n");
    EXPECT_TRUE(IsHalted(*ozzy));
  }
}

TEST(BootRomTest, ScreenRatherThanTeletype) {
  OzzySettings settings;
  settings.teletype = OzzyTeletypeSettings();
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, GetBannerRows("OZZY 0.1  1 MHz  1 core  56K RAM"));
  EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(), "");
}

//==============================================================================
// No banner
//==============================================================================

TEST(BootRomTest, NoScreenOrTeletype) {
  OzzySettings settings;
  settings.screen.reset();
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  EXPECT_TRUE(IsHalted(*ozzy));
}

}  // namespace
}  // namespace oz3
