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
#include "absl/strings/str_cat.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/devices/keyboard.h"
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

// The default Ozzy's banner.
constexpr std::string_view kDefaultBanner = "OZZY 0.1  1 MHz  1 core  56K RAM";

// Expects the boot ROM's source to have each of `expected`'s constants, with
// its value.
void ExpectSourceConstants(std::map<std::string_view, uint32_t> expected) {
  for (const ProgramMap::Constant& constant :
       GetBootRom().GetMap().GetConstants()) {
    if (auto it = expected.find(constant.name); it != expected.end()) {
      EXPECT_EQ(constant.dword_value, it->second) << it->first;
      expected.erase(it);
    }
  }
  EXPECT_THAT(expected, IsEmpty());
}

// Returns whether core 0 has halted.
bool IsHalted(Ozzy& ozzy) {
  return ozzy.GetProcessor()->GetCore(0)->GetState() == CpuCore::State::kIdle;
}

// Runs `ozzy` long enough for the screen to refresh, 30 times a second, and
// the teletype to print the character its port holds.
void RunToShow(Ozzy& ozzy) { ozzy.Run(absl::Milliseconds(34)); }

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
  RunToShow(booted);
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

// Expects the screen to show `rows` of text, and the cursor at `column` and
// `row`.
void ExpectScreenAndCursor(Ozzy& ozzy,
                           const std::vector<std::string_view>& rows,
                           int column, int row) {
  ASSERT_NE(ozzy.GetScreen(), nullptr);
  const ScreenPicture& picture = ozzy.GetScreen()->GetPicture();
  ASSERT_EQ(static_cast<int>(rows.size()), picture.rows);
  for (int i = 0; i < picture.rows; ++i) {
    EXPECT_EQ(GetRowText(picture, i), PadRow(rows[i], picture.columns))
        << "Row " << i;
  }
  EXPECT_EQ(picture.cursor_column, column);
  EXPECT_EQ(picture.cursor_row, row);
  EXPECT_EQ(picture.cursor_mode, ScreenCursorMode::kSlowBlink);
  EXPECT_EQ(picture.cursor_style, ScreenCursorStyle::kUnderline);
}

// Expects the screen to show `rows` of text, and the cursor at the start of
// the third row, below the banner and a blank line, as it always is once the
// boot ROM has printed it.
void ExpectScreen(Ozzy& ozzy, const std::vector<std::string_view>& rows) {
  ExpectScreenAndCursor(ozzy, rows, 0, 2);
}

// Presses each of `keys`, and runs `ozzy` long enough to echo them, and show
// them.
void TypeKeys(Ozzy& ozzy, absl::Span<const uint16_t> keys) {
  for (uint16_t key : keys) {
    EXPECT_TRUE(ozzy.GetKeyboard()->PressKey(key));
  }
  RunToShow(ozzy);
}

// Types the keys of each character of `text`, which are their key events (such
// as '\b' for Backspace).
void TypeText(Ozzy& ozzy, std::string_view text) {
  const std::vector<uint16_t> keys(text.begin(), text.end());
  TypeKeys(ozzy, keys);
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
  ExpectSourceConstants({
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
      {"KEYBOARD", static_cast<uint32_t>(MachineDeviceType::kKeyboard)},
      {"TELETYPE", static_cast<uint32_t>(MachineDeviceType::kTeletype)},
  });
}

TEST(BootRomTest, SourceKnowsTheKeyboard) {
  // The source's own copy of the keyboard's wiring and keys matches the
  // Ozzy's. Only core 0 runs the boot ROM, so the keyboard's interrupt must
  // reach it.
  ExpectSourceConstants({
      {"KEYBOARD_PORT", kOzzyKeyboardPort},
      {"KEYBOARD_INTERRUPT", kOzzyKeyboardInterrupt},
      {"KEY_BACKSPACE", kKeyBackspace},
      {"KEY_ENTER", kKeyEnter},
  });
  EXPECT_NE(kOzzyKeyboardInterruptCores & 1, 0);
}

//==============================================================================
// The banner on the screen
//==============================================================================

TEST(BootRomTest, DefaultOzzy) {
  std::unique_ptr<Ozzy> ozzy = BootOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, GetBannerRows(kDefaultBanner));
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
  ExpectScreen(*ozzy, {kDefaultBanner, "", "", ""});
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
              absl::StrCat(kDefaultBanner, "\r\n\r\n"));
    EXPECT_TRUE(IsHalted(*ozzy));
  }
}

TEST(BootRomTest, ScreenRatherThanTeletype) {
  OzzySettings settings;
  settings.teletype = OzzyTeletypeSettings();
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  ExpectScreen(*ozzy, GetBannerRows(kDefaultBanner));
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

  // Keys echo nowhere.
  TypeText(*ozzy, "a\r\b");
  EXPECT_TRUE(IsHalted(*ozzy));
}

//==============================================================================
// Echoing keys on the screen
//==============================================================================

TEST(BootRomTest, KeysEchoBetweenRuns) {
  std::unique_ptr<Ozzy> ozzy = BootOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  std::vector<std::string_view> rows = GetBannerRows(kDefaultBanner);

  TypeText(*ozzy, "Hi");
  rows[2] = "Hi";
  ExpectScreenAndCursor(*ozzy, rows, 2, 2);
  EXPECT_TRUE(IsHalted(*ozzy));

  TypeText(*ozzy, " there!");
  rows[2] = "Hi there!";
  ExpectScreenAndCursor(*ozzy, rows, 9, 2);
  EXPECT_TRUE(IsHalted(*ozzy));
}

TEST(BootRomTest, KeysEchoWithEveryCore) {
  // The keyboard's interrupt is shared by every core, but only core 0 has a
  // handler, so the others drop it and core 0 still echoes every key.
  OzzySettings settings;
  settings.cores = kMaxCores;
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  std::vector<std::string_view> rows =
      GetBannerRows("OZZY 0.1  1 MHz  8 cores  56K RAM");

  TypeText(*ozzy, "Hi there!");
  rows[2] = "Hi there!";
  ExpectScreenAndCursor(*ozzy, rows, 9, 2);
  EXPECT_TRUE(IsHalted(*ozzy));
}

TEST(BootRomTest, KeysBeforeTheBannerEchoAfterIt) {
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(OzzySettings());
  ASSERT_TRUE(ozzy.ok()) << ozzy.status();
  EXPECT_TRUE((*ozzy)->GetKeyboard()->PressKey('a'));
  EXPECT_TRUE((*ozzy)->GetKeyboard()->PressKey('b'));
  (*ozzy)->Run(absl::Milliseconds(100));
  std::vector<std::string_view> rows = GetBannerRows(kDefaultBanner);
  rows[2] = "ab";
  ExpectScreenAndCursor(**ozzy, rows, 2, 2);
}

TEST(BootRomTest, EveryCharacterEchoes) {
  std::string characters;
  for (int c = 0x20; c <= 0x7E; ++c) {
    characters.push_back(static_cast<char>(c));
  }
  OzzySettings settings;
  settings.screen->columns = 95;
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  // As many keys at a time as the keyboard holds.
  for (int i = 0; i < static_cast<int>(characters.size());
       i += Keyboard::kQueueSize) {
    TypeText(*ozzy,
             std::string_view(characters).substr(i, Keyboard::kQueueSize));
  }
  std::vector<std::string_view> rows = GetBannerRows(kDefaultBanner);
  rows[2] = characters;

  // The line is full, so the cursor stays on its last column.
  ExpectScreenAndCursor(*ozzy, rows, 94, 2);
}

TEST(BootRomTest, OtherKeysAreIgnored) {
  std::unique_ptr<Ozzy> ozzy = BootOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  TypeKeys(*ozzy, {kKeyTab, kKeyEscape, kKeyDelete, kKeyUp, kKeyInsert, kKeyF1,
                   kKeyF12, 0x01, 0x1A, 0xFF});
  ExpectScreen(*ozzy, GetBannerRows(kDefaultBanner));
}

TEST(BootRomTest, ModifiersMakeNoDifference) {
  // The key is what echoes, whatever modifier keys are held with it.
  std::unique_ptr<Ozzy> ozzy = BootOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  TypeKeys(*ozzy,
           {kKeyShift | 'A', kKeyCtrl | 'b', kKeyAlt | 'c',
            kKeyCtrl | kKeyBackspace, kKeyShift | kKeyEnter, kKeyCtrl | 0x01});
  std::vector<std::string_view> rows = GetBannerRows(kDefaultBanner);
  rows[2] = "Ab";
  ExpectScreenAndCursor(*ozzy, rows, 0, 3);
}

TEST(BootRomTest, Enter) {
  std::unique_ptr<Ozzy> ozzy = BootOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  TypeText(*ozzy, "ab\rcd\r\r");
  std::vector<std::string_view> rows = GetBannerRows(kDefaultBanner);
  rows[2] = "ab";
  rows[3] = "cd";
  ExpectScreenAndCursor(*ozzy, rows, 0, 5);
}

TEST(BootRomTest, Backspace) {
  std::unique_ptr<Ozzy> ozzy = BootOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  std::vector<std::string_view> rows = GetBannerRows(kDefaultBanner);

  TypeText(*ozzy, "abc\b\bx");
  rows[2] = "ax";
  ExpectScreenAndCursor(*ozzy, rows, 2, 2);

  // At the start of a row, Backspace does nothing, so it never goes back to
  // the row above.
  TypeText(*ozzy, "\b\b\b\b");
  rows[2] = "";
  ExpectScreenAndCursor(*ozzy, rows, 0, 2);
  TypeText(*ozzy, "\ry\b\b");
  ExpectScreenAndCursor(*ozzy, rows, 0, 3);
}

TEST(BootRomTest, KeysWrapAndScroll) {
  // The banner leaves "AM" on a 10 by 3 screen, and the cursor on the bottom
  // row.
  OzzySettings settings;
  settings.screen->columns = 10;
  settings.screen->rows = 3;
  std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
  ASSERT_NE(ozzy, nullptr);

  // A full row keeps the cursor on its last column, until the next key.
  TypeText(*ozzy, "0123456789");
  ExpectScreenAndCursor(*ozzy, {"AM", "", "0123456789"}, 9, 2);

  // Backspace in a full row erases its last column.
  TypeText(*ozzy, "\bX");
  ExpectScreenAndCursor(*ozzy, {"AM", "", "012345678X"}, 9, 2);

  // The next key wraps, and scrolls the screen up a row, clearing the new
  // bottom row.
  TypeText(*ozzy, "a");
  ExpectScreenAndCursor(*ozzy, {"", "012345678X", "a"}, 1, 2);

  // Enter on the bottom row scrolls too.
  TypeText(*ozzy, "\r");
  ExpectScreenAndCursor(*ozzy, {"012345678X", "a", ""}, 0, 2);
}

//==============================================================================
// Echoing keys on the teletype
//==============================================================================

TEST(BootRomTest, KeysOnTheTeletype) {
  for (int baud : {0, 9600}) {
    SCOPED_TRACE(baud);
    OzzySettings settings;
    settings.screen.reset();
    settings.teletype = OzzyTeletypeSettings();
    settings.teletype->baud = baud;
    std::unique_ptr<Ozzy> ozzy = BootOzzy(settings);
    ASSERT_NE(ozzy, nullptr);
    EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(),
              absl::StrCat(kDefaultBanner, "\r\n\r\n"));

    // Backspace even at the start of a line, as the teletype doesn't know
    // where it is. At 9600 baud, 10 characters take 10 milliseconds.
    TypeText(*ozzy, "\bab\b\tc\r");
    EXPECT_EQ(ozzy->GetTeletype()->TakeOutput(), "\b \bab\b \bc\r\n");
    EXPECT_TRUE(IsHalted(*ozzy));
  }
}

}  // namespace
}  // namespace oz3
