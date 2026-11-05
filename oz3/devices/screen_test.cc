// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/screen.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <iterator>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/device_config.h"
#include "oz3/core/lockable.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/processor_config.h"
#include "oz3/devices/device_program_test.h"
#include "oz3/devices/screen_characters.h"

namespace oz3 {
namespace {

using ::testing::AllOf;
using ::testing::Each;
using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::Field;
using ::testing::HasSubstr;
using ::testing::IsEmpty;

// The page the screen is on in bank 0, unless a test says otherwise, and the
// interrupt it raises.
constexpr int kPage = 15;
constexpr int kInterrupt = 6;

// A clock rate where refresh n at 60 Hz is at cycle n * 100 / 6: 16, 33, 50,
// 66, 83, 100, and so on.
constexpr int64_t kSlowClockHz = 1000;

// Returns the cursor word for `mode` and `style`.
uint16_t CursorWord(ScreenCursorMode mode, ScreenCursorStyle style) {
  return static_cast<uint16_t>(mode) | static_cast<uint16_t>(style);
}

// A cell's pixels, a string a row: '.' is off, '+' dim, and '#' bright.
using CellPixels = std::array<std::string_view, kScreenGlyphHeight>;

// The test character set's glyph for 'A', and its alternate glyph for 'A'.
// Every other glyph is blank.
constexpr CellPixels kGlyphA = {
    "........",  //
    "...##...",  //
    "..#..#..",  //
    ".#....#.",  //
    ".#....#.",  //
    ".######.",  //
    ".#....#.",  //
    ".#....#.",  //
    "........",  //
    "........",  //
};
constexpr CellPixels kAlternateGlyphA = {
    "#.#.#.#.",  //
    ".#.#.#.#",  //
    "#.#.#.#.",  //
    ".#.#.#.#",  //
    "#.#.#.#.",  //
    ".#.#.#.#",  //
    "#.#.#.#.",  //
    ".#.#.#.#",  //
    "#.#.#.#.",  //
    ".#.#.#.#",  //
};

// kGlyphA drawn bright, inverted, underlined, and with its left edge lit, as
// either a cell's attributes or the cursor draw them, and a cell with every
// pixel lit bright.
constexpr CellPixels kInverseGlyphA = {
    "########",  //
    "###..###",  //
    "##.##.##",  //
    "#.####.#",  //
    "#.####.#",  //
    "#......#",  //
    "#.####.#",  //
    "#.####.#",  //
    "########",  //
    "########",  //
};
constexpr CellPixels kUnderlinedGlyphA = {
    "........",  //
    "...##...",  //
    "..#..#..",  //
    ".#....#.",  //
    ".#....#.",  //
    ".######.",  //
    ".#....#.",  //
    ".#....#.",  //
    "........",  //
    "########",  //
};
constexpr CellPixels kLeftEdgeGlyphA = {
    "#.......",  //
    "#..##...",  //
    "#.#..#..",  //
    "##....#.",  //
    "##....#.",  //
    "#######.",  //
    "##....#.",  //
    "##....#.",  //
    "#.......",  //
    "#.......",  //
};
constexpr CellPixels kBlockPixels = {
    "########", "########", "########", "########", "########",
    "########", "########", "########", "########", "########",
};

// Returns `pixels` packed as a glyph, with every pixel not '.' lit.
std::array<uint16_t, kScreenGlyphSize> PackGlyph(const CellPixels& pixels) {
  std::array<uint16_t, kScreenGlyphSize> words = {};
  for (int y = 0; y < kScreenGlyphHeight; ++y) {
    int row = 0;
    for (int x = 0; x < kScreenGlyphWidth; ++x) {
      if (pixels[y][x] != '.') {
        row |= 0x80 >> x;
      }
    }
    words[y / 2] |= static_cast<uint16_t>(y % 2 == 0 ? row << 8 : row);
  }
  return words;
}

// Returns the test character set: kGlyphA and kAlternateGlyphA, and blank
// glyphs.
std::vector<uint16_t> TestCharacters() {
  std::vector<uint16_t> characters(kScreenCharacterSetSize);
  std::ranges::copy(PackGlyph(kGlyphA),
                    characters.begin() + 'A' * kScreenGlyphSize);
  std::ranges::copy(PackGlyph(kAlternateGlyphA),
                    characters.begin() + (256 + 'A') * kScreenGlyphSize);
  return characters;
}

class ScreenTest : public DeviceProgramTest {
 protected:
  // The most cycles ExecuteUntilRefresh waits.
  static constexpr Cycles kMaxRefreshCycles = 1'000'000;

  // Initializes a processor with a screen named "Screen" with `settings`, and
  // sets screen_ to it. Returns whether it did.
  bool InitScreen(const ScreenSettings& settings,
                  int64_t clock_hz = kSlowClockHz, int num_memory_banks = 1) {
    absl::StatusOr<std::unique_ptr<ScreenFactory>> factory =
        ScreenFactory::Create("Screen", settings);
    EXPECT_TRUE(factory.ok()) << factory.status();
    if (!factory.ok() || !Init({.num_memory_banks = num_memory_banks,
                                .clock_hz = clock_hz,
                                .devices = {*std::move(factory)}})) {
      return false;
    }
    bank_ = settings.bank;
    start_ = settings.page * kMemoryBankPageSize;
    screen_ = static_cast<Screen*>(GetProcessor().GetDevice("Screen"));
    return screen_ != nullptr;
  }

  // As above, with a screen of `columns` by `rows` on kPage, raising
  // kInterrupt, refreshing at 60 Hz, with the test character set.
  bool InitScreen(int columns = 80, int rows = 24) {
    return InitScreen({.page = kPage,
                       .interrupt = {.interrupt = kInterrupt},
                       .columns = columns,
                       .rows = rows,
                       .refresh_hz = 60,
                       .characters = TestCharacters()});
  }

  // Returns a lock on the screen's bank, which no core may be holding.
  std::unique_ptr<Lock> LockBank() {
    std::unique_ptr<Lock> lock = GetProcessor().GetMemory(bank_)->RequestLock();
    EXPECT_TRUE(lock->IsLocked());
    return lock;
  }

  // Writes `words` through the bank, as a core does, from the screen's own
  // `address`.
  void WriteScreen(int address, absl::Span<const uint16_t> words) {
    std::unique_ptr<Lock> lock = LockBank();
    MemoryBank& bank = *GetProcessor().GetMemory(bank_);
    bank.SetAddress(*lock, static_cast<uint16_t>(start_ + address));
    bank.WriteWords(*lock, words);
  }

  // Reads `size` words through the bank, as a core does, from the screen's own
  // `address`.
  std::vector<uint16_t> ReadScreen(int address, int size = 1) {
    std::unique_ptr<Lock> lock = LockBank();
    MemoryBank& bank = *GetProcessor().GetMemory(bank_);
    std::vector<uint16_t> words(size);
    bank.SetAddress(*lock, static_cast<uint16_t>(start_ + address));
    bank.ReadWords(*lock, absl::MakeSpan(words));
    return words;
  }

  // Executes until the next refresh, and returns the processor's cycles when
  // it is done (one past the refresh's cycle), or -1 if it isn't within
  // kMaxRefreshCycles.
  Cycles ExecuteUntilRefresh() {
    const int64_t frame_count = screen_->GetFrameCount();
    for (Cycles cycles = 0; cycles < kMaxRefreshCycles; ++cycles) {
      Execute(1);
      if (screen_->GetFrameCount() != frame_count) {
        return GetCycles();
      }
    }
    return -1;
  }

  // Returns whether kInterrupt is raised on core 0.
  bool IsInterruptRaised() {
    return (GetState().core.GetInterrupts() & (1u << kInterrupt)) != 0;
  }

  // Returns every row of the picture's pixels, as CellPixels describes.
  std::vector<std::string> GetPixelRows() {
    const ScreenPicture& picture = screen_->GetPicture();
    std::vector<std::string> pixels;
    for (int y = 0; y < picture.pixel_height; ++y) {
      std::string& line = pixels.emplace_back();
      for (int x = 0; x < picture.pixel_width; ++x) {
        line += std::string_view(".+#").at(
            picture.pixels[y * picture.pixel_width + x]);
      }
    }
    return pixels;
  }

  // Returns the picture's pixels of the cell at `column` and `row`, with no
  // compact rows, as CellPixels describes.
  std::vector<std::string> GetCellPixels(int column = 0, int row = 0) {
    const std::vector<std::string> pixel_rows = GetPixelRows();
    std::vector<std::string> pixels;
    for (int y = 0; y < kScreenGlyphHeight; ++y) {
      pixels.push_back(pixel_rows[row * kScreenGlyphHeight + y].substr(
          column * kScreenGlyphWidth, kScreenGlyphWidth));
    }
    return pixels;
  }

  Screen* screen_ = nullptr;
  int bank_ = 0;
  int start_ = 0;  // The screen's first address in its bank.
};

TEST_F(ScreenTest, IsBlankAtPowerOn) {
  ASSERT_TRUE(InitScreen(80, 24));
  const ScreenPicture& picture = screen_->GetPicture();
  EXPECT_EQ(picture.columns, 80);
  EXPECT_EQ(picture.rows, 24);
  EXPECT_EQ(picture.cells, std::vector<uint16_t>(80 * 24));
  EXPECT_EQ(picture.cursor_column, 0);
  EXPECT_EQ(picture.cursor_row, 0);
  EXPECT_EQ(picture.cursor_mode, ScreenCursorMode::kOff);
  EXPECT_EQ(picture.cursor_style, ScreenCursorStyle::kBlock);
  EXPECT_EQ(picture.pixel_width, 640);
  EXPECT_EQ(picture.pixel_height, 240);
  EXPECT_EQ(picture.pixels, std::vector<uint8_t>(640 * 240));
  EXPECT_EQ(picture.drawn_height, 240);
  EXPECT_EQ(screen_->GetFrameCount(), 0);

  EXPECT_EQ(ReadScreen(0, ScreenMemory::kColumnsAddress),
            std::vector<uint16_t>(ScreenMemory::kColumnsAddress));
  EXPECT_THAT(ReadScreen(ScreenMemory::kColumnsAddress, 16),
              ElementsAre(80, 24, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0));
}

TEST_F(ScreenTest, CellsWrittenByAProgramAppearAtTheNextRefresh) {
  // Refresh 1 is at cycle 125.
  ASSERT_TRUE(InitScreen({.page = kPage, .refresh_hz = 8}));
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        MOV BE, 0F000h      ; The screen's page, in EXTRA
        MOV R0, 0148h       ; 'H' in inverse
        MOV E(0), R0
        MOV R0, 0F869h      ; Alternate 'i', boxed by its four lines
        MOV E(81), R0       ; Row 1, column 1
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  ASSERT_LT(GetCycles(), 125);
  EXPECT_THAT(ReadScreen(0), ElementsAre(kScreenInverse | 'H'));
  EXPECT_EQ(screen_->GetPicture().cells, std::vector<uint16_t>(80 * 24));
  Execute(125 - GetCycles());
  EXPECT_EQ(screen_->GetPicture().cells, std::vector<uint16_t>(80 * 24));
  Execute(1);
  std::vector<uint16_t> cells(80 * 24);
  cells[0] = kScreenInverse | 'H';
  cells[81] = kScreenAlternate | kScreenLineTop | kScreenLineBottom |
              kScreenLineLeft | kScreenLineRight | 'i';
  EXPECT_EQ(screen_->GetPicture().cells, cells);
}

TEST_F(ScreenTest, RefreshesOnScheduleWithoutDrift) {
  ASSERT_TRUE(InitScreen());

  // A refresh is 16 2/3 cycles, so refresh n is at cycle n * 100 / 6.
  for (const Cycles refresh_cycle : {16, 33, 50, 66, 83, 100, 116}) {
    EXPECT_EQ(ExecuteUntilRefresh(), refresh_cycle + 1);
  }
  EXPECT_EQ(screen_->GetFrameCount(), 7);
  EXPECT_THAT(ReadScreen(ScreenMemory::kFrameCountAddress), ElementsAre(7));
}

TEST_F(ScreenTest, RefreshesAtTheDefaultRates) {
  // 30 Hz at 1 MHz.
  ASSERT_TRUE(InitScreen({.page = kPage},
                         /*clock_hz=*/ProcessorConfig::kDefaultClockHz));
  for (const Cycles refresh_cycle : {33'333, 66'666, 100'000}) {
    EXPECT_EQ(ExecuteUntilRefresh(), refresh_cycle + 1);
  }
}

TEST_F(ScreenTest, TwiceTheClockRateTakesTwiceTheCycles) {
  ASSERT_TRUE(InitScreen({.page = kPage},
                         /*clock_hz=*/2 * ProcessorConfig::kDefaultClockHz));
  for (const Cycles refresh_cycle : {66'666, 133'333, 200'000}) {
    EXPECT_EQ(ExecuteUntilRefresh(), refresh_cycle + 1);
  }
}

TEST_F(ScreenTest, TwiceTheRefreshRateTakesHalfTheCycles) {
  ASSERT_TRUE(InitScreen({.page = kPage, .refresh_hz = 60},
                         /*clock_hz=*/ProcessorConfig::kDefaultClockHz));
  for (const Cycles refresh_cycle : {16'666, 33'333, 50'000, 66'666}) {
    EXPECT_EQ(ExecuteUntilRefresh(), refresh_cycle + 1);
  }
}

TEST_F(ScreenTest, FrameCounterHoldsTheLow16Bits) {
  // At a refresh rate of the clock rate, the screen refreshes every cycle. One
  // cell keeps the 65,537 refreshes quick.
  ASSERT_TRUE(
      InitScreen({.page = kPage, .columns = 1, .rows = 1, .refresh_hz = 60},
                 /*clock_hz=*/60));
  Execute(0x10002);
  EXPECT_EQ(screen_->GetFrameCount(), 0x10001);
  EXPECT_THAT(ReadScreen(ScreenMemory::kFrameCountAddress), ElementsAre(1));
}

TEST_F(ScreenTest, RaisesTheInterruptAtEachRefresh) {
  // Refreshes are 100 cycles apart, which is time for the handler.
  ASSERT_TRUE(InitScreen({.page = kPage,
                          .interrupt = {.interrupt = kInterrupt},
                          .refresh_hz = 10}));
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        SETI 6, handler
        MVQ R6, 0
        EI
    idle:
        JP idle

    handler:                ; Counts the refreshes
        ADQ R6, 1
        IRT
  )---"));
  ASSERT_TRUE(ExecuteUntilIp(GetLabel("IDLE")));
  ASSERT_EQ(screen_->GetFrameCount(), 0);
  const CoreState& state = GetState();
  EXPECT_EQ(state.r6, 0);
  for (int i = 1; i <= 5; ++i) {
    // The core holds the bank while it runs, so a refresh may wait for it.
    ASSERT_NE(ExecuteUntilRefresh(), -1);
    EXPECT_TRUE(IsInterruptRaised());
    Execute(50);
    EXPECT_EQ(state.r6, i);
  }
}

TEST_F(ScreenTest, ScrollRowIsShownAtTheTop) {
  ASSERT_TRUE(InitScreen(2, 3));
  WriteScreen(0, {0x10, 0x11, 0x20, 0x21, 0x30, 0x31});
  ExecuteUntilRefresh();
  EXPECT_THAT(screen_->GetPicture().cells,
              ElementsAre(0x10, 0x11, 0x20, 0x21, 0x30, 0x31));

  WriteScreen(ScreenMemory::kScrollRowAddress, {1});
  ExecuteUntilRefresh();
  EXPECT_THAT(screen_->GetPicture().cells,
              ElementsAre(0x20, 0x21, 0x30, 0x31, 0x10, 0x11));

  // The scroll row is taken modulo the rows.
  WriteScreen(ScreenMemory::kScrollRowAddress, {5});
  ExecuteUntilRefresh();
  EXPECT_THAT(screen_->GetPicture().cells,
              ElementsAre(0x30, 0x31, 0x10, 0x11, 0x20, 0x21));
  WriteScreen(ScreenMemory::kScrollRowAddress, {0xFFFF});  // 65535 % 3 is 0
  ExecuteUntilRefresh();
  EXPECT_THAT(screen_->GetPicture().cells,
              ElementsAre(0x10, 0x11, 0x20, 0x21, 0x30, 0x31));
}

TEST_F(ScreenTest, CursorWordSetsTheModeAndStyle) {
  ASSERT_TRUE(InitScreen(80, 24));
  WriteScreen(ScreenMemory::kCursorColumnAddress,
              {79, 23,
               CursorWord(ScreenCursorMode::kSlowBlink,
                          ScreenCursorStyle::kUnderline)});
  ExecuteUntilRefresh();
  const ScreenPicture& picture = screen_->GetPicture();
  EXPECT_EQ(picture.cursor_column, 79);
  EXPECT_EQ(picture.cursor_row, 23);
  EXPECT_EQ(picture.cursor_mode, ScreenCursorMode::kSlowBlink);
  EXPECT_EQ(picture.cursor_style, ScreenCursorStyle::kUnderline);

  for (const ScreenCursorMode mode :
       {ScreenCursorMode::kOff, ScreenCursorMode::kOn,
        ScreenCursorMode::kSlowBlink, ScreenCursorMode::kFastBlink}) {
    for (const ScreenCursorStyle style :
         {ScreenCursorStyle::kBlock, ScreenCursorStyle::kInverse,
          ScreenCursorStyle::kUnderline, ScreenCursorStyle::kLeftEdge}) {
      // The reserved bits are ignored.
      WriteScreen(ScreenMemory::kCursorAddress,
                  {static_cast<uint16_t>(0xFFF0 | CursorWord(mode, style))});
      ExecuteUntilRefresh();
      EXPECT_EQ(picture.cursor_mode, mode);
      EXPECT_EQ(picture.cursor_style, style);
    }
  }
}

TEST_F(ScreenTest, CursorOutsideTheScreenIsOff) {
  ASSERT_TRUE(InitScreen(80, 24));
  const uint16_t on =
      CursorWord(ScreenCursorMode::kOn, ScreenCursorStyle::kInverse);
  const ScreenPicture& picture = screen_->GetPicture();
  WriteScreen(ScreenMemory::kCursorColumnAddress, {80, 0, on});
  ExecuteUntilRefresh();
  EXPECT_EQ(picture.cursor_column, 80);
  EXPECT_EQ(picture.cursor_mode, ScreenCursorMode::kOff);
  EXPECT_EQ(picture.cursor_style, ScreenCursorStyle::kInverse);
  WriteScreen(ScreenMemory::kCursorColumnAddress, {0, 24});
  ExecuteUntilRefresh();
  EXPECT_EQ(picture.cursor_row, 24);
  EXPECT_EQ(picture.cursor_mode, ScreenCursorMode::kOff);
  WriteScreen(ScreenMemory::kCursorRowAddress, {0});
  ExecuteUntilRefresh();
  EXPECT_EQ(picture.cursor_mode, ScreenCursorMode::kOn);
}

TEST_F(ScreenTest, ReadOnlyAndReservedWordsIgnoreWrites) {
  ASSERT_TRUE(InitScreen(80, 24));
  ExecuteUntilRefresh();
  WriteScreen(ScreenMemory::kColumnsAddress, std::vector<uint16_t>(16, 0xFFFF));
  EXPECT_THAT(ReadScreen(ScreenMemory::kColumnsAddress, 16),
              ElementsAre(80, 24, 1, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0,
                          0xFFFF, 0xFFFF, 0xFFFF, 0, 0, 0, 0));
}

TEST_F(ScreenTest, UnusedWordsHoldWhatIsWritten) {
  ASSERT_TRUE(InitScreen(80, 24));
  const int unused = ScreenMemory::kColumnsAddress - 80 * 24;
  std::vector<uint16_t> words(unused);
  for (int i = 0; i < unused; ++i) {
    words[i] = static_cast<uint16_t>(i + 1);
  }
  WriteScreen(80 * 24, words);
  ExecuteUntilRefresh();
  EXPECT_EQ(ReadScreen(80 * 24, unused), words);
  EXPECT_EQ(screen_->GetPicture().cells, std::vector<uint16_t>(80 * 24));
}

TEST_F(ScreenTest, PictureIsUnchangedWithoutWrites) {
  ASSERT_TRUE(InitScreen(2, 2));
  WriteScreen(0, {1, 2, 3, 4});
  WriteScreen(
      ScreenMemory::kCursorColumnAddress,
      {1, 1, CursorWord(ScreenCursorMode::kOn, ScreenCursorStyle::kBlock)});
  ExecuteUntilRefresh();
  const ScreenPicture picture = screen_->GetPicture();
  ExecuteUntilRefresh();
  EXPECT_EQ(screen_->GetPicture(), picture);

  WriteScreen(ScreenMemory::kCursorAddress, {0});
  ExecuteUntilRefresh();
  EXPECT_NE(screen_->GetPicture(), picture);
}

TEST_F(ScreenTest, ClearRowFillsARowWithTheFillWord) {
  ASSERT_TRUE(InitScreen(2, 3));
  WriteScreen(0, {0x10, 0x11, 0x20, 0x21, 0x30, 0x31});

  // The fill word is 0 at power on.
  WriteScreen(ScreenMemory::kClearRowAddress, {0});
  EXPECT_THAT(ReadScreen(0, 6), ElementsAre(0, 0, 0x20, 0x21, 0x30, 0x31));

  // The row is cleared as clear row is written, after the fill word before it
  // in the same write. Clear row reads 0, and the next refresh shows the row.
  const uint16_t fill = kScreenInverse | ' ';
  WriteScreen(ScreenMemory::kFillAddress, {fill, 2});
  EXPECT_THAT(ReadScreen(0, 6), ElementsAre(0, 0, 0x20, 0x21, fill, fill));
  EXPECT_THAT(ReadScreen(ScreenMemory::kFillAddress, 2), ElementsAre(fill, 0));
  ExecuteUntilRefresh();
  EXPECT_THAT(screen_->GetPicture().cells,
              ElementsAre(0, 0, 0x20, 0x21, fill, fill));
}

TEST_F(ScreenTest, ClearRowIsInScreenPositions) {
  ASSERT_TRUE(InitScreen(2, 3));

  // With the cells' row 1 at the top, screen row 0 is the cells' row 1, and
  // screen row 2 wraps to the cells' row 0.
  WriteScreen(ScreenMemory::kScrollRowAddress, {1});
  WriteScreen(ScreenMemory::kFillAddress, {1, 0});
  WriteScreen(ScreenMemory::kFillAddress, {2, 2});
  EXPECT_THAT(ReadScreen(0, 6), ElementsAre(2, 2, 1, 1, 0, 0));

  // The scroll row is taken modulo the rows: 0FFFEh % 3 is 2.
  WriteScreen(ScreenMemory::kScrollRowAddress, {0xFFFE});
  WriteScreen(ScreenMemory::kFillAddress, {3, 0});
  EXPECT_THAT(ReadScreen(0, 6), ElementsAre(2, 2, 1, 1, 3, 3));
  ExecuteUntilRefresh();
  EXPECT_THAT(screen_->GetPicture().cells, ElementsAre(3, 3, 2, 2, 1, 1));
}

TEST_F(ScreenTest, ClearRowOutsideTheScreenClearsNothing) {
  ASSERT_TRUE(InitScreen(2, 3));
  WriteScreen(0, {1, 2, 3, 4, 5, 6});
  WriteScreen(ScreenMemory::kFillAddress, {0xFF});
  for (const int row : {3, 4, 0xFFFF}) {
    WriteScreen(ScreenMemory::kClearRowAddress, {static_cast<uint16_t>(row)});
  }

  // Nor does it clear the unused words after the cells.
  EXPECT_THAT(ReadScreen(0, 8), ElementsAre(1, 2, 3, 4, 5, 6, 0, 0));
}

TEST_F(ScreenTest, LineFeedOnTheBottomRowIsTwoWrites) {
  ASSERT_TRUE(InitScreen(2, 3));
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        MOV BE, 0F000h      ; The screen's page, in EXTRA
        MOV R0, E(0FF3h)    ; Scroll the text up a row
        ADQ R0, 1
        MOV E(0FF3h), R0
        MOV R0, E(0FF1h)    ; Clear the bottom row, rows - 1
        SBQ R0, 1
        MOV E(0FF8h), R0
        HALT
  )---"));
  WriteScreen(0, {0x10, 0x11, 0x20, 0x21, 0x30, 0x31});
  WriteScreen(ScreenMemory::kFillAddress, {' '});
  ASSERT_TRUE(ExecuteUntilHalt());
  ExecuteUntilRefresh();
  EXPECT_THAT(screen_->GetPicture().cells,
              ElementsAre(0x20, 0x21, 0x30, 0x31, ' ', ' '));
}

TEST_F(ScreenTest, CharacterWindowSendsWritesToTheCharacterSet) {
  ASSERT_TRUE(InitScreen(3, 1));
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        MOV BE, 0F000h      ; The screen's page, in EXTRA
        MVQ R0, 1           ; Open the character set window
        MOV E(0FF9h), R0
        MOV R0, 0FF81h      ; Glyph 0's rows 0 and 1
        MOV E(0), R0
        MOV R0, 8142h       ; Glyph 0's rows 8 and 9
        MOV E(4), R0
        MOV R0, 1234h       ; Glyph 511's rows 8 and 9
        MOV E(9FFh), R0
        MOV R1, E(0)        ; Reads still read the cells
        MVQ R0, 0           ; Close the window
        MOV E(0FF9h), R0
        HALT
  )---"));
  WriteScreen(0, {'Z', 0, kScreenAlternate | 0xFF});
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(GetState().r1, 'Z');
  EXPECT_THAT(ReadScreen(0, 3), ElementsAre('Z', 0, kScreenAlternate | 0xFF));
  EXPECT_THAT(ReadScreen(ScreenMemory::kCharacterWindowAddress),
              ElementsAre(0));

  ExecuteUntilRefresh();
  EXPECT_THAT(GetCellPixels(1), ElementsAre("########",  //
                                            "#......#",  //
                                            "........",  //
                                            "........",  //
                                            "........",  //
                                            "........",  //
                                            "........",  //
                                            "........",  //
                                            "#......#",  //
                                            ".#....#."));
  EXPECT_THAT(GetCellPixels(2), ElementsAre("........",  //
                                            "........",  //
                                            "........",  //
                                            "........",  //
                                            "........",  //
                                            "........",  //
                                            "........",  //
                                            "........",  //
                                            "...#..#.",  //
                                            "..##.#.."));
}

TEST_F(ScreenTest, CharacterWindowLeavesAddressesFromA00hAsTheyAre) {
  ASSERT_TRUE(InitScreen(80, 51));
  WriteScreen(ScreenMemory::kCharacterWindowAddress,
              {kScreenCharacterWindowOpen});
  WriteScreen(0x9FF, {0x1234, 0x5678});
  EXPECT_THAT(ReadScreen(0x9FF, 2), ElementsAre(0, 0x5678));

  // Control words are written as ever, too.
  WriteScreen(ScreenMemory::kFillAddress, {'A', 50});
  EXPECT_EQ(ReadScreen(80 * 50, 80), std::vector<uint16_t>(80, 'A'));
}

TEST_F(ScreenTest, ChangedGlyphShowsAtTheNextRefresh) {
  ASSERT_TRUE(InitScreen(1, 1));
  WriteScreen(0, {'A'});
  ExecuteUntilRefresh();
  EXPECT_THAT(GetCellPixels(), ElementsAreArray(kGlyphA));

  WriteScreen(ScreenMemory::kCharacterWindowAddress,
              {kScreenCharacterWindowOpen});
  WriteScreen('A' * kScreenGlyphSize, std::vector<uint16_t>(5, 0xFF00));
  WriteScreen(ScreenMemory::kCharacterWindowAddress, {0});
  EXPECT_THAT(GetCellPixels(), ElementsAreArray(kGlyphA));
  EXPECT_THAT(ReadScreen(0), ElementsAre('A'));

  ExecuteUntilRefresh();
  EXPECT_THAT(GetCellPixels(), ElementsAre("########",  //
                                           "........",  //
                                           "########",  //
                                           "........",  //
                                           "########",  //
                                           "........",  //
                                           "########",  //
                                           "........",  //
                                           "########",  //
                                           "........"));
}

TEST_F(ScreenTest, UnsetCharactersAreTheBuiltInCharacterSet) {
  // Every glyph, the alternates included, is drawn as the built-in character
  // set packs it.
  ASSERT_TRUE(InitScreen({.page = kPage, .columns = 32, .rows = 16}));
  std::vector<uint16_t> cells(kScreenGlyphCount);
  for (int glyph = 0; glyph < kScreenGlyphCount; ++glyph) {
    cells[glyph] = static_cast<uint16_t>(
        glyph < 256 ? glyph : kScreenAlternate | (glyph - 256));
  }
  WriteScreen(0, cells);
  ExecuteUntilRefresh();
  std::vector<uint16_t> characters;
  for (int glyph = 0; glyph < kScreenGlyphCount; ++glyph) {
    const std::vector<std::string> pixels =
        GetCellPixels(glyph % 32, glyph / 32);
    CellPixels cell_pixels;
    std::ranges::copy(pixels, cell_pixels.begin());
    std::ranges::copy(PackGlyph(cell_pixels), std::back_inserter(characters));
  }
  EXPECT_THAT(characters, ElementsAreArray(GetDefaultScreenCharacters()));
}

TEST_F(ScreenTest, CellIsDrawnFromItsGlyphWithItsAttributes) {
  // The glyphs are the settings' character set.
  ASSERT_TRUE(InitScreen(1, 1));
  const struct {
    uint16_t cell;
    CellPixels pixels;
  } cells[] = {
      {'A', kGlyphA},
      {kScreenHalfIntensity | 'A',
       {
           "........",  //
           "...++...",  //
           "..+..+..",  //
           ".+....+.",  //
           ".+....+.",  //
           ".++++++.",  //
           ".+....+.",  //
           ".+....+.",  //
           "........",  //
           "........",  //
       }},
      {kScreenInverse | 'A', kInverseGlyphA},
      {kScreenLineTop | 'A',
       {
           "########",  //
           "...##...",  //
           "..#..#..",  //
           ".#....#.",  //
           ".#....#.",  //
           ".######.",  //
           ".#....#.",  //
           ".#....#.",  //
           "........",  //
           "........",  //
       }},
      {kScreenLineBottom | 'A', kUnderlinedGlyphA},
      {kScreenLineLeft | 'A', kLeftEdgeGlyphA},
      {kScreenLineRight | 'A',
       {
           ".......#",  //
           "...##..#",  //
           "..#..#.#",  //
           ".#....##",  //
           ".#....##",  //
           ".#######",  //
           ".#....##",  //
           ".#....##",  //
           ".......#",  //
           ".......#",  //
       }},
      {kScreenHalfIntensity | kScreenLineTop | kScreenLineBottom |
           kScreenLineLeft | kScreenLineRight | 'A',
       {
           "++++++++",  //
           "+..++..+",  //
           "+.+..+.+",  //
           "++....++",  //
           "++....++",  //
           "++++++++",  //
           "++....++",  //
           "++....++",  //
           "+......+",  //
           "++++++++",  //
       }},
      {kScreenAlternate | 'A', kAlternateGlyphA},
      // Lines are drawn after inverse, so they always show.
      {kScreenAlternate | kScreenInverse | kScreenLineTop | 'A',
       {
           "########",  //
           "#.#.#.#.",  //
           ".#.#.#.#",  //
           "#.#.#.#.",  //
           ".#.#.#.#",  //
           "#.#.#.#.",  //
           ".#.#.#.#",  //
           "#.#.#.#.",  //
           ".#.#.#.#",  //
           "#.#.#.#.",  //
       }},
  };
  for (const auto& [cell, pixels] : cells) {
    WriteScreen(0, {cell});
    ExecuteUntilRefresh();
    EXPECT_THAT(GetCellPixels(), ElementsAreArray(pixels))
        << "Cell " << std::hex << cell;
  }
}

TEST_F(ScreenTest, CursorIsDrawnOnItsCellInItsStyle) {
  ASSERT_TRUE(InitScreen(2, 1));
  WriteScreen(0, {'A', 'A'});
  const struct {
    uint16_t cursor;
    CellPixels pixels;
  } cursors[] = {
      {CursorWord(ScreenCursorMode::kOff, ScreenCursorStyle::kBlock), kGlyphA},
      {CursorWord(ScreenCursorMode::kOn, ScreenCursorStyle::kBlock),
       kBlockPixels},
      {CursorWord(ScreenCursorMode::kOn, ScreenCursorStyle::kInverse),
       kInverseGlyphA},
      {CursorWord(ScreenCursorMode::kOn, ScreenCursorStyle::kUnderline),
       kUnderlinedGlyphA},
      {CursorWord(ScreenCursorMode::kOn, ScreenCursorStyle::kLeftEdge),
       kLeftEdgeGlyphA},
  };
  WriteScreen(ScreenMemory::kCursorColumnAddress, {1, 0});
  for (const auto& [cursor, pixels] : cursors) {
    WriteScreen(ScreenMemory::kCursorAddress, {cursor});
    ExecuteUntilRefresh();
    EXPECT_THAT(GetCellPixels(0), ElementsAreArray(kGlyphA));
    EXPECT_THAT(GetCellPixels(1), ElementsAreArray(pixels))
        << "Cursor " << std::hex << cursor;
  }
}

TEST_F(ScreenTest, CursorIsDrawnAtItsCellsLevel) {
  ASSERT_TRUE(InitScreen(1, 1));
  WriteScreen(0, {kScreenHalfIntensity | 'A'});
  WriteScreen(ScreenMemory::kCursorAddress,
              {CursorWord(ScreenCursorMode::kOn, ScreenCursorStyle::kBlock)});
  ExecuteUntilRefresh();
  EXPECT_THAT(GetCellPixels(), Each("++++++++"));
}

TEST_F(ScreenTest, CursorIsDrawnOverABlinkingCellInItsOffPhase) {
  // At 2 Hz, a slow blink of 1 Hz is off at frame 1.
  ASSERT_TRUE(InitScreen({.page = kPage,
                          .columns = 1,
                          .rows = 1,
                          .refresh_hz = 2,
                          .slow_blink_hz = 1,
                          .fast_blink_hz = 1,
                          .characters = TestCharacters()}));
  WriteScreen(0, {kScreenBlink | 'A'});
  ExecuteUntilRefresh();
  EXPECT_THAT(GetCellPixels(), Each("........"));

  WriteScreen(ScreenMemory::kCursorAddress,
              {CursorWord(ScreenCursorMode::kOn, ScreenCursorStyle::kBlock)});
  ExecuteUntilRefresh();  // Frame 2 is on, and frame 3 off.
  ExecuteUntilRefresh();
  EXPECT_THAT(GetCellPixels(), ElementsAreArray(kBlockPixels));
}

TEST_F(ScreenTest, CompactRowsArePacked8PixelRowsApart) {
  ASSERT_TRUE(InitScreen(1, 3));
  WriteScreen(0, {kScreenHalfIntensity | kScreenLineBottom | 'A',
                  kScreenLineTop | 'A', kScreenLineBottom | 'A'});
  ExecuteUntilRefresh();
  EXPECT_EQ(screen_->GetPicture().drawn_height, 30);

  // Row 0's bottom two pixel rows overlap row 1's top two, where each pixel is
  // the brighter of the two, and the bottom two pixel rows, which row 2 lit at
  // the last refresh, are off.
  WriteScreen(ScreenMemory::kCompactRowAddress, {0, 1});
  ExecuteUntilRefresh();
  EXPECT_EQ(screen_->GetPicture().drawn_height, 28);
  EXPECT_THAT(GetPixelRows(), ElementsAre("........",  // Row 0
                                          "...++...",  //
                                          "..+..+..",  //
                                          ".+....+.",  //
                                          ".+....+.",  //
                                          ".++++++.",  //
                                          ".+....+.",  //
                                          ".+....+.",  //
                                          "########",  // Row 0 and row 1
                                          "+++##+++",  //
                                          "..#..#..",  // Row 1
                                          ".#....#.",  //
                                          ".#....#.",  //
                                          ".######.",  //
                                          ".#....#.",  //
                                          ".#....#.",  //
                                          "........",  //
                                          "........",  //
                                          "........",  // Row 2
                                          "...##...",  //
                                          "..#..#..",  //
                                          ".#....#.",  //
                                          ".#....#.",  //
                                          ".######.",  //
                                          ".#....#.",  //
                                          ".#....#.",  //
                                          "........",  //
                                          "########",  //
                                          "........",  // Not drawn
                                          "........"));
}

TEST_F(ScreenTest, CompactBottomRowIsDrawnWhole) {
  ASSERT_TRUE(InitScreen(1, 2));
  WriteScreen(0, {kScreenLineBottom | 'A', kScreenLineBottom | 'A'});
  WriteScreen(ScreenMemory::kCompactRowAddress, {0, 2});
  ExecuteUntilRefresh();
  EXPECT_EQ(screen_->GetPicture().drawn_height, 18);
  EXPECT_THAT(GetPixelRows(), ElementsAre("........",  // Row 0
                                          "...##...",  //
                                          "..#..#..",  //
                                          ".#....#.",  //
                                          ".#....#.",  //
                                          ".######.",  //
                                          ".#....#.",  //
                                          ".#....#.",  //
                                          "........",  // Row 0 and row 1
                                          "########",  //
                                          "..#..#..",  // Row 1
                                          ".#....#.",  //
                                          ".#....#.",  //
                                          ".######.",  //
                                          ".#....#.",  //
                                          ".#....#.",  //
                                          "........",  //
                                          "########",  //
                                          "........",  // Not drawn
                                          "........"));
}

TEST_F(ScreenTest, CompactRowsStayPutAsTheCellsScroll) {
  // Screen row 0 is compact, and shows the cells' row 1, while the cells' row
  // 0, which would be compact if the band were in the cells' rows, is shown on
  // the bottom row.
  ASSERT_TRUE(InitScreen(1, 3));
  WriteScreen(0, {kScreenLineBottom | 'A', kScreenLineTop});
  WriteScreen(ScreenMemory::kScrollRowAddress, {1});
  WriteScreen(ScreenMemory::kCompactRowAddress, {0, 1});
  ExecuteUntilRefresh();
  EXPECT_EQ(screen_->GetPicture().drawn_height, 28);
  EXPECT_THAT(GetPixelRows(), ElementsAre("########",  // Row 0
                                          "........",  //
                                          "........",  //
                                          "........",  //
                                          "........",  //
                                          "........",  //
                                          "........",  //
                                          "........",  //
                                          "........",  // Row 0 and row 1
                                          "........",  //
                                          "........",  // Row 1
                                          "........",  //
                                          "........",  //
                                          "........",  //
                                          "........",  //
                                          "........",  //
                                          "........",  //
                                          "........",  //
                                          "........",  // Row 2
                                          "...##...",  //
                                          "..#..#..",  //
                                          ".#....#.",  //
                                          ".#....#.",  //
                                          ".######.",  //
                                          ".#....#.",  //
                                          ".#....#.",  //
                                          "........",  //
                                          "########",  //
                                          "........",  // Not drawn
                                          "........"));
}

TEST_F(ScreenTest, DrawnHeightCountsTheCompactRowsAboveTheBottom) {
  ASSERT_TRUE(InitScreen(1, 3));
  const struct {
    uint16_t compact_row;
    uint16_t compact_row_count;
    int drawn_height;
  } bands[] = {
      {0, 0, 30},
      {1, 0, 30},
      {0, 1, 28},
      {1, 1, 28},
      {2, 1, 30},
      {0, 2, 26},
      {0, 3, 26},
      {1, 2, 28},
      // Rows of the band outside the screen are ignored.
      {0, 0xFFFF, 26},
      {2, 5, 30},
      {3, 1, 30},
      {0xFFFF, 0xFFFF, 30},
  };
  for (const auto& [compact_row, compact_row_count, drawn_height] : bands) {
    WriteScreen(ScreenMemory::kCompactRowAddress,
                {compact_row, compact_row_count});
    ExecuteUntilRefresh();
    EXPECT_EQ(screen_->GetPicture().drawn_height, drawn_height)
        << "Band " << compact_row << ", " << compact_row_count;
  }
}

class ScreenBlinkTest : public ScreenTest {
 protected:
  // Whether a blinking cell, and a blinking cursor, were drawn at each refresh,
  // as a '1' or a '0' for each.
  struct Phases {
    std::string cell;
    std::string cursor;
  };

  // Initializes a 2 by 1 screen refreshing at `refresh_hz`, with the blink
  // rates, a blinking 'A' on cell 0, and a block cursor of `cursor_mode` on
  // cell 1.
  bool InitBlink(int refresh_hz, int slow_blink_hz, int fast_blink_hz,
                 ScreenCursorMode cursor_mode) {
    if (!InitScreen({.page = kPage,
                     .columns = 2,
                     .rows = 1,
                     .refresh_hz = refresh_hz,
                     .slow_blink_hz = slow_blink_hz,
                     .fast_blink_hz = fast_blink_hz,
                     .characters = TestCharacters()})) {
      return false;
    }
    WriteScreen(0, {kScreenBlink | 'A', 'A'});
    WriteScreen(ScreenMemory::kCursorColumnAddress,
                {1, 0, CursorWord(cursor_mode, ScreenCursorStyle::kBlock)});
    return true;
  }

  // Executes `frames` refreshes, and returns the phases drawn.
  Phases ExecutePhases(int frames) {
    Phases phases;
    for (int frame = 1; frame <= frames; ++frame) {
      ExecuteUntilRefresh();
      EXPECT_EQ(screen_->GetFrameCount(), frame);
      phases.cell += GetCellPixels(0)[1] == kGlyphA[1] ? '1' : '0';
      phases.cursor += GetCellPixels(1)[0] == "########" ? '1' : '0';
    }
    return phases;
  }
};

TEST_F(ScreenBlinkTest, BlinksAtTheDefaultRates) {
  // A blink is on while frames * 2 * blink_hz / 30 is even.
  ASSERT_TRUE(InitBlink(30, 1, 4, ScreenCursorMode::kFastBlink));
  const Phases phases = ExecutePhases(31);
  EXPECT_EQ(phases.cell,
            "11111111111111"
            "000000000000000"
            "11");
  EXPECT_EQ(phases.cursor,
            "111"
            "0000"
            "1111"
            "000"
            "1111"
            "0000"
            "1111"
            "000"
            "11");
}

TEST_F(ScreenBlinkTest, BlinksAtOtherRates) {
  ASSERT_TRUE(InitBlink(10, 2, 5, ScreenCursorMode::kFastBlink));
  const Phases phases = ExecutePhases(10);
  EXPECT_EQ(phases.cell,
            "11"
            "00"
            "111"
            "00"
            "1");
  EXPECT_EQ(phases.cursor, "0101010101");
}

TEST_F(ScreenBlinkTest, SlowBlinkingCursorBlinksWithBlinkingCells) {
  ASSERT_TRUE(InitBlink(10, 2, 5, ScreenCursorMode::kSlowBlink));
  const Phases phases = ExecutePhases(10);
  EXPECT_EQ(phases.cell, "1100111001");
  EXPECT_EQ(phases.cursor, "1100111001");
}

TEST_F(ScreenBlinkTest, CursorThatIsOnDoesNotBlink) {
  ASSERT_TRUE(InitBlink(10, 2, 5, ScreenCursorMode::kOn));
  EXPECT_EQ(ExecutePhases(10).cursor, "1111111111");
}

TEST_F(ScreenTest, RefreshWaitsWhileTheBankIsHeld) {
  ASSERT_TRUE(InitScreen(2, 2));
  WriteScreen(0, {1, 2, 3, 4});

  // Refresh 1 is at cycle 16, and waits until the bank is released at cycle
  // 20, taking the cells as they are then.
  std::unique_ptr<Lock> lock = LockBank();
  Execute(20);
  EXPECT_EQ(screen_->GetFrameCount(), 0);
  MemoryBank& bank = *GetProcessor().GetMemory(0);
  bank.SetAddress(*lock, static_cast<uint16_t>(start_));
  bank.StoreWord(*lock, 5);
  lock.reset();
  EXPECT_EQ(ExecuteUntilRefresh(), 21);
  EXPECT_THAT(screen_->GetPicture().cells, ElementsAre(5, 2, 3, 4));

  // Refresh 2 is still at cycle 33.
  EXPECT_EQ(ExecuteUntilRefresh(), 34);
}

TEST_F(ScreenTest, RefreshesMissedWhileTheBankIsHeldCatchUp) {
  ASSERT_TRUE(InitScreen(2, 2));

  // Refreshes 1 and 2 (at cycles 16 and 33) are both due once the bank is
  // released at cycle 40, so they are a cycle apart, and none is skipped.
  std::unique_ptr<Lock> lock = LockBank();
  Execute(40);
  EXPECT_EQ(screen_->GetFrameCount(), 0);
  lock.reset();
  EXPECT_EQ(ExecuteUntilRefresh(), 41);
  EXPECT_EQ(ExecuteUntilRefresh(), 42);
  EXPECT_THAT(ReadScreen(ScreenMemory::kFrameCountAddress), ElementsAre(2));

  // Refresh 3 is back on the schedule, at cycle 50.
  EXPECT_EQ(ExecuteUntilRefresh(), 51);
}

TEST_F(ScreenTest, RefreshTakesTheBankFromACoreAsItReleasesIt) {
  // At a refresh rate of the clock rate, the screen refreshes every cycle.
  ASSERT_TRUE(InitScreen({.page = kPage, .refresh_hz = 60}, /*clock_hz=*/60));
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        NOP
        NOP
        NOP
    done:
        NOP
  )---"));

  // A NOP holds the code bank for its fetch, and the next instruction requests
  // it again in the cycle the NOP releases it. A refresh waiting for the bank
  // is granted it first, and the core gets it once the refresh is done, which
  // is after the core has run that cycle: so each NOP takes 4 cycles rather
  // than 3. This is the lock handoff's cost, which "Order-independent lock
  // handoff" in the backlog removes.
  EXPECT_EQ(CyclesUntilIp(GetLabel("DONE") + 1), 12);
}

TEST_F(ScreenTest, ScreenCanBeAnywhereInABank) {
  ASSERT_TRUE(InitScreen({.bank = 1,
                          .page = 3,
                          .interrupt = {.interrupt = kInterrupt},
                          .columns = 2,
                          .rows = 1},
                         kSlowClockHz, /*num_memory_banks=*/2));
  EXPECT_EQ(start_, 0x3000);
  EXPECT_THAT(ReadScreen(ScreenMemory::kColumnsAddress, 2), ElementsAre(2, 1));
  WriteScreen(0, {'O', 'K'});
  ExecuteUntilRefresh();
  EXPECT_THAT(screen_->GetPicture().cells, ElementsAre('O', 'K'));
}

TEST_F(ScreenTest, CreateFailsForInvalidSettings) {
  const struct {
    ScreenSettings settings;
    std::string_view error;
  } invalid[] = {
      {{.columns = 0}, "is 0 by 24"},
      {{.rows = 0}, "is 80 by 0"},
      {{.columns = -1}, "is -1 by 24"},
      {{.columns = 133},
       "is 133 by 24, but it can have at most 132 columns "
       "and 64 rows"},
      {{.columns = 1, .rows = 65}, "is 1 by 65"},
      {{.columns = 4080, .rows = 1}, "is 4080 by 1"},
      {{.columns = 1, .rows = 4080}, "is 1 by 4080"},
      {{.columns = 100'000, .rows = 100'000}, "is 100000 by 100000"},
      {{.columns = 77, .rows = 53}, "at most 4080 cells"},
      {{.columns = 64, .rows = 64}, "at most 4080 cells"},
      {{.refresh_hz = 0}, "refresh rate of 0"},
      {{.slow_blink_hz = 0}, "slow blink rate of 0"},
      {{.slow_blink_hz = 16}, "slow blink rate of 16"},
      {{.fast_blink_hz = 0}, "fast blink rate of 0"},
      {{.refresh_hz = 60, .fast_blink_hz = 31}, "fast blink rate of 31"},
      {{.refresh_hz = 1}, "slow blink rate of 1"},
      {{.characters = std::vector<uint16_t>()}, "character set of 0 words"},
      {{.characters = std::vector<uint16_t>(kScreenCharacterSetSize - 1)},
       "character set of 2559 words"},
      {{.characters = std::vector<uint16_t>(kScreenCharacterSetSize + 1)},
       "character set of 2561 words"},
  };
  for (const auto& [settings, error] : invalid) {
    absl::StatusOr<std::unique_ptr<ScreenFactory>> factory =
        ScreenFactory::Create("Screen", settings);
    EXPECT_FALSE(factory.ok()) << error;
    EXPECT_THAT(factory.status().message(),
                AllOf(HasSubstr("Screen Screen"), HasSubstr(error)));
  }
}

TEST_F(ScreenTest, CreateSucceedsForTheLargestSizesThatFit) {
  for (const auto& [columns, rows] :
       {std::pair{80, 51}, std::pair{132, 30}, std::pair{63, 64},
        std::pair{132, 1}, std::pair{1, 64}}) {
    EXPECT_TRUE(
        ScreenFactory::Create("Screen", {.columns = columns, .rows = rows})
            .ok())
        << columns << " by " << rows;
  }
  ASSERT_TRUE(InitScreen(80, 51));
  WriteScreen(80 * 51 - 1, {'Z'});
  ExecuteUntilRefresh();
  EXPECT_EQ(screen_->GetPicture().cells.back(), 'Z');
}

TEST_F(ScreenTest, CreateSucceedsForBlinkRatesUpToHalfTheRefreshRate) {
  EXPECT_TRUE(
      ScreenFactory::Create(
          "Screen", {.refresh_hz = 8, .slow_blink_hz = 4, .fast_blink_hz = 4})
          .ok());
  EXPECT_TRUE(
      ScreenFactory::Create(
          "Screen", {.refresh_hz = 9, .slow_blink_hz = 1, .fast_blink_hz = 4})
          .ok());
}

TEST_F(ScreenTest, FactoryWiresThePageAndInterrupt) {
  absl::StatusOr<std::unique_ptr<ScreenFactory>> factory =
      ScreenFactory::Create(
          "Display",
          {.bank = 2, .page = 7, .interrupt = {.interrupt = 5, .cores = 2}});
  ASSERT_TRUE(factory.ok()) << factory.status();
  const DeviceConfig& config = (*factory)->GetConfig();
  EXPECT_EQ(config.name, "Display");
  EXPECT_THAT(config.ports, IsEmpty());
  EXPECT_THAT(config.interrupts,
              ElementsAre(AllOf(Field(&DeviceInterrupt::interrupt, 5),
                                Field(&DeviceInterrupt::cores, 2))));
  EXPECT_EQ(config.memory.bank, 2);
  EXPECT_EQ(config.memory.pages.start_page, 7);
  EXPECT_EQ(config.memory.pages.page_count, 1);
  EXPECT_EQ(config.memory.masks.read_mask, 1);
  EXPECT_EQ(config.memory.masks.write_mask, 1);
}

}  // namespace
}  // namespace oz3
