// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/screen.h"

#include <cstdint>
#include <memory>
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

namespace oz3 {
namespace {

using ::testing::AllOf;
using ::testing::ElementsAre;
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
  // kInterrupt, and refreshing at 60 Hz.
  bool InitScreen(int columns = 80, int rows = 24) {
    return InitScreen({.page = kPage,
                       .interrupt = {.interrupt = kInterrupt},
                       .columns = columns,
                       .rows = rows,
                       .refresh_hz = 60});
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
  EXPECT_EQ(screen_->GetFrameCount(), 0);

  EXPECT_EQ(ReadScreen(0, ScreenMemory::kColumnsAddress),
            std::vector<uint16_t>(ScreenMemory::kColumnsAddress));
  EXPECT_THAT(ReadScreen(ScreenMemory::kColumnsAddress, 16),
              ElementsAre(80, 24, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0));
}

TEST_F(ScreenTest, CellsWrittenByAProgramAppearAtTheNextRefresh) {
  // Refresh 1 is at cycle 1000.
  ASSERT_TRUE(InitScreen({.page = kPage, .refresh_hz = 1}));
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
  ASSERT_LT(GetCycles(), 1000);
  EXPECT_THAT(ReadScreen(0), ElementsAre(kScreenInverse | 'H'));
  EXPECT_EQ(screen_->GetPicture().cells, std::vector<uint16_t>(80 * 24));
  Execute(1000 - GetCycles());
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
              ElementsAre(80, 24, 1, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0, 0, 0, 0,
                          0, 0, 0, 0, 0));
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
      {{.columns = 77, .rows = 53}, "at most 4080 cells"},
      {{.columns = 100'000, .rows = 100'000}, "at most 4080 cells"},
      {{.refresh_hz = 0}, "refresh rate of 0"},
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
       {std::pair{80, 51}, std::pair{132, 30}, std::pair{4080, 1}}) {
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
