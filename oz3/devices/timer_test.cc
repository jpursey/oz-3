// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/timer.h"

#include <cstdint>
#include <memory>
#include <utility>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/time/civil_time.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/device_config.h"
#include "oz3/core/lockable.h"
#include "oz3/core/port.h"
#include "oz3/devices/device_program_test.h"

namespace oz3 {
namespace {

using ::testing::AllOf;
using ::testing::ElementsAre;
using ::testing::Field;
using ::testing::HasSubstr;

// The processor ports of the ticks and the clock, and the tick's interrupt.
constexpr int kTickPort = 3;
constexpr int kClockPort = 4;
constexpr int kInterrupt = 2;

// A clock rate where ticks are a few cycles apart, so tick n at 60 Hz is at
// cycle n * 1000 / 60.
constexpr int64_t kSlowClockHz = 1000;

// A port's words and status.
struct PortWords {
  bool operator==(const PortWords&) const = default;

  uint16_t word0 = 0;
  uint16_t word1 = 0;
  uint16_t status = 0;
};

class TimerTest : public DeviceProgramTest {
 protected:
  // The most cycles ExecuteUntilTick waits.
  static constexpr Cycles kMaxTickCycles = 1'000'000;

  // Initializes a processor with a timer named "Timer" with `settings`, and
  // sets timer_ to it. Returns whether it did.
  bool InitTimer(
      const TimerSettings& settings = {.port = kTickPort,
                                       .interrupt = {.interrupt = kInterrupt}},
      int64_t clock_hz = kSlowClockHz) {
    absl::StatusOr<std::unique_ptr<TimerFactory>> factory =
        TimerFactory::Create("Timer", settings);
    EXPECT_TRUE(factory.ok()) << factory.status();
    if (!factory.ok() || !Init({.num_ports = settings.port + Timer::kPorts,
                                .clock_hz = clock_hz,
                                .devices = {*std::move(factory)}})) {
      return false;
    }
    timer_ = static_cast<Timer*>(GetProcessor().GetDevice("Timer"));
    return timer_ != nullptr;
  }

  // Returns the words and status of `port`.
  PortWords GetWords(int port) {
    const Port& p = GetPort(port);
    return {.word0 = p.GetValue(0),
            .word1 = p.GetValue(1),
            .status = p.GetStatus()};
  }

  // Asks the clock port for `index`, writing it as OUT does, which sets its
  // status, and returns its answer the cycle after, as a dword. No core may be
  // using the port.
  uint32_t AskClock(uint16_t index) {
    {
      std::unique_ptr<Lock> lock = LockPort(kClockPort);
      EXPECT_TRUE(lock->IsLocked());
      GetPort(kClockPort).StoreWord(*lock, Port::S, index);
    }
    Execute(1);
    const PortWords words = GetWords(kClockPort);
    EXPECT_EQ(words.status, 0);
    return words.word0 | (static_cast<uint32_t>(words.word1) << 16);
  }

  // Executes until the next tick, and returns the processor's cycles when it
  // is done (one past the tick's cycle), or -1 if it isn't within
  // kMaxTickCycles.
  Cycles ExecuteUntilTick() {
    const int64_t tick_count = timer_->GetTickCount();
    for (Cycles cycles = 0; cycles < kMaxTickCycles; ++cycles) {
      Execute(1);
      if (timer_->GetTickCount() != tick_count) {
        return GetCycles();
      }
    }
    return -1;
  }

  // Returns whether kInterrupt is raised on core 0.
  bool IsInterruptRaised() {
    return (GetState().core.GetInterrupts() & (1u << kInterrupt)) != 0;
  }

  // Gives the timer a clock that returns `now`.
  void SetNow(absl::CivilSecond now) {
    now_ = now;
    timer_->SetClock([this] { return GetNow(); });
  }

  // The timer's clock, which counts its calls.
  absl::CivilSecond GetNow() {
    ++clock_calls_;
    return now_;
  }

  Timer* timer_ = nullptr;
  absl::CivilSecond now_;
  int clock_calls_ = 0;
};

TEST_F(TimerTest, TickPortIsClearAtPowerOn) {
  ASSERT_TRUE(InitTimer());
  Execute(1);
  EXPECT_EQ(timer_->GetTickCount(), 0);
  EXPECT_EQ(GetWords(kTickPort), PortWords{});
  EXPECT_FALSE(IsInterruptRaised());
}

TEST_F(TimerTest, TicksOnScheduleWithoutDrift) {
  ASSERT_TRUE(InitTimer());

  // A tick is 16 2/3 cycles, so tick n is at cycle n * 1000 / 60.
  uint16_t tick = 0;
  for (const Cycles tick_cycle : {16, 33, 50, 66, 83, 100, 116}) {
    EXPECT_EQ(ExecuteUntilTick(), tick_cycle + 1);
    ++tick;
    EXPECT_EQ(GetWords(kTickPort),
              (PortWords{.word0 = tick, .word1 = 0, .status = 1}));
  }
  EXPECT_EQ(timer_->GetTickCount(), 7);
}

TEST_F(TimerTest, TicksAtTheDefaultRate) {
  ASSERT_TRUE(InitTimer({.port = kTickPort}, ProcessorConfig::kDefaultClockHz));
  EXPECT_EQ(ExecuteUntilTick(), 16666 + 1);
  EXPECT_EQ(ExecuteUntilTick(), 33333 + 1);
}

TEST_F(TimerTest, TicksAtOtherRates) {
  ASSERT_TRUE(InitTimer({.port = kTickPort, .tick_hz = 1000}));
  EXPECT_EQ(ExecuteUntilTick(), 2);
  EXPECT_EQ(ExecuteUntilTick(), 3);
}

TEST_F(TimerTest, TickPortHoldsTheLow32Bits) {
  // At a tick rate of the clock rate, the timer ticks every cycle.
  ASSERT_TRUE(InitTimer({.port = kTickPort}, /*clock_hz=*/60));
  Execute(0x10001);
  EXPECT_EQ(timer_->GetTickCount(), 0x10000);
  EXPECT_EQ(GetWords(kTickPort),
            (PortWords{.word0 = 0, .word1 = 1, .status = 1}));
}

TEST_F(TimerTest, RaisesTheInterruptAtEachTick) {
  ASSERT_TRUE(InitTimer());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        SETI 2, handler
        MVQ R6, 0
        EI
    idle:
        JP idle

    handler:                ; Counts the ticks
        ADQ R6, 1
        IRT
  )---"));
  ASSERT_TRUE(ExecuteUntilIp(GetLabel("IDLE")));
  ASSERT_LT(GetCycles(), 16);
  const CoreState& state = GetState();
  Execute(100 - GetCycles());
  EXPECT_EQ(state.r6, 5);
  Execute(1);
  EXPECT_EQ(state.r6, 5);
  Execute(100);
  EXPECT_EQ(state.r6, 11);
}

TEST_F(TimerTest, ProgramWaitsForTicksWithIns) {
  ASSERT_TRUE(InitTimer());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
    wait0:
        INS 3, D0
        JC NS, wait0
    wait1:
        INS 3, D1
        JC NS, wait1
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  const CoreState& state = GetState();
  EXPECT_EQ(state.d0(), 1);
  EXPECT_EQ(state.d1(), 2);
  EXPECT_GT(GetCycles(), 33);
  EXPECT_LT(GetCycles(), 50);
  EXPECT_EQ(GetPort(kTickPort).GetStatus(), 0);
}

TEST_F(TimerTest, TickWaitsWhileThePortIsHeld) {
  ASSERT_TRUE(InitTimer());

  // Tick 1 is at cycle 16, and waits until the port is released at cycle 20.
  std::unique_ptr<Lock> lock = LockPort(kTickPort);
  ASSERT_TRUE(lock->IsLocked());
  Execute(20);
  EXPECT_EQ(timer_->GetTickCount(), 0);
  EXPECT_FALSE(IsInterruptRaised());
  lock.reset();
  EXPECT_EQ(ExecuteUntilTick(), 21);
  EXPECT_TRUE(IsInterruptRaised());

  // Tick 2 is still at cycle 33.
  EXPECT_EQ(ExecuteUntilTick(), 34);
}

TEST_F(TimerTest, TicksMissedWhileThePortIsHeldCatchUp) {
  ASSERT_TRUE(InitTimer());

  // Ticks 1 and 2 (at cycles 16 and 33) are both due once the port is
  // released at cycle 40, so they are a cycle apart, and none is skipped.
  std::unique_ptr<Lock> lock = LockPort(kTickPort);
  ASSERT_TRUE(lock->IsLocked());
  Execute(40);
  lock.reset();
  EXPECT_EQ(ExecuteUntilTick(), 41);
  EXPECT_EQ(ExecuteUntilTick(), 42);
  EXPECT_EQ(GetWords(kTickPort).word0, 2);

  // Tick 3 is back on the schedule, at cycle 50.
  EXPECT_EQ(ExecuteUntilTick(), 51);
}

TEST_F(TimerTest, SleepsUntilTheNextTick) {
  ASSERT_TRUE(InitTimer());
  Execute(1);
  EXPECT_EQ(timer_->GetDueCycle(), 16);
  EXPECT_EQ(ExecuteUntilTick(), 17);
  Execute(1);
  EXPECT_EQ(timer_->GetDueCycle(), 33);
}

TEST_F(TimerTest, ProgramReadsTheCycleCountRightAfterAskingForIt) {
  ASSERT_TRUE(InitTimer());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUT.IW 4, 0           ; kTimerCycles
        IN 4, D0
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());

  // The OUT writes the index in cycle 5 (its 6th), and the timer answers in
  // the cycle after.
  EXPECT_EQ(GetState().d0(), 6);
  EXPECT_EQ(GetPort(kClockPort).GetStatus(), 0);
}

TEST_F(TimerTest, CycleCountIsTheCycleTheTimerAnswers) {
  ASSERT_TRUE(InitTimer());
  Execute(1000);
  EXPECT_EQ(AskClock(kTimerCycles), 1000);
  Execute(10);
  EXPECT_EQ(AskClock(kTimerCycles), 1011);
}

TEST_F(TimerTest, TickAndAnswerWaitForTheirPortsTogether) {
  ASSERT_TRUE(InitTimer());
  Execute(10);

  // Tick 1 is due at cycle 16, and the question is asked at cycle 10, but both
  // ports are held until cycle 20.
  std::unique_ptr<Lock> tick_lock = LockPort(kTickPort);
  std::unique_ptr<Lock> clock_lock = LockPort(kClockPort);
  ASSERT_TRUE(tick_lock->IsLocked());
  ASSERT_TRUE(clock_lock->IsLocked());
  GetPort(kClockPort).StoreWord(*clock_lock, Port::S, kTimerCycles);
  Execute(10);
  EXPECT_EQ(timer_->GetTickCount(), 0);
  tick_lock.reset();
  clock_lock.reset();
  Execute(1);
  EXPECT_EQ(timer_->GetTickCount(), 1);
  EXPECT_EQ(GetWords(kClockPort),
            (PortWords{.word0 = 20, .word1 = 0, .status = 0}));
}

TEST_F(TimerTest, SecondQuestionReplacesTheFirst) {
  ASSERT_TRUE(InitTimer());
  SetNow(absl::CivilSecond(2026, 11, 7, 13, 45, 30));
  Execute(10);
  std::unique_ptr<Lock> lock = LockPort(kClockPort);
  ASSERT_TRUE(lock->IsLocked());
  GetPort(kClockPort).StoreWord(*lock, Port::S, kTimerCycles);
  GetPort(kClockPort).StoreWord(*lock, Port::S, kTimerTime);
  lock.reset();
  Execute(1);
  EXPECT_EQ(GetWords(kClockPort),
            (PortWords{.word0 = 13, .word1 = 0x2D1E, .status = 0}));
}

TEST_F(TimerTest, MillisecondsAreTheCycleCountAtTheClockRate) {
  // A cycle is a tenth of a millisecond.
  ASSERT_TRUE(InitTimer({.port = kTickPort}, /*clock_hz=*/10'000));
  Execute(12'345);
  EXPECT_EQ(AskClock(kTimerMilliseconds), 1234);
  Execute(30'000 - GetCycles());
  EXPECT_EQ(AskClock(kTimerMilliseconds), 3000);
}

TEST_F(TimerTest, MillisecondsRoundDown) {
  // A cycle is 333 1/3 milliseconds.
  ASSERT_TRUE(InitTimer({.port = kTickPort, .tick_hz = 1}, /*clock_hz=*/3));
  Execute(1);
  EXPECT_EQ(AskClock(kTimerMilliseconds), 333);
  EXPECT_EQ(AskClock(kTimerMilliseconds), 666);
  EXPECT_EQ(AskClock(kTimerMilliseconds), 1000);
  EXPECT_EQ(AskClock(kTimerMilliseconds), 1333);
}

TEST_F(TimerTest, AnswerWaitsWhileThePortIsHeld) {
  ASSERT_TRUE(InitTimer());
  std::unique_ptr<Lock> lock = LockPort(kClockPort);
  ASSERT_TRUE(lock->IsLocked());
  GetPort(kClockPort).StoreWord(*lock, Port::S, kTimerCycles);
  Execute(10);
  EXPECT_EQ(GetWords(kClockPort),
            (PortWords{.word0 = kTimerCycles, .word1 = 0, .status = 1}));
  lock.reset();
  Execute(1);
  EXPECT_EQ(GetWords(kClockPort),
            (PortWords{.word0 = 10, .word1 = 0, .status = 0}));
}

TEST_F(TimerTest, ReadBeforeTheAnswerTakesTheIndexBack) {
  ASSERT_TRUE(InitTimer());
  std::unique_ptr<Lock> lock = LockPort(kClockPort);
  ASSERT_TRUE(lock->IsLocked());
  Port& port = GetPort(kClockPort);
  port.StoreWord(*lock, Port::S, kTimerCycles);
  Execute(10);
  uint16_t value = 0;
  port.LoadWord(*lock, Port::S, value);
  lock.reset();
  Execute(10);
  EXPECT_EQ(GetWords(kClockPort),
            (PortWords{.word0 = kTimerCycles, .word1 = 0, .status = 0}));
}

TEST_F(TimerTest, DateAndTimeAreZerosWithoutAClock) {
  ASSERT_TRUE(InitTimer());
  Execute(1);
  EXPECT_EQ(AskClock(kTimerTime), 0);
  EXPECT_EQ(AskClock(kTimerDate), 0);

  // Taking the clock away goes back to zeros.
  SetNow(absl::CivilSecond(2026, 11, 7, 13, 45, 30));
  EXPECT_EQ(AskClock(kTimerTime), 0x2D1E'000D);
  EXPECT_EQ(AskClock(kTimerDate), 0x0B07'07EA);
  timer_->SetClock(nullptr);
  EXPECT_EQ(AskClock(kTimerTime), 0);
  EXPECT_EQ(AskClock(kTimerDate), 0);
}

TEST_F(TimerTest, DateIsZerosBeforeTheFirstTime) {
  ASSERT_TRUE(InitTimer());
  SetNow(absl::CivilSecond(2026, 11, 7, 13, 45, 30));
  Execute(1);
  EXPECT_EQ(AskClock(kTimerDate), 0);
}

TEST_F(TimerTest, ClockIsCalledOnlyForTheTime) {
  ASSERT_TRUE(InitTimer());
  SetNow(absl::CivilSecond(2026, 11, 7, 13, 45, 30));
  Execute(1000);
  for (const uint16_t index :
       {kTimerCycles, kTimerMilliseconds, kTimerDate, uint16_t{4}}) {
    AskClock(index);
  }
  EXPECT_EQ(clock_calls_, 0);
  AskClock(kTimerTime);
  EXPECT_EQ(clock_calls_, 1);
  AskClock(kTimerDate);
  EXPECT_EQ(clock_calls_, 1);
}

TEST_F(TimerTest, ProgramReadsTheTimeAndDate) {
  ASSERT_TRUE(InitTimer());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUT.IW 4, 3           ; kTimerTime
        IN 4, D1
        OUT.IW 4, 2           ; kTimerDate
        IN 4, D0
        HALT
  )---"));

  SetNow(absl::CivilSecond(2026, 11, 7, 13, 45, 30));
  ASSERT_TRUE(ExecuteUntilHalt());
  const CoreState& state = GetState();
  EXPECT_EQ(state.d0(), 0x0B07'07EA);
  EXPECT_EQ(state.d1(), 0x2D1E'000D);
}

TEST_F(TimerTest, TimeIsCurrentWithoutTheDate) {
  ASSERT_TRUE(InitTimer());
  Execute(1);
  SetNow(absl::CivilSecond(2026, 11, 7, 13, 45, 30));
  EXPECT_EQ(AskClock(kTimerTime), 0x2D1E'000D);
  SetNow(absl::CivilSecond(2026, 11, 7, 13, 45, 31));
  EXPECT_EQ(AskClock(kTimerTime), 0x2D1F'000D);
}

TEST_F(TimerTest, YearOutsideAWordIsItsLow16Bits) {
  ASSERT_TRUE(InitTimer());
  Execute(1);
  SetNow(absl::CivilSecond(65536 + 2026, 11, 7, 0, 0, 0));
  AskClock(kTimerTime);
  EXPECT_EQ(AskClock(kTimerDate), 0x0B07'07EA);
  SetNow(absl::CivilSecond(-1, 11, 7, 0, 0, 0));
  AskClock(kTimerTime);
  EXPECT_EQ(AskClock(kTimerDate), 0x0B07'FFFF);
}

TEST_F(TimerTest, DateIsLatchedByTheTime) {
  ASSERT_TRUE(InitTimer());
  Execute(1);

  // Just before midnight, the time is latched, and the date read after
  // midnight is still the date latched with it.
  SetNow(absl::CivilSecond(1999, 12, 31, 23, 59, 59));
  EXPECT_EQ(AskClock(kTimerTime), 0x3B3B'0017);
  SetNow(absl::CivilSecond(2000, 1, 1, 0, 0, 0));
  EXPECT_EQ(AskClock(kTimerDate), 0x0C1F'07CF);

  // The next time latches the new date and time.
  EXPECT_EQ(AskClock(kTimerTime), 0);
  EXPECT_EQ(AskClock(kTimerDate), 0x0101'07D0);
}

TEST_F(TimerTest, OtherIndexesAnswer0) {
  ASSERT_TRUE(InitTimer());
  Execute(100);
  for (const uint16_t index : {4, 0x100, 0xFFFF}) {
    EXPECT_EQ(AskClock(index), 0) << "Index " << index;
  }
}

TEST_F(TimerTest, FactoryWiresThePortsAndInterrupt) {
  absl::StatusOr<std::unique_ptr<TimerFactory>> factory = TimerFactory::Create(
      "Clock", {.port = 7, .interrupt = {.interrupt = 5, .cores = 2}});
  ASSERT_TRUE(factory.ok()) << factory.status();
  const DeviceConfig& config = (*factory)->GetConfig();
  EXPECT_EQ(config.name, "Clock");
  EXPECT_THAT(config.ports, ElementsAre(7, 8));
  EXPECT_THAT(config.interrupts,
              ElementsAre(AllOf(Field(&DeviceInterrupt::interrupt, 5),
                                Field(&DeviceInterrupt::cores, 2))));
}

TEST_F(TimerTest, CreateValidatesTheTickRate) {
  for (const int tick_hz : {0, -1}) {
    absl::StatusOr<std::unique_ptr<TimerFactory>> factory =
        TimerFactory::Create("Clock", {.tick_hz = tick_hz});
    EXPECT_EQ(factory.status().code(), absl::StatusCode::kInvalidArgument);
    EXPECT_THAT(factory.status().message(), HasSubstr("Clock"));
    EXPECT_THAT(factory.status().message(), HasSubstr("tick rate"));
  }
  EXPECT_TRUE(TimerFactory::Create("Clock", {.tick_hz = 1}).ok());
}

}  // namespace
}  // namespace oz3
