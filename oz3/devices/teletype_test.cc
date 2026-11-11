// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/teletype.h"

#include <cstdint>
#include <memory>
#include <utility>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/device_config.h"
#include "oz3/core/lockable.h"
#include "oz3/core/port.h"
#include "oz3/core/processor_config.h"
#include "oz3/devices/device_program_test.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::HasSubstr;

// The processor port the teletype is on.
constexpr int kPort = 2;

class TeletypeTest : public DeviceProgramTest {
 protected:
  // The most cycles CyclesUntilTaken waits.
  static constexpr Cycles kMaxTakeCycles = 10000;

  // Initializes a processor at `clock_hz` with a teletype named "Teletype" on
  // kPort, with `settings` otherwise, and sets teletype_ to it. Returns whether
  // it did.
  bool InitTeletype(TeletypeSettings settings = {},
                    int64_t clock_hz = ProcessorConfig::kDefaultClockHz) {
    settings.port = kPort;
    absl::StatusOr<std::unique_ptr<TeletypeFactory>> factory =
        TeletypeFactory::Create("Teletype", settings);
    if (!factory.ok()) {
      ADD_FAILURE() << factory.status();
      return false;
    }
    if (!Init({.num_ports = kPort + 1,
               .clock_hz = clock_hz,
               .devices = {*std::move(factory)}})) {
      return false;
    }
    teletype_ = static_cast<Teletype*>(GetProcessor().GetDevice("Teletype"));
    return teletype_ != nullptr;
  }

  // Writes `value` to the teletype's port as OUT does. No core may be using the
  // port, so its lock is granted at once.
  void WritePort(uint16_t value) {
    std::unique_ptr<Lock> lock = LockPort(kPort);
    ASSERT_TRUE(lock->IsLocked());
    GetPort(kPort).StoreWord(*lock, Port::S, value);
  }

  // Executes until the teletype takes what is in its port, and returns the
  // cycles that took, or -1 if it doesn't within kMaxTakeCycles.
  Cycles CyclesUntilTaken() {
    for (Cycles cycles = 1; cycles <= kMaxTakeCycles; ++cycles) {
      Execute(1);
      if (GetPort(kPort).GetStatus() == 0) {
        return cycles;
      }
    }
    return -1;
  }

  Teletype* teletype_ = nullptr;
};

TEST_F(TeletypeTest, WritesCharactersAndPackedWords) {
  ASSERT_TRUE(InitTeletype());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUTS.IW 2, 'H'         ; One character, with a zero high byte
        OUTS.IW 2, "i!"        ; Two packed characters
        OUTS.IW 2, 0           ; Nothing
        OUTS.IW 2, 0A0Dh       ; Control characters pass through
        OUTS.IW 2, 0700h       ; A zero low byte is skipped
        OUTS.IW 2, 8
        OUTS.IW 2, 0C180h      ; 7 bits: C1h is 'A', and 80h is skipped
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(teletype_->TakeOutput(), "Hi!\r\n\a\bA");
  EXPECT_EQ(GetPort(kPort).GetStatus(), 0);
  EXPECT_EQ(teletype_->TakeOutput(), "");
}

TEST_F(TeletypeTest, OutrRunsToTheEnd) {
  ASSERT_TRUE(InitTeletype());
  ASSERT_TRUE(Load(R"---(
    .STACK 16

    .SEGMENT main CODE
        MOV R1, message     ; R1 is the message's address in DATA
        MVQ R7, 3           ; Its words
        NOP
    start:
        OUTR 2, (R1)
        NOP
    end:
        HALT

    .SEGMENT strings DATA ROM
    message:
        .DW "Hello!"

    .SEGMENT stack STACK
  )---"));
  ASSERT_TRUE(ExecuteUntilIp(GetLabel("START")));

  // The teletype takes each word before the next one, so OUTR writes all of
  // them, in the cycles it takes for a port that is always ready.
  EXPECT_EQ(CyclesUntilIp(GetLabel("END")), 3 * 9);
  const CoreState& state = GetState();
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st & CpuCore::S, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(teletype_->TakeOutput(), "Hello!");
}

TEST_F(TeletypeTest, OutrOfDwordsRunsToTheEnd) {
  ASSERT_TRUE(InitTeletype());
  ASSERT_TRUE(Load(R"---(
    .STACK 16

    .SEGMENT main CODE
        MOV R1, message     ; R1 is the message's address in DATA
        MVQ R7, 3           ; Its dwords
        NOP
    start:
        OUTR.ID 2, [R1]
        NOP
    end:
        OUTS.IW 2, 'x'      ; Not followed by the last dword's word 1
        HALT

    .SEGMENT strings DATA ROM
    message:
        .DW "Hello, world"

    .SEGMENT stack STACK
  )---"));
  ASSERT_TRUE(ExecuteUntilIp(GetLabel("START")));

  // Each dword is two words, word 0 first.
  EXPECT_EQ(CyclesUntilIp(GetLabel("END")), 3 * 11);
  const CoreState& state = GetState();
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st & CpuCore::S, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(teletype_->TakeOutput(), "Hello, worldx");
  EXPECT_EQ(GetPort(kPort).GetValue(1), 0);
}

TEST_F(TeletypeTest, BaudRateKeepsItBusyForEachCharacter) {
  // 10 bits a character at 100,000 baud is 100 cycles a character at 1 MHz.
  ASSERT_TRUE(InitTeletype({.baud = 100'000}));
  Execute(1);

  // Idle, it takes a write the cycle after.
  WritePort('A');
  EXPECT_EQ(CyclesUntilTaken(), 1);

  // Then it is busy printing what it took, while the port holds the next
  // write.
  WritePort(0x4342);  // "BC"
  EXPECT_EQ(CyclesUntilTaken(), 100);
  WritePort(0x4400);  // "D", as a zero byte takes no time
  EXPECT_EQ(CyclesUntilTaken(), 200);
  WritePort(0);  // Nothing to print
  EXPECT_EQ(CyclesUntilTaken(), 100);
  WritePort('E');
  EXPECT_EQ(CyclesUntilTaken(), 1);

  // Once it is done, it takes a write the cycle after again.
  Execute(1000);
  WritePort('F');
  EXPECT_EQ(CyclesUntilTaken(), 1);
  EXPECT_EQ(teletype_->TakeOutput(), "ABCDEF");
}

TEST_F(TeletypeTest, BaudRateIsInRealTime) {
  // Twice the clock rate is twice the cycles a character.
  ASSERT_TRUE(InitTeletype({.baud = 100'000}, 2'000'000));
  Execute(1);
  WritePort('A');
  EXPECT_EQ(CyclesUntilTaken(), 1);
  WritePort('B');
  EXPECT_EQ(CyclesUntilTaken(), 200);
}

TEST_F(TeletypeTest, OutrStopsEarlyAtABaudRate) {
  ASSERT_TRUE(InitTeletype({.baud = 100'000}));
  ASSERT_TRUE(Load(R"---(
    .STACK 16

    .SEGMENT main CODE
        MOV R1, message     ; R1 is the message's address in DATA
        MVQ R7, 3           ; Its words
        OUTR 2, (R1)
        HALT

    .SEGMENT strings DATA ROM
    message:
        .DW "Hello!"

    .SEGMENT stack STACK
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());

  // The teletype took the first word, and the port held the second, so the
  // third wasn't written.
  const CoreState& state = GetState();
  EXPECT_EQ(state.r7, 1);
  EXPECT_EQ(state.st & CpuCore::S, 0);
  Execute(1000);
  EXPECT_EQ(teletype_->TakeOutput(), "Hell");
}

TEST_F(TeletypeTest, FullBufferHoldsThePortUntilOutputIsTaken) {
  // After four characters, there is room for a word, but not a dword.
  ASSERT_TRUE(InitTeletype({.buffer_size = 6}));
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        OUTS.IW 2, "ab"
        OUTS.IW 2, "cd"
        OUTS.IW 2, "ef"        ; Written, but the teletype doesn't take it
        OUTS.IW 2, "gh"        ; Not written, as the status is still set
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(GetState().st & CpuCore::S, 0);
  EXPECT_EQ(GetPort(kPort).GetStatus(), 1);
  EXPECT_EQ(GetPort(kPort).GetValue(0), 0x6665);

  // Waits for the host as long as it takes.
  Execute(1000);
  EXPECT_EQ(GetPort(kPort).GetStatus(), 1);

  // Taking the output makes room, so it takes the word at the next cycle.
  EXPECT_EQ(teletype_->TakeOutput(), "abcd");
  EXPECT_EQ(CyclesUntilTaken(), 1);
  EXPECT_EQ(teletype_->TakeOutput(), "ef");
}

TEST_F(TeletypeTest, SleepsUntilItsPortIsUsed) {
  ASSERT_TRUE(InitTeletype());
  Execute(100);
  EXPECT_EQ(teletype_->GetDueCycle(), kSleepForever);
  const Cycles cycles = teletype_->GetCycles();
  EXPECT_LT(cycles, 100);

  WritePort('A');
  EXPECT_EQ(CyclesUntilTaken(), 1);
  EXPECT_EQ(teletype_->TakeOutput(), "A");
  EXPECT_EQ(teletype_->GetDueCycle(), kSleepForever);
  EXPECT_GT(teletype_->GetCycles(), cycles);
}

TEST_F(TeletypeTest, CreateValidatesSettings) {
  absl::StatusOr<std::unique_ptr<TeletypeFactory>> factory =
      TeletypeFactory::Create("Printer", {.port = kPort, .baud = -1});
  EXPECT_EQ(factory.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(factory.status().message(), HasSubstr("Printer"));
  EXPECT_THAT(factory.status().message(), HasSubstr("baud rate"));

  factory =
      TeletypeFactory::Create("Printer", {.port = kPort, .buffer_size = 3});
  EXPECT_EQ(factory.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(factory.status().message(), HasSubstr("Printer"));
  EXPECT_THAT(factory.status().message(), HasSubstr("buffer size"));

  factory =
      TeletypeFactory::Create("Printer", {.port = kPort, .buffer_size = 4});
  ASSERT_TRUE(factory.ok()) << factory.status();
  const DeviceConfig& config = (*factory)->GetConfig();
  EXPECT_EQ(config.name, "Printer");
  EXPECT_THAT(config.ports, ElementsAre(kPort));
}

}  // namespace
}  // namespace oz3
