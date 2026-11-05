// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/keyboard.h"

#include <cstdint>
#include <memory>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/device_config.h"
#include "oz3/core/lockable.h"
#include "oz3/core/port.h"
#include "oz3/devices/device_program_test.h"

namespace oz3 {
namespace {

using ::testing::AllOf;
using ::testing::ElementsAre;
using ::testing::Field;

// The processor port the keyboard is on, and the interrupt it raises.
constexpr int kPort = 1;
constexpr int kInterrupt = 1;

class KeyboardTest : public DeviceProgramTest {
 protected:
  // The most cycles CyclesUntilReady waits.
  static constexpr Cycles kMaxReadyCycles = 10000;

  // Initializes a processor with a keyboard named "Keyboard" on kPort, raising
  // kInterrupt, and sets keyboard_ to it. Returns whether it did.
  bool InitKeyboard() {
    if (!Init(
            {.num_ports = kPort + 1,
             .devices = {std::make_shared<KeyboardFactory>(
                 "Keyboard",
                 KeyboardSettings{.port = kPort,
                                  .interrupt = {.interrupt = kInterrupt}})}})) {
      return false;
    }
    keyboard_ = static_cast<Keyboard*>(GetProcessor().GetDevice("Keyboard"));
    return keyboard_ != nullptr;
  }

  // Reads the keyboard's port as INS does, which clears its status, and
  // returns the word read. No core may be using the port, so its lock is
  // granted at once, and the status must be set.
  uint16_t ReadPort() {
    std::unique_ptr<Lock> lock = LockPort(kPort);
    EXPECT_TRUE(lock->IsLocked());
    uint16_t value = 0;
    EXPECT_EQ(GetPort(kPort).LoadWord(*lock, Port::T | Port::S, value), 1);
    return value;
  }

  // Writes `value` to the keyboard's port as OUTS does, which sets its status.
  // No core may be using the port.
  void WritePort(uint16_t value) {
    std::unique_ptr<Lock> lock = LockPort(kPort);
    ASSERT_TRUE(lock->IsLocked());
    GetPort(kPort).StoreWord(*lock, Port::S, value);
  }

  // Executes until the port's status is set, and returns the cycles that took,
  // or -1 if it isn't within kMaxReadyCycles.
  Cycles CyclesUntilReady() {
    for (Cycles cycles = 1; cycles <= kMaxReadyCycles; ++cycles) {
      Execute(1);
      if (GetPort(kPort).GetStatus() != 0) {
        return cycles;
      }
    }
    return -1;
  }

  // Returns whether kInterrupt is raised on core 0.
  bool IsInterruptRaised() {
    return (GetState().core.GetInterrupts() & (1u << kInterrupt)) != 0;
  }

  Keyboard* keyboard_ = nullptr;
};

TEST_F(KeyboardTest, KeyIsInThePortTheCycleAfterItIsPressed) {
  ASSERT_TRUE(InitKeyboard());
  Execute(1);
  EXPECT_EQ(GetPort(kPort).GetStatus(), 0);
  EXPECT_FALSE(IsInterruptRaised());

  EXPECT_TRUE(keyboard_->PressKey(kKeyCtrl | 'C'));
  EXPECT_EQ(CyclesUntilReady(), 1);
  EXPECT_EQ(GetPort(kPort).GetValue(0), 0x0243);
  EXPECT_TRUE(IsInterruptRaised());
}

TEST_F(KeyboardTest, ProgramReadsKeysWithIns) {
  ASSERT_TRUE(InitKeyboard());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
    wait0:
        INS 1, R0
        JC NS, wait0
    wait1:
        INS 1, R1
        JC NS, wait1
    wait2:
        INS 1, R2
        JC NS, wait2
        HALT
  )---"));
  EXPECT_TRUE(keyboard_->PressKey('h'));
  EXPECT_TRUE(keyboard_->PressKey(kKeyShift | 'I'));
  EXPECT_TRUE(keyboard_->PressKey(kKeyF1));
  ASSERT_TRUE(ExecuteUntilHalt());
  const CoreState& state = GetState();
  EXPECT_EQ(state.r0, 'h');
  EXPECT_EQ(state.r1, 0x0149);
  EXPECT_EQ(state.r2, 0x0090);
  EXPECT_EQ(GetPort(kPort).GetStatus(), 0);
}

TEST_F(KeyboardTest, NextKeyIsReadyForTheNextIns) {
  ASSERT_TRUE(InitKeyboard());
  EXPECT_TRUE(keyboard_->PressKey('a'));
  EXPECT_TRUE(keyboard_->PressKey('b'));
  EXPECT_EQ(CyclesUntilReady(), 1);

  // Each INS reads a key, without waiting for it.
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        INS 1, R0
        INS 1, R1
        HALT
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  const CoreState& state = GetState();
  EXPECT_EQ(state.r0, 'a');
  EXPECT_EQ(state.r1, 'b');
}

TEST_F(KeyboardTest, EachKeyIsInThePortTheCycleAfterTheLastIsRead) {
  ASSERT_TRUE(InitKeyboard());
  Execute(1);
  EXPECT_TRUE(keyboard_->PressKey('a'));
  EXPECT_TRUE(keyboard_->PressKey('b'));
  EXPECT_TRUE(keyboard_->PressKey('c'));
  EXPECT_EQ(CyclesUntilReady(), 1);

  // A key stays in the port until it is read, however long that is.
  Execute(1000);
  EXPECT_EQ(ReadPort(), 'a');
  EXPECT_EQ(CyclesUntilReady(), 1);
  EXPECT_EQ(ReadPort(), 'b');
  EXPECT_EQ(CyclesUntilReady(), 1);
  EXPECT_EQ(ReadPort(), 'c');
  EXPECT_EQ(CyclesUntilReady(), -1);
}

TEST_F(KeyboardTest, RaisesTheInterruptOnceForEachKey) {
  ASSERT_TRUE(InitKeyboard());
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        SETI 1, handler
        MVQ R6, 0
        EI
    idle:
        JP idle

    handler:                ; Counts the interrupts, without reading the key
        ADQ R6, 1
        IRT
  )---"));
  ASSERT_TRUE(ExecuteUntilIp(GetLabel("IDLE")));
  const CoreState& state = GetState();
  EXPECT_TRUE(keyboard_->PressKey('a'));
  Execute(100);
  EXPECT_EQ(state.r6, 1);

  // While a key waits in the port, more keys don't raise the interrupt again.
  EXPECT_TRUE(keyboard_->PressKey('b'));
  EXPECT_TRUE(keyboard_->PressKey('c'));
  Execute(100);
  EXPECT_EQ(state.r6, 1);

  // Each key raises it once it is in the port.
  EXPECT_EQ(ReadPort(), 'a');
  Execute(100);
  EXPECT_EQ(state.r6, 2);
  EXPECT_EQ(ReadPort(), 'b');
  Execute(100);
  EXPECT_EQ(state.r6, 3);
  EXPECT_EQ(ReadPort(), 'c');
  Execute(100);
  EXPECT_EQ(state.r6, 3);
}

TEST_F(KeyboardTest, InterruptHandlerReadsEachKey) {
  ASSERT_TRUE(InitKeyboard());
  ASSERT_TRUE(Load(R"---(
    .STACK 16

    .SEGMENT main CODE
        SETI 1, handler
        MOV R1, keys        ; R1 is the next key's address in DATA
        MVQ R6, 0           ; The keys read
        EI
    loop:
        CMP R6, 3
        JC NZ, loop
        HALT

    handler:
        INS 1, (R1)
        ADQ R1, 1
        ADQ R6, 1
        IRT

    .SEGMENT vars DATA
    keys:
        .DS 3

    .SEGMENT stack STACK
  )---"));
  EXPECT_TRUE(keyboard_->PressKey(kKeyUp));
  EXPECT_TRUE(keyboard_->PressKey(kKeyAlt | 'x'));
  EXPECT_TRUE(keyboard_->PressKey(kKeyEnter));
  ASSERT_TRUE(ExecuteUntilHalt());
  MemAccessor data = GetMemory();
  data.SetAddress(GetState().bd + GetLabel("KEYS"));
  EXPECT_EQ(data.GetValue(), kKeyUp);
  EXPECT_EQ(data.GetValue(), kKeyAlt | 'x');
  EXPECT_EQ(data.GetValue(), kKeyEnter);
}

TEST_F(KeyboardTest, FullQueueDropsKeys) {
  ASSERT_TRUE(InitKeyboard());
  for (int i = 0; i < Keyboard::kQueueSize; ++i) {
    EXPECT_TRUE(keyboard_->PressKey(static_cast<uint16_t>('a' + i)));
  }
  EXPECT_FALSE(keyboard_->PressKey('!'));

  // Once a key is in the port, there is room for one more.
  EXPECT_EQ(CyclesUntilReady(), 1);
  EXPECT_TRUE(keyboard_->PressKey('A'));
  EXPECT_FALSE(keyboard_->PressKey('!'));

  for (int i = 0; i < Keyboard::kQueueSize; ++i) {
    EXPECT_EQ(ReadPort(), 'a' + i);
    EXPECT_EQ(CyclesUntilReady(), 1);
  }
  EXPECT_EQ(ReadPort(), 'A');
  EXPECT_EQ(CyclesUntilReady(), -1);
}

TEST_F(KeyboardTest, WaitsForAWordWrittenByACoreToBeRead) {
  ASSERT_TRUE(InitKeyboard());
  Execute(1);
  WritePort(0x1234);
  EXPECT_TRUE(keyboard_->PressKey('a'));
  Execute(100);
  EXPECT_EQ(GetPort(kPort).GetValue(0), 0x1234);
  EXPECT_FALSE(IsInterruptRaised());

  EXPECT_EQ(ReadPort(), 0x1234);
  EXPECT_EQ(CyclesUntilReady(), 1);
  EXPECT_EQ(ReadPort(), 'a');
}

TEST_F(KeyboardTest, SleepsWithNoKeysWaiting) {
  ASSERT_TRUE(InitKeyboard());
  Execute(100);
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);
  EXPECT_LT(keyboard_->GetCycles(), 100);

  // It sleeps with a key in the port and none waiting, and with keys waiting
  // for the port.
  EXPECT_TRUE(keyboard_->PressKey('a'));
  EXPECT_EQ(CyclesUntilReady(), 1);
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);
  EXPECT_TRUE(keyboard_->PressKey('b'));
  Execute(100);
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);
}

TEST_F(KeyboardTest, FactoryWiresThePortAndInterrupt) {
  const KeyboardFactory factory(
      "Keys", {.port = 3, .interrupt = {.interrupt = 5, .cores = 2}});
  const DeviceConfig& config = factory.GetConfig();
  EXPECT_EQ(config.name, "Keys");
  EXPECT_THAT(config.ports, ElementsAre(3));
  EXPECT_THAT(config.interrupts,
              ElementsAre(AllOf(Field(&DeviceInterrupt::interrupt, 5),
                                Field(&DeviceInterrupt::cores, 2))));
}

}  // namespace
}  // namespace oz3
