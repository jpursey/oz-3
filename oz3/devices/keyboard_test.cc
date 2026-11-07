// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/keyboard.h"

#include <cstdint>
#include <initializer_list>
#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
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
using ::testing::ElementsAreArray;
using ::testing::Field;
using ::testing::HasSubstr;

// The processor port of the typed keys, and the interrupt they raise.
constexpr int kPort = 1;
constexpr int kInterrupt = 1;

// The first processor port of the key state.
constexpr int kKeyStatePort = 4;

// A keyboard with typed keys only, key state only, and both.
constexpr KeyboardSettings kTypedKeys = {
    .typed_port = kPort, .typed_interrupt = {.interrupt = kInterrupt}};
constexpr KeyboardSettings kKeyState = {.state_port = kKeyStatePort,
                                        .typed_port = std::nullopt};
constexpr KeyboardSettings kBoth = {
    .state_port = kKeyStatePort,
    .typed_port = kPort,
    .typed_interrupt = {.interrupt = kInterrupt}};

// Returns the processor port that holds the key with code `key`, and its bit.
int GetKeyPort(int key) { return kKeyStatePort + key / 16; }
uint16_t GetKeyBit(int key) { return static_cast<uint16_t>(1 << (key % 16)); }

// Returns the bits of `keys`, which are on one port.
uint16_t GetKeyBits(std::initializer_list<int> keys) {
  uint16_t bits = 0;
  for (int key : keys) {
    bits |= GetKeyBit(key);
  }
  return bits;
}

// Returns the config of a keyboard named "Keys" wired as `settings`, which
// must be valid.
DeviceConfig GetFactoryConfig(const KeyboardSettings& settings) {
  absl::StatusOr<std::unique_ptr<KeyboardFactory>> factory =
      KeyboardFactory::Create("Keys", settings);
  if (!factory.ok()) {
    ADD_FAILURE() << factory.status();
    return {};
  }
  return (*factory)->GetConfig();
}

// Returns `ports` followed by the key state's ports from `first`.
std::vector<int> GetKeyStatePorts(std::vector<int> ports, int first) {
  for (int i = 0; i < Keyboard::kKeyStatePorts; ++i) {
    ports.push_back(first + i);
  }
  return ports;
}

// A key state port's words and status.
struct KeyStateWords {
  bool operator==(const KeyStateWords&) const = default;

  uint16_t held = 0;
  uint16_t pressed = 0;
  uint16_t status = 0;
};

class KeyboardTest : public DeviceProgramTest {
 protected:
  // The most cycles CyclesUntilReady waits.
  static constexpr Cycles kMaxReadyCycles = 10000;

  // Initializes a processor with a keyboard named "Keyboard" wired as
  // `settings`, and sets keyboard_ to it. Returns whether it did.
  bool InitKeyboard(const KeyboardSettings& settings = kTypedKeys) {
    absl::StatusOr<std::unique_ptr<KeyboardFactory>> factory =
        KeyboardFactory::Create("Keyboard", settings);
    if (!factory.ok()) {
      ADD_FAILURE() << factory.status();
      return false;
    }
    if (!Init({.num_ports = kKeyStatePort + Keyboard::kKeyStatePorts,
               .devices = {std::move(*factory)}})) {
      return false;
    }
    keyboard_ = static_cast<Keyboard*>(GetProcessor().GetDevice("Keyboard"));
    return keyboard_ != nullptr;
  }

  // Returns the words and status of the key state port that holds `key`.
  KeyStateWords GetKeyState(int key) {
    const Port& port = GetPort(GetKeyPort(key));
    return {.held = port.GetValue(0),
            .pressed = port.GetValue(1),
            .status = port.GetStatus()};
  }

  // Reads the key state port that holds `key` as IN.RD does, which clears its
  // status, and returns the words read, with the status before. No core may
  // be using the port.
  KeyStateWords ReadKeyState(int key) {
    std::unique_ptr<Lock> lock = LockPort(GetKeyPort(key));
    EXPECT_TRUE(lock->IsLocked());
    return ReadKeyState(*lock, key);
  }

  // Reads the key state port that holds `key` with `lock`, which holds it, as
  // ReadKeyState(key) does.
  KeyStateWords ReadKeyState(const Lock& lock, int key) {
    Port& port = GetPort(GetKeyPort(key));
    KeyStateWords words;
    port.LoadWord(lock, Port::A, words.held);
    words.status = port.LoadWord(lock, Port::S, words.pressed);
    return words;
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

  // Writes `value` to port word 0 of `port` with `mode`, by default to the
  // typed keys' port as OUTS does, which sets its status. No core may be using
  // the port.
  void WritePort(uint16_t value, int port = kPort, uint16_t mode = Port::S) {
    std::unique_ptr<Lock> lock = LockPort(port);
    ASSERT_TRUE(lock->IsLocked());
    GetPort(port).StoreWord(*lock, mode, value);
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

  EXPECT_TRUE(keyboard_->TypeKey(kKeyCtrl | 'C'));
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
  EXPECT_TRUE(keyboard_->TypeKey('h'));
  EXPECT_TRUE(keyboard_->TypeKey(kKeyShift | 'I'));
  EXPECT_TRUE(keyboard_->TypeKey(kKeyF1));
  ASSERT_TRUE(ExecuteUntilHalt());
  const CoreState& state = GetState();
  EXPECT_EQ(state.r0, 'h');
  EXPECT_EQ(state.r1, 0x0149);
  EXPECT_EQ(state.r2, 0x0090);
  EXPECT_EQ(GetPort(kPort).GetStatus(), 0);
}

TEST_F(KeyboardTest, NextKeyIsReadyForTheNextIns) {
  ASSERT_TRUE(InitKeyboard());
  EXPECT_TRUE(keyboard_->TypeKey('a'));
  EXPECT_TRUE(keyboard_->TypeKey('b'));
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
  EXPECT_TRUE(keyboard_->TypeKey('a'));
  EXPECT_TRUE(keyboard_->TypeKey('b'));
  EXPECT_TRUE(keyboard_->TypeKey('c'));
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
  EXPECT_TRUE(keyboard_->TypeKey('a'));
  Execute(100);
  EXPECT_EQ(state.r6, 1);

  // While a key waits in the port, more keys don't raise the interrupt again.
  EXPECT_TRUE(keyboard_->TypeKey('b'));
  EXPECT_TRUE(keyboard_->TypeKey('c'));
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
  EXPECT_TRUE(keyboard_->TypeKey(kKeyUp));
  EXPECT_TRUE(keyboard_->TypeKey(kKeyAlt | 'x'));
  EXPECT_TRUE(keyboard_->TypeKey(kKeyEnter));
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
    EXPECT_TRUE(keyboard_->TypeKey(static_cast<uint16_t>('a' + i)));
  }
  EXPECT_FALSE(keyboard_->TypeKey('!'));

  // Once a key is in the port, there is room for one more.
  EXPECT_EQ(CyclesUntilReady(), 1);
  EXPECT_TRUE(keyboard_->TypeKey('A'));
  EXPECT_FALSE(keyboard_->TypeKey('!'));

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
  EXPECT_TRUE(keyboard_->TypeKey('a'));
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
  EXPECT_TRUE(keyboard_->TypeKey('a'));
  EXPECT_EQ(CyclesUntilReady(), 1);
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);
  EXPECT_TRUE(keyboard_->TypeKey('b'));
  Execute(100);
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);
}

TEST_F(KeyboardTest, FactoryWiresThePortAndInterrupt) {
  const DeviceConfig config = GetFactoryConfig(
      {.typed_port = 3, .typed_interrupt = {.interrupt = 5, .cores = 2}});
  EXPECT_EQ(config.name, "Keys");
  EXPECT_THAT(config.ports, ElementsAre(3));
  EXPECT_THAT(config.interrupts,
              ElementsAre(AllOf(Field(&DeviceInterrupt::interrupt, 5),
                                Field(&DeviceInterrupt::cores, 2))));
}

TEST_F(KeyboardTest, FactoryWiresKeyStateWithoutAnInterrupt) {
  const DeviceConfig config =
      GetFactoryConfig({.state_port = 20,
                        .typed_port = std::nullopt,
                        .typed_interrupt = {.interrupt = 5}});
  EXPECT_THAT(config.ports, ElementsAreArray(GetKeyStatePorts({}, 20)));
  EXPECT_THAT(config.interrupts, ElementsAre());
}

TEST_F(KeyboardTest, FactoryWiresTypedKeysFirst) {
  const DeviceConfig config = GetFactoryConfig(
      {.state_port = 20, .typed_port = 3, .typed_interrupt = {.interrupt = 5}});
  EXPECT_THAT(config.ports, ElementsAreArray(GetKeyStatePorts({3}, 20)));
  EXPECT_THAT(config.interrupts,
              ElementsAre(Field(&DeviceInterrupt::interrupt, 5)));
}

TEST_F(KeyboardTest, CreateValidatesSettings) {
  absl::StatusOr<std::unique_ptr<KeyboardFactory>> factory =
      KeyboardFactory::Create("Keys", {.typed_port = std::nullopt});
  EXPECT_EQ(factory.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(factory.status().message(), HasSubstr("Keys"));
  EXPECT_THAT(factory.status().message(), HasSubstr("no port"));

  // The typed keys' port can't be any of the key state's, but can be next to
  // them.
  for (int port : {20, 25, 29}) {
    factory =
        KeyboardFactory::Create("Keys", {.state_port = 20, .typed_port = port});
    EXPECT_EQ(factory.status().code(), absl::StatusCode::kInvalidArgument)
        << port;
    EXPECT_THAT(factory.status().message(), HasSubstr("20 to 29"));
  }
  for (int port : {19, 30}) {
    factory =
        KeyboardFactory::Create("Keys", {.state_port = 20, .typed_port = port});
    EXPECT_TRUE(factory.ok()) << port << ": " << factory.status();
  }
}

TEST_F(KeyboardTest, KeyIsHeldAndPressedTheCycleAfter) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  Execute(1);
  EXPECT_EQ(GetKeyState('a'), KeyStateWords{});

  keyboard_->SetKeyState('a', true);
  Execute(1);
  EXPECT_EQ(GetKeyState('a'), (KeyStateWords{.held = GetKeyBit('a'),
                                             .pressed = GetKeyBit('a'),
                                             .status = 1}));

  // Released, it is no longer held, but still pressed until it is read.
  keyboard_->SetKeyState('a', false);
  Execute(1);
  EXPECT_EQ(GetKeyState('a'),
            (KeyStateWords{.pressed = GetKeyBit('a'), .status = 1}));
}

TEST_F(KeyboardTest, EachKeyIsInThePortAndBitOfItsCode) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  for (int key :
       {int{kKeyBackspace}, int{' '}, int{'0'}, int{'z'}, int{kKeyDelete},
        int{kKeyUp}, int{kKeyLeftShift}, int{kKeyRightAlt}, int{kKeyF12}}) {
    SCOPED_TRACE(key);
    keyboard_->SetKeyState(key, true);
    Execute(1);
    EXPECT_EQ(GetKeyState(key).held, GetKeyBit(key));
    keyboard_->SetKeyState(key, false);
    Execute(1);
    EXPECT_EQ(GetKeyState(key).held, 0);
  }

  // The last key is the last bit of the last port.
  constexpr int kLastKey = Keyboard::kKeyStateKeys - 1;
  EXPECT_EQ(GetKeyPort(kLastKey), kKeyStatePort + Keyboard::kKeyStatePorts - 1);
  EXPECT_EQ(GetKeyBit(kLastKey), 0x8000);
  keyboard_->SetKeyState(kLastKey, true);
  Execute(1);
  EXPECT_EQ(GetKeyState(kLastKey).held, GetKeyBit(kLastKey));
}

TEST_F(KeyboardTest, KeysOnAPortChangeTogether) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  keyboard_->SetKeyState('a', true);
  keyboard_->SetKeyState('b', true);
  keyboard_->SetKeyState('c', true);
  keyboard_->SetKeyState('b', false);
  Execute(1);
  const uint16_t a_and_c = GetKeyBits({'a', 'c'});
  EXPECT_EQ(GetKeyState('a'),
            (KeyStateWords{.held = a_and_c,
                           .pressed = GetKeyBits({'a', 'b', 'c'}),
                           .status = 1}));
}

TEST_F(KeyboardTest, ReadingTakesThePressedKeys) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  keyboard_->SetKeyState('a', true);
  keyboard_->SetKeyState('b', true);
  keyboard_->SetKeyState('b', false);
  Execute(1);
  EXPECT_EQ(ReadKeyState('a'), (KeyStateWords{.held = GetKeyBit('a'),
                                              .pressed = GetKeyBits({'a', 'b'}),
                                              .status = 1}));

  // The keyboard clears them the cycle after, and the held keys stay.
  Execute(1);
  EXPECT_EQ(GetKeyState('a'), KeyStateWords{.held = GetKeyBit('a')});
  EXPECT_EQ(ReadKeyState('a'), KeyStateWords{.held = GetKeyBit('a')});
}

TEST_F(KeyboardTest, HoldingAKeyAgainDoesntPressItAgain) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  keyboard_->SetKeyState('a', true);
  Execute(1);
  ReadKeyState('a');
  Execute(1);

  // As a host's auto-repeat does.
  keyboard_->SetKeyState('a', true);
  keyboard_->SetKeyState('a', true);
  Execute(1);
  EXPECT_EQ(GetKeyState('a'), KeyStateWords{.held = GetKeyBit('a')});

  // Released and held again, it is pressed again.
  keyboard_->SetKeyState('a', false);
  keyboard_->SetKeyState('a', true);
  Execute(1);
  EXPECT_EQ(GetKeyState('a'), (KeyStateWords{.held = GetKeyBit('a'),
                                             .pressed = GetKeyBit('a'),
                                             .status = 1}));
}

TEST_F(KeyboardTest, KeyPressedWhileAReadIsTakenIsntLost) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  keyboard_->SetKeyState('a', true);
  Execute(1);

  // A read holds the port, so the keyboard waits to take what it read, while
  // the host presses another key on the port.
  std::unique_ptr<Lock> lock = LockPort(GetKeyPort('a'));
  ASSERT_TRUE(lock->IsLocked());
  EXPECT_EQ(ReadKeyState(*lock, 'a').pressed, GetKeyBit('a'));
  Execute(1);
  keyboard_->SetKeyState('b', true);
  lock.reset();
  Execute(1);
  EXPECT_EQ(GetKeyState('a'), (KeyStateWords{.held = GetKeyBits({'a', 'b'}),
                                             .pressed = GetKeyBit('b'),
                                             .status = 1}));
}

TEST_F(KeyboardTest, ProgramReadsKeyStateWithIn) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        IN 10, D0       ; 'a' to 'o': held and pressed
        IN 10, D1       ; Pressed again: none
        HALT
  )---"));
  keyboard_->SetKeyState('a', true);
  keyboard_->SetKeyState('b', true);
  keyboard_->SetKeyState('b', false);
  ASSERT_TRUE(ExecuteUntilHalt());
  const CoreState& state = GetState();
  EXPECT_EQ(state.r0, GetKeyBit('a'));
  EXPECT_EQ(state.r1, GetKeyBits({'a', 'b'}));
  EXPECT_EQ(state.r2, GetKeyBit('a'));
  EXPECT_EQ(state.r3, 0);
}

TEST_F(KeyboardTest, ProgramReadsPressedKeysWithIns) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        MOV R0, 0FFFFh
        MOV R1, 0FFFFh
        INS 10, D0      ; Nothing pressed: reads nothing
    wait:
        INS 10, D1
    check:
        JC NS, wait
        HALT
  )---"));
  keyboard_->SetKeyState('a', true);
  Execute(1);
  ReadKeyState('a');

  // IP passes an instruction's words before it runs, so the first INS has run
  // once IP reaches the JC.
  ASSERT_TRUE(ExecuteUntilIp(GetLabel("CHECK")));
  keyboard_->SetKeyState('c', true);
  ASSERT_TRUE(ExecuteUntilHalt());
  const CoreState& state = GetState();
  EXPECT_EQ(state.r0, 0xFFFF);
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.r2, GetKeyBits({'a', 'c'}));
  EXPECT_EQ(state.r3, GetKeyBit('c'));
}

TEST_F(KeyboardTest, ReadingTheHeldKeysAloneTakesThePressedKeys) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        IN 10, R0
        HALT
  )---"));
  keyboard_->SetKeyState('a', true);
  ASSERT_TRUE(ExecuteUntilHalt());
  Execute(1);
  EXPECT_EQ(GetState().r0, GetKeyBit('a'));
  EXPECT_EQ(GetKeyState('a'), KeyStateWords{.held = GetKeyBit('a')});
}

TEST_F(KeyboardTest, CoreWriteStaysUntilTheKeyboardWritesThePort) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  WritePort(0x1234, GetKeyPort('a'), 0);
  Execute(100);
  EXPECT_EQ(GetKeyState('a').held, 0x1234);

  keyboard_->SetKeyState('a', true);
  Execute(1);
  EXPECT_EQ(GetKeyState('a').held, GetKeyBit('a'));
}

TEST_F(KeyboardTest, KeyStateSleepsAndTakesNoCycles) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  Execute(100);
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);

  // It writes the port in the cycle it is woken, and sleeps again.
  keyboard_->SetKeyState('a', true);
  Execute(1);
  EXPECT_EQ(keyboard_->GetCycles(), 100);
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);

  // A read wakes it, and it sleeps again once it takes the pressed keys.
  ReadKeyState('a');
  Execute(1);
  EXPECT_EQ(GetKeyState('a').pressed, 0);
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);
  ReadKeyState('a');
  Execute(1);
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);
}

TEST_F(KeyboardTest, TypedKeysAndKeyStateWorkTogether) {
  ASSERT_TRUE(InitKeyboard(kBoth));
  EXPECT_TRUE(keyboard_->TypeKey(kKeyShift | 'A'));
  keyboard_->SetKeyState(kKeyLeftShift, true);
  keyboard_->SetKeyState('a', true);
  Execute(1);
  EXPECT_EQ(GetPort(kPort).GetValue(0), kKeyShift | 'A');
  EXPECT_EQ(GetPort(kPort).GetStatus(), 1);
  EXPECT_TRUE(IsInterruptRaised());
  EXPECT_EQ(GetKeyState('a').held, GetKeyBit('a'));
  EXPECT_EQ(GetKeyState(kKeyLeftShift).held, GetKeyBit(kKeyLeftShift));
}

TEST_F(KeyboardTest, KeyStateAloneHasNoTypedKeys) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  EXPECT_FALSE(keyboard_->TypeKey('a'));
  Execute(1);
  EXPECT_EQ(GetPort(kPort).GetStatus(), 0);
  EXPECT_FALSE(IsInterruptRaised());
}

TEST_F(KeyboardTest, TypedKeysAloneHaveNoKeyState) {
  ASSERT_TRUE(InitKeyboard(kTypedKeys));
  keyboard_->SetKeyState('a', true);
  Execute(1);
  EXPECT_EQ(GetKeyState('a'), KeyStateWords{});
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);
}

TEST_F(KeyboardTest, KeyCodesOutOfRangeAreIgnored) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  keyboard_->SetKeyState(-1, true);
  keyboard_->SetKeyState(Keyboard::kKeyStateKeys, true);
  Execute(1);
  for (int key = 0; key < Keyboard::kKeyStateKeys; key += 16) {
    EXPECT_EQ(GetKeyState(key), KeyStateWords{}) << key;
  }
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);
}

TEST_F(KeyboardTest, ReleasesThePortWhenACoreSetsTheStatusWhileItWaits) {
  ASSERT_TRUE(InitKeyboard(kKeyState));
  keyboard_->SetKeyState('a', true);
  Execute(1);

  // A read clears the status while holding the port, so the keyboard waits
  // for it. Then a write sets the status again before the keyboard has it.
  std::unique_ptr<Lock> lock = LockPort(GetKeyPort('a'));
  ASSERT_TRUE(lock->IsLocked());
  ReadKeyState(*lock, 'a');
  Execute(1);
  GetPort(GetKeyPort('a')).StoreWord(*lock, Port::S, 0x1234);
  lock.reset();
  Execute(1);

  // The keyboard still writes the port, and releases it. With the status set,
  // it takes the pressed keys as not read.
  EXPECT_EQ(GetKeyState('a'), (KeyStateWords{.held = GetKeyBit('a'),
                                             .pressed = GetKeyBit('a'),
                                             .status = 1}));
  EXPECT_FALSE(GetPort(GetKeyPort('a')).IsLocked());
  EXPECT_EQ(keyboard_->GetDueCycle(), kSleepForever);
}

}  // namespace
}  // namespace oz3
