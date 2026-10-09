// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/instruction_sets/instruction_test.h"

namespace oz3 {
namespace {

TEST_F(InstructionTest, EI) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::Z | CpuCore::C},
                      {CpuCore::R0, 1},
                      {CpuCore::SP, 500}});

  state.code.AddValue(Encode("SETI", "$v")).AddValue(1).AddValue(100);
  const uint16_t ip0 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INT", {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("EI"));
  state.code.SetAddress(100);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  ASSERT_TRUE(ExecuteUntilIp(ip0));  // SETI 1, 100
  EXPECT_EQ(CyclesUntilIp(ip1), 3);  // INT R0
  EXPECT_EQ(state.core.GetInterrupts(), 1 << 1);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  // The interrupt starts as soon as EI completes.
  EXPECT_EQ(CyclesUntilIp(ip2), 3 + kCpuCoreStartInterruptCycles);  // EI
  EXPECT_EQ(state.core.GetInterrupts(), 0);
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(),
            CpuCore::I | CpuCore::Z | CpuCore::C);
  EXPECT_EQ(state.stack.GetValue(), ip1 + 1);
}

TEST_F(InstructionTest, DI) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::I | CpuCore::Z | CpuCore::C},
                      {CpuCore::R0, 1},
                      {CpuCore::SP, 500}});

  state.code.AddValue(Encode("SETI", "$v")).AddValue(1).AddValue(100);
  const uint16_t ip0 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DI"));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INT", {"$r", CpuCore::R0}));
  const uint16_t ip2 = state.code.AddNopGetAddress();

  ASSERT_TRUE(ExecuteUntilIp(ip0));  // SETI 1, 100
  EXPECT_EQ(CyclesUntilIp(ip1), 3);  // DI
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  // The interrupt stays pending, as interrupts are disabled.
  EXPECT_EQ(CyclesUntilIp(ip2), 3);  // INT R0
  EXPECT_EQ(state.core.GetInterrupts(), 1 << 1);
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, SETI) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::R0, 100}});

  state.code.AddValue(Encode("SETI", {"$r", CpuCore::R0})).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("SETI", "$v")).AddValue(2).AddValue(200);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("SETI", "$v")).AddValue(35).AddValue(300);
  const uint16_t ip3 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // SETI 1, R0
  EXPECT_EQ(state.core.GetInterruptAddress(1), 100);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip2), 6);  // SETI 2, 200
  EXPECT_EQ(state.core.GetInterruptAddress(2), 200);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  // Only the low 5 bits of the interrupt index are used.
  EXPECT_EQ(CyclesUntilIp(ip3), 6);  // SETI 35, 300
  EXPECT_EQ(state.core.GetInterruptAddress(3), 300);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, SETI_Cycles) {
  ASSERT_TRUE(InitAndReset());
  GetState().SetRegisters({{CpuCore::R1, 300}});
  RunCycleCases({
      {"SETI 1, R1", 5, {Encode("SETI", {"$r", CpuCore::R1}), 1}},
      {"SETI 1, 100", 6, {Encode("SETI", "$v"), 1, 100}},
      {"SETI 1, (R1)", 7, {Encode("SETI", {"($r)", CpuCore::R1}), 1}},
      {"SETI 1, (R1 + 1)",
       9,
       {Encode("SETI", {"($r + $v)", CpuCore::R1}), 1, 1}},
  });
}

TEST_F(InstructionTest, GETI) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::R3, 5}});

  state.code.AddValue(Encode("SETI", "$v")).AddValue(1).AddValue(100);
  const uint16_t ip0 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("GETI", CpuCore::R1)).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("GETI", CpuCore::R2)).AddValue(33);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("GETI", CpuCore::R3)).AddValue(2);
  const uint16_t ip3 = state.code.AddNopGetAddress();

  ASSERT_TRUE(ExecuteUntilIp(ip0));  // SETI 1, 100
  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // GETI R1, 1
  EXPECT_EQ(state.r1, 100);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  // Only the low 5 bits of the interrupt index are used.
  EXPECT_EQ(CyclesUntilIp(ip2), 5);  // GETI R2, 33
  EXPECT_EQ(state.r2, 100);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip3), 5);  // GETI R3, 2
  EXPECT_EQ(state.r3, 0);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, INT) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::I | CpuCore::Z | CpuCore::C},
                      {CpuCore::R0, 1},
                      {CpuCore::SP, 500}});

  state.code.AddValue(Encode("SETI", "$v")).AddValue(1).AddValue(100);
  const uint16_t ip0 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INT", {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INT", "$v")).AddValue(2);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INT", "$v")).AddValue(33);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.SetAddress(100);
  const uint16_t handler_ip = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IRT"));

  ASSERT_TRUE(ExecuteUntilIp(ip0));  // SETI 1, 100

  // The interrupt starts as soon as INT completes.
  EXPECT_EQ(CyclesUntilIp(handler_ip),
            3 + kCpuCoreStartInterruptCycles);  // INT R0
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(),
            CpuCore::I | CpuCore::Z | CpuCore::C);
  EXPECT_EQ(state.stack.GetValue(), ip1 - 1);
  ASSERT_TRUE(ExecuteUntilIp(ip1));  // IRT

  // Interrupt 2 has no handler, so it is cleared without being called.
  EXPECT_EQ(CyclesUntilIp(ip2), 4);  // INT 2
  EXPECT_EQ(state.core.GetInterrupts(), 0);
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::Z | CpuCore::C);

  // Only the low 5 bits of the interrupt index are used.
  EXPECT_EQ(CyclesUntilIp(handler_ip),
            4 + kCpuCoreStartInterruptCycles);  // INT 33
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp + 1).GetValue(),
            ip3 - 1);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // IRT
}

// With interrupts disabled, the interrupt isn't handled, so INT takes only its
// own cycles. INT times its register and integer forms above.
TEST_F(InstructionTest, INT_AddressCycles) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 300).AddValue(1).AddValue(1);
  state.SetRegisters({{CpuCore::ST, 0}, {CpuCore::R1, 300}});
  RunCycleCases({
      {"INT (R1)", 5, {Encode("INT", {"($r)", CpuCore::R1})}},
      {"INT (R1 + 1)", 7, {Encode("INT", {"($r + $v)", CpuCore::R1}), 1}},
  });
}

TEST_F(InstructionTest, IRT) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.SetAddress(500).PushValue(50).PushValue(CpuCore::I | CpuCore::S |
                                                      CpuCore::O);
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::SP, 498}});

  state.code.AddValue(Encode("IRT"));
  state.code.SetAddress(50);
  const uint16_t ip1 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 6);  // IRT
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::S | CpuCore::O);
}

TEST_F(InstructionTest, IRT_ResumesWait) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::R0, 30}, {CpuCore::SP, 500}});

  state.code.AddValue(Encode("SETI", "$v")).AddValue(1).AddValue(100);
  const uint16_t ip0 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("WAIT", CpuCore::R0));
  state.code.SetAddress(100);
  state.code.AddValue(Encode("IRT"));

  ASSERT_TRUE(ExecuteUntilIp(ip0));  // SETI 1, 100

  // WAIT counts from the core's cycles when it is fetched, which is now.
  const Cycles wait_end = state.core.GetCycles() + 30;
  ASSERT_TRUE(ExecuteUntil(
      [&] { return state.core.GetState() == CpuCore::State::kWaiting; }));

  // The interrupt runs, and then the core goes back to waiting.
  state.core.RaiseInterrupt(1);
  Execute(wait_end - 5 - GetCycles());
  EXPECT_EQ(state.core.GetInterrupts(), 0);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kWaiting);

  Execute(wait_end - GetCycles() + 1);
  EXPECT_NE(state.core.GetState(), CpuCore::State::kWaiting);
  state.Update();
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st & CpuCore::W, 0);
}

TEST_F(InstructionTest, IRT_A) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.SetAddress(500).PushValue(50).PushValue(CpuCore::I | CpuCore::S |
                                                      CpuCore::O);
  state.stack.PushValue(1).PushValue(2);
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::SP, 496}});

  state.code.AddValue(Encode("IRT.A", 2));
  state.code.SetAddress(50);
  const uint16_t ip1 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 7);  // IRT.A 2
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::S | CpuCore::O);
}

TEST_F(InstructionTest, IRTC) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.SetAddress(500).PushValue(50).PushValue(CpuCore::I | CpuCore::S |
                                                      CpuCore::O);
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::SP, 498}});

  state.code.AddValue(Encode("IRTC", CpuCore::kConditionNC));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IRTC", CpuCore::kConditionC));
  state.code.SetAddress(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 3);  // IRTC NC
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip2), 7);  // IRTC C
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::S | CpuCore::O);
}

TEST_F(InstructionTest, IRTC_A) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.SetAddress(500).PushValue(50).PushValue(CpuCore::I | CpuCore::S |
                                                      CpuCore::O);
  state.stack.PushValue(1).PushValue(2);
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::SP, 496}});

  state.code.AddValue(Encode("IRTC.A", CpuCore::kConditionNC, 2));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IRTC.A", CpuCore::kConditionC, 2));
  state.code.SetAddress(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 3);  // IRTC.A NC, 2
  EXPECT_EQ(state.sp, 496);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip2), 8);  // IRTC.A C, 2
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::S | CpuCore::O);
}

}  // namespace
}  // namespace oz3
