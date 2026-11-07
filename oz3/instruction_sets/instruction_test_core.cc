// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/instruction_sets/instruction_test.h"

namespace oz3 {
namespace {

constexpr uint16_t ZC = CpuCore::Z | CpuCore::C;

class CoreTest : public InstructionTest {
 protected:
  // Initializes two cores and two memory banks, and resets core 0 with the same
  // banks, BD, and BE as InitAndReset. Core 1 is left idle with all registers
  // zero, as cores are before anything resets them.
  bool InitTwoCores() {
    if (!Init({.num_cores = 2, .num_memory_banks = 2})) {
      return false;
    }
    ResetCore0();
    return true;
  }

  // Initializes two cores and two memory banks, and starts core 1 running from
  // 0 in bank 1, with its stack in bank 1 below 500. It enables interrupts with
  // its handler for interrupt 5 at 100 (which the test writes), WAITs for R0
  // (1000) cycles, then sets R6 to 1 and halts. Returns once core 1 is waiting,
  // with core 0 reset as for InitTwoCores.
  bool InitWaitingCore1() {
    if (!Init({.num_cores = 2, .num_memory_banks = 2})) {
      return false;
    }
    auto& state1 = GetState(1);
    GetMemory(1)
        .AddValue(Encode("SETI", "$v"))
        .AddValue(5)
        .AddValue(100)
        .AddValue(Encode("EI"))
        .AddValue(Encode("WAIT", CpuCore::R0))
        .AddValue(Encode("MVQ.LW", CpuCore::R6, 1))
        .AddValue(Encode("HALT"));
    state1.ResetCore({.mask = CpuCore::ResetParams::ALL,
                      .mb = CpuCore::Banks().SetCode(1).SetStack(1).ToWord()});
    state1.SetRegisters({{CpuCore::R0, 1000}, {CpuCore::SP, 500}});
    if (!ExecuteUntil(1, [&] {
          return state1.core.GetState() == CpuCore::State::kWaiting;
        })) {
      return false;
    }
    ResetCore0();
    return true;
  }

 private:
  void ResetCore0() {
    GetState(0).ResetCore({.mask = CpuCore::ResetParams::ALL,
                           .mb = CpuCore::Banks().SetExtra(1).ToWord(),
                           .bd = 1000,
                           .be = 2000});
  }
};

TEST_F(CoreTest, RST_AllBanks) {
  ASSERT_TRUE(InitTwoCores());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  const uint16_t mb =
      CpuCore::Banks().SetCode(1).SetStack(1).SetData(0).SetExtra(1).ToWord();
  state0.SetRegisters({{CpuCore::ST, ZC},
                       {CpuCore::R0, 11},
                       {CpuCore::R1, 12},
                       {CpuCore::R3, mb},
                       {CpuCore::R4, 100},
                       {CpuCore::R5, 200},
                       {CpuCore::R6, 300},
                       {CpuCore::R7, 400}});
  state1.SetRegisters({{CpuCore::SP, 50}, {CpuCore::FP, 60}});

  // Core 1 runs from 100 in bank 1 once reset.
  GetMemory(1)
      .SetAddress(100)
      .AddValue(Encode("MVQ.LW", CpuCore::R5, 7))
      .AddValue(Encode("HALT"));

  state0.code.AddValue(Encode("RST", {"$#3", 1}, 0xF));
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  ASSERT_EQ(state1.core.GetState(), CpuCore::State::kIdle);
  EXPECT_EQ(CyclesUntilIp(ip1), 93);  // RST 1, CODE|STACK|DATA|EXTRA
  EXPECT_EQ(state0.st, ZC);
  EXPECT_EQ(state1.mb, mb);
  EXPECT_EQ(state1.bc, 100);
  EXPECT_EQ(state1.r0, 11);
  EXPECT_EQ(state1.r1, 12);
  EXPECT_EQ(state1.bs, 200);
  EXPECT_EQ(state1.sp, 0);
  EXPECT_EQ(state1.fp, 0);
  EXPECT_EQ(state1.bd, 300);
  EXPECT_EQ(state1.be, 400);

  // Core 1 starts running from its new code location.
  ExecuteUntilHalt(1);
  EXPECT_EQ(state1.r5, 7);
  EXPECT_EQ(state1.ip, 1);
}

TEST_F(CoreTest, RST_Code) {
  ASSERT_TRUE(InitTwoCores());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  const uint16_t mb =
      CpuCore::Banks().SetCode(1).SetStack(1).SetData(1).SetExtra(1).ToWord();
  state0.SetRegisters({{CpuCore::ST, ZC},
                       {CpuCore::R0, 11},
                       {CpuCore::R1, 12},
                       {CpuCore::R3, mb},
                       {CpuCore::R4, 100},
                       {CpuCore::R5, 200},
                       {CpuCore::R6, 300},
                       {CpuCore::R7, 400}});
  state1.SetRegisters(
      {{CpuCore::ST, CpuCore::T | ZC}, {CpuCore::SP, 50}, {CpuCore::FP, 60}});

  // Interrupts are disabled on core 1, so its interrupt stays pending.
  state1.core.RaiseInterrupt(3);

  // Core 1 runs from 100 in bank 1 once reset.
  GetMemory(1)
      .SetAddress(100)
      .AddValue(Encode("MVQ.LW", CpuCore::R5, 7))
      .AddValue(Encode("HALT"));

  state0.code.AddValue(Encode("RST", {"$#3", 1}, 0x1));
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  // Only the code bank and registers, and the status flags, change. The trap
  // flag and the pending interrupt are kept.
  EXPECT_EQ(CyclesUntilIp(ip1), 84);  // RST 1, CODE
  EXPECT_EQ(state0.st, ZC);
  EXPECT_EQ(state1.st, CpuCore::T);
  EXPECT_EQ(state1.core.GetInterrupts(), 1 << 3);
  EXPECT_EQ(state1.mb, CpuCore::Banks().SetCode(1).ToWord());
  EXPECT_EQ(state1.bc, 100);
  EXPECT_EQ(state1.r0, 11);
  EXPECT_EQ(state1.r1, 12);
  EXPECT_EQ(state1.bs, 0);
  EXPECT_EQ(state1.sp, 50);
  EXPECT_EQ(state1.fp, 60);
  EXPECT_EQ(state1.bd, 0);
  EXPECT_EQ(state1.be, 0);

  // Core 1 starts running from its new code location.
  ExecuteUntilHalt(1);
  EXPECT_EQ(state1.r5, 7);
  EXPECT_EQ(state1.ip, 1);
}

TEST_F(CoreTest, RST_StackAndExtra) {
  ASSERT_TRUE(InitTwoCores());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  const uint16_t mb =
      CpuCore::Banks().SetCode(1).SetStack(1).SetData(1).SetExtra(1).ToWord();
  state0.SetRegisters({{CpuCore::ST, ZC},
                       {CpuCore::R0, 11},
                       {CpuCore::R1, 12},
                       {CpuCore::R3, mb},
                       {CpuCore::R4, 100},
                       {CpuCore::R5, 200},
                       {CpuCore::R6, 300},
                       {CpuCore::R7, 400}});
  state1.SetRegisters({{CpuCore::SP, 50}, {CpuCore::FP, 60}});

  state0.code.AddValue(Encode("RST", {"$#3", 1}, 0xA));
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  // Only the stack and extra banks and registers change, and core 1 stays
  // idle.
  EXPECT_EQ(CyclesUntilIp(ip1), 18);  // RST 1, STACK|EXTRA
  EXPECT_EQ(state0.st, ZC);
  EXPECT_EQ(state1.mb, CpuCore::Banks().SetStack(1).SetExtra(1).ToWord());
  EXPECT_EQ(state1.bc, 0);
  EXPECT_EQ(state1.r0, 0);
  EXPECT_EQ(state1.r1, 0);
  EXPECT_EQ(state1.bs, 200);
  EXPECT_EQ(state1.sp, 0);
  EXPECT_EQ(state1.fp, 0);
  EXPECT_EQ(state1.bd, 0);
  EXPECT_EQ(state1.be, 400);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kIdle);
}

TEST_F(CoreTest, RST_CodeWaiting) {
  ASSERT_TRUE(InitWaitingCore1());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);

  // Core 0 resets core 1's code location to 100 in bank 1, where it sets R5 and
  // halts.
  GetMemory(1)
      .SetAddress(100)
      .AddValue(Encode("MVQ.LW", CpuCore::R5, 7))
      .AddValue(Encode("HALT"));
  state0.SetRegisters({{CpuCore::ST, ZC},
                       {CpuCore::R3, CpuCore::Banks().SetCode(1).ToWord()},
                       {CpuCore::R4, 100}});
  state0.code.AddValue(Encode("RST", {"$#3", 1}, 0x1));
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  // The WAIT ends, and core 1 runs from its new code location, with its
  // interrupt handler cleared and interrupts disabled.
  EXPECT_EQ(CyclesUntilIp(ip1), 84);  // RST 1, CODE
  EXPECT_NE(state1.core.GetState(), CpuCore::State::kWaiting);
  EXPECT_EQ(state1.st, 0);
  EXPECT_EQ(state1.core.GetInterruptAddress(5), 0);
  ExecuteUntilHalt(1);
  EXPECT_EQ(state1.r5, 7);
  EXPECT_EQ(state1.ip, 1);
  EXPECT_LT(state1.core.GetCycles(), 1000);
}

TEST_F(CoreTest, RST_SELF_CodeClearsEveryHandler) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  // The core sets a handler for every interrupt, and then resets its code to
  // 500, where it halts.
  for (int i = 0; i < CpuCore::kInterruptCount; ++i) {
    state.code.AddValue(Encode("SETI", "$v")).AddValue(i).AddValue(100 + i);
  }
  const uint16_t rst_ip = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RST", "SELF", 0x1));
  state.code.SetAddress(500).AddValue(Encode("HALT"));
  state.SetRegisters({{CpuCore::R3, 0}, {CpuCore::R4, 500}});

  ASSERT_TRUE(ExecuteUntilIp(rst_ip));
  for (int i = 0; i < CpuCore::kInterruptCount; ++i) {
    ASSERT_EQ(state.core.GetInterruptAddress(i), 100 + i) << "Interrupt " << i;
  }
  ExecuteUntilHalt();
  EXPECT_EQ(state.bc, 500);
  for (int i = 0; i < CpuCore::kInterruptCount; ++i) {
    EXPECT_EQ(state.core.GetInterruptAddress(i), 0) << "Interrupt " << i;
  }
}

TEST_F(CoreTest, RST_NoBanks) {
  ASSERT_TRUE(InitTwoCores());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  const uint16_t mb =
      CpuCore::Banks().SetCode(1).SetStack(1).SetData(1).SetExtra(1).ToWord();
  state0.SetRegisters({{CpuCore::ST, ZC},
                       {CpuCore::R0, 11},
                       {CpuCore::R1, 12},
                       {CpuCore::R3, mb},
                       {CpuCore::R4, 100},
                       {CpuCore::R5, 200},
                       {CpuCore::R6, 300},
                       {CpuCore::R7, 400}});

  state0.code.AddValue(Encode("RST", {"$#3", 1}, 0));
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  // With no banks specified, nothing changes.
  EXPECT_EQ(CyclesUntilIp(ip1), 11);  // RST 1, 0
  EXPECT_EQ(state0.st, ZC);
  EXPECT_EQ(state1.mb, 0);
  EXPECT_EQ(state1.bc, 0);
  EXPECT_EQ(state1.r0, 0);
  EXPECT_EQ(state1.bs, 0);
  EXPECT_EQ(state1.bd, 0);
  EXPECT_EQ(state1.be, 0);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kIdle);
}

TEST_F(CoreTest, RST_R2) {
  ASSERT_TRUE(InitTwoCores());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  state0.SetRegisters({{CpuCore::ST, ZC},
                       {CpuCore::R2, 1},
                       {CpuCore::R3, CpuCore::Banks().SetData(1).ToWord()},
                       {CpuCore::R6, 300}});

  // Each RST after the first is preceded by setting R2, which is not timed.
  state0.code.AddValue(Encode("RST", "R2", 0x4));
  const uint16_t ip1 = state0.code.AddNopGetAddress();
  state0.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(-1);
  const uint16_t setup2 = state0.code.AddNopGetAddress();
  state0.code.AddValue(Encode("RST", "R2", 0x4));
  const uint16_t ip2 = state0.code.AddNopGetAddress();
  state0.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(5);
  const uint16_t setup3 = state0.code.AddNopGetAddress();
  state0.code.AddValue(Encode("RST", "R2", 0xF));
  const uint16_t ip3 = state0.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 13);  // RST R2 (1), DATA
  EXPECT_EQ(state1.mb, CpuCore::Banks().SetData(1).ToWord());
  EXPECT_EQ(state1.bd, 300);
  EXPECT_EQ(state0.bd, 1000);
  EXPECT_EQ(state0.st, ZC);

  // R2 == -1 resets this core.
  ASSERT_TRUE(ExecuteUntilIp(setup2));  // MOV.LW R2, -1
  EXPECT_EQ(CyclesUntilIp(ip2), 13);    // RST R2 (-1), DATA
  EXPECT_EQ(state0.mb, CpuCore::Banks().SetData(1).SetExtra(1).ToWord());
  EXPECT_EQ(state0.bd, 300);
  EXPECT_EQ(state0.st, ZC);

  // There is no core 5, so nothing changes.
  ASSERT_TRUE(ExecuteUntilIp(setup3));  // MOV.LW R2, 5
  EXPECT_EQ(CyclesUntilIp(ip3), 93);    // RST R2 (5), CODE|STACK|DATA|EXTRA
  EXPECT_EQ(state0.mb, CpuCore::Banks().SetData(1).SetExtra(1).ToWord());
  EXPECT_EQ(state0.bc, 0);
  EXPECT_EQ(state1.bc, 0);
  EXPECT_EQ(state0.st, ZC);
}

TEST_F(CoreTest, RST_SELF) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters(
      {{CpuCore::ST, ZC},
       {CpuCore::IP, 10},
       {CpuCore::R0, 11},
       {CpuCore::R1, 12},
       {CpuCore::R3, CpuCore::Banks().SetCode(0).SetData(0).ToWord()},
       {CpuCore::R4, 200},
       {CpuCore::R6, 300}});

  // The core continues from its new code location, at 200.
  state.code.SetAddress(10);
  state.code.AddValue(Encode("RST", "SELF", 0x5));
  state.code.SetAddress(200);
  const uint16_t ip1 = state.code.AddNopGetAddress() - 200;

  EXPECT_EQ(CyclesUntilIp(ip1), 87);  // RST SELF, CODE|DATA
  EXPECT_EQ(state.bc, 200);
  EXPECT_EQ(state.bd, 300);
  EXPECT_EQ(state.r0, 11);
  EXPECT_EQ(state.r1, 12);

  // Resetting CODE clears the status flags the instruction ends with.
  EXPECT_EQ(state.st, 0);
}

// SELF takes a cycle more than a core index, to load -1, so resetting every
// bank of this core is the costliest RST.
TEST_F(CoreTest, RST_SELF_AllBanks) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::IP, 10},
                      {CpuCore::R3, 0},  // Bank 0 for every bank
                      {CpuCore::R4, 200},
                      {CpuCore::R5, 300},
                      {CpuCore::R6, 400},
                      {CpuCore::R7, 500}});

  // The core continues from its new code location, at 200.
  state.code.SetAddress(10);
  state.code.AddValue(Encode("RST", "SELF", 0xF));
  state.code.SetAddress(200);
  const uint16_t ip1 = state.code.AddNopGetAddress() - 200;

  EXPECT_EQ(CyclesUntilIp(ip1), 94);  // RST SELF, CODE|STACK|DATA|EXTRA
  EXPECT_EQ(state.bc, 200);
  EXPECT_EQ(state.bs, 300);
  EXPECT_EQ(state.bd, 400);
  EXPECT_EQ(state.be, 500);
}

// With no banks, RST changes nothing, so it falls through. There is no core 1.
TEST_F(CoreTest, RST_NoBanksCycles) {
  ASSERT_TRUE(InitAndReset());
  GetState().SetRegisters({{CpuCore::R2, 1}});
  RunCycleCases({
      {"RST R2 (1), 0", 11, {Encode("RST", "R2", 0)}},
      {"RST SELF, 0", 12, {Encode("RST", "SELF", 0)}},
  });
}

TEST_F(CoreTest, INTC_I) {
  ASSERT_TRUE(InitWaitingCore1());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  state0.SetRegisters({{CpuCore::ST, ZC}});
  GetMemory(1)
      .SetAddress(100)
      .AddValue(Encode("MVQ.LW", CpuCore::R5, 7))
      .AddValue(Encode("IRT"));

  state0.code.AddValue(Encode("INTC.I", {"$#3", 1})).AddValue(37);
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  // Only the low 5 bits of the interrupt are used, so this raises interrupt 5.
  EXPECT_EQ(CyclesUntilIp(ip1), 4);  // INTC.I 1, 37
  EXPECT_EQ(state0.st, ZC);
  EXPECT_EQ(state0.core.GetInterrupts(), 0);

  // Core 1 handles it during its WAIT, and then goes back to waiting.
  ASSERT_TRUE(ExecuteUntil(1, [&] { return state1.r5 == 7; }));
  ASSERT_TRUE(ExecuteUntil(
      1, [&] { return state1.core.GetState() == CpuCore::State::kWaiting; }));
  EXPECT_EQ(state1.sp, 500);
  EXPECT_EQ(state1.r6, 0);
}

// Core 1 has no handler for interrupt 6, so it drops it, and goes on waiting.
TEST_F(CoreTest, INTC_NoHandler) {
  ASSERT_TRUE(InitWaitingCore1());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);

  state0.code.AddValue(Encode("INTC.I", {"$#3", 1})).AddValue(6);
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 4);  // INTC.I 1, 6
  ASSERT_TRUE(
      ExecuteUntil(1, [&] { return state1.core.GetInterrupts() == 0; }));
  ASSERT_TRUE(ExecuteUntil(
      1, [&] { return state1.core.GetState() == CpuCore::State::kWaiting; }));
  EXPECT_EQ(state1.sp, 500);
  EXPECT_EQ(state1.r6, 0);
}

// Core 1 has interrupts disabled, so the interrupts stay raised.
TEST_F(CoreTest, INTC_R) {
  ASSERT_TRUE(InitTwoCores());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  state0.SetRegisters({{CpuCore::ST, ZC},
                       {CpuCore::R1, 3},
                       {CpuCore::R2, 1},
                       {CpuCore::R3, 36}});

  state0.code.AddValue(Encode("INTC.R", {"$#3", 1}, CpuCore::R1));
  const uint16_t ip1 = state0.code.AddNopGetAddress();
  state0.code.AddValue(Encode("INTC.R", "R2", CpuCore::R3));
  const uint16_t ip2 = state0.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 3);  // INTC.R 1, R1
  EXPECT_EQ(state1.core.GetInterrupts(), 1u << 3);
  EXPECT_EQ(state0.st, ZC);

  // Only the low 5 bits of the interrupt are used, so this raises interrupt 4.
  EXPECT_EQ(CyclesUntilIp(ip2), 3);  // INTC.R R2 (1), R3
  EXPECT_EQ(state1.core.GetInterrupts(), (1u << 3) | (1u << 4));
  EXPECT_EQ(state0.st, ZC);
  EXPECT_EQ(state0.core.GetInterrupts(), 0);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kIdle);
}

// SELF, or -1 in R2, raises the interrupt on the running core, as INT does.
// Interrupts are disabled, so they stay raised.
TEST_F(CoreTest, INTC_SELF) {
  ASSERT_TRUE(InitTwoCores());
  auto& state0 = GetState(0);
  state0.SetRegisters({{CpuCore::ST, ZC},
                       {CpuCore::R1, 6},
                       {CpuCore::R2, 0xFFFF},
                       {CpuCore::R3, 7}});

  state0.code.AddValue(Encode("INTC.I", "SELF")).AddValue(2);
  const uint16_t ip1 = state0.code.AddNopGetAddress();
  state0.code.AddValue(Encode("INTC.R", "SELF", CpuCore::R1));
  const uint16_t ip2 = state0.code.AddNopGetAddress();
  state0.code.AddValue(Encode("INTC.R", "R2", CpuCore::R3));
  const uint16_t ip3 = state0.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // INTC.I SELF, 2
  EXPECT_EQ(state0.core.GetInterrupts(), 1u << 2);
  EXPECT_EQ(CyclesUntilIp(ip2), 4);  // INTC.R SELF, R1
  EXPECT_EQ(state0.core.GetInterrupts(), (1u << 2) | (1u << 6));
  EXPECT_EQ(CyclesUntilIp(ip3), 3);  // INTC.R R2 (-1), R3
  EXPECT_EQ(state0.core.GetInterrupts(), (1u << 2) | (1u << 6) | (1u << 7));
  EXPECT_EQ(state0.st, ZC);
  EXPECT_EQ(GetState(1).core.GetInterrupts(), 0);
}

// There is no core 1, so nothing happens. Interrupts are disabled, so an
// interrupt raised on this core would stay raised.
TEST_F(CoreTest, INTC_NoCore) {
  ASSERT_TRUE(InitAndReset());
  GetState().SetRegisters(
      {{CpuCore::ST, 0}, {CpuCore::R1, 3}, {CpuCore::R2, 1}});
  RunCycleCases({
      {"INTC.I 1, 3", 4, {Encode("INTC.I", {"$#3", 1}), 3}},
      {"INTC.R R2 (1), R1", 3, {Encode("INTC.R", "R2", CpuCore::R1)}},
  });
  EXPECT_EQ(GetState().core.GetInterrupts(), 0);
}

// The WAIT ends without completing, so its register isn't updated.
TEST_F(CoreTest, WAKE_Waiting) {
  ASSERT_TRUE(InitWaitingCore1());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  state0.SetRegisters({{CpuCore::ST, ZC}});

  state0.code.AddValue(Encode("WAKE", {"$#3", 1}));
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 4);  // WAKE 1
  EXPECT_EQ(state0.st, ZC);
  EXPECT_NE(state1.core.GetState(), CpuCore::State::kWaiting);
  ExecuteUntilHalt(1);
  EXPECT_EQ(state1.r6, 1);
  EXPECT_EQ(state1.r0, 1000);
  EXPECT_LT(state1.core.GetCycles(), 1000);
}

// A WAKE SELF in an interrupt handler ends the WAIT it interrupted, so the
// handler returns to the instruction after the WAIT.
TEST_F(CoreTest, WAKE_SELF_InInterrupt) {
  ASSERT_TRUE(InitWaitingCore1());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  GetMemory(1)
      .SetAddress(100)
      .AddValue(Encode("WAKE", "SELF"))
      .AddValue(Encode("IRT"));

  state0.code.AddValue(Encode("INTC.I", {"$#3", 1})).AddValue(5);
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // INTC.I 1, 5
  ExecuteUntilHalt(1);
  EXPECT_EQ(state1.r6, 1);
  EXPECT_EQ(state1.r0, 1000);
  EXPECT_EQ(state1.sp, 500);
  EXPECT_LT(state1.core.GetCycles(), 1000);
}

// WAKE starts an idle core from its IP. A second WAKE, while the core is
// running, changes nothing.
TEST_F(CoreTest, WAKE_Idle) {
  ASSERT_TRUE(InitTwoCores());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  state0.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R2, 1}});
  state1.SetRegisters({{CpuCore::IP, 200}});

  // Core 1 shares bank 0 with core 0, and runs from 200.
  GetMemory(0)
      .SetAddress(200)
      .AddValue(Encode("MVQ.LW", CpuCore::R5, 7))
      .AddValue(Encode("MVQ.LW", CpuCore::R6, 8))
      .AddValue(Encode("MVQ.LW", CpuCore::R7, 9))
      .AddValue(Encode("HALT"));

  state0.code.AddValue(Encode("WAKE", "R2"));
  const uint16_t ip1 = state0.code.AddNopGetAddress();
  state0.code.AddValue(Encode("WAKE", "R2"));
  const uint16_t ip2 = state0.code.AddNopGetAddress();

  ASSERT_EQ(state1.core.GetState(), CpuCore::State::kIdle);
  EXPECT_EQ(CyclesUntilIp(ip1), 4);  // WAKE R2 (1)
  EXPECT_EQ(state0.st, ZC);

  // The second WAKE waits for core 1 to finish its instruction, to lock it, so
  // it isn't timed.
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // WAKE R2 (1)
  EXPECT_NE(state1.core.GetState(), CpuCore::State::kIdle);
  ExecuteUntilHalt(1);
  EXPECT_EQ(state1.r5, 7);
  EXPECT_EQ(state1.r6, 8);
  EXPECT_EQ(state1.r7, 9);
  EXPECT_EQ(state1.ip, 203);
}

// A core idle from HALT runs the HALT again, as HALT moves IP back to itself,
// so it stays halted rather than running what follows.
TEST_F(CoreTest, WAKE_Halted) {
  ASSERT_TRUE(InitTwoCores());
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);
  state0.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R2, 1}});
  state1.SetRegisters({{CpuCore::IP, 200}});

  // Core 1 shares bank 0 with core 0, and is on a HALT at 200.
  GetMemory(0)
      .SetAddress(200)
      .AddValue(Encode("HALT"))
      .AddValue(Encode("MVQ.LW", CpuCore::R5, 7))
      .AddValue(Encode("HALT"));

  state0.code.AddValue(Encode("WAKE", "R2"));
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  // Core 1 runs, if only for the HALT.
  bool ran = false;
  EXPECT_EQ(CyclesUntilIp(ip1,
                          [&] {
                            ran = ran || state1.core.GetState() !=
                                             CpuCore::State::kIdle;
                          }),
            4);  // WAKE R2 (1)
  EXPECT_TRUE(ran);
  ExecuteUntilHalt(1);
  EXPECT_EQ(state1.r5, 0);
  EXPECT_EQ(state1.ip, 200);
}

// On a running core that isn't in an interrupt, WAKE SELF changes nothing, so
// it falls through. There is no core 1.
TEST_F(CoreTest, WAKE_Cycles) {
  ASSERT_TRUE(InitAndReset());
  GetState().SetRegisters({{CpuCore::R2, 1}});
  RunCycleCases({
      {"WAKE 1", 4, {Encode("WAKE", {"$#3", 1})}},
      {"WAKE R2 (1)", 4, {Encode("WAKE", "R2")}},
      {"WAKE SELF", 5, {Encode("WAKE", "SELF")}},
  });
}

}  // namespace
}  // namespace oz3
