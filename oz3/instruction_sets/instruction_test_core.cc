// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/instruction_sets/instruction_test.h"

namespace oz3 {
namespace {

constexpr uint16_t ZC = CpuCore::Z | CpuCore::C;

class RstTest : public InstructionTest {
 protected:
  // Initializes two cores and two memory banks, and resets core 0 with the same
  // banks, BD, and BE as InitAndReset. Core 1 is left idle with all registers
  // zero, as cores are before anything resets them.
  bool InitTwoCores() {
    if (!Init({.num_cores = 2, .num_memory_banks = 2})) {
      return false;
    }
    GetState(0).ResetCore({.mask = CpuCore::ResetParams::ALL,
                           .mb = CpuCore::Banks().SetExtra(1).ToWord(),
                           .bd = 1000,
                           .be = 2000});
    return true;
  }
};

TEST_F(RstTest, RST_AllBanks) {
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
  EXPECT_EQ(CyclesUntilIp(ip1), 27);  // RST 1, CODE|STACK|DATA|EXTRA
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
  state1.Update();
  EXPECT_EQ(state1.r5, 7);
  EXPECT_EQ(state1.ip, 1);
}

TEST_F(RstTest, RST_Code) {
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

  // Core 1 runs from 100 in bank 1 once reset.
  GetMemory(1)
      .SetAddress(100)
      .AddValue(Encode("MVQ.LW", CpuCore::R5, 7))
      .AddValue(Encode("HALT"));

  state0.code.AddValue(Encode("RST", {"$#3", 1}, 0x1));
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  // Only the code bank and registers change.
  EXPECT_EQ(CyclesUntilIp(ip1), 18);  // RST 1, CODE
  EXPECT_EQ(state0.st, ZC);
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
  state1.Update();
  EXPECT_EQ(state1.r5, 7);
  EXPECT_EQ(state1.ip, 1);
}

TEST_F(RstTest, RST_StackAndExtra) {
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

TEST_F(RstTest, RST_CodeWaiting) {
  ASSERT_TRUE(Init({.num_cores = 2, .num_memory_banks = 2}));
  auto& state0 = GetState(0);
  auto& state1 = GetState(1);

  // Core 1 starts first, and waits for a long time. Once reset, it runs from
  // 100 in bank 1.
  GetMemory(1)
      .AddValue(Encode("WAIT", CpuCore::R0))
      .SetAddress(100)
      .AddValue(Encode("MVQ.LW", CpuCore::R5, 7))
      .AddValue(Encode("HALT"));
  state1.ResetCore({.mask = CpuCore::ResetParams::ALL,
                    .mb = CpuCore::Banks().SetCode(1).ToWord()});
  state1.SetRegisters({{CpuCore::R0, 1000}});
  ASSERT_TRUE(ExecuteUntil(
      1, [&] { return state1.core.GetState() == CpuCore::State::kWaiting; }));

  // Core 0 then resets core 1's code location.
  state0.ResetCore({.mask = CpuCore::ResetParams::ALL,
                    .mb = CpuCore::Banks().SetExtra(1).ToWord(),
                    .bd = 1000,
                    .be = 2000});
  state0.SetRegisters({{CpuCore::ST, ZC},
                       {CpuCore::R3, CpuCore::Banks().SetCode(1).ToWord()},
                       {CpuCore::R4, 100}});
  state0.code.AddValue(Encode("RST", {"$#3", 1}, 0x1));
  const uint16_t ip1 = state0.code.AddNopGetAddress();

  // The WAIT ends, and core 1 runs from its new code location.
  EXPECT_EQ(CyclesUntilIp(ip1), 18);  // RST 1, CODE
  EXPECT_NE(state1.core.GetState(), CpuCore::State::kWaiting);
  EXPECT_EQ(state1.st & CpuCore::W, 0);
  ExecuteUntilHalt(1);
  state1.Update();
  EXPECT_EQ(state1.r5, 7);
  EXPECT_EQ(state1.ip, 1);
  EXPECT_LT(state1.core.GetCycles(), 1000);
}

TEST_F(RstTest, RST_NoBanks) {
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

TEST_F(RstTest, RST_R2) {
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
  EXPECT_EQ(CyclesUntilIp(ip3), 27);    // RST R2 (5), CODE|STACK|DATA|EXTRA
  EXPECT_EQ(state0.mb, CpuCore::Banks().SetData(1).SetExtra(1).ToWord());
  EXPECT_EQ(state0.bc, 0);
  EXPECT_EQ(state1.bc, 0);
  EXPECT_EQ(state0.st, ZC);
}

TEST_F(RstTest, RST_SELF) {
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

  EXPECT_EQ(CyclesUntilIp(ip1), 21);  // RST SELF, CODE|DATA
  EXPECT_EQ(state.bc, 200);
  EXPECT_EQ(state.bd, 300);
  EXPECT_EQ(state.r0, 11);
  EXPECT_EQ(state.r1, 12);
  EXPECT_EQ(state.st, ZC);
}

// SELF takes a cycle more than a core index, to load -1, so resetting every
// bank of this core is the costliest RST.
TEST_F(RstTest, RST_SELF_AllBanks) {
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

  EXPECT_EQ(CyclesUntilIp(ip1), 28);  // RST SELF, CODE|STACK|DATA|EXTRA
  EXPECT_EQ(state.bc, 200);
  EXPECT_EQ(state.bs, 300);
  EXPECT_EQ(state.bd, 400);
  EXPECT_EQ(state.be, 500);
}

// With no banks, RST changes nothing, so it falls through. There is no core 1.
TEST_F(RstTest, RST_NoBanksCycles) {
  ASSERT_TRUE(InitAndReset());
  GetState().SetRegisters({{CpuCore::R2, 1}});
  RunCycleCases({
      {"RST R2 (1), 0", 11, {Encode("RST", "R2", 0)}},
      {"RST SELF, 0", 12, {Encode("RST", "SELF", 0)}},
  });
}

}  // namespace
}  // namespace oz3
