// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_test.h"

namespace oz3 {
namespace {

TEST_F(InstructionTest, MOV_LW) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.PushValue(100);  // FP
  state.stack.PushValue(200);  // SP
  state.data.SetAddress(state.data.GetAddress() + 100).AddValue(300);
  state.extra.SetAddress(state.extra.GetAddress() + 200).AddValue(400);
  state.SetRegisters({{CpuCore::R0, 42},   // For "$r"
                      {CpuCore::R1, 100},  // For "($r)"
                      {CpuCore::R2, 50},   // For "($r + $v)", v == 50
                      {CpuCore::R4, 150},  // For "($r + $v)", v == 50
                      {CpuCore::R6, -2},   // For "($r)"
                      {CpuCore::SP, -2},
                      {CpuCore::FP, -1}});

  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, {"($r)", CpuCore::R1}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, {"($r + $v)", CpuCore::R2}))
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, {"($r)", CpuCore::R6}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, "(SP)"));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, "(SP + $v)")).AddValue(1);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, "(FP)"));
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, "(FP + $v)")).AddValue(-1);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, "$v")).AddValue(24);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, "S($v)")).AddValue(-1);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, "D($v)")).AddValue(100);
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, "E($v)")).AddValue(200);
  const uint16_t ip13 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // MOV.LW R7, R0
  EXPECT_EQ(state.r7, 42);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // MOV.LW R7, (R1)
  EXPECT_EQ(state.r7, 300);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // MOV.LW R7, (R2 + 50)
  EXPECT_EQ(state.r7, 300);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // MOV.LW R7, (R4 + 50)
  EXPECT_EQ(state.r7, 400);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // MOV.LW R7, (R6)
  EXPECT_EQ(state.r7, 200);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // MOV.LW R7, (SP)
  EXPECT_EQ(state.r7, 200);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // MOV.LW R7, (SP + 1)
  EXPECT_EQ(state.r7, 100);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // MOV.LW R7, (FP)
  EXPECT_EQ(state.r7, 100);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // MOV.LW R7, (FP - 1)
  EXPECT_EQ(state.r7, 200);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // MOV.LW R7, 24
  EXPECT_EQ(state.r7, 24);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // MOV.LW R7, S(-1)
  EXPECT_EQ(state.r7, 100);
  ASSERT_TRUE(ExecuteUntilIp(ip12));  // MOV.LW R7, D(100)
  EXPECT_EQ(state.r7, 300);
  ASSERT_TRUE(ExecuteUntilIp(ip13));  // MOV.LW R7, E(200)
  EXPECT_EQ(state.r7, 400);
}

TEST_F(InstructionTest, MOV_LD) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.PushValue32(0x10002);  // FP
  state.stack.PushValue32(0x30004);  // SP
  state.data.SetAddress(state.data.GetAddress() + 100).AddValue32(0x40005);
  state.extra.SetAddress(state.extra.GetAddress() + 200).AddValue32(0x60007);
  state.SetRegisters({{CpuCore::R0, 9},    // For "$R"
                      {CpuCore::R1, 8},    // For "$R"
                      {CpuCore::R4, 150},  // For "($r + $v)", v == 50
                      {CpuCore::R6, -2},   // For "($r)"
                      {CpuCore::SP, -4},
                      {CpuCore::FP, -2}});

  state.code.AddValue(Encode("MOV.LD", 1, {"$R", 0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 0, "$V")).AddValue32(50 | (100 << 16));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, {"[$r]", CpuCore::R1}));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, {"[$r + $v]", CpuCore::R0}))
      .AddValue(50);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, {"[$r]", CpuCore::R6}));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, "[SP]"));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, "[SP + $v]")).AddValue(2);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, "[FP]"));
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, "[FP + $v]")).AddValue(-2);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, "S[$v]")).AddValue(-2);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, "D[$v]")).AddValue(100);
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LD", 1, "E[$v]")).AddValue(200);
  const uint16_t ip13 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // MOV.LD D1, D0
  EXPECT_EQ(state.d1(), 0x80009);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // MOV.LD D0, $V
  EXPECT_EQ(state.d0(), 50 | (100 << 16));
  EXPECT_EQ(state.r0, 50);
  EXPECT_EQ(state.r1, 100);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // MOV.LD D1, [R1]
  EXPECT_EQ(state.d1(), 0x40005);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // MOV.LD D1, [R0 + 50]
  EXPECT_EQ(state.d1(), 0x40005);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // MOV.LD D1, [R4 + 50]
  EXPECT_EQ(state.d1(), 0x60007);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // MOV.LD D1, [R6]
  EXPECT_EQ(state.d1(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // MOV.LD D1, [SP]
  EXPECT_EQ(state.d1(), 0x30004);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // MOV.LD D1, [SP + 2]
  EXPECT_EQ(state.d1(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // MOV.LD D1, [FP]
  EXPECT_EQ(state.d1(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // MOV.LD D1, [FP - 2]
  EXPECT_EQ(state.d1(), 0x30004);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // MOV.LD D1, S[-2]
  EXPECT_EQ(state.d1(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip12));  // MOV.LD D1, D[100]
  EXPECT_EQ(state.d1(), 0x40005);
  ASSERT_TRUE(ExecuteUntilIp(ip13));  // MOV.LD D1, E[200]
  EXPECT_EQ(state.d1(), 0x60007);
}

TEST_F(InstructionTest, MOV_SWR) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::R1, 100},   // For "($r)"
                      {CpuCore::R2, 50},    // For "($r + $v)", v == 51
                      {CpuCore::R4, 150},   // For "($r + $v)", v == 50
                      {CpuCore::R6, -5},    // For "($r)"
                      {CpuCore::R7, 42},    // For all
                      {CpuCore::SP, -3},    // For "(SP)" and "(SP + $v)"
                      {CpuCore::FP, -1}});  // For "(FP)" and "(FP + $v)"

  state.code.AddValue(Encode("MOV.SWR", {"($r)", CpuCore::R1}, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code
      .AddValue(Encode("MOV.SWR", {"($r + $v)", CpuCore::R2}, CpuCore::R7))
      .AddValue(51);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code
      .AddValue(Encode("MOV.SWR", {"($r + $v)", CpuCore::R4}, CpuCore::R7))
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWR", {"($r)", CpuCore::R6}, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWR", "(SP)", CpuCore::R7));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWR", "(SP + $v)", CpuCore::R7)).AddValue(-1);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWR", "(FP)", CpuCore::R7));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWR", "(FP + $v)", CpuCore::R7)).AddValue(-1);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWR", "S($v)", CpuCore::R7)).AddValue(-6);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWR", "D($v)", CpuCore::R7)).AddValue(102);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWR", "E($v)", CpuCore::R7)).AddValue(202);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // MOV.SWR (R1), R7
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // MOV.SWR (R2 + 51), R7
  EXPECT_EQ(state.data.SetAddress(state.bd + 101).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // MOV.SWR (R4 + 50), R7
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // MOV.SWR (R6), R7
  EXPECT_EQ(state.stack.SetAddress(state.bs - 5).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // MOV.SWR (SP), R7
  EXPECT_EQ(state.stack.SetAddress(state.bs - 3).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // MOV.SWR (SP - 1), R7
  EXPECT_EQ(state.stack.SetAddress(state.bs - 4).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // MOV.SWR (FP), R7
  EXPECT_EQ(state.stack.SetAddress(state.bs - 1).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // MOV.SWR (FP - 1), R7
  EXPECT_EQ(state.stack.SetAddress(state.bs - 2).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // MOV.SWR S(-6), R7
  EXPECT_EQ(state.stack.SetAddress(state.bs - 6).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // MOV.SWR D(102), R7
  EXPECT_EQ(state.data.SetAddress(state.bd + 102).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // MOV.SWR E(202), R7
  EXPECT_EQ(state.extra.SetAddress(state.be + 202).GetValue(), 42);
}

TEST_F(InstructionTest, MOV_SWV) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::R1, 100},   // For "($r)"
                      {CpuCore::R2, 50},    // For "($r + $v)", v == 51
                      {CpuCore::R4, 150},   // For "($r + $v)", v == 50
                      {CpuCore::R6, -5},    // For "($r)"
                      {CpuCore::SP, -3},    // For "(SP)" and "(SP + $v)"
                      {CpuCore::FP, -1}});  // For "(FP)" and "(FP + $v)"

  state.code.AddValue(Encode("MOV.SWV", {"($r), $v", CpuCore::R1}))
      .AddValue(42);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWV", {"($r + $v), $v", CpuCore::R2}))
      .AddValue(51)
      .AddValue(42);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWV", {"($r + $v), $v", CpuCore::R4}))
      .AddValue(50)
      .AddValue(42);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWV", {"($r), $v", CpuCore::R6}))
      .AddValue(42);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWV", "(SP), $v")).AddValue(42);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWV", "(SP + $v), $v"))
      .AddValue(-1)
      .AddValue(42);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWV", "(FP), $v")).AddValue(42);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWV", "(FP + $v), $v"))
      .AddValue(-1)
      .AddValue(42);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWV", "S($v), $v")).AddValue(-6).AddValue(42);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWV", "D($v), $v"))
      .AddValue(102)
      .AddValue(42);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SWV", "E($v), $v"))
      .AddValue(202)
      .AddValue(42);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // MOV.SWR (R1), 42
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // MOV.SWR (R2 + 51), 42
  EXPECT_EQ(state.data.SetAddress(state.bd + 101).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // MOV.SWR (R4 + 50), 42
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // MOV.SWR (R6), 42
  EXPECT_EQ(state.stack.SetAddress(state.bs - 5).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // MOV.SWR (SP), 42
  EXPECT_EQ(state.stack.SetAddress(state.bs - 3).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // MOV.SWR (SP - 1), 42
  EXPECT_EQ(state.stack.SetAddress(state.bs - 4).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // MOV.SWR (FP), 42
  EXPECT_EQ(state.stack.SetAddress(state.bs - 1).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // MOV.SWR (FP - 1), 42
  EXPECT_EQ(state.stack.SetAddress(state.bs - 2).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // MOV.SWR S(-6), 42
  EXPECT_EQ(state.stack.SetAddress(state.bs - 6).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // MOV.SWR D(102), 42
  EXPECT_EQ(state.data.SetAddress(state.bd + 102).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // MOV.SWR E(202), 42
  EXPECT_EQ(state.extra.SetAddress(state.be + 202).GetValue(), 42);
}

TEST_F(InstructionTest, MOV_SDR) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::R0, 100},   // For "[$r]"
                      {CpuCore::R1, 50},    // For "[$r + $v]", v == 52
                      {CpuCore::R2, 1},     // For all (low word)
                      {CpuCore::R3, 2},     // For all (high word)
                      {CpuCore::R4, 150},   // For "[$r + $v]", v == 50
                      {CpuCore::R6, -10},   // For "[$r]"
                      {CpuCore::SP, -6},    // For "[SP]" and "[SP + $v]"
                      {CpuCore::FP, -2}});  // For "[FP]" and "[FP + $v]"

  state.code.AddValue(Encode("MOV.SDR", {"[$r]", CpuCore::R0}, 1));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDR", {"[$r + $v]", CpuCore::R1}, 1))
      .AddValue(52);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDR", {"[$r + $v]", CpuCore::R4}, 1))
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDR", {"[$r]", CpuCore::R6}, 1));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDR", "[SP]", 1));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDR", "[SP + $v]", 1)).AddValue(-2);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDR", "[FP]", 1));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDR", "[FP + $v]", 1)).AddValue(-2);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDR", "S[$v]", 1)).AddValue(-12);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDR", "D[$v]", 1)).AddValue(102);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDR", "E[$v]", 1)).AddValue(202);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // MOV.SDR [R0], D1
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // MOV.SDR [R1 + 52], D1
  EXPECT_EQ(state.data.SetAddress(state.bd + 102).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // MOV.SDR [R4 + 50], D1
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // MOV.SDR [R6], D1
  EXPECT_EQ(state.stack.SetAddress(state.bs - 10).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // MOV.SDR [SP], D1
  EXPECT_EQ(state.stack.SetAddress(state.bs - 6).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // MOV.SDR [SP - 2], D1
  EXPECT_EQ(state.stack.SetAddress(state.bs - 8).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // MOV.SDR [FP], D1
  EXPECT_EQ(state.stack.SetAddress(state.bs - 2).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // MOV.SDR [FP - 2], D1
  EXPECT_EQ(state.stack.SetAddress(state.bs - 4).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // MOV.SDR S[-12], D1
  EXPECT_EQ(state.stack.SetAddress(state.bs - 12).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // MOV.SDR D[102], D1
  EXPECT_EQ(state.data.SetAddress(state.bd + 102).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // MOV.SDR E[202], D1
}

TEST_F(InstructionTest, MOV_SDV) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::R0, 100},   // For "[$r]"
                      {CpuCore::R1, 50},    // For "[$r + $v]", v == 52
                      {CpuCore::R4, 150},   // For "[$r + $v]", v == 50
                      {CpuCore::R6, -10},   // For "[$r]"
                      {CpuCore::SP, -6},    // For "[SP]" and "[SP + $v]"
                      {CpuCore::FP, -2}});  // For "[FP]" and "[FP + $v]"
  state.code.AddValue(Encode("MOV.SDV", {"[$r], $v", CpuCore::R0}))
      .AddValue32(0x20001);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDV", {"[$r + $v], $v", CpuCore::R1}))
      .AddValue(52)
      .AddValue32(0x20001);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDV", {"[$r + $v], $v", CpuCore::R4}))
      .AddValue(50)
      .AddValue32(0x20001);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDV", {"[$r], $v", CpuCore::R6}))
      .AddValue32(0x20001);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDV", "[SP], $v")).AddValue32(0x20001);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDV", "[SP + $v], $v"))
      .AddValue(-2)
      .AddValue32(0x20001);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDV", "[FP], $v")).AddValue32(0x20001);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDV", "[FP + $v], $v"))
      .AddValue(-2)
      .AddValue32(0x20001);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDV", "S[$v], $v"))
      .AddValue(-12)
      .AddValue32(0x20001);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDV", "D[$v], $v"))
      .AddValue(102)
      .AddValue32(0x20001);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.SDV", "E[$v], $v"))
      .AddValue(202)
      .AddValue32(0x20001);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // MOV.SDR [R0], 0x20001
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // MOV.SDR [R1 + 52], 0x20001
  EXPECT_EQ(state.data.SetAddress(state.bd + 102).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // MOV.SDR [R4 + 50], 0x20001
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // MOV.SDR [R6], 0x20001
  EXPECT_EQ(state.stack.SetAddress(state.bs - 10).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // MOV.SDR [SP], 0x20001
  EXPECT_EQ(state.stack.SetAddress(state.bs - 6).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // MOV.SDR [SP - 2], 0x20001
  EXPECT_EQ(state.stack.SetAddress(state.bs - 8).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // MOV.SDR [FP], 0x20001
  EXPECT_EQ(state.stack.SetAddress(state.bs - 2).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // MOV.SDR [FP - 2], 0x20001
  EXPECT_EQ(state.stack.SetAddress(state.bs - 4).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // MOV.SDR S[-12], 0x20001
  EXPECT_EQ(state.stack.SetAddress(state.bs - 12).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // MOV.SDR D[102], 0x20001
  EXPECT_EQ(state.data.SetAddress(state.bd + 102).GetValue32(), 0x20001);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // MOV.SDR E[202], 0x20001
  EXPECT_EQ(state.extra.SetAddress(state.be + 202).GetValue32(), 0x20001);
}

TEST_F(InstructionTest, MOV_S_CodeAndStack) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  state.code.AddValue(Encode("MOV.S", "BS, $v")).AddValue(200);
  state.code.AddValue(Encode("MOV.S", "BC, $v")).AddValue(100);
  uint16_t offset = state.code.GetAddress();
  state.code.AddValue(Encode("HALT"));

  state.code.SetAddress(100 + offset);
  state.code.AddValue(Encode("MOV.S", {"$r, BS", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.S", {"$r, BC", CpuCore::R1}));
  const uint16_t ip1 = state.code.AddNopGetAddress() - 100;
  state.code.AddValue(Encode("MOV.S", {"BS, $r", CpuCore::R1}));
  state.code.AddValue(Encode("MOV.S", {"BC, $r", CpuCore::R0}));
  offset = state.code.GetAddress() - 100;
  state.code.AddValue(Encode("HALT"));

  state.code.SetAddress(100 + offset);
  const uint16_t ip2 = state.code.AddNopGetAddress() - 100;
  state.code.AddValue(Encode("HALT"));

  // MOV.S BS, 200
  // MOV.S BC, 100
  // MOV.S R0, BS
  // MOV.S R1, BC
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.bs, 200);
  EXPECT_EQ(state.bc, 100);
  EXPECT_EQ(state.r0, 200);
  EXPECT_EQ(state.r1, 100);

  // MOV.S BS, R1
  // MOV.S BC, R0
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.bs, 100);
  EXPECT_EQ(state.bc, 200);
}

TEST_F(InstructionTest, MOV_S_DataAndExtra) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  state.code.AddValue(Encode("MOV.S", "BD, $v")).AddValue(100);
  state.code.AddValue(Encode("MOV.S", "BE, $v")).AddValue(200);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.S", {"$r, BD", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.S", {"$r, BE", CpuCore::R1}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.S", {"BD, $r", CpuCore::R1}));
  state.code.AddValue(Encode("MOV.S", {"BE, $r", CpuCore::R0}));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  // MOV.S BD, 100
  // MOV.S BE, 200
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.bd, 100);
  EXPECT_EQ(state.be, 200);

  // MOV.S R0, BD
  // MOV.S R1, BE
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r0, 100);
  EXPECT_EQ(state.r1, 200);

  // MOV.S BD, R1
  // MOV.S BE, R0
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.bd, 200);
  EXPECT_EQ(state.be, 100);
}

TEST_F(InstructionTest, MOV_S_StackAndFramePointers) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  state.code.AddValue(Encode("MOV.S", "SP, $v")).AddValue(100);
  state.code.AddValue(Encode("MOV.S", "FP, $v")).AddValue(200);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.S", {"$r, SP", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.S", {"$r, FP", CpuCore::R1}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.S", {"SP, $r", CpuCore::R1}));
  state.code.AddValue(Encode("MOV.S", {"FP, $r", CpuCore::R0}));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  // MOV.S SP, 100
  // MOV.S FP, 200
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.sp, 100);
  EXPECT_EQ(state.fp, 200);

  // MOV.S R0, SP
  // MOV.S R1, FP
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r0, 100);
  EXPECT_EQ(state.r1, 200);

  // MOV.S SP, R1
  // MOV.S FP, R0
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.sp, 200);
  EXPECT_EQ(state.fp, 100);
}

TEST_F(InstructionTest, MVQ_LW) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::R0, 42}});

  state.code.AddValue(Encode("MVQ.LW", CpuCore::R0, 0));
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R1, 31));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.r1, 31);
}

TEST_F(InstructionTest, MVQ_LD) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters(
      {{CpuCore::R0, 1}, {CpuCore::R1, 2}, {CpuCore::R2, 3}, {CpuCore::R3, 4}});

  state.code.AddValue(Encode("MVQ.LD", 0, 0));
  state.code.AddValue(Encode("MVQ.LD", 1, 31));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.d0(), 0);
  EXPECT_EQ(state.d1(), 31);
}

TEST_F(InstructionTest, PUSH_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.PushValue(100);  // FP
  state.stack.PushValue(200);  // SP
  state.data.SetAddress(state.data.GetAddress() + 100).AddValue(300);
  state.extra.SetAddress(state.extra.GetAddress() + 200).AddValue(400);
  state.SetRegisters({{CpuCore::R0, 42},   // For "$r"
                      {CpuCore::R1, 100},  // For "($r)"
                      {CpuCore::R2, 50},   // For "($r + $v)", v == 50
                      {CpuCore::R4, 150},  // For "($r + $v)", v == 50
                      {CpuCore::R6, -2},   // For "($r)"
                      {CpuCore::SP, -2},
                      {CpuCore::FP, -1}});

  state.code.AddValue(Encode("PUSH.W", {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", {"($r)", CpuCore::R1}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", {"($r + $v)", CpuCore::R2}))
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", {"($r)", CpuCore::R6}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", "(SP)"));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", "(SP + $v)")).AddValue(2);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", "(FP)"));
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", "(FP + $v)")).AddValue(-1);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", "$v")).AddValue(24);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", "S($v)")).AddValue(-1);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", "D($v)")).AddValue(100);
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.W", "E($v)")).AddValue(200);
  const uint16_t ip13 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  uint16_t sp = state.sp;
  ASSERT_TRUE(ExecuteUntilIp(ip1));  // PUSH.W R0
  EXPECT_EQ(state.sp, sp - 1);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 42);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // PUSH.W (R1)
  EXPECT_EQ(state.sp, sp - 2);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 300);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // PUSH.W (R2 + 50)
  EXPECT_EQ(state.sp, sp - 3);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 300);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // PUSH.W (R4 + 50)
  EXPECT_EQ(state.sp, sp - 4);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 400);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // PUSH.W (R6)
  EXPECT_EQ(state.sp, sp - 5);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 200);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // PUSH.W (SP)
  EXPECT_EQ(state.sp, sp - 6);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 200);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // PUSH.W (SP + 2)
  EXPECT_EQ(state.sp, sp - 7);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 400);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // PUSH.W (FP)
  EXPECT_EQ(state.sp, sp - 8);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 100);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // PUSH.W (FP - 1)
  EXPECT_EQ(state.sp, sp - 9);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 200);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // PUSH.W 24
  EXPECT_EQ(state.sp, sp - 10);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 24);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // PUSH.W S(-1)
  EXPECT_EQ(state.sp, sp - 11);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 100);
  ASSERT_TRUE(ExecuteUntilIp(ip12));  // PUSH.W D(100)
  EXPECT_EQ(state.sp, sp - 12);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 300);
  ASSERT_TRUE(ExecuteUntilIp(ip13));  // PUSH.W E(200)
  EXPECT_EQ(state.sp, sp - 13);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 400);
}

TEST_F(InstructionTest, PUSH_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.PushValue32(0x10002);  // FP
  state.stack.PushValue32(0x30004);  // SP
  state.data.SetAddress(state.data.GetAddress() + 100).AddValue32(0x40005);
  state.extra.SetAddress(state.extra.GetAddress() + 200).AddValue32(0x60007);
  state.SetRegisters({{CpuCore::R0, 9},
                      {CpuCore::R1, 8},
                      {CpuCore::R2, 100},  // For "[$r]"
                      {CpuCore::R3, 50},   // For "[$r + $v]", v == 50
                      {CpuCore::R4, 150},  // For "[$r + $v]", v == 50
                      {CpuCore::R6, -4},   // For "[$r]"
                      {CpuCore::SP, -4},
                      {CpuCore::FP, -2}});

  state.code.AddValue(Encode("PUSH.D", {"$R", 0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", {"[$r]", CpuCore::R2}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", {"[$r + $v]", CpuCore::R3}))
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", {"[$r]", CpuCore::R6}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", "[SP]"));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", "[SP + $v]")).AddValue(4);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", "[FP]"));
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", "[FP + $v]")).AddValue(-2);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", "$V")).AddValue32(0x12345678);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", "S[$v]")).AddValue(-2);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", "D[$v]")).AddValue(100);
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("PUSH.D", "E[$v]")).AddValue(200);
  const uint16_t ip13 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  uint16_t sp = state.sp;
  ASSERT_TRUE(ExecuteUntilIp(ip1));  // PUSH.D D0
  EXPECT_EQ(state.sp, sp - 2);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x80009);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // PUSH.D [R2]
  EXPECT_EQ(state.sp, sp - 4);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x40005);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // PUSH.D [R3 + 50]
  EXPECT_EQ(state.sp, sp - 6);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x40005);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // PUSH.D [R4 + 50]
  EXPECT_EQ(state.sp, sp - 8);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x60007);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // PUSH.D [R6]
  EXPECT_EQ(state.sp, sp - 10);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x30004);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // PUSH.D [SP]
  EXPECT_EQ(state.sp, sp - 12);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x30004);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // PUSH.D [SP + 4]
  EXPECT_EQ(state.sp, sp - 14);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x60007);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // PUSH.D [FP]
  EXPECT_EQ(state.sp, sp - 16);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // PUSH.D [FP - 2]
  EXPECT_EQ(state.sp, sp - 18);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x30004);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // PUSH.D 0x12345678
  EXPECT_EQ(state.sp, sp - 20);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(),
            0x12345678);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // PUSH.D S(-2)
  EXPECT_EQ(state.sp, sp - 22);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip12));  // PUSH.D D(100)
  EXPECT_EQ(state.sp, sp - 24);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x40005);
  ASSERT_TRUE(ExecuteUntilIp(ip13));  // PUSH.D E(200)
  EXPECT_EQ(state.sp, sp - 26);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x60007);
}

TEST_F(InstructionTest, POP_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.PushValue(100);  // FP
  state.stack.PushValue(200);
  state.stack.PushValue(1).PushValue(2).PushValue(3).PushValue(4);
  state.stack.PushValue(5).PushValue(6).PushValue(7).PushValue(8);
  state.stack.PushValue(9).PushValue(10).PushValue(11).PushValue(12);  // SP
  state.data.SetAddress(state.bd + 100).AddValue(300);
  state.extra.SetAddress(state.be + 200).AddValue(400);
  state.SetRegisters({{CpuCore::R2, 100},  // For "($r)"
                      {CpuCore::R3, 50},   // For "($r + $v)", v == 50
                      {CpuCore::R4, 150},  // For "($r + $v)", v == 50
                      {CpuCore::R6, -2},   // For "($r)"
                      {CpuCore::SP, -14},
                      {CpuCore::FP, -1}});

  state.code.AddValue(Encode("POP.W", {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", {"($r)", CpuCore::R2}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", {"($r + $v)", CpuCore::R3})).AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", {"($r + $v)", CpuCore::R4})).AddValue(50);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", {"($r)", CpuCore::R6}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", "(SP)"));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", "(SP + $v)")).AddValue(-2);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", "(FP)"));
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", "(FP + $v)")).AddValue(-1);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", "S($v)")).AddValue(-20);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", "D($v)")).AddValue(100);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.W", "E($v)")).AddValue(200);
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  uint16_t sp = state.sp;
  ASSERT_TRUE(ExecuteUntilIp(ip1));  // POP.W R0
  EXPECT_EQ(state.sp, sp + 1);
  EXPECT_EQ(state.r0, 12);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // POP.W (R2)
  EXPECT_EQ(state.sp, sp + 2);
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue(), 11);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // POP.W (R3 + 50)
  EXPECT_EQ(state.sp, sp + 3);
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue(), 10);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // POP.W (R4 + 50)
  EXPECT_EQ(state.sp, sp + 4);
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 9);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // POP.W (R6)
  EXPECT_EQ(state.sp, sp + 5);
  EXPECT_EQ(state.stack.SetAddress(state.bs - 2).GetValue(), 8);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // POP.W (SP)
  EXPECT_EQ(state.sp, sp + 6);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 7);
  state.stack.SetAddress(state.bs + state.sp).AddValue(6);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // POP.W (SP - 2)
  EXPECT_EQ(state.sp, sp + 7);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp - 2).GetValue(), 6);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // POP.W (FP)
  EXPECT_EQ(state.sp, sp + 8);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.fp).GetValue(), 5);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // POP.W (FP - 1)
  EXPECT_EQ(state.sp, sp + 9);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.fp - 1).GetValue(), 4);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // POP.W S(-20)
  EXPECT_EQ(state.sp, sp + 10);
  EXPECT_EQ(state.stack.SetAddress(state.bs - 20).GetValue(), 3);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // POP.W D(100)
  EXPECT_EQ(state.sp, sp + 11);
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue(), 2);
  ASSERT_TRUE(ExecuteUntilIp(ip12));  // POP.W E(200)
  EXPECT_EQ(state.sp, sp + 12);
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 1);
}

TEST_F(InstructionTest, POP_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.PushValue32(0x10002);  // FP
  state.stack.PushValue32(0x30004);
  state.stack.PushValue32(0x80009)
      .PushValue32(0x70008)
      .PushValue32(0x60007)
      .PushValue32(0x50006);
  state.stack.PushValue32(0x40005)
      .PushValue32(0x30004)
      .PushValue32(0x20003)
      .PushValue32(0x10002);
  state.stack.PushValue32(0xA000B)
      .PushValue32(0xC000D)
      .PushValue32(0xE000F)
      .PushValue32(0x12345678);  // SP
  state.data.SetAddress(state.bd + 100).AddValue32(0x11111111);
  state.extra.SetAddress(state.be + 200).AddValue32(0x22222222);
  state.SetRegisters({{CpuCore::R2, 100},  // For "[$r]"
                      {CpuCore::R3, 50},   // For "[$r + $v]", v == 50
                      {CpuCore::R4, 150},  // For "[$r + $v]", v == 50
                      {CpuCore::R6, -4},   // For "[$r]"
                      {CpuCore::SP, -28},
                      {CpuCore::FP, -2}});

  state.code.AddValue(Encode("POP.D", {"$R", 0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", {"[$r]", CpuCore::R2}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", {"[$r + $v]", CpuCore::R3})).AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", {"[$r + $v]", CpuCore::R4})).AddValue(50);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", {"[$r]", CpuCore::R6}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", "[SP]"));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", "[SP + $v]")).AddValue(-4);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", "[FP]"));
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", "[FP + $v]")).AddValue(-2);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", "S[$v]")).AddValue(-30);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", "D[$v]")).AddValue(100);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("POP.D", "E[$v]")).AddValue(200);
  const uint16_t ip12 = state.code.AddNopGetAddress();

  uint16_t sp = state.sp;
  ASSERT_TRUE(ExecuteUntilIp(ip1));  // POP.D D0
  EXPECT_EQ(state.sp, sp + 2);
  EXPECT_EQ(state.d0(), 0x12345678);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // POP.D [R2]
  EXPECT_EQ(state.sp, sp + 4);
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue32(), 0xE000F);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // POP.D [R3 + 50]
  EXPECT_EQ(state.sp, sp + 6);
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue32(), 0xC000D);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // POP.D [R4 + 50]
  EXPECT_EQ(state.sp, sp + 8);
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0xA000B);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // POP.D [R6]
  EXPECT_EQ(state.sp, sp + 10);
  EXPECT_EQ(state.stack.SetAddress(state.bs - 4).GetValue32(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // POP.D [SP]
  EXPECT_EQ(state.sp, sp + 12);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue32(), 0x20003);
  state.stack.SetAddress(state.bs + state.sp).AddValue32(0x30004);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // POP.D [SP - 4]
  EXPECT_EQ(state.sp, sp + 14);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp - 4).GetValue32(),
            0x30004);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // POP.D [FP]
  EXPECT_EQ(state.sp, sp + 16);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.fp).GetValue32(), 0x40005);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // POP.D [FP - 2]
  EXPECT_EQ(state.sp, sp + 18);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.fp - 2).GetValue32(),
            0x50006);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // POP.D S(-30)
  EXPECT_EQ(state.sp, sp + 20);
  EXPECT_EQ(state.stack.SetAddress(state.bs - 30).GetValue32(), 0x60007);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // POP.D D(100)
  EXPECT_EQ(state.sp, sp + 22);
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue32(), 0x70008);
  ASSERT_TRUE(ExecuteUntilIp(ip12));  // POP.D E(200)
  EXPECT_EQ(state.sp, sp + 24);
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0x80009);
}

}  // namespace
}  // namespace oz3
