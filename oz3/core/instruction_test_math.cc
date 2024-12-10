// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_test.h"

namespace oz3 {
namespace {

// All word-size math operations use the same macro GetWord to retrieve the
// second argument, so we test it once here with ADD.W.
TEST_F(InstructionTest, MathOpsGetWord) {
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

  state.code.AddValue(Encode("ADD.W", CpuCore::R7, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, {"($r)", CpuCore::R1}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, {"($r + $v)", CpuCore::R2}))
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, {"($r)", CpuCore::R6}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, "(SP)"));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, "(SP + $v)")).AddValue(1);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, "(FP)"));
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, "(FP + $v)")).AddValue(-1);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, "$v")).AddValue(24);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, "S($v)")).AddValue(-1);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, "D($v)")).AddValue(100);
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  state.code.AddValue(Encode("ADD.W", CpuCore::R7, "E($v)")).AddValue(200);
  const uint16_t ip13 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // ADD.W R7, R0
  EXPECT_EQ(state.r7, 42);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // ADD.W R7, (R1)
  EXPECT_EQ(state.r7, 300);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // ADD.W R7, (R2 + 50)
  EXPECT_EQ(state.r7, 300);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // ADD.W R7, (R4 + 50)
  EXPECT_EQ(state.r7, 400);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // ADD.W R7, (R6)
  EXPECT_EQ(state.r7, 200);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // ADD.W R7, (SP)
  EXPECT_EQ(state.r7, 200);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // ADD.W R7, (SP + 1)
  EXPECT_EQ(state.r7, 100);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // ADD.W R7, (FP)
  EXPECT_EQ(state.r7, 100);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // ADD.W R7, (FP - 1)
  EXPECT_EQ(state.r7, 200);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // ADD.W R7, 24
  EXPECT_EQ(state.r7, 24);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // ADD.W R7, S(-1)
  EXPECT_EQ(state.r7, 100);
  ASSERT_TRUE(ExecuteUntilIp(ip12));  // ADD.W R7, D(100)
  EXPECT_EQ(state.r7, 300);
  ASSERT_TRUE(ExecuteUntilIp(ip13));  // ADD.W R7, E(200)
  EXPECT_EQ(state.r7, 400);
}

// All dword-size math operations use the same macro GetDword to retrieve the
// second argument, so we test it once here with ADD.D.
TEST_F(InstructionTest, MathOpsGetDword) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.PushValue32(0x10002);  // FP
  state.stack.PushValue32(0x30004);  // SP
  state.data.SetAddress(state.data.GetAddress() + 100).AddValue32(0x40005);
  state.extra.SetAddress(state.extra.GetAddress() + 200).AddValue32(0x60007);
  state.SetRegisters({{CpuCore::R2, 9},    // For "$R"
                      {CpuCore::R3, 8},    // For "$R"
                      {CpuCore::R4, 150},  // For "[$r + $v]", v == 50
                      {CpuCore::R6, -2},   // For "[$r]"
                      {CpuCore::SP, -4},
                      {CpuCore::FP, -2}});

  state.code.AddValue(Encode("ADD.D", 0, {"$R", 1}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 0, 0));
  state.code.AddValue(Encode("ADD.D", 0, "$V")).AddValue32(50 | (100 << 16));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, {"[$r]", CpuCore::R1}));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, {"[$r + $v]", CpuCore::R0}))
      .AddValue(50);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, {"[$r]", CpuCore::R6}));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, "[SP]"));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, "[SP + $v]")).AddValue(2);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, "[FP]"));
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, "[FP + $v]")).AddValue(-2);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, "S[$v]")).AddValue(-2);
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, "D[$v]")).AddValue(100);
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("ADD.D", 1, "E[$v]")).AddValue(200);
  const uint16_t ip13 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LD", 1, 0));
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // ADD.D D0, D1
  EXPECT_EQ(state.d0(), 0x80009);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // ADD.D D0, $V
  EXPECT_EQ(state.d0(), 50 | (100 << 16));
  EXPECT_EQ(state.r0, 50);
  EXPECT_EQ(state.r1, 100);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // ADD.D D1, [R1]
  EXPECT_EQ(state.d1(), 0x40005);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // ADD.D D1, [R0 + 50]
  EXPECT_EQ(state.d1(), 0x40005);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // ADD.D D1, [R4 + 50]
  EXPECT_EQ(state.d1(), 0x60007);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // ADD.D D1, [R6]
  EXPECT_EQ(state.d1(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // ADD.D D1, [SP]
  EXPECT_EQ(state.d1(), 0x30004);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // ADD.D D1, [SP + 2]
  EXPECT_EQ(state.d1(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip9));  // ADD.D D1, [FP]
  EXPECT_EQ(state.d1(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip10));  // ADD.D D1, [FP - 2]
  EXPECT_EQ(state.d1(), 0x30004);
  ASSERT_TRUE(ExecuteUntilIp(ip11));  // ADD.D D1, S[-2]
  EXPECT_EQ(state.d1(), 0x10002);
  ASSERT_TRUE(ExecuteUntilIp(ip12));  // ADD.D D1, D[100]
  EXPECT_EQ(state.d1(), 0x40005);
  ASSERT_TRUE(ExecuteUntilIp(ip13));  // ADD.D D1, E[200]
  EXPECT_EQ(state.d1(), 0x60007);
}

TEST_F(InstructionTest, ADD_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  state.code.AddValue(Encode("ADD.W", CpuCore::R0, "$v")).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.W", CpuCore::R0, "$v")).AddValue(0x7FFF);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.W", CpuCore::R0, "$v")).AddValue(0x8000);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.W", CpuCore::R0, "$v")).AddValue(0xFFFF);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.W", CpuCore::R0, "$v")).AddValue(1);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // ADD.W R0, 1
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // ADD.W R0, 0x7FFF
  EXPECT_EQ(state.r0, 0x8000);
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::O);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // ADD.W R0, 0x8000
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C | CpuCore::O);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // ADD.W R0, 0xFFFF
  EXPECT_EQ(state.r0, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // ADD.W R0, 1
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, ADD_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  state.code.AddValue(Encode("ADD.D", 0, "$V")).AddValue32(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.D", 0, "$V")).AddValue32(0x7FFFFFFF);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.D", 0, "$V")).AddValue32(0x80000000);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.D", 0, "$V")).AddValue32(0xFFFFFFFF);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.D", 0, "$V")).AddValue32(1);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // ADD.D D0, 1
  EXPECT_EQ(state.d0(), 1);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // ADD.D D0, 0x7FFFFFFF
  EXPECT_EQ(state.d0(), 0x80000000);
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::O);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // ADD.D D0, 0x80000000
  EXPECT_EQ(state.d0(), 0);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C | CpuCore::O);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // ADD.D D0, 0xFFFFFFFF
  EXPECT_EQ(state.d0(), 0xFFFFFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // ADD.D D0, 1
  EXPECT_EQ(state.d0(), 0);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, ADD_S) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  state.code.AddValue(Encode("ADD.S", "$v")).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.S", "$v")).AddValue(0x7FFF);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.S", "$v")).AddValue(0x8000);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.S", "$v")).AddValue(0xFFFF);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("ADD.S", "$v")).AddValue(1);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // ADD.S SP, 1
  EXPECT_EQ(state.sp, 1);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // ADD.S SP, 0x7FFF
  EXPECT_EQ(state.sp, 0x8000);
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::O);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // ADD.S SP, 0x8000
  EXPECT_EQ(state.sp, 0);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C | CpuCore::O);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // ADD.S SP, 0xFFFF
  EXPECT_EQ(state.sp, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // ADD.S SP, 1
  EXPECT_EQ(state.sp, 0);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

}  // namespace
}  // namespace oz3