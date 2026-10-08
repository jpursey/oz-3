// Copyright (c) 2025 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/instruction_sets/instruction_test.h"

namespace oz3 {
namespace {

TEST_F(InstructionTest, JP) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::R0, 1}, {CpuCore::R1, 100}, {CpuCore::R2, 50}});

  state.code.AddValue(Encode("JP", {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(42);
  const uint16_t ip1 = state.code.AddNopGetAddress();

  state.code.AddValue(Encode("JP", {"$r", CpuCore::R1}));
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(99);
  state.code.AddValue(Encode("HALT"));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(24);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  state.code.AddValue(Encode("JP", {"$r", CpuCore::R2}));
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(49);
  state.code.AddValue(Encode("HALT"));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(12);
  const uint16_t ip3 = state.code.AddNopGetAddress();

  state.code.AddValue(Encode("JP", "$v")).AddValue(75);
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(74);
  state.code.AddValue(Encode("HALT"));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(6);
  const uint16_t ip4 = state.code.AddNopGetAddress();

  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // JP $r, R0
  EXPECT_EQ(state.r4, 42);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // JP $r, R1
  EXPECT_EQ(state.r4, 24);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // JP $r, R2
  EXPECT_EQ(state.r4, 12);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // JP $v, 25
  EXPECT_EQ(state.r4, 6);
}

TEST_F(InstructionTest, JPR) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters(
      {{CpuCore::R0, 0}, {CpuCore::R1, 100}, {CpuCore::R2, -50}});

  state.code.AddValue(Encode("JPR", {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(42);
  const uint16_t ip1 = state.code.AddNopGetAddress();

  state.code.AddValue(Encode("JPR", {"$r", CpuCore::R1}));
  uint16_t jp_address = state.code.GetAddress() + 100;
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(jp_address - 1);
  state.code.AddValue(Encode("HALT"));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(24);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  state.code.AddValue(Encode("JPR", {"$r", CpuCore::R2}));
  jp_address = state.code.GetAddress() - 50;
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(jp_address - 1);
  state.code.AddValue(Encode("HALT"));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(12);
  const uint16_t ip3 = state.code.AddNopGetAddress();

  state.code.AddValue(Encode("JPR", "$v")).AddValue(25);
  jp_address = state.code.GetAddress() + 25;
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(jp_address - 1);
  state.code.AddValue(Encode("HALT"));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(6);
  const uint16_t ip4 = state.code.AddNopGetAddress();

  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // JPR $r, R0
  EXPECT_EQ(state.r4, 42);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // JPR $r, R1
  EXPECT_EQ(state.r4, 24);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // JPR $r, R2
  EXPECT_EQ(state.r4, 12);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // JPR $v, 25
  EXPECT_EQ(state.r4, 6);
}

TEST_F(InstructionTest, JC) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::R0, 100}});

  state.code.AddValue(Encode("JC", CpuCore::kConditionNZ, {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JC", CpuCore::kConditionS, {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(2);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JC", CpuCore::kConditionNC, {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(3);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JC", CpuCore::kConditionO, {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(4);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JC", CpuCore::kConditionZ, "$v")).AddValue(50);
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(50);
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(5);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JC", CpuCore::kConditionNS, "$v")).AddValue(60);
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(60);
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(6);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JC", CpuCore::kConditionC, "$v")).AddValue(70);
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(70);
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(7);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JC", CpuCore::kConditionNO, "$v")).AddValue(80);
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(80);
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(8);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(100);
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // JC NZ, R0
  EXPECT_EQ(state.r4, 1);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // JC S, R0
  EXPECT_EQ(state.r4, 2);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // JC NC, R0
  EXPECT_EQ(state.r4, 3);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // JC O, R0
  EXPECT_EQ(state.r4, 4);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // JC Z, 50
  EXPECT_EQ(state.r4, 5);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // JC NS, 60
  EXPECT_EQ(state.r4, 6);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // JC C, 70
  EXPECT_EQ(state.r4, 7);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // JC NO, 80
  EXPECT_EQ(state.r4, 8);
}

TEST_F(InstructionTest, JCR) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::R0, 100}});

  state.code.AddValue(
      Encode("JCR", CpuCore::kConditionNZ, {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JCR", CpuCore::kConditionS, {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(2);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(
      Encode("JCR", CpuCore::kConditionNC, {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(3);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JCR", CpuCore::kConditionO, {"$r", CpuCore::R0}));
  const uint16_t fail_offset = state.code.GetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(4);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JCR", CpuCore::kConditionZ, "$v")).AddValue(5);
  uint16_t jp_address = state.code.GetAddress() + 5;
  state.code.SetAddress(jp_address - 1);
  state.code.AddValue(Encode("HALT"));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(5);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JCR", CpuCore::kConditionNS, "$v")).AddValue(5);
  jp_address = state.code.GetAddress() + 5;
  state.code.SetAddress(jp_address - 1);
  state.code.AddValue(Encode("HALT"));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(6);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JCR", CpuCore::kConditionC, "$v")).AddValue(5);
  jp_address = state.code.GetAddress() + 5;
  state.code.SetAddress(jp_address - 1);
  state.code.AddValue(Encode("HALT"));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(7);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JCR", CpuCore::kConditionNO, "$v")).AddValue(5);
  jp_address = state.code.GetAddress() + 5;
  state.code.SetAddress(jp_address - 1);
  state.code.AddValue(Encode("HALT"));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(8);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(100 + fail_offset);
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // JCR NZ, R0
  EXPECT_EQ(state.r4, 1);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // JCR S, R0
  EXPECT_EQ(state.r4, 2);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // JCR NC, R0
  EXPECT_EQ(state.r4, 3);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // JCR O, R0
  EXPECT_EQ(state.r4, 4);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // JCR Z, 5
  EXPECT_EQ(state.r4, 5);
  ASSERT_TRUE(ExecuteUntilIp(ip6));  // JCR NS, 5
  EXPECT_EQ(state.r4, 6);
  ASSERT_TRUE(ExecuteUntilIp(ip7));  // JCR C, 5
  EXPECT_EQ(state.r4, 7);
  ASSERT_TRUE(ExecuteUntilIp(ip8));  // JCR NO, 5
  EXPECT_EQ(state.r4, 8);
}

TEST_F(InstructionTest, JD) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 50).AddValue(180);
  state.data.SetAddress(state.bd + 60).AddValue(120);
  state.SetRegisters({{CpuCore::ST, CpuCore::Z | CpuCore::C},
                      {CpuCore::R0, 2},
                      {CpuCore::R1, 50},
                      {CpuCore::R2, 0},
                      {CpuCore::R3, 1},
                      {CpuCore::R4, 1},
                      {CpuCore::R6, 151}});

  state.code.AddValue(Encode("JD", CpuCore::R0, {"$r", CpuCore::R1}));
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(50);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JD", CpuCore::R0, {"$r", CpuCore::R1}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JD", CpuCore::R2, "$v")).AddValue(70);
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(70);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JD", CpuCore::R3, "$v")).AddValue(0);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JD", CpuCore::R4, {"($r)", CpuCore::R1}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JD", CpuCore::R4, {"($r + $v)", CpuCore::R1}))
      .AddValue(10);
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(120);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JD", CpuCore::R6, {"$r", CpuCore::R6}));
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(150);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JD", CpuCore::R1, {"($r)", CpuCore::R1}));
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(180);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // JD R0, R1
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(CyclesUntilIp(ip2), 4);  // JD R0, R1
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 6);  // JD R2, 70
  EXPECT_EQ(state.r2, 0xFFFF);
  EXPECT_EQ(CyclesUntilIp(ip4), 5);  // JD R3, 0
  EXPECT_EQ(state.r3, 0);
  EXPECT_EQ(CyclesUntilIp(ip5), 6);  // JD R4, (R1)
  EXPECT_EQ(state.r4, 0);
  EXPECT_EQ(CyclesUntilIp(ip6), 9);  // JD R4, (R1 + 10)
  EXPECT_EQ(state.r4, 0xFFFF);
  EXPECT_EQ(CyclesUntilIp(ip7), 5);  // JD R6, R6
  EXPECT_EQ(state.r6, 150);
  EXPECT_EQ(CyclesUntilIp(ip8), 7);  // JD R1, (R1)
  EXPECT_EQ(state.r1, 49);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, JDR) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 50).AddValue(999);
  state.data.SetAddress(state.bd + 60).AddValue(40);
  state.SetRegisters({{CpuCore::ST, CpuCore::Z | CpuCore::C},
                      {CpuCore::R0, 2},
                      {CpuCore::R1, 50},
                      {CpuCore::R2, 0},
                      {CpuCore::R3, 1},
                      {CpuCore::R4, 1},
                      {CpuCore::R5, -30}});

  state.code.AddValue(Encode("JDR", CpuCore::R0, {"$r", CpuCore::R1}));
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(51);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JDR", CpuCore::R0, {"$r", CpuCore::R1}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JDR", CpuCore::R2, "$v")).AddValue(20);
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(76);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JDR", CpuCore::R3, "$v")).AddValue(0);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JDR", CpuCore::R4, {"($r)", CpuCore::R1}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JDR", CpuCore::R4, {"($r + $v)", CpuCore::R1}))
      .AddValue(10);
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(124);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JDR", CpuCore::R5, {"$r", CpuCore::R5}));
  state.code.AddValue(Encode("HALT"));
  state.code.SetAddress(95);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // JDR R0, R1
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(CyclesUntilIp(ip2), 4);  // JDR R0, R1
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 6);  // JDR R2, 20
  EXPECT_EQ(state.r2, 0xFFFF);
  EXPECT_EQ(CyclesUntilIp(ip4), 5);  // JDR R3, 0
  EXPECT_EQ(state.r3, 0);
  EXPECT_EQ(CyclesUntilIp(ip5), 6);  // JDR R4, (R1)
  EXPECT_EQ(state.r4, 0);
  EXPECT_EQ(CyclesUntilIp(ip6), 9);  // JDR R4, (R1 + 10)
  EXPECT_EQ(state.r4, 0xFFFF);
  EXPECT_EQ(CyclesUntilIp(ip7), 5);  // JDR R5, R5
  EXPECT_EQ(state.r5, static_cast<uint16_t>(-31));
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, CALL) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::Z | CpuCore::C},
                      {CpuCore::R0, 100},
                      {CpuCore::SP, 500}});

  state.code.AddValue(Encode("CALL", {"$r", CpuCore::R0}));
  state.code.SetAddress(100);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CALL", "$v")).AddValue(200);
  state.code.SetAddress(200);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 6);  // CALL R0
  EXPECT_EQ(state.sp, 499);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 1);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip2), 7);  // CALL 200
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 103);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, CALLR) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::Z | CpuCore::C},
                      {CpuCore::R0, 100},
                      {CpuCore::R1, -50},
                      {CpuCore::SP, 500}});

  state.code.AddValue(Encode("CALLR", {"$r", CpuCore::R0}));
  state.code.SetAddress(101);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CALLR", {"$r", CpuCore::R1}));
  state.code.SetAddress(53);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CALLR", "$v")).AddValue(25);
  state.code.SetAddress(81);
  const uint16_t ip3 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 6);  // CALLR R0
  EXPECT_EQ(state.sp, 499);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 1);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip2), 6);  // CALLR R1
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 103);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip3), 7);  // CALLR 25
  EXPECT_EQ(state.sp, 497);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 56);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, RET) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.SetAddress(500).PushValue(100).PushValue(50);
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::SP, 498}});

  state.code.AddValue(Encode("RET"));
  state.code.SetAddress(50);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RET"));
  state.code.SetAddress(100);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // RET
  EXPECT_EQ(state.sp, 499);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip2), 5);  // RET
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, RET_A) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.SetAddress(500).PushValue(100).PushValue(50).PushValue(1);
  state.stack.PushValue(2);
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::SP, 496}});

  state.code.AddValue(Encode("RET.A", 2));
  state.code.SetAddress(50);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RET.A", 0));
  state.code.SetAddress(100);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 6);  // RET.A 2
  EXPECT_EQ(state.sp, 499);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip2), 6);  // RET.A 0
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, RETC) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.SetAddress(500).PushValue(40).PushValue(30).PushValue(20);
  state.stack.PushValue(10);
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::SP, 496}});

  state.code.AddValue(Encode("RETC", CpuCore::kConditionNZ));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RETC", CpuCore::kConditionS));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RETC", CpuCore::kConditionNC));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RETC", CpuCore::kConditionO));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RETC", CpuCore::kConditionZ));
  state.code.SetAddress(10);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RETC", CpuCore::kConditionNS));
  state.code.SetAddress(20);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RETC", CpuCore::kConditionC));
  state.code.SetAddress(30);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RETC", CpuCore::kConditionNO));
  state.code.SetAddress(40);
  const uint16_t ip8 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 3);  // RETC NZ
  EXPECT_EQ(state.sp, 496);
  EXPECT_EQ(CyclesUntilIp(ip2), 3);  // RETC S
  EXPECT_EQ(state.sp, 496);
  EXPECT_EQ(CyclesUntilIp(ip3), 3);  // RETC NC
  EXPECT_EQ(state.sp, 496);
  EXPECT_EQ(CyclesUntilIp(ip4), 3);  // RETC O
  EXPECT_EQ(state.sp, 496);
  EXPECT_EQ(CyclesUntilIp(ip5), 6);  // RETC Z
  EXPECT_EQ(state.sp, 497);
  EXPECT_EQ(CyclesUntilIp(ip6), 6);  // RETC NS
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(CyclesUntilIp(ip7), 6);  // RETC C
  EXPECT_EQ(state.sp, 499);
  EXPECT_EQ(CyclesUntilIp(ip8), 6);  // RETC NO
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, RETC_A) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.SetAddress(500).PushValue(50).PushValue(1).PushValue(2);
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::Z | CpuCore::C}, {CpuCore::SP, 497}});

  state.code.AddValue(Encode("RETC.A", CpuCore::kConditionNZ, 2));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("RETC.A", CpuCore::kConditionZ, 2));
  state.code.SetAddress(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 3);  // RETC.A NZ, 2
  EXPECT_EQ(state.sp, 497);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip2), 7);  // RETC.A Z, 2
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, FBGN) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::Z | CpuCore::C},
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 300}});

  state.code.AddValue(Encode("FBGN"));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("FBGN"));
  const uint16_t ip2 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 6);  // FBGN
  EXPECT_EQ(state.sp, 499);
  EXPECT_EQ(state.fp, 499);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 300);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip2), 6);  // FBGN
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.fp, 498);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), 499);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

TEST_F(InstructionTest, FEND) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.SetAddress(500).PushValue(300).PushValue(499);
  state.SetRegisters({{CpuCore::ST, CpuCore::Z | CpuCore::C},
                      {CpuCore::SP, 490},
                      {CpuCore::FP, 498}});

  state.code.AddValue(Encode("FEND"));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("FEND"));
  const uint16_t ip2 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // FEND
  EXPECT_EQ(state.sp, 499);
  EXPECT_EQ(state.fp, 499);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);

  EXPECT_EQ(CyclesUntilIp(ip2), 5);  // FEND
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.fp, 300);
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
}

}  // namespace
}  // namespace oz3
