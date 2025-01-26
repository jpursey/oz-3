// Copyright (c) 2025 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_test.h"

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

  state.code.AddValue(Encode("JCR", CpuCore::kConditionNZ, {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JCR", CpuCore::kConditionS, {"$r", CpuCore::R0}));
  state.code.AddValue(Encode("MOV.LW", CpuCore::R4, "$v")).AddValue(2);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("JCR", CpuCore::kConditionNC, {"$r", CpuCore::R0}));
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

}  // namespace
}  // namespace oz3
