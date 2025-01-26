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
  state.SetRegisters(
      {{CpuCore::R0, 1}, {CpuCore::R1, 100}, {CpuCore::R2, 50}});

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

}  // namespace
}  // namespace oz3
