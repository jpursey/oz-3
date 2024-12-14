// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_test.h"

namespace oz3 {
namespace {

TEST_F(InstructionTest, NOT_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}, {CpuCore::R1, 0x5555}});

  state.code.AddValue(Encode("NOT.W", CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOT.W", CpuCore::R0));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOT.W", CpuCore::R1));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOT.W", CpuCore::R1));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // NOT.W R0
  EXPECT_EQ(state.r0, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // NOT.W R0
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // NOT.W R1
  EXPECT_EQ(state.r1, 0xAAAA);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // NOT.W R1
  EXPECT_EQ(state.r1, 0x5555);
  EXPECT_EQ(state.st, 0);
}

TEST_F(InstructionTest, NOT_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters(
      {{CpuCore::ST, 0}, {CpuCore::R2, 0x5555}, {CpuCore::R3, 0xAAAA}});

  state.code.AddValue(Encode("NOT.D", 0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOT.D", 0));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOT.D", 1));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOT.D", 1));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // NOT.D D0
  EXPECT_EQ(state.d0(), 0xFFFFFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // NOT.D D0
  EXPECT_EQ(state.d0(), 0);
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // NOT.D D1
  EXPECT_EQ(state.d1(), 0x5555AAAA);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // NOT.D D1
  EXPECT_EQ(state.d1(), 0xAAAA5555);
  EXPECT_EQ(state.st, CpuCore::S);
}

}  // namespace
}  // namespace oz3