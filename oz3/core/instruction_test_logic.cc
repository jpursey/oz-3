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

TEST_F(InstructionTest, AND_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters(
      {{CpuCore::ST, 0}, {CpuCore::R0, 0x1248}, {CpuCore::R1, 0x9AC8}});

  state.code.AddValue(Encode("AND.W", CpuCore::R0, "$v")).AddValue(0xFFFF);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("AND.W", CpuCore::R0, "$v")).AddValue(0xEDB7);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("AND.W", CpuCore::R1, "$v")).AddValue(0x8777);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("AND.W", CpuCore::R1, "$v")).AddValue(0);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // AND.W R0, 0xFFFF
  EXPECT_EQ(state.r0, 0x1248);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // AND.W R0, 0xEDB7
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // AND.W R1, 0x8777
  EXPECT_EQ(state.r1, 0x8240);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // AND.W R1, 0
  EXPECT_EQ(state.r1, 0);
  EXPECT_EQ(state.st, CpuCore::Z);
}

TEST_F(InstructionTest, AND_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 0xA53C},
                      {CpuCore::R1, 0x1248},
                      {CpuCore::R3, 0x9AC8}});

  state.code.AddValue(Encode("AND.D", 0, "$V")).AddValue32(0xFFFFFFFF);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("AND.D", 0, "$V")).AddValue32(0xEDB78421);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("AND.D", 1, "$V")).AddValue32(0x8777FFFF);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("AND.D", 1, "$V")).AddValue32(0);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // AND.D D0, 0xFFFFFFFF
  EXPECT_EQ(state.d0(), 0x1248A53C);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // AND.D D0, 0xEDB78421
  EXPECT_EQ(state.d0(), 0x8420);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // AND.D D1, 0x8777FFFF
  EXPECT_EQ(state.d1(), 0x82400000);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // AND.D D1, 0
  EXPECT_EQ(state.d1(), 0);
  EXPECT_EQ(state.st, CpuCore::Z);
}

TEST_F(InstructionTest, OR_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  state.code.AddValue(Encode("OR.W", CpuCore::R0, "$v")).AddValue(0);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OR.W", CpuCore::R0, "$v")).AddValue(0x1248);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OR.W", CpuCore::R0, "$v")).AddValue(0x8421);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OR.W", CpuCore::R0, "$v")).AddValue(0x36C9);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // OR.W R0, 0
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // OR.W R0, 0x1248
  EXPECT_EQ(state.r0, 0x1248);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // OR.W R0, 0x8421
  EXPECT_EQ(state.r0, 0x9669);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // OR.W R0, 0x36C9
  EXPECT_EQ(state.r0, 0xB6E9);
  EXPECT_EQ(state.st, CpuCore::S);
}

TEST_F(InstructionTest, OR_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  state.code.AddValue(Encode("OR.D", 0, "$V")).AddValue32(0);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OR.D", 0, "$V")).AddValue32(0x1248A53C);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OR.D", 0, "$V")).AddValue32(0x84200000);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OR.D", 0, "$V")).AddValue32(0x000036C9);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // OR.D D0, 0
  EXPECT_EQ(state.d0(), 0);
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // OR.D D0, 0x1248A53C
  EXPECT_EQ(state.d0(), 0x1248A53C);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // OR.D D0, 0x84200000
  EXPECT_EQ(state.d0(), 0x9668A53C);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // OR.D D0, 0x000036C9
  EXPECT_EQ(state.d0(), 0x9668B7FD);
  EXPECT_EQ(state.st, CpuCore::S);
}

}  // namespace
}  // namespace oz3