// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/instruction_sets/instruction_test.h"

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

TEST_F(InstructionTest, NOT_Cycles) {
  ASSERT_TRUE(InitAndReset());
  RunCycleCases({
      {"NOT.W R0", 4, {Encode("NOT.W", CpuCore::R0)}},
      {"NOT.D D0", 5, {Encode("NOT.D", 0)}},
  });
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

TEST_F(InstructionTest, AND_Cycles) {
  ASSERT_TRUE(InitAndReset());
  RunCycleCases({
      {"AND.W R0, R1", 4, {Encode("AND.W", CpuCore::R0, {"$r", CpuCore::R1})}},
      {"AND.W R0, 5", 5, {Encode("AND.W", CpuCore::R0, "$v"), 5}},
      {"AND.W R0, (R1)",
       6,
       {Encode("AND.W", CpuCore::R0, {"($r)", CpuCore::R1})}},
      {"AND.W R0, (R1 + 1)",
       8,
       {Encode("AND.W", CpuCore::R0, {"($r + $v)", CpuCore::R1}), 1}},
      {"AND.D D0, D1", 5, {Encode("AND.D", 0, {"$R", 1})}},
      {"AND.D D0, 5", 7, {Encode("AND.D", 0, "$V"), 5, 0}},
      {"AND.D D0, [R4]", 8, {Encode("AND.D", 0, {"[$r]", CpuCore::R4})}},
      {"AND.D D0, [R4 + 1]",
       10,
       {Encode("AND.D", 0, {"[$r + $v]", CpuCore::R4}), 1}},
  });
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

TEST_F(InstructionTest, OR_Cycles) {
  ASSERT_TRUE(InitAndReset());
  RunCycleCases({
      {"OR.W R0, R1", 4, {Encode("OR.W", CpuCore::R0, {"$r", CpuCore::R1})}},
      {"OR.W R0, 5", 5, {Encode("OR.W", CpuCore::R0, "$v"), 5}},
      {"OR.W R0, (R1)",
       6,
       {Encode("OR.W", CpuCore::R0, {"($r)", CpuCore::R1})}},
      {"OR.W R0, (R1 + 1)",
       8,
       {Encode("OR.W", CpuCore::R0, {"($r + $v)", CpuCore::R1}), 1}},
      {"OR.D D0, D1", 5, {Encode("OR.D", 0, {"$R", 1})}},
      {"OR.D D0, 5", 7, {Encode("OR.D", 0, "$V"), 5, 0}},
      {"OR.D D0, [R4]", 8, {Encode("OR.D", 0, {"[$r]", CpuCore::R4})}},
      {"OR.D D0, [R4 + 1]",
       10,
       {Encode("OR.D", 0, {"[$r + $v]", CpuCore::R4}), 1}},
  });
}

TEST_F(InstructionTest, XOR_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  state.code.AddValue(Encode("XOR.W", CpuCore::R0, "$v")).AddValue(0);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("XOR.W", CpuCore::R0, "$v")).AddValue(0x1248);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("XOR.W", CpuCore::R0, "$v")).AddValue(0x8421);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("XOR.W", CpuCore::R0, "$v")).AddValue(0x36C9);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("XOR.W", CpuCore::R0, {"$r", CpuCore::R0}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // XOR.W R0, 0
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // XOR.W R0, 0x1248
  EXPECT_EQ(state.r0, 0x1248);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // XOR.W R0, 0x8421
  EXPECT_EQ(state.r0, 0x9669);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // XOR.W R0, 0x36C9
  EXPECT_EQ(state.r0, 0xA0A0);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // XOR.W R0, R0
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, CpuCore::Z);
}

TEST_F(InstructionTest, XOR_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  state.code.AddValue(Encode("XOR.D", 0, "$V")).AddValue32(0);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("XOR.D", 0, "$V")).AddValue32(0x1248A53C);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("XOR.D", 0, "$V")).AddValue32(0x84200000);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("XOR.D", 0, "$V")).AddValue32(0x000036C9);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("XOR.D", 0, {"$R", 0}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));  // XOR.D D0, 0
  EXPECT_EQ(state.d0(), 0);
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip2));  // XOR.D D0, 0x1248A53C
  EXPECT_EQ(state.d0(), 0x1248A53C);
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip3));  // XOR.D D0, 0x84200000
  EXPECT_EQ(state.d0(), 0x9668A53C);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip4));  // XOR.D D0, 0x000036C9
  EXPECT_EQ(state.d0(), 0x966893F5);
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip5));  // XOR.D D0, D0
  EXPECT_EQ(state.d0(), 0);
  EXPECT_EQ(state.st, CpuCore::Z);
}

TEST_F(InstructionTest, XOR_Cycles) {
  ASSERT_TRUE(InitAndReset());
  RunCycleCases({
      {"XOR.W R0, R1", 4, {Encode("XOR.W", CpuCore::R0, {"$r", CpuCore::R1})}},
      {"XOR.W R0, 5", 5, {Encode("XOR.W", CpuCore::R0, "$v"), 5}},
      {"XOR.W R0, (R1)",
       6,
       {Encode("XOR.W", CpuCore::R0, {"($r)", CpuCore::R1})}},
      {"XOR.W R0, (R1 + 1)",
       8,
       {Encode("XOR.W", CpuCore::R0, {"($r + $v)", CpuCore::R1}), 1}},
      {"XOR.D D0, D1", 5, {Encode("XOR.D", 0, {"$R", 1})}},
      {"XOR.D D0, 5", 7, {Encode("XOR.D", 0, "$V"), 5, 0}},
      {"XOR.D D0, [R4]", 8, {Encode("XOR.D", 0, {"[$r]", CpuCore::R4})}},
      {"XOR.D D0, [R4 + 1]",
       10,
       {Encode("XOR.D", 0, {"[$r + $v]", CpuCore::R4}), 1}},
  });
}

}  // namespace
}  // namespace oz3
