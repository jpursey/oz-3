// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_test.h"

namespace oz3 {
namespace {

// All word-size bit operations use the same macro Get16BitMask to retrieve the
// mask associated with a bit index, so we test it once here with SETB.W.
TEST_F(InstructionTest, BitOps16BitMaskByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  uint16_t ips[16] = {};
  for (int i = 0; i < 16; ++i) {
    state.code.AddValue(Encode("MVQ.LW", CpuCore::R0, 0));
    std::string bit_index = absl::StrCat(i);
    state.code.AddValue(Encode("SETB.W", CpuCore::R0, Arg(bit_index)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 16; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 1 << i) << "SETB.W R0, " << i;
  }
}

TEST_F(InstructionTest, BitOps16BitMaskByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  uint16_t ips[17] = {};
  for (int i = 0; i < 17; ++i) {
    state.code.AddValue(Encode("MVQ.LW", CpuCore::R0, 0));
    state.code.AddValue(Encode("MVQ.LW", CpuCore::R1, i));
    state.code.AddValue(Encode("SETB.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 16; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 1 << i) << "SETB.W R0, R1=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SETB.D R0, R1=16";
}

// All dword-size bit operations use the same macro Get32BitMask to retrieve the
// mask associated with a bit index, so we test it once here with SETB.D.
TEST_F(InstructionTest, BitOps32BitMaskByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  uint16_t ips[32] = {};
  for (int i = 0; i < 32; ++i) {
    state.code.AddValue(Encode("MVQ.LD", 0, 0));
    std::string bit_index = absl::StrCat(i);
    state.code.AddValue(Encode("SETB.D", 0, Arg(bit_index)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 32; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 1u << i) << "SETB.D D0, " << i;
  }
}

TEST_F(InstructionTest, BitOps32BitMaskByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  uint16_t ips[33] = {};
  for (int i = 0; i < 33; ++i) {
    state.code.AddValue(Encode("MVQ.LD", 0, 0));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("SETB.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 32; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 1u << i) << "SETB.D D0, R2=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SETB.D D0, R2=32";
}

TEST_F(InstructionTest, CLRB_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::R0, 0xFFFF}});

  state.code.AddValue(Encode("CLRB.W", CpuCore::R0, "8"));
  const uint16_t ip = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip));
  EXPECT_EQ(state.r0, 0xFEFF);
}

TEST_F(InstructionTest, CLRB_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::R0, 0xFFFF}, {CpuCore::R1, 0xFFFF}});

  state.code.AddValue(Encode("CLRB.D", 0, "8"));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRB.D", 0, "24"));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.d0(), 0xFFFFFEFF);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.d0(), 0xFEFFFEFF);
}

TEST_F(InstructionTest, SETB_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  state.code.AddValue(Encode("SETB.W", CpuCore::R0, "8"));
  const uint16_t ip = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip));
  EXPECT_EQ(state.r0, 0x0100);
}

TEST_F(InstructionTest, SETB_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  state.code.AddValue(Encode("SETB.D", 0, "8"));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("SETB.D", 0, "24"));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.d0(), 0x00000100);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.d0(), 0x01000100);
}

TEST_F(InstructionTest, NOTB_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  state.code.AddValue(Encode("NOTB.W", CpuCore::R0, "8"));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOTB.W", CpuCore::R0, "8"));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0100);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r0, 0x0000);
}

TEST_F(InstructionTest, NOTB_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  state.code.AddValue(Encode("NOTB.D", 0, "8"));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOTB.D", 0, "24"));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOTB.D", 0, "8"));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOTB.D", 0, "24"));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.d0(), 0x00000100);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.d0(), 0x01000100);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.d0(), 0x01000000);
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.d0(), 0x00000000);
}

}  // namespace
}  // namespace oz3
