// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdint>

#include "oz3/instruction_sets/instruction_test.h"

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

// Register bit positions past the width of a word or dword, including positions
// with the high bit set. Every one gets an empty mask in the same cycles.
constexpr uint16_t kWordLargePositions[] = {16, 0x7FFF, 0x8000, 0x8010, 0xFFFF};
constexpr uint16_t kDwordLargePositions[] = {32, 0x7FFF, 0x8000, 0x8020,
                                             0xFFFF};

TEST_F(InstructionTest, BitOps16BitMaskByLargeRegister) {
  RunCountCases("SETB.W", CountArg::kRegister,
                SameCountCases(kWordLargePositions, 0x1234, 0x1234, 0, 6));
}

TEST_F(InstructionTest, BitOps32BitMaskByLargeRegister) {
  RunCountCases(
      "SETB.D", CountArg::kRegister,
      SameCountCases(kDwordLargePositions, 0x12345678, 0x12345678, 0, 9));
}

// A mask from a register takes 2 cycles a bit. For a dword, the mask for the
// high word starts at bit 16.
TEST_F(InstructionTest, BitOps16BitMaskByRegisterCycles) {
  RunCountCases(
      "SETB.W", CountArg::kRegister,
      {{0, 0, 0x0001, 0, 8}, {0, 1, 0x0002, 0, 9}, {0, 15, 0x8000, 0, 37}});
}

TEST_F(InstructionTest, BitOps32BitMaskByRegisterCycles) {
  RunCountCases("SETB.D", CountArg::kRegister,
                {{0, 0, 0x00000001, 0, 9},
                 {0, 1, 0x00000002, 0, 10},
                 {0, 15, 0x00008000, 0, 38},
                 {0, 16, 0x00010000, 0, 8},
                 {0, 17, 0x00020000, 0, 12},
                 {0, 31, 0x80000000, 0, 40}});
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

// The bit operation cycle tests put the positions with the cheapest and
// costliest masks (see BitOps16BitMaskByRegisterCycles and
// BitOps32BitMaskByRegisterCycles) in R1 and R2 for a word, and R3 and R4 for
// a dword, which use D3 so as not to overlap them.
TEST_F(InstructionTest, CLRB_Cycles) {
  ASSERT_TRUE(InitAndReset());
  GetState().SetRegisters({{CpuCore::R1, 16},
                           {CpuCore::R2, 15},
                           {CpuCore::R3, 16},
                           {CpuCore::R4, 31}});
  RunCycleCases({
      {"CLRB.W R0, R1 (16)",
       7,
       {Encode("CLRB.W", CpuCore::R0, {"$r", CpuCore::R1})}},
      {"CLRB.W R0, R2 (15)",
       38,
       {Encode("CLRB.W", CpuCore::R0, {"$r", CpuCore::R2})}},
      {"CLRB.W R0, 15", 6, {Encode("CLRB.W", CpuCore::R0, "15")}},
      {"CLRB.D D3, R3 (16)", 10, {Encode("CLRB.D", 3, {"$r", CpuCore::R3})}},
      {"CLRB.D D3, R4 (31)", 42, {Encode("CLRB.D", 3, {"$r", CpuCore::R4})}},
      {"CLRB.D D3, 31", 8, {Encode("CLRB.D", 3, "31")}},
  });
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

TEST_F(InstructionTest, SETB_Cycles) {
  ASSERT_TRUE(InitAndReset());
  GetState().SetRegisters({{CpuCore::R1, 16},
                           {CpuCore::R2, 15},
                           {CpuCore::R3, 16},
                           {CpuCore::R4, 31}});
  RunCycleCases({
      {"SETB.W R0, R1 (16)",
       6,
       {Encode("SETB.W", CpuCore::R0, {"$r", CpuCore::R1})}},
      {"SETB.W R0, R2 (15)",
       37,
       {Encode("SETB.W", CpuCore::R0, {"$r", CpuCore::R2})}},
      {"SETB.W R0, 15", 5, {Encode("SETB.W", CpuCore::R0, "15")}},
      {"SETB.D D3, R3 (16)", 8, {Encode("SETB.D", 3, {"$r", CpuCore::R3})}},
      {"SETB.D D3, R4 (31)", 40, {Encode("SETB.D", 3, {"$r", CpuCore::R4})}},
      {"SETB.D D3, 31", 6, {Encode("SETB.D", 3, "31")}},
  });
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

TEST_F(InstructionTest, NOTB_Cycles) {
  ASSERT_TRUE(InitAndReset());
  GetState().SetRegisters({{CpuCore::R1, 16},
                           {CpuCore::R2, 15},
                           {CpuCore::R3, 16},
                           {CpuCore::R4, 31}});
  RunCycleCases({
      {"NOTB.W R0, R1 (16)",
       6,
       {Encode("NOTB.W", CpuCore::R0, {"$r", CpuCore::R1})}},
      {"NOTB.W R0, R2 (15)",
       37,
       {Encode("NOTB.W", CpuCore::R0, {"$r", CpuCore::R2})}},
      {"NOTB.W R0, 15", 5, {Encode("NOTB.W", CpuCore::R0, "15")}},
      {"NOTB.D D3, R3 (16)", 8, {Encode("NOTB.D", 3, {"$r", CpuCore::R3})}},
      {"NOTB.D D3, R4 (31)", 40, {Encode("NOTB.D", 3, {"$r", CpuCore::R4})}},
      {"NOTB.D D3, 31", 6, {Encode("NOTB.D", 3, "31")}},
  });
}

TEST_F(InstructionTest, TSTB_W) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}, {CpuCore::R0, 0x0100}});

  state.code.AddValue(Encode("TSTB.W", CpuCore::R0, "8"));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("TSTB.W", CpuCore::R0, "7"));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::Z);
}

TEST_F(InstructionTest, TSTB_D) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters(
      {{CpuCore::ST, 0}, {CpuCore::R0, 0x0100}, {CpuCore::R1, 0x0010}});

  state.code.AddValue(Encode("TSTB.D", 0, "8"));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("TSTB.D", 0, "7"));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("TSTB.D", 0, "20"));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("TSTB.D", 0, "19"));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.st, CpuCore::Z);
}

TEST_F(InstructionTest, TSTB_Cycles) {
  ASSERT_TRUE(InitAndReset());
  GetState().SetRegisters({{CpuCore::R1, 16},
                           {CpuCore::R2, 15},
                           {CpuCore::R3, 16},
                           {CpuCore::R4, 31}});
  RunCycleCases({
      {"TSTB.W R0, R1 (16)",
       6,
       {Encode("TSTB.W", CpuCore::R0, {"$r", CpuCore::R1})}},
      {"TSTB.W R0, R2 (15)",
       37,
       {Encode("TSTB.W", CpuCore::R0, {"$r", CpuCore::R2})}},
      {"TSTB.W R0, 15", 5, {Encode("TSTB.W", CpuCore::R0, "15")}},
      {"TSTB.D D3, R3 (16)", 8, {Encode("TSTB.D", 3, {"$r", CpuCore::R3})}},
      {"TSTB.D D3, R4 (31)", 40, {Encode("TSTB.D", 3, {"$r", CpuCore::R4})}},
      {"TSTB.D D3, 31", 6, {Encode("TSTB.D", 3, "31")}},
  });
}

TEST_F(InstructionTest, CLRF) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::ZSCOI}});

  state.code.AddValue(Encode("CLRF", CpuCore::Z));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S | CpuCore::O));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::C));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, CpuCore::ZSCOI - CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::C | CpuCore::I);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, CpuCore::I);
}

TEST_F(InstructionTest, SETF) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::I}});

  state.code.AddValue(Encode("SETF", CpuCore::Z));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("SETF", CpuCore::S | CpuCore::O));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("SETF", CpuCore::C));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::I);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::ZSCOI - CpuCore::C);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, CpuCore::ZSCOI);
}

TEST_F(InstructionTest, NOTF) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::ZSCOI}});

  state.code.AddValue(Encode("NOTF", CpuCore::Z));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOTF", CpuCore::S | CpuCore::O));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("NOTF", CpuCore::Z | CpuCore::C));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, CpuCore::ZSCOI - CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::C | CpuCore::I);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::I);
}

TEST_F(InstructionTest, FlagOps_Cycles) {
  ASSERT_TRUE(InitAndReset());
  const uint16_t flags = CpuCore::Z | CpuCore::S | CpuCore::C | CpuCore::O;
  RunCycleCases({
      {"CLRF ZSCO", 3, {Encode("CLRF", flags)}},
      {"SETF ZSCO", 3, {Encode("SETF", flags)}},
      {"NOTF ZSCO", 3, {Encode("NOTF", flags)}},
  });
}

}  // namespace
}  // namespace oz3
