// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdint>
#include <iterator>
#include <type_traits>
#include <vector>

#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "oz3/instruction_sets/instruction_test.h"

namespace oz3 {
namespace {

uint16_t RotateLeft16(uint16_t value, int shift) {
  shift %= 16;
  return (value << shift) | (value >> (16 - shift));
}

uint16_t RotateRight16(uint16_t value, int shift) {
  shift %= 16;
  return (value >> shift) | (value << (16 - shift));
}

uint32_t RotateLeft32(uint32_t value, int shift) {
  shift %= 32;
  return (value << shift) | (value >> (32 - shift));
}

uint32_t RotateRight32(uint32_t value, int shift) {
  shift %= 32;
  return (value >> shift) | (value << (32 - shift));
}

uint16_t RotateLeft16Carry(uint16_t value, int shift, bool& carry) {
  shift %= 17;
  uint32_t result = value | (carry ? 0x10000 : 0);
  result = (result << shift) | (result >> (17 - shift));
  carry = (result & 0x10000) != 0;
  return result & 0xFFFF;
}

uint16_t RotateRight16Carry(uint16_t value, int shift, bool& carry) {
  shift %= 17;
  uint32_t result = value | (carry ? 0x10000 : 0);
  result = (result >> shift) | (result << (17 - shift));
  carry = (result & 0x10000) != 0;
  return result & 0xFFFF;
}

uint32_t RotateLeft32Carry(uint32_t value, int shift, bool& carry) {
  shift %= 33;
  uint64_t result = value | (carry ? 0x100000000 : 0);
  result = (result << shift) | (result >> (33 - shift));
  carry = (result & 0x100000000) != 0;
  return result & 0xFFFFFFFF;
}

uint32_t RotateRight32Carry(uint32_t value, int shift, bool& carry) {
  shift %= 33;
  uint64_t result = value | (carry ? 0x100000000 : 0);
  result = (result >> shift) | (result << (33 - shift));
  carry = (result & 0x100000000) != 0;
  return result & 0xFFFFFFFF;
}

// A rotate count, and the cycles the rotate takes.
struct CountCycles {
  int count;
  Cycles cycles;
};

// Immediate counts for a word rotate through carry, from the fewest cycles to
// the most.
constexpr CountCycles kWordCarryValueCounts[] = {{1, 4}, {8, 11}, {16, 19}};
constexpr int kWordCarryValueCountsSize =
    static_cast<int>(std::size(kWordCarryValueCounts));

// Immediate counts for a dword rotate through carry, from the fewest cycles to
// the most.
constexpr CountCycles kDwordCarryValueCounts[] = {{1, 10}, {16, 55}, {32, 103}};
constexpr int kDwordCarryValueCountsSize =
    static_cast<int>(std::size(kDwordCarryValueCounts));

// Register counts for a word rotate through carry. A count of 17 or more is
// reduced modulo 17 for a fixed cost.
constexpr CountCycles kWordCarryRegisterCounts[] = {
    {0, 6},   {1, 8},    {16, 38},     {17, 32},     {18, 35},    {33, 65},
    {34, 32}, {100, 63}, {0x8800, 32}, {0xFFFE, 65}, {0xFFFF, 32}};
constexpr int kWordCarryRegisterCountsSize =
    static_cast<int>(std::size(kWordCarryRegisterCounts));

// Register counts for a dword rotate through carry. A count of 33 or more is
// reduced modulo 33 for a fixed cost.
constexpr CountCycles kDwordCarryRegisterCounts[] = {
    {0, 7},    {1, 9},   {32, 102},  {33, 41},     {34, 44},
    {65, 137}, {66, 41}, {1000, 71}, {0x8400, 41}, {0xFFFF, 131}};
constexpr int kDwordCarryRegisterCountsSize =
    static_cast<int>(std::size(kDwordCarryRegisterCounts));

TEST_F(InstructionTest, ROL_W_ByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16(0x5A3C, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROL.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "ROL.W R0, " << i;
    EXPECT_EQ(state.st, 0) << "ROL.W R0, " << i;
  }
}

TEST_F(InstructionTest, ROL_W_ByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16(0x5A3C, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROL.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 18; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "ROL.W R0, R1=" << i;
    EXPECT_EQ(state.st, 0) << "ROL.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, ROL_W_OneByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16(1, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROL.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 1) << "ROL.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::C) << "ROL.W R0, " << i;
  }
}

TEST_F(InstructionTest, ROL_W_OneByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16(1, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROL.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.r0, 1) << "ROL.W R0, R1=0";
  EXPECT_EQ(state.st, 0) << "ROL.W R0, R1=0";
  for (int i = 1; i < 18; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 1) << "ROL.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::C) << "ROL.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, ROL_W_SignByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16(0x8000, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROL.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "ROL.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::S) << "ROL.W R0, " << i;
  }
}

TEST_F(InstructionTest, ROL_W_SignByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16(0x8000, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROL.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 18; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "ROL.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::S) << "ROL.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, ROL_D_ByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32(0x12485A3C, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROL.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "ROL.D D0, " << i;
    EXPECT_EQ(state.st, 0) << "ROL.D D0, " << i;
  }
}

TEST_F(InstructionTest, ROL_D_ByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32(0x12485A3C, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROL.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 34; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "ROL.D D0, R2=" << i;
    EXPECT_EQ(state.st, 0) << "ROL.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, ROL_D_OneByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32(1, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROL.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 1) << "ROL.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::C) << "ROL.D D0, " << i;
  }
}

TEST_F(InstructionTest, ROL_D_OneByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32(1, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROL.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.d0(), 1) << "ROL.D D0, R2=0";
  EXPECT_EQ(state.st, 0) << "ROL.D D0, R2=0";
  for (int i = 1; i < 34; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 1) << "ROL.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::C) << "ROL.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, ROL_D_SignByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32(0x80000000, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROL.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "ROL.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::S) << "ROL.D D0, " << i;
  }
}

TEST_F(InstructionTest, ROL_D_SignByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32(0x80000000, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROL.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 34; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "ROL.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::S) << "ROL.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, ROR_W_ByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16(0x5A3C, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROR.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "ROL.W R0, " << i;
    EXPECT_EQ(state.st, 0) << "ROL.W R0, " << i;
  }
}

TEST_F(InstructionTest, ROR_W_ByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16(0x5A3C, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROR.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 18; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "ROL.W R0, R1=" << i;
    EXPECT_EQ(state.st, 0) << "ROL.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, ROR_W_OneByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16(1, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROR.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 1) << "ROR.W R0, " << i;
    EXPECT_EQ(state.st, 0) << "ROR.W R0, " << i;
  }
}

TEST_F(InstructionTest, ROR_W_OneByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16(1, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROR.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 18; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 1) << "ROR.W R0, R1=" << i;
    EXPECT_EQ(state.st, 0) << "ROR.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, ROR_W_SignByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16(0x8000, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROR.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "ROR.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "ROR.W R0, " << i;
  }
}

TEST_F(InstructionTest, ROR_W_SignByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16(0x8000, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROR.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.r0, 0x8000) << "ROR.W R0, R1=0";
  EXPECT_EQ(state.st, CpuCore::S) << "ROR.W R0, R1=0";
  for (int i = 1; i < 18; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "ROR.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "ROR.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, ROR_D_ByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32(0x12485A3C, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROR.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "ROR.D D0, " << i;
    EXPECT_EQ(state.st, 0) << "ROR.D D0, " << i;
  }
}

TEST_F(InstructionTest, ROR_D_ByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32(0x12485A3C, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROR.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 34; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "ROR.D D0, R2=" << i;
    EXPECT_EQ(state.st, 0) << "ROR.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, ROR_D_OneByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32(1, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROR.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 1) << "ROR.D D0, " << i;
    EXPECT_EQ(state.st, 0) << "ROR.D D0, " << i;
  }
}

TEST_F(InstructionTest, ROR_D_OneByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32(1, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROR.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 34; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 1) << "ROR.D D0, R2=" << i;
    EXPECT_EQ(state.st, 0) << "ROR.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, ROR_D_SignByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32(0x80000000, i));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("ROR.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "ROR.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "ROR.D D0, " << i;
  }
}

TEST_F(InstructionTest, ROR_D_SignByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32(0x80000000, i));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("ROR.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.d0(), 0x80000000) << "ROR.D D0, R2=0";
  EXPECT_EQ(state.st, CpuCore::S) << "ROR.D D0, R2=0";
  for (int i = 1; i < 34; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "ROR.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "ROR.D D0, R2=" << i;
  }
}

// Register counts for a word rotate, and their cycles. Each bit up to 15 takes
// 2 cycles, and a multiple of 16 bits (including zero) a fixed 9.
constexpr CountCycles kWordRegisterCounts[] = {{0, 9},  {1, 7},  {15, 35},
                                               {16, 9}, {17, 7}, {0xFFFF, 35}};

// Immediate counts for a word rotate, and their cycles.
constexpr CountCycles kWordValueCounts[] = {{1, 4}, {15, 18}, {16, 5}};

// Counts for a dword rotate, and their cycles. From 16 bits the words are
// swapped first, and the rest take 4 cycles a bit. ROR.D takes 2 cycles more
// than ROL.D whenever it rotates a bit at a time.
constexpr CountCycles kRolDwordRegisterCounts[] = {
    {0, 10},  {1, 11},  {15, 67}, {16, 14},
    {17, 14}, {31, 70}, {32, 10}, {0xFFFF, 70}};
constexpr CountCycles kRolDwordValueCounts[] = {{1, 12},  {15, 68}, {16, 15},
                                                {17, 15}, {31, 71}, {32, 11}};
constexpr CountCycles kRorDwordRegisterCounts[] = {
    {0, 10},  {1, 13},  {15, 69}, {16, 14},
    {17, 16}, {31, 72}, {32, 10}, {0xFFFF, 72}};
constexpr CountCycles kRorDwordValueCounts[] = {{1, 14},  {15, 70}, {16, 15},
                                                {17, 17}, {31, 73}, {32, 11}};

// Returns a case for each of `counts`, which rotates a value to `result`, and
// `unrotate` returns that value from `result` and the count. The flags are all
// clear, as `result` has neither its low nor its high bit set.
template <typename T>
std::vector<CountCase> RotateCases(absl::Span<const CountCycles> counts,
                                   std::type_identity_t<T> result,
                                   T (*unrotate)(T, int)) {
  std::vector<CountCase> cases;
  for (const CountCycles& c : counts) {
    cases.push_back({unrotate(result, c.count), static_cast<uint16_t>(c.count),
                     result, 0, c.cycles});
  }
  return cases;
}

TEST_F(InstructionTest, ROL_W_ByRegisterCycles) {
  RunCountCases("ROL.W", CountArg::kRegister,
                RotateCases(kWordRegisterCounts, 0x5A3C, RotateRight16));
}

TEST_F(InstructionTest, ROL_W_ByValueCycles) {
  RunCountCases("ROL.W", CountArg::kImmediate,
                RotateCases(kWordValueCounts, 0x5A3C, RotateRight16));
}

TEST_F(InstructionTest, ROR_W_ByRegisterCycles) {
  RunCountCases("ROR.W", CountArg::kRegister,
                RotateCases(kWordRegisterCounts, 0x5A3C, RotateLeft16));
}

TEST_F(InstructionTest, ROR_W_ByValueCycles) {
  RunCountCases("ROR.W", CountArg::kImmediate,
                RotateCases(kWordValueCounts, 0x5A3C, RotateLeft16));
}

TEST_F(InstructionTest, ROL_D_ByRegisterCycles) {
  RunCountCases(
      "ROL.D", CountArg::kRegister,
      RotateCases(kRolDwordRegisterCounts, 0x12485A3C, RotateRight32));
}

TEST_F(InstructionTest, ROL_D_ByValueCycles) {
  RunCountCases("ROL.D", CountArg::kImmediate,
                RotateCases(kRolDwordValueCounts, 0x12485A3C, RotateRight32));
}

TEST_F(InstructionTest, ROR_D_ByRegisterCycles) {
  RunCountCases("ROR.D", CountArg::kRegister,
                RotateCases(kRorDwordRegisterCounts, 0x12485A3C, RotateLeft32));
}

TEST_F(InstructionTest, ROR_D_ByValueCycles) {
  RunCountCases("ROR.D", CountArg::kImmediate,
                RotateCases(kRorDwordValueCounts, 0x12485A3C, RotateLeft32));
}

// Rotating zero gives zero, with only Z set, on every path.
TEST_F(InstructionTest, ROL_W_ZeroByRegister) {
  RunCountCases("ROL.W", CountArg::kRegister,
                {{0, 0, 0, CpuCore::Z, 9},
                 {0, 1, 0, CpuCore::Z, 7},
                 {0, 16, 0, CpuCore::Z, 9}});
}

TEST_F(InstructionTest, ROR_W_ZeroByRegister) {
  RunCountCases("ROR.W", CountArg::kRegister,
                {{0, 0, 0, CpuCore::Z, 9},
                 {0, 1, 0, CpuCore::Z, 7},
                 {0, 16, 0, CpuCore::Z, 9}});
}

TEST_F(InstructionTest, ROL_D_ZeroByRegister) {
  RunCountCases("ROL.D", CountArg::kRegister,
                {{0, 0, 0, CpuCore::Z, 10},
                 {0, 1, 0, CpuCore::Z, 11},
                 {0, 16, 0, CpuCore::Z, 14},
                 {0, 17, 0, CpuCore::Z, 14},
                 {0, 32, 0, CpuCore::Z, 10}});
}

TEST_F(InstructionTest, ROR_D_ZeroByRegister) {
  RunCountCases("ROR.D", CountArg::kRegister,
                {{0, 0, 0, CpuCore::Z, 10},
                 {0, 1, 0, CpuCore::Z, 13},
                 {0, 16, 0, CpuCore::Z, 14},
                 {0, 17, 0, CpuCore::Z, 16},
                 {0, 32, 0, CpuCore::Z, 10}});
}

TEST_F(InstructionTest, RLC_W_ByValueNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16Carry(0x5A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RLC.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "RLC.W R0, " << i;
    EXPECT_EQ(state.st, 0) << "RLC.W R0, " << i;
  }
}

TEST_F(InstructionTest, RLC_W_ByRegisterNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[19] = {};
  for (int i = 0; i < 19; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16Carry(0x5A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("RLC.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 19; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "RLC.W R0, R1=" << i;
    EXPECT_EQ(state.st, 0) << "RLC.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, RLC_W_ByValueWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16Carry(0x5A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RLC.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "RLC.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::C) << "RLC.W R0, " << i;
  }
}

TEST_F(InstructionTest, RLC_W_ByRegisterWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[19] = {};
  for (int i = 0; i < 19; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16Carry(0x5A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("RLC.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 19; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "RLC.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::C) << "RLC.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, RLC_W_SignByValueNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16Carry(0x8000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RLC.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "RLC.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::S) << "RLC.W R0, " << i;
  }
}

TEST_F(InstructionTest, RLC_W_SignByRegisterNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[19] = {};
  for (int i = 0; i < 19; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16Carry(0x8000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("RLC.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 19; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "RLC.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::S) << "RLC.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, RLC_W_SignByValueWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16Carry(0x8000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RLC.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "RLC.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "RLC.W R0, " << i;
  }
}

TEST_F(InstructionTest, RLC_W_SignByRegisterWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[19] = {};
  for (int i = 0; i < 19; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16Carry(0x8000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("RLC.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 19; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "RLC.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "RLC.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, RLC_W_ByRegisterCycles) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t start_ips[kWordCarryRegisterCountsSize] = {};
  uint16_t end_ips[kWordCarryRegisterCountsSize] = {};
  for (int i = 0; i < kWordCarryRegisterCountsSize; ++i) {
    const int count = kWordCarryRegisterCounts[i].count;
    bool carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16Carry(0x5A3C, count, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(count);
    start_ips[i] = state.code.AddNopGetAddress();
    state.code.AddValue(Encode("RLC.W", CpuCore::R0, {"$r", CpuCore::R1}));
    end_ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < kWordCarryRegisterCountsSize; ++i) {
    const int count = kWordCarryRegisterCounts[i].count;
    ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
    EXPECT_EQ(CyclesUntilIp(end_ips[i]), kWordCarryRegisterCounts[i].cycles)
        << "RLC.W R0, R1=" << count;
    EXPECT_EQ(state.r0, 0x5A3C) << "RLC.W R0, R1=" << count;
    EXPECT_EQ(state.st, CpuCore::C) << "RLC.W R0, R1=" << count;
  }
}

TEST_F(InstructionTest, RLC_W_ByValueCycles) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t start_ips[kWordCarryValueCountsSize] = {};
  uint16_t end_ips[kWordCarryValueCountsSize] = {};
  for (int i = 0; i < kWordCarryValueCountsSize; ++i) {
    const int count = kWordCarryValueCounts[i].count;
    bool carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateRight16Carry(0x5A3C, count, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    start_ips[i] = state.code.AddNopGetAddress();
    state.code.AddValue(Encode("RLC.W", CpuCore::R0, Arg(absl::StrCat(count))));
    end_ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < kWordCarryValueCountsSize; ++i) {
    const int count = kWordCarryValueCounts[i].count;
    ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
    EXPECT_EQ(CyclesUntilIp(end_ips[i]), kWordCarryValueCounts[i].cycles)
        << "RLC.W R0, " << count;
    EXPECT_EQ(state.r0, 0x5A3C) << "RLC.W R0, " << count;
    EXPECT_EQ(state.st, CpuCore::C) << "RLC.W R0, " << count;
  }
}

TEST_F(InstructionTest, RLC_D_ByValueNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32Carry(0x12485A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RLC.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RLC.D D0, " << i;
    EXPECT_EQ(state.st, 0) << "RLC.D D0, " << i;
  }
}

TEST_F(InstructionTest, RLC_D_ByRegisterNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[35] = {};
  for (int i = 0; i < 35; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32Carry(0x12485A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("RLC.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 35; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RLC.D D0, R2=" << i;
    EXPECT_EQ(state.st, 0) << "RLC.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, RLC_D_ByValueWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});
  bool carry = false;

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32Carry(0x12485A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RLC.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RLC.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::C) << "RLC.D D0, " << i;
  }
}

TEST_F(InstructionTest, RLC_D_ByRegisterWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[35] = {};
  for (int i = 0; i < 35; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32Carry(0x12485A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("RLC.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 35; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RLC.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::C) << "RLC.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, RLC_D_SignByValueNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32Carry(0x80000000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RLC.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "RLC.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::S) << "RLC.D D0, " << i;
  }
}

TEST_F(InstructionTest, RLC_D_SignByRegisterNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[35] = {};
  for (int i = 0; i < 35; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32Carry(0x80000000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("RLC.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 35; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "RLC.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::S) << "RLC.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, RLC_D_SignByValueWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32Carry(0x80000000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RLC.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "RLC.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "RLC.D D0, " << i;
  }
}

TEST_F(InstructionTest, RLC_D_SignByRegisterWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[35] = {};
  for (int i = 0; i < 35; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32Carry(0x80000000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("RLC.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 35; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "RLC.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "RLC.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, RLC_D_ByRegisterCycles) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t start_ips[kDwordCarryRegisterCountsSize] = {};
  uint16_t end_ips[kDwordCarryRegisterCountsSize] = {};
  for (int i = 0; i < kDwordCarryRegisterCountsSize; ++i) {
    const int count = kDwordCarryRegisterCounts[i].count;
    bool carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32Carry(0x12485A3C, count, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(count);
    start_ips[i] = state.code.AddNopGetAddress();
    state.code.AddValue(Encode("RLC.D", 0, {"$r", CpuCore::R2}));
    end_ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < kDwordCarryRegisterCountsSize; ++i) {
    const int count = kDwordCarryRegisterCounts[i].count;
    ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
    EXPECT_EQ(CyclesUntilIp(end_ips[i]), kDwordCarryRegisterCounts[i].cycles)
        << "RLC.D D0, R2=" << count;
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RLC.D D0, R2=" << count;
    EXPECT_EQ(state.st, CpuCore::C) << "RLC.D D0, R2=" << count;
  }
}

TEST_F(InstructionTest, RLC_D_ByValueCycles) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t start_ips[kDwordCarryValueCountsSize] = {};
  uint16_t end_ips[kDwordCarryValueCountsSize] = {};
  for (int i = 0; i < kDwordCarryValueCountsSize; ++i) {
    const int count = kDwordCarryValueCounts[i].count;
    bool carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateRight32Carry(0x12485A3C, count, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    start_ips[i] = state.code.AddNopGetAddress();
    state.code.AddValue(Encode("RLC.D", 0, Arg(absl::StrCat(count))));
    end_ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < kDwordCarryValueCountsSize; ++i) {
    const int count = kDwordCarryValueCounts[i].count;
    ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
    EXPECT_EQ(CyclesUntilIp(end_ips[i]), kDwordCarryValueCounts[i].cycles)
        << "RLC.D D0, " << count;
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RLC.D D0, " << count;
    EXPECT_EQ(state.st, CpuCore::C) << "RLC.D D0, " << count;
  }
}

TEST_F(InstructionTest, RRC_W_ByValueNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16Carry(0x5A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RRC.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "RRC.W R0, " << i;
    EXPECT_EQ(state.st, 0) << "RRC.W R0, " << i;
  }
}

TEST_F(InstructionTest, RRC_W_ByRegisterNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[19] = {};
  for (int i = 0; i < 19; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16Carry(0x5A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("RRC.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 19; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "RRC.W R0, R1=" << i;
    EXPECT_EQ(state.st, 0) << "RRC.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, RRC_W_ByValueWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16Carry(0x5A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RRC.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "RRC.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::C) << "RRC.W R0, " << i;
  }
}

TEST_F(InstructionTest, RRC_W_ByRegisterWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[19] = {};
  for (int i = 0; i < 19; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16Carry(0x5A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("RRC.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 19; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x5A3C) << "RRC.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::C) << "RRC.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, RRC_W_SignByValueNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16Carry(0x8000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RRC.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "RRC.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::S) << "RRC.W R0, " << i;
  }
}

TEST_F(InstructionTest, RRC_W_SignByRegisterNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[19] = {};
  for (int i = 0; i < 19; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16Carry(0x8000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("RRC.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 19; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "RRC.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::S) << "RRC.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, RRC_W_SignByValueWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16Carry(0x8000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RRC.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 17; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "RRC.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "RRC.W R0, " << i;
  }
}

TEST_F(InstructionTest, RRC_W_SignByRegisterWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[19] = {};
  for (int i = 0; i < 19; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16Carry(0x8000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    state.code.AddValue(Encode("RRC.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 19; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000) << "RRC.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "RRC.W R0, R1=" << i;
  }
}

TEST_F(InstructionTest, RRC_W_ByRegisterCycles) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t start_ips[kWordCarryRegisterCountsSize] = {};
  uint16_t end_ips[kWordCarryRegisterCountsSize] = {};
  for (int i = 0; i < kWordCarryRegisterCountsSize; ++i) {
    const int count = kWordCarryRegisterCounts[i].count;
    bool carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16Carry(0x5A3C, count, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(count);
    start_ips[i] = state.code.AddNopGetAddress();
    state.code.AddValue(Encode("RRC.W", CpuCore::R0, {"$r", CpuCore::R1}));
    end_ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < kWordCarryRegisterCountsSize; ++i) {
    const int count = kWordCarryRegisterCounts[i].count;
    ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
    EXPECT_EQ(CyclesUntilIp(end_ips[i]), kWordCarryRegisterCounts[i].cycles)
        << "RRC.W R0, R1=" << count;
    EXPECT_EQ(state.r0, 0x5A3C) << "RRC.W R0, R1=" << count;
    EXPECT_EQ(state.st, CpuCore::C) << "RRC.W R0, R1=" << count;
  }
}

TEST_F(InstructionTest, RRC_W_ByValueCycles) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t start_ips[kWordCarryValueCountsSize] = {};
  uint16_t end_ips[kWordCarryValueCountsSize] = {};
  for (int i = 0; i < kWordCarryValueCountsSize; ++i) {
    const int count = kWordCarryValueCounts[i].count;
    bool carry = true;
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(RotateLeft16Carry(0x5A3C, count, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    start_ips[i] = state.code.AddNopGetAddress();
    state.code.AddValue(Encode("RRC.W", CpuCore::R0, Arg(absl::StrCat(count))));
    end_ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < kWordCarryValueCountsSize; ++i) {
    const int count = kWordCarryValueCounts[i].count;
    ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
    EXPECT_EQ(CyclesUntilIp(end_ips[i]), kWordCarryValueCounts[i].cycles)
        << "RRC.W R0, " << count;
    EXPECT_EQ(state.r0, 0x5A3C) << "RRC.W R0, " << count;
    EXPECT_EQ(state.st, CpuCore::C) << "RRC.W R0, " << count;
  }
}

TEST_F(InstructionTest, RRC_D_ByValueNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32Carry(0x12485A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RRC.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RRC.D D0, " << i;
    EXPECT_EQ(state.st, 0) << "RRC.D D0, " << i;
  }
}

TEST_F(InstructionTest, RRC_D_ByRegisterNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[35] = {};
  for (int i = 0; i < 35; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32Carry(0x12485A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("RRC.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 35; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RRC.D D0, R2=" << i;
    EXPECT_EQ(state.st, 0) << "RRC.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, RRC_D_ByValueWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});
  bool carry = false;

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32Carry(0x12485A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RRC.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RRC.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::C) << "RRC.D D0, " << i;
  }
}

TEST_F(InstructionTest, RRC_D_ByRegisterWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[35] = {};
  for (int i = 0; i < 35; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32Carry(0x12485A3C, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("RRC.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 35; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RRC.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::C) << "RRC.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, RRC_D_SignByValueNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32Carry(0x80000000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RRC.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "RRC.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::S) << "RRC.D D0, " << i;
  }
}

TEST_F(InstructionTest, RRC_D_SignByRegisterNoCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  bool carry = false;
  uint16_t ips[35] = {};
  for (int i = 0; i < 35; ++i) {
    carry = false;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32Carry(0x80000000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("RRC.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 35; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "RRC.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::S) << "RRC.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, RRC_D_SignByValueWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32Carry(0x80000000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("RRC.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 33; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "RRC.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "RRC.D D0, " << i;
  }
}

TEST_F(InstructionTest, RRC_D_SignByRegisterWithCarry) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, CpuCore::C}});

  bool carry = false;
  uint16_t ips[35] = {};
  for (int i = 0; i < 35; ++i) {
    carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32Carry(0x80000000, i, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    state.code.AddValue(Encode("RRC.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 35; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000) << "RRC.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "RRC.D D0, R2=" << i;
  }
}

TEST_F(InstructionTest, RRC_D_ByRegisterCycles) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t start_ips[kDwordCarryRegisterCountsSize] = {};
  uint16_t end_ips[kDwordCarryRegisterCountsSize] = {};
  for (int i = 0; i < kDwordCarryRegisterCountsSize; ++i) {
    const int count = kDwordCarryRegisterCounts[i].count;
    bool carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32Carry(0x12485A3C, count, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(count);
    start_ips[i] = state.code.AddNopGetAddress();
    state.code.AddValue(Encode("RRC.D", 0, {"$r", CpuCore::R2}));
    end_ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < kDwordCarryRegisterCountsSize; ++i) {
    const int count = kDwordCarryRegisterCounts[i].count;
    ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
    EXPECT_EQ(CyclesUntilIp(end_ips[i]), kDwordCarryRegisterCounts[i].cycles)
        << "RRC.D D0, R2=" << count;
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RRC.D D0, R2=" << count;
    EXPECT_EQ(state.st, CpuCore::C) << "RRC.D D0, R2=" << count;
  }
}

TEST_F(InstructionTest, RRC_D_ByValueCycles) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t start_ips[kDwordCarryValueCountsSize] = {};
  uint16_t end_ips[kDwordCarryValueCountsSize] = {};
  for (int i = 0; i < kDwordCarryValueCountsSize; ++i) {
    const int count = kDwordCarryValueCounts[i].count;
    bool carry = true;
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(RotateLeft32Carry(0x12485A3C, count, carry));
    state.code.AddValue(Encode((carry ? "SETF" : "CLRF"), CpuCore::C));
    start_ips[i] = state.code.AddNopGetAddress();
    state.code.AddValue(Encode("RRC.D", 0, Arg(absl::StrCat(count))));
    end_ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < kDwordCarryValueCountsSize; ++i) {
    const int count = kDwordCarryValueCounts[i].count;
    ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
    EXPECT_EQ(CyclesUntilIp(end_ips[i]), kDwordCarryValueCounts[i].cycles)
        << "RRC.D D0, " << count;
    EXPECT_EQ(state.d0(), 0x12485A3C) << "RRC.D D0, " << count;
    EXPECT_EQ(state.st, CpuCore::C) << "RRC.D D0, " << count;
  }
}

}  // namespace
}  // namespace oz3