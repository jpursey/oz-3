// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_test.h"

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

}  // namespace
}  // namespace oz3