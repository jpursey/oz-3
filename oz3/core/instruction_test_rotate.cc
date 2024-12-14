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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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
    const std::string shift_value = absl::StrCat(i);
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

}  // namespace
}  // namespace oz3