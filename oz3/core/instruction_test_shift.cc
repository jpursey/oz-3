// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "absl/strings/str_cat.h"
#include "oz3/core/instruction_test.h"

namespace oz3 {
namespace {

TEST_F(InstructionTest, SHL_W_SignByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue((0x8000 >> i) | 1);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHL.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 15; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000 | (1 << i)) << "SHL.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::S) << "SHL.W R0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[15]));
  EXPECT_EQ(state.r0, 0x8000) << "SHL.W R0, 15";
  EXPECT_EQ(state.st, CpuCore::S) << "SHL.W R0, 15";
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SHL.W R0, 16";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHL.W R0, 16";
}

TEST_F(InstructionTest, SHL_W_SignByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue((0x8000 >> i) | 1);
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHL.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 15; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000 | (1 << i)) << "SHL.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::S) << "SHL.W R0, R1=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[15]));
  EXPECT_EQ(state.r0, 0x8000) << "SHL.W R0, R1=15";
  EXPECT_EQ(state.st, CpuCore::S) << "SHL.W R0, R1=15";
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SHL.W R0, R1=16";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHL.W R0, R1=16";
  ASSERT_TRUE(ExecuteUntilIp(ips[17]));
  EXPECT_EQ(state.r0, 0) << "SHL.W R0, R1=17";
  EXPECT_EQ(state.st, CpuCore::Z) << "SHL.W R0, R1=17";
}

TEST_F(InstructionTest, SHL_W_CarryByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue((0x8000 >> (i - 1)) | 1);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHL.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 15; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 1 << i) << "SHL.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::C) << "SHL.W R0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[15]));
  EXPECT_EQ(state.r0, 0x8000) << "SHL.W R0, 15";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SHL.W R0, 15";
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SHL.W R0, 16";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHL.W R0, 16";
}

TEST_F(InstructionTest, SHL_W_CarryByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue((0x8000 >> std::max(0, i - 1)) | 1);
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHL.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.r0, 0x8001) << "SHL.W R0, R1=0";
  EXPECT_EQ(state.st, CpuCore::S) << "SHL.W R0, R1=0";
  for (int i = 1; i < 15; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 1 << i) << "SHL.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::C) << "SHL.W R0, R1=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[15]));
  EXPECT_EQ(state.r0, 0x8000) << "SHL.W R0, R1=15";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SHL.W R0, R1=15";
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SHL.W R0, R1=16";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHL.W R0, R1=16";
  ASSERT_TRUE(ExecuteUntilIp(ips[17]));
  EXPECT_EQ(state.r0, 0) << "SHL.W R0, R1=17";
  EXPECT_EQ(state.st, CpuCore::Z) << "SHL.W R0, R1=17";
}

TEST_F(InstructionTest, SHL_D_SignByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32((0x80000000 >> i) | 1);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHL.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));
  for (int i = 1; i < 31; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000 | (1 << i)) << "SHL.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::S) << "SHL.D D0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[31]));
  EXPECT_EQ(state.d0(), 0x80000000) << "SHL.D D0, 31";
  EXPECT_EQ(state.st, CpuCore::S) << "SHL.D D0, 31";
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SHL.D D0, 32";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHL.D D0, 32";
}

TEST_F(InstructionTest, SHL_D_SignByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32((0x80000000 >> i) | 1);
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHL.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 31; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000 | (1 << i)) << "SHL.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::S) << "SHL.D D0, R2=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[31]));
  EXPECT_EQ(state.d0(), 0x80000000) << "SHL.D D0, R2=31";
  EXPECT_EQ(state.st, CpuCore::S) << "SHL.D D0, R2=31";
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SHL.D D0, R2=32";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHL.D D0, R2=32";
  ASSERT_TRUE(ExecuteUntilIp(ips[33]));
  EXPECT_EQ(state.d0(), 0) << "SHL.D D0, R2=33";
  EXPECT_EQ(state.st, CpuCore::Z) << "SHL.D D0, R2=33";
}

TEST_F(InstructionTest, SHL_D_CarryByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32((0x80000000 >> (i - 1)) | 1);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHL.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 31; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 1 << i) << "SHL.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::C) << "SHL.D D0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[31]));
  EXPECT_EQ(state.d0(), 0x80000000) << "SHL.D D0, 31";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SHL.D D0, 31";
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SHL.D D0, 32";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHL.D D0, 32";
}

TEST_F(InstructionTest, SHL_D_CarryByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32((0x80000000 >> std::max(0, i - 1)) | 1);
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHL.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.d0(), 0x80000001) << "SHL.D D0, R2=0";
  EXPECT_EQ(state.st, CpuCore::S) << "SHL.D D0, R2=0";
  for (int i = 1; i < 31; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 1 << i) << "SHL.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::C) << "SHL.D D0, R2=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[31]));
  EXPECT_EQ(state.d0(), 0x80000000) << "SHL.D D0, R2=31";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SHL.D D0, R2=31";
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SHL.D D0, R2=32";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHL.D D0, R2=32";
  ASSERT_TRUE(ExecuteUntilIp(ips[33]));
  EXPECT_EQ(state.d0(), 0) << "SHL.D D0, R2=33";
  EXPECT_EQ(state.st, CpuCore::Z) << "SHL.D D0, R2=33";
}

}  // namespace
}  // namespace oz3