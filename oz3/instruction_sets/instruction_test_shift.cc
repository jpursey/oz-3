// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdint>
#include <vector>

#include "absl/strings/str_cat.h"
#include "oz3/instruction_sets/instruction_test.h"

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

TEST_F(InstructionTest, SHR_W_ByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v")).AddValue(0x8000);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHR.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 15; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000 >> i) << "SHR.W R0, " << i;
    EXPECT_EQ(state.st, 0) << "SHR.W R0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[15]));
  EXPECT_EQ(state.r0, 1) << "SHR.W R0, 15";
  EXPECT_EQ(state.st, 0) << "SHR.W R0, 15";
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SHR.W R0, 16";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHR.W R0, 16";
}

TEST_F(InstructionTest, SHR_W_ByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v")).AddValue(0x8000);
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHR.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.r0, 0x8000) << "SHR.W R0, R1=0";
  EXPECT_EQ(state.st, CpuCore::S) << "SHR.W R0, R1=0";
  for (int i = 1; i < 15; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000 >> i) << "SHR.W R0, R1=" << i;
    EXPECT_EQ(state.st, 0) << "SHR.W R0, R1=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[15]));
  EXPECT_EQ(state.r0, 1) << "SHR.W R0, R1=15";
  EXPECT_EQ(state.st, 0) << "SHR.W R0, R1=15";
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SHR.W R0, R1=16";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHR.W R0, R1=16";
  ASSERT_TRUE(ExecuteUntilIp(ips[17]));
  EXPECT_EQ(state.r0, 0) << "SHR.W R0, R1=17";
  EXPECT_EQ(state.st, CpuCore::Z) << "SHR.W R0, R1=17";
}

TEST_F(InstructionTest, SHR_W_CarryByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});
  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(0x8000 | (1 << (i - 1)));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHR.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));
  for (int i = 1; i < 15; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000 >> i) << "SHR.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::C) << "SHR.W R0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[15]));
  EXPECT_EQ(state.r0, 1) << "SHR.W R0, 15";
  EXPECT_EQ(state.st, CpuCore::C) << "SHR.W R0, 15";
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SHR.W R0, 16";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHR.W R0, 16";
}

TEST_F(InstructionTest, SHR_W_CarryByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(0x8000 | (1 << std::max(0, i - 1)));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHR.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.r0, 0x8001) << "SHR.W R0, R1=0";
  EXPECT_EQ(state.st, CpuCore::S) << "SHR.W R0, R1=0";
  for (int i = 1; i < 15; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x8000 >> i) << "SHR.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::C) << "SHR.W R0, R1=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[15]));
  EXPECT_EQ(state.r0, 1) << "SHR.W R0, R1=15";
  EXPECT_EQ(state.st, CpuCore::C) << "SHR.W R0, R1=15";
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SHR.W R0, R1=16";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHR.W R0, R1=16";
  ASSERT_TRUE(ExecuteUntilIp(ips[17]));
  EXPECT_EQ(state.r0, 0) << "SHR.W R0, R1=17";
  EXPECT_EQ(state.st, CpuCore::Z) << "SHR.W R0, R1=17";
}

TEST_F(InstructionTest, SHR_D_ByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V")).AddValue32(0x80000000);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHR.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 31; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000 >> i) << "SHR.D D0, " << i;
    EXPECT_EQ(state.st, 0) << "SHR.D D0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[31]));
  EXPECT_EQ(state.d0(), 1) << "SHR.D D0, 31";
  EXPECT_EQ(state.st, 0) << "SHR.D D0, 31";
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SHR.D D0, 32";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHR.D D0, 32";
}

TEST_F(InstructionTest, SHR_D_ByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V")).AddValue32(0x80000000);
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHR.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.d0(), 0x80000000) << "SHR.D D0, R2=0";
  EXPECT_EQ(state.st, CpuCore::S) << "SHR.D D0, R2=0";
  for (int i = 1; i < 31; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000 >> i) << "SHR.D D0, R2=" << i;
    EXPECT_EQ(state.st, 0) << "SHR.D D0, R2=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[31]));
  EXPECT_EQ(state.d0(), 1) << "SHR.D D0, R2=31";
  EXPECT_EQ(state.st, 0) << "SHR.D D0, R2=31";
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SHR.D D0, R2=32";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHR.D D0, R2=32";
  ASSERT_TRUE(ExecuteUntilIp(ips[33]));
  EXPECT_EQ(state.d0(), 0) << "SHR.D D0, R2=33";
  EXPECT_EQ(state.st, CpuCore::Z) << "SHR.D D0, R2=33";
}

TEST_F(InstructionTest, SHR_D_CarryByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(0x80000000 | (1 << (i - 1)));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHR.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 31; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000 >> i) << "SHR.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::C) << "SHR.D D0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[31]));
  EXPECT_EQ(state.d0(), 1) << "SHR.D D0, 31";
  EXPECT_EQ(state.st, CpuCore::C) << "SHR.D D0, 31";
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SHR.D D0, 32";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHR.D D0, 32";
}

TEST_F(InstructionTest, SHR_D_CarryByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(0x80000000 | (1 << std::max(0, i - 1)));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SHR.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.d0(), 0x80000001) << "SHR.D D0, R2=0";
  EXPECT_EQ(state.st, CpuCore::S) << "SHR.D D0, R2=0";
  for (int i = 1; i < 31; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x80000000 >> i) << "SHR.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::C) << "SHR.D D0, R2=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[31]));
  EXPECT_EQ(state.d0(), 1) << "SHR.D D0, R2=31";
  EXPECT_EQ(state.st, CpuCore::C) << "SHR.D D0, R2=31";
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SHR.D D0, R2=32";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SHR.D D0, R2=32";
  ASSERT_TRUE(ExecuteUntilIp(ips[33]));
  EXPECT_EQ(state.d0(), 0) << "SHR.D D0, R2=33";
  EXPECT_EQ(state.st, CpuCore::Z) << "SHR.D D0, R2=33";
}

TEST_F(InstructionTest, SRA_W_ByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});
  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v")).AddValue(0x4000);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));
  for (int i = 1; i < 15; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x4000 >> i) << "SRA.W R0, " << i;
    EXPECT_EQ(state.st, 0) << "SRA.W R0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[15]));
  EXPECT_EQ(state.r0, 0) << "SRA.W R0, 15";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SRA.W R0, 15";
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SRA.W R0, 16";
  EXPECT_EQ(state.st, CpuCore::Z) << "SRA.W R0, 16";
}

TEST_F(InstructionTest, SRA_W_ByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});
  uint16_t ips[18] = {};

  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v")).AddValue(0x4000);
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 15; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0x4000 >> i) << "SRA.W R0, R1=" << i;
    EXPECT_EQ(state.st, 0) << "SRA.W R0, R1=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[15]));
  EXPECT_EQ(state.r0, 0) << "SRA.W R0, R1=15";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SRA.W R0, R1=15";
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0) << "SRA.W R0, R1=16";
  EXPECT_EQ(state.st, CpuCore::Z) << "SRA.W R0, R1=16";
  ASSERT_TRUE(ExecuteUntilIp(ips[17]));
  EXPECT_EQ(state.r0, 0) << "SRA.W R0, R1=17";
  EXPECT_EQ(state.st, CpuCore::Z) << "SRA.W R0, R1=17";
}

TEST_F(InstructionTest, SRA_W_SignByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});
  uint16_t ips[17] = {};

  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v")).AddValue(0x8000);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 16; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0xFFFF & ~((0x8000 >> i) - 1)) << "SRA.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::S) << "SRA.W R0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0xFFFF) << "SRA.W R0, 16";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.W R0, 16";
}

TEST_F(InstructionTest, SRA_W_SignByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v")).AddValue(0x8000);
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.r0, 0x8000) << "SRA.W R0, R1=0";
  EXPECT_EQ(state.st, CpuCore::S) << "SRA.W R0, R1=0";
  for (int i = 1; i < 16; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0xFFFF & ~((0x8000 >> i) - 1)) << "SRA.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::S) << "SRA.W R0, R1=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0xFFFF) << "SRA.W R0, R1=16";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.W R0, R1=16";
  ASSERT_TRUE(ExecuteUntilIp(ips[17]));
  EXPECT_EQ(state.r0, 0xFFFF) << "SRA.W R0, R1=17";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.W R0, R1=17";
}

TEST_F(InstructionTest, SRA_W_CarryByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[17] = {};
  for (int i = 1; i < 17; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(0x8000 | (1 << (i - 1)));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.W", CpuCore::R0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 16; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0xFFFF & ~((0x8000 >> i) - 1)) << "SRA.W R0, " << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.W R0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0xFFFF) << "SRA.W R0, 16";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.W R0, 16";
}

TEST_F(InstructionTest, SRA_W_CarryByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[18] = {};
  for (int i = 0; i < 18; ++i) {
    state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
        .AddValue(0x8000 | (1 << std::max(0, i - 1)));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.W", CpuCore::R0, {"$r", CpuCore::R1}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.r0, 0x8001) << "SRA.W R0, R1=0";
  EXPECT_EQ(state.st, CpuCore::S) << "SRA.W R0, R1=0";
  for (int i = 1; i < 16; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.r0, 0xFFFF & ~((0x8000 >> i) - 1)) << "SRA.W R0, R1=" << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.W R0, R1=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[16]));
  EXPECT_EQ(state.r0, 0xFFFF) << "SRA.W R0, R1=16";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.W R0, R1=16";
  ASSERT_TRUE(ExecuteUntilIp(ips[17]));
  EXPECT_EQ(state.r0, 0xFFFF) << "SRA.W R0, R1=17";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.W R0, R1=17";
}

TEST_F(InstructionTest, SRA_D_ByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V")).AddValue32(0x40000000);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 31; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x40000000 >> i) << "SRA.D D0, " << i;
    EXPECT_EQ(state.st, 0) << "SRA.D D0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[31]));
  EXPECT_EQ(state.d0(), 0) << "SRA.D D0, 31";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SRA.D D0, 31";
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SRA.D D0, 32";
  EXPECT_EQ(state.st, CpuCore::Z) << "SRA.D D0, 32";
}

TEST_F(InstructionTest, SRA_D_ByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});
  uint16_t ips[34] = {};

  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V")).AddValue32(0x40000000);
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 0; i < 31; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0x40000000 >> i) << "SRA.D D0, R2=" << i;
    EXPECT_EQ(state.st, 0) << "SRA.D D0, R2=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[31]));
  EXPECT_EQ(state.d0(), 0) << "SRA.D D0, R2=31";
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C) << "SRA.D D0, R2=31";
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0) << "SRA.D D0, R2=32";
  EXPECT_EQ(state.st, CpuCore::Z) << "SRA.D D0, R2=32";
  ASSERT_TRUE(ExecuteUntilIp(ips[33]));
  EXPECT_EQ(state.d0(), 0) << "SRA.D D0, R2=33";
  EXPECT_EQ(state.st, CpuCore::Z) << "SRA.D D0, R2=33";
}

TEST_F(InstructionTest, SRA_D_SignByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V")).AddValue32(0x80000000);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 32; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0xFFFFFFFF & ~((0x80000000 >> i) - 1))
        << "SRA.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::S) << "SRA.D D0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0xFFFFFFFF) << "SRA.D D0, 32";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.D D0, 32";
}

TEST_F(InstructionTest, SRA_D_SignByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V")).AddValue32(0x80000000);
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.d0(), 0x80000000) << "SRA.D D0, R2=0";
  EXPECT_EQ(state.st, CpuCore::S) << "SRA.D D0, R2=0";
  for (int i = 1; i < 32; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0xFFFFFFFF & ~((0x80000000 >> i) - 1))
        << "SRA.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::S) << "SRA.D D0, R2=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0xFFFFFFFF) << "SRA.D D0, R2=32";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.D D0, R2=32";
  ASSERT_TRUE(ExecuteUntilIp(ips[33]));
  EXPECT_EQ(state.d0(), 0xFFFFFFFF) << "SRA.D D0, R2=33";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.D D0, R2=33";
}

TEST_F(InstructionTest, SRA_D_CarryByValue) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[33] = {};
  for (int i = 1; i < 33; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(0x80000000 | (1 << (i - 1)));
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.D", 0, Arg(shift_value)));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  for (int i = 1; i < 32; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0xFFFFFFFF & ~((0x80000000 >> i) - 1))
        << "SRA.D D0, " << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.D D0, " << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0xFFFFFFFF) << "SRA.D D0, 32";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.D D0, 32";
}

TEST_F(InstructionTest, SRA_D_CarryByRegister) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, 0}});

  uint16_t ips[34] = {};
  for (int i = 0; i < 34; ++i) {
    state.code.AddValue(Encode("MOV.LD", 0, "$V"))
        .AddValue32(0x80000000 | (1 << std::max(0, i - 1)));
    state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v")).AddValue(i);
    const std::string shift_value = absl::StrCat(i);
    state.code.AddValue(Encode("SRA.D", 0, {"$r", CpuCore::R2}));
    ips[i] = state.code.AddNopGetAddress();
  }
  state.code.AddValue(Encode("HALT"));

  ASSERT_TRUE(ExecuteUntilIp(ips[0]));
  EXPECT_EQ(state.d0(), 0x80000001) << "SRA.D D0, R2=0";
  EXPECT_EQ(state.st, CpuCore::S) << "SRA.D D0, R2=0";
  for (int i = 1; i < 32; ++i) {
    ASSERT_TRUE(ExecuteUntilIp(ips[i]));
    EXPECT_EQ(state.d0(), 0xFFFFFFFF & ~((0x80000000 >> i) - 1))
        << "SRA.D D0, R2=" << i;
    EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.D D0, R2=" << i;
  }
  ASSERT_TRUE(ExecuteUntilIp(ips[32]));
  EXPECT_EQ(state.d0(), 0xFFFFFFFF) << "SRA.D D0, R2=32";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.D D0, R2=32";
  ASSERT_TRUE(ExecuteUntilIp(ips[33]));
  EXPECT_EQ(state.d0(), 0xFFFFFFFF) << "SRA.D D0, R2=33";
  EXPECT_EQ(state.st, CpuCore::S | CpuCore::C) << "SRA.D D0, R2=33";
}

// Register counts past the width of a word or dword, including counts with the
// high bit set. Every one shifts out the whole register in the same cycles.
constexpr uint16_t kWordLargeCounts[] = {17, 0x7FFF, 0x8000, 0x8011, 0xFFFF};
constexpr uint16_t kDwordLargeCounts[] = {33, 0x7FFF, 0x8000, 0x8021, 0xFFFF};

TEST_F(InstructionTest, SHL_W_ByLargeRegister) {
  RunCountCases("SHL.W", CountArg::kRegister,
                SameCountCases(kWordLargeCounts, 0xFFFF, 0, CpuCore::Z, 7));
}

TEST_F(InstructionTest, SHL_D_ByLargeRegister) {
  RunCountCases(
      "SHL.D", CountArg::kRegister,
      SameCountCases(kDwordLargeCounts, 0xFFFFFFFF, 0, CpuCore::Z, 10));
}

TEST_F(InstructionTest, SHR_W_ByLargeRegister) {
  RunCountCases("SHR.W", CountArg::kRegister,
                SameCountCases(kWordLargeCounts, 0xFFFF, 0, CpuCore::Z, 7));
}

TEST_F(InstructionTest, SHR_D_ByLargeRegister) {
  RunCountCases(
      "SHR.D", CountArg::kRegister,
      SameCountCases(kDwordLargeCounts, 0xFFFFFFFF, 0, CpuCore::Z, 10));
}

TEST_F(InstructionTest, SRA_W_ByLargeRegister) {
  std::vector<CountCase> cases =
      SameCountCases(kWordLargeCounts, 0x7FFF, 0, CpuCore::Z, 8);
  for (const CountCase& c : SameCountCases(kWordLargeCounts, 0x8000, 0xFFFF,
                                           CpuCore::S | CpuCore::C, 9)) {
    cases.push_back(c);
  }
  RunCountCases("SRA.W", CountArg::kRegister, cases);
}

TEST_F(InstructionTest, SRA_D_ByLargeRegister) {
  std::vector<CountCase> cases =
      SameCountCases(kDwordLargeCounts, 0x7FFFFFFF, 0, CpuCore::Z, 11);
  for (const CountCase& c :
       SameCountCases(kDwordLargeCounts, 0x80000000, 0xFFFFFFFF,
                      CpuCore::S | CpuCore::C, 12)) {
    cases.push_back(c);
  }
  RunCountCases("SRA.D", CountArg::kRegister, cases);
}

// A word shift by a register takes 2 cycles a bit.
TEST_F(InstructionTest, SHL_W_ByRegisterCycles) {
  RunCountCases("SHL.W", CountArg::kRegister,
                {{0x1234, 0, 0x1234, 0, 6},
                 {0x1234, 1, 0x2468, 0, 7},
                 {0x1234, 16, 0, CpuCore::Z, 37}});
}

TEST_F(InstructionTest, SHR_W_ByRegisterCycles) {
  RunCountCases("SHR.W", CountArg::kRegister,
                {{0x1234, 0, 0x1234, 0, 6},
                 {0x1234, 1, 0x091A, 0, 7},
                 {0x1234, 16, 0, CpuCore::Z, 37}});
}

TEST_F(InstructionTest, SRA_W_ByRegisterCycles) {
  RunCountCases("SRA.W", CountArg::kRegister,
                {{0x8000, 0, 0x8000, CpuCore::S, 6},
                 {0x8000, 1, 0xC000, CpuCore::S, 7},
                 {0x8000, 16, 0xFFFF, CpuCore::S | CpuCore::C, 37}});
}

// A dword shift by a register takes 3 cycles a bit up to 15 bits. From 16
// bits, a word moves first, and the rest take 2 cycles a bit.
TEST_F(InstructionTest, SHL_D_ByRegisterCycles) {
  RunCountCases("SHL.D", CountArg::kRegister,
                {{0x00018001, 0, 0x00018001, 0, 7},
                 {0x00018001, 1, 0x00030002, 0, 8},
                 {0x00018001, 15, 0xC0008000, CpuCore::S, 50},
                 {0x00018001, 16, 0x80010000, CpuCore::S | CpuCore::C, 10},
                 {0x00018001, 17, 0x00020000, CpuCore::C, 12},
                 {0x00018001, 32, 0, CpuCore::Z | CpuCore::C, 42}});
}

TEST_F(InstructionTest, SHR_D_ByRegisterCycles) {
  RunCountCases("SHR.D", CountArg::kRegister,
                {{0x00018001, 0, 0x00018001, 0, 7},
                 {0x00018001, 1, 0x0000C000, CpuCore::C, 8},
                 {0x00018001, 15, 0x00000003, 0, 50},
                 {0x00018001, 16, 0x00000001, CpuCore::C, 10},
                 {0x00018001, 17, 0, CpuCore::Z | CpuCore::C, 12},
                 {0x00018001, 32, 0, CpuCore::Z, 42}});
}

// As SHL.D and SHR.D, with one more cycle to fill the high word with the sign
// from 17 bits, and one more for a negative value at 16 bits.
TEST_F(InstructionTest, SRA_D_ByRegisterCycles) {
  RunCountCases("SRA.D", CountArg::kRegister,
                {{0x80018001, 0, 0x80018001, CpuCore::S, 7},
                 {0x80018001, 1, 0xC000C000, CpuCore::S | CpuCore::C, 8},
                 {0x80018001, 15, 0xFFFF0003, CpuCore::S, 50},
                 {0x40018001, 16, 0x00004001, CpuCore::C, 10},
                 {0x80018001, 16, 0xFFFF8001, CpuCore::S | CpuCore::C, 11},
                 {0x40018001, 17, 0x00002000, CpuCore::C, 13},
                 {0x80018001, 17, 0xFFFFC000, CpuCore::S | CpuCore::C, 13},
                 {0x40018001, 32, 0, CpuCore::Z, 43},
                 {0x80018001, 32, 0xFFFFFFFF, CpuCore::S | CpuCore::C, 43}});
}

// From 17 bits, the high word moves to the low word and is filled with the
// sign, and only the low word is shifted.
TEST_F(InstructionTest, SRA_D_ByValueCycles) {
  RunCountCases("SRA.D", CountArg::kImmediate,
                {{0x40018001, 16, 0x00004001, CpuCore::C, 7},
                 {0x80018001, 16, 0xFFFF8001, CpuCore::S | CpuCore::C, 8},
                 {0x40018001, 17, 0x00002000, CpuCore::C, 7},
                 {0x80018001, 17, 0xFFFFC000, CpuCore::S | CpuCore::C, 7},
                 {0x40018001, 31, 0, CpuCore::Z | CpuCore::C, 21},
                 {0x80018001, 31, 0xFFFFFFFF, CpuCore::S, 21}});
}

}  // namespace
}  // namespace oz3
