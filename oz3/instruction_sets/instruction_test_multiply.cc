// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdint>
#include <string_view>
#include <vector>

#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "oz3/instruction_sets/instruction_test.h"

namespace oz3 {
namespace {

// A word operation on a register and a value, and what it produces.
struct WordCase {
  uint16_t reg;
  uint16_t value;
  uint16_t result;
  uint16_t st;
  Cycles cycles;
};

// A dword operation on a register and a word value, and what it produces.
struct DwordCase {
  uint32_t reg;
  uint16_t value;
  uint32_t result;
  uint16_t st;
  Cycles cycles;
};

constexpr uint16_t kZCO = CpuCore::Z | CpuCore::C | CpuCore::O;
constexpr uint16_t kSCO = CpuCore::S | CpuCore::C | CpuCore::O;
constexpr uint16_t kCO = CpuCore::C | CpuCore::O;

// MUL.W cases, from the fewest cycles to the most. The cycles depend on the
// value, and overflow is found by a bit shifted out of the register (0x8000 *
// 2), a carry in the loop (0x6000 * 7), or a carry in the last add (0x6000 *
// 3).
constexpr WordCase kMulWordCases[] = {
    {0xFFFF, 1, 0xFFFF, CpuCore::S, 7}, {5, 0, 0, CpuCore::Z, 8},
    {0x8000, 2, 0, kZCO, 10},           {0xFFFF, 3, 0xFFFD, kSCO, 12},
    {0x6000, 3, 0x2000, kCO, 12},       {3, 5, 15, 0, 13},
    {0x6000, 7, 0xA000, kSCO, 16},      {0x100, 0x100, 0, kZCO, 24},
    {300, 200, 60000, CpuCore::S, 25},  {0, 1234, 0, CpuCore::Z, 35},
    {0xFFFF, 0xFFFF, 1, kCO, 68},
};

// MULS.W cases, from the fewest cycles to the most.
constexpr WordCase kMulsWordCases[] = {
    {0, 0, 0, CpuCore::Z, 12},
    {static_cast<uint16_t>(-1), static_cast<uint16_t>(-1), 1, 0, 12},
    {0x8000, 1, 0x8000, CpuCore::S, 14},
    {0x8000, static_cast<uint16_t>(-1), 0x8000, kSCO, 14},
    {static_cast<uint16_t>(-3), static_cast<uint16_t>(-5), 15, 0, 18},
    {3, static_cast<uint16_t>(-5), static_cast<uint16_t>(-15), CpuCore::S, 20},
    {0, static_cast<uint16_t>(-5), 0, CpuCore::Z, 20},
    {256, static_cast<uint16_t>(-128), 0x8000, CpuCore::S, 28},
    {256, static_cast<uint16_t>(-129), 0x7F00, kCO, 31},
    {200, 200, 40000, kSCO, 32},
    {1000, 1000, 0x4240, kCO, 41},
    {static_cast<uint16_t>(-1000), 1000, 0xBDC0, kSCO, 43},
    {static_cast<uint16_t>(-2), 0x7FFF, 2, kCO, 71},
};

// MUL.DW cases, from the fewest cycles to the most.
constexpr DwordCase kMulDwordCases[] = {
    {0xFFFFFFFF, 1, 0xFFFFFFFF, CpuCore::S, 8},
    {7, 0, 0, CpuCore::Z, 9},
    {0x80000000, 2, 0, kZCO, 12},
    {3, 5, 15, 0, 17},
    {0x12345, 0x100, 0x1234500, 0, 32},
    {0x10000, 0x8000, 0x80000000, CpuCore::S, 53},
    {0, 0xFFFF, 0, CpuCore::Z, 98},
    {0xFFFFFFFF, 0xFFFF, 0xFFFF0001, kSCO, 99},
};

// DIV.W cases, from the fewest cycles to the most.
constexpr WordCase kDivWordCases[] = {
    {1234, 0, 1234, CpuCore::O, 5}, {100, 7, 14, 0, 86},
    {0, 5, 0, CpuCore::Z, 86},      {0x1234, 0x5678, 0, CpuCore::Z, 86},
    {0xFFFE, 2, 0x7FFF, 0, 86},     {0xFFFF, 1, 0xFFFF, CpuCore::S, 86},
    {0xFFFF, 0xFFFF, 1, 0, 86},     {0xFFFF, 0x8001, 1, 0, 86},
    {0x8000, 0x8000, 1, 0, 86},
};

// DIVS.W cases, from the fewest cycles to the most.
constexpr WordCase kDivsWordCases[] = {
    {1234, 0, 1234, CpuCore::O, 6},
    {0, 0, 0, CpuCore::O, 6},
    {100, 7, 14, 0, 91},
    {7, 2, 3, 0, 91},
    {3, 5, 0, CpuCore::Z, 91},
    {static_cast<uint16_t>(-3), 5, 0, CpuCore::Z, 91},
    {static_cast<uint16_t>(-100), 7, static_cast<uint16_t>(-14), CpuCore::S,
     91},
    {0x8000, 1, 0x8000, CpuCore::S, 91},
    {0x8000, 2, 0xC000, CpuCore::S, 91},
    {100, static_cast<uint16_t>(-7), static_cast<uint16_t>(-14), CpuCore::S,
     93},
    {static_cast<uint16_t>(-100), static_cast<uint16_t>(-7), 14, 0, 93},
    {100, static_cast<uint16_t>(-1), static_cast<uint16_t>(-100), CpuCore::S,
     93},
    {static_cast<uint16_t>(-100), static_cast<uint16_t>(-1), 100, 0, 93},
    {0, static_cast<uint16_t>(-1), 0, CpuCore::Z, 93},
    {0x7FFF, 0x8000, 0, CpuCore::Z, 93},
    {0x8000, 0x8000, 1, 0, 93},
    {0x8000, static_cast<uint16_t>(-1), 0x8000, CpuCore::O, 94},
};

// MOD.W cases, from the fewest cycles to the most.
constexpr WordCase kModWordCases[] = {
    {1234, 0, 1234, CpuCore::O, 5},  {100, 7, 2, 0, 86},
    {35, 7, 0, CpuCore::Z, 86},      {0xFFFF, 0x10, 0xF, 0, 86},
    {0xFFFF, 0x8001, 0x7FFE, 0, 86}, {0x9000, 0xA000, 0x9000, CpuCore::S, 86},
};

// DIV.DW cases, from the fewest cycles to the most.
constexpr DwordCase kDivDwordCases[] = {
    {1234, 0, 1234, CpuCore::O, 5},
    {1000000, 7, 142857, 0, 168},
    {5, 7, 0, CpuCore::Z, 168},
    {0x10000, 2, 0x8000, 0, 168},
    {0x12345678, 0x8001, 9320, 0, 168},
    {0x80000000, 1, 0x80000000, CpuCore::S, 168},
    {0xFFFFFFFF, 1, 0xFFFFFFFF, CpuCore::S, 168},
    {0xFFFFFFFF, 0xFFFF, 0x10001, 0, 168},
};

// MOD.DW cases, from the fewest cycles to the most.
constexpr DwordCase kModDwordCases[] = {
    {1234, 0, 1234, CpuCore::O, 5},
    {1000000, 7, 1, 0, 167},
    {0x12345678, 0x8001, 12816, 0, 167},
    {0xFFFFFFFF, 0xFFFF, 0, CpuCore::Z, 167},
    {0x9000, 0xA000, 0x9000, 0, 167},
};

// DVMD.W cases, from the fewest cycles to the most. The register's high word
// is ignored, and gets the remainder.
constexpr DwordCase kDvmdWordCases[] = {
    {0x1234FFFF, 0, 0x1234FFFF, CpuCore::O, 5},
    {0xABCD0064, 7, 0x2000E, 0, 87},
    {5, 7, 0x50000, CpuCore::Z, 87},
    {0xFFFF, 1, 0xFFFF, CpuCore::S, 87},
    {0xFFFF, 0x8001, 0x7FFE0001, 0, 87},
};

class MulDivTest : public InstructionTest {
 protected:
  // Runs `op` R0, R1 for each case, with R0 set to the case's register and R1
  // to its value, and checks the result in R0, the flags, and the cycles. R1
  // and MB must be unchanged.
  void RunWordCases(std::string_view op, absl::Span<const WordCase> cases) {
    ASSERT_TRUE(InitAndReset());
    auto& state = GetState();
    state.SetRegisters({{CpuCore::ST, 0}});
    const uint16_t mb = state.mb;

    std::vector<uint16_t> start_ips;
    std::vector<uint16_t> end_ips;
    for (const WordCase& test : cases) {
      state.code.AddValue(Encode("MOV.LW", CpuCore::R0, "$v"))
          .AddValue(test.reg);
      state.code.AddValue(Encode("MOV.LW", CpuCore::R1, "$v"))
          .AddValue(test.value);
      start_ips.push_back(state.code.AddNopGetAddress());
      state.code.AddValue(Encode(op, CpuCore::R0, {"$r", CpuCore::R1}));
      end_ips.push_back(state.code.AddNopGetAddress());
    }
    state.code.AddValue(Encode("HALT"));

    for (int i = 0; i < static_cast<int>(cases.size()); ++i) {
      const WordCase& test = cases[i];
      SCOPED_TRACE(absl::StrCat(op, " 0x", absl::Hex(test.reg), ", 0x",
                                absl::Hex(test.value)));
      ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
      EXPECT_EQ(CyclesUntilIp(end_ips[i]), test.cycles);
      EXPECT_EQ(state.r0, test.result);
      EXPECT_EQ(state.r1, test.value);
      EXPECT_EQ(state.st, test.st);
      EXPECT_EQ(state.mb, mb);
    }
  }

  // Runs `op` D0, R2 for each case, with D0 set to the case's register and R2
  // to its value, and checks the result in D0, the flags, and the cycles. R2
  // and MB must be unchanged.
  void RunDwordCases(std::string_view op, absl::Span<const DwordCase> cases) {
    ASSERT_TRUE(InitAndReset());
    auto& state = GetState();
    state.SetRegisters({{CpuCore::ST, 0}});
    const uint16_t mb = state.mb;

    std::vector<uint16_t> start_ips;
    std::vector<uint16_t> end_ips;
    for (const DwordCase& test : cases) {
      state.code.AddValue(Encode("MOV.LD", 0, "$V")).AddValue32(test.reg);
      state.code.AddValue(Encode("MOV.LW", CpuCore::R2, "$v"))
          .AddValue(test.value);
      start_ips.push_back(state.code.AddNopGetAddress());
      state.code.AddValue(Encode(op, 0, {"$r", CpuCore::R2}));
      end_ips.push_back(state.code.AddNopGetAddress());
    }
    state.code.AddValue(Encode("HALT"));

    for (int i = 0; i < static_cast<int>(cases.size()); ++i) {
      const DwordCase& test = cases[i];
      SCOPED_TRACE(absl::StrCat(op, " 0x", absl::Hex(test.reg), ", 0x",
                                absl::Hex(test.value)));
      ASSERT_TRUE(ExecuteUntilIp(start_ips[i]));
      EXPECT_EQ(CyclesUntilIp(end_ips[i]), test.cycles);
      EXPECT_EQ(state.d0(), test.result);
      EXPECT_EQ(state.r2, test.value);
      EXPECT_EQ(state.st, test.st);
      EXPECT_EQ(state.mb, mb);
    }
  }
};

TEST_F(MulDivTest, MUL_W) { RunWordCases("MUL.W", kMulWordCases); }

TEST_F(MulDivTest, MUL_W_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(1).AddValue(0xFFFF);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 300},
                      {CpuCore::R1, 3},
                      {CpuCore::R2, 100},
                      {CpuCore::R3, 2}});

  state.code.AddValue(Encode("MUL.W", CpuCore::R0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.W", CpuCore::R1, "$v")).AddValue(1);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.W", CpuCore::R1, "$v")).AddValue(0xFFFF);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.W", CpuCore::R3, {"($r)", CpuCore::R2}));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.W", CpuCore::R3, {"($r + $v)", CpuCore::R2}))
      .AddValue(1);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 30);  // MUL.W R0, R0
  EXPECT_EQ(state.r0, static_cast<uint16_t>(300 * 300));
  EXPECT_EQ(state.st, kCO);
  EXPECT_EQ(CyclesUntilIp(ip2), 7);  // MUL.W R1, 1
  EXPECT_EQ(state.r1, 3);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 68);  // MUL.W R1, 0xFFFF
  EXPECT_EQ(state.r1, 0xFFFD);
  EXPECT_EQ(state.st, kSCO);
  EXPECT_EQ(CyclesUntilIp(ip4), 8);  // MUL.W R3, (R2)
  EXPECT_EQ(state.r3, 2);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip5), 71);  // MUL.W R3, (R2 + 1)
  EXPECT_EQ(state.r3, 0xFFFE);
  EXPECT_EQ(state.st, kSCO);
}

TEST_F(MulDivTest, MUL_DW) { RunDwordCases("MUL.DW", kMulDwordCases); }

TEST_F(MulDivTest, MUL_DW_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.extra.SetAddress(state.be + 99).AddValue(1).AddValue(0xFFFF);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 3},
                      {CpuCore::R1, 1},
                      {CpuCore::R2, 0},
                      {CpuCore::R3, 0x8000},
                      {CpuCore::R4, 99}});

  state.code.AddValue(Encode("MUL.DW", 0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.DW", 1, "$v")).AddValue(1);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.DW", 0, "$v")).AddValue(0xFFFF);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.DW", 0, {"($r + $v)", CpuCore::R4}))
      .AddValue(1);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.DW", 1, {"($r)", CpuCore::R4}));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 14);  // MUL.DW D0, R0
  EXPECT_EQ(state.d0(), 0x30009);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip2), 8);  // MUL.DW D1, 1
  EXPECT_EQ(state.d1(), 0x80000000);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(CyclesUntilIp(ip3), 99);  // MUL.DW D0, 0xFFFF
  EXPECT_EQ(state.d0(), 0x0005FFF7);
  EXPECT_EQ(state.st, kCO);
  EXPECT_EQ(CyclesUntilIp(ip4), 102);  // MUL.DW D0, (R4 + 1)
  EXPECT_EQ(state.d0(), 0xFFF10009);
  EXPECT_EQ(state.st, kSCO);
  EXPECT_EQ(CyclesUntilIp(ip5), 9);  // MUL.DW D1, (R4)
  EXPECT_EQ(state.d1(), 0x80000000);
  EXPECT_EQ(state.st, CpuCore::S);
}

TEST_F(MulDivTest, MULS_W) { RunWordCases("MULS.W", kMulsWordCases); }

TEST_F(MulDivTest, MULS_W_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(1).AddValue(0x7FFF);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, static_cast<uint16_t>(-200)},
                      {CpuCore::R1, 3},
                      {CpuCore::R2, 100},
                      {CpuCore::R3, 2},
                      {CpuCore::R4, static_cast<uint16_t>(-2)},
                      {CpuCore::R5, static_cast<uint16_t>(-2)}});

  state.code.AddValue(Encode("MULS.W", CpuCore::R0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MULS.W", CpuCore::R1, "$v")).AddValue(1);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MULS.W", CpuCore::R4, "$v")).AddValue(0x7FFF);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MULS.W", CpuCore::R3, {"($r)", CpuCore::R2}));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MULS.W", CpuCore::R5, {"($r + $v)", CpuCore::R2}))
      .AddValue(1);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 32);  // MULS.W R0, R0
  EXPECT_EQ(state.r0, 40000);
  EXPECT_EQ(state.st, kSCO);
  EXPECT_EQ(CyclesUntilIp(ip2), 12);  // MULS.W R1, 1
  EXPECT_EQ(state.r1, 3);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 71);  // MULS.W R4, 0x7FFF
  EXPECT_EQ(state.r4, 2);
  EXPECT_EQ(state.st, kCO);
  EXPECT_EQ(CyclesUntilIp(ip4), 13);  // MULS.W R3, (R2)
  EXPECT_EQ(state.r3, 2);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip5), 74);  // MULS.W R5, (R2 + 1)
  EXPECT_EQ(state.r5, 2);
  EXPECT_EQ(state.st, kCO);
}

TEST_F(MulDivTest, DIV_W) { RunWordCases("DIV.W", kDivWordCases); }

TEST_F(MulDivTest, DIV_W_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(10).AddValue(3);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 100},
                      {CpuCore::R1, 100},
                      {CpuCore::R2, 100},
                      {CpuCore::R3, 100}});

  state.code.AddValue(Encode("DIV.W", CpuCore::R0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DIV.W", CpuCore::R1, "$v")).AddValue(7);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DIV.W", CpuCore::R1, "$v")).AddValue(0);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DIV.W", CpuCore::R3, {"($r)", CpuCore::R2}));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DIV.W", CpuCore::R3, {"($r + $v)", CpuCore::R2}))
      .AddValue(1);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 86);  // DIV.W R0, R0
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip2), 87);  // DIV.W R1, 7
  EXPECT_EQ(state.r1, 14);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 6);  // DIV.W R1, 0
  EXPECT_EQ(state.r1, 14);
  EXPECT_EQ(state.st, CpuCore::O);
  EXPECT_EQ(CyclesUntilIp(ip4), 88);  // DIV.W R3, (R2)
  EXPECT_EQ(state.r3, 10);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip5), 90);  // DIV.W R3, (R2 + 1)
  EXPECT_EQ(state.r3, 3);
  EXPECT_EQ(state.st, 0);
}

TEST_F(MulDivTest, DIV_DW) { RunDwordCases("DIV.DW", kDivDwordCases); }

TEST_F(MulDivTest, DIV_DW_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.extra.SetAddress(state.be + 100).AddValue(0x100);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 100},
                      {CpuCore::R1, 1},
                      {CpuCore::R2, 0},
                      {CpuCore::R3, 7},
                      {CpuCore::R4, 99}});

  state.code.AddValue(Encode("DIV.DW", 0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DIV.DW", 1, "$v")).AddValue(7);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DIV.DW", 1, {"($r + $v)", CpuCore::R4}))
      .AddValue(1);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 168);  // DIV.DW D0, R0
  EXPECT_EQ(state.d0(), 0x10064 / 100);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip2), 169);  // DIV.DW D1, 7
  EXPECT_EQ(state.d1(), 0x10000);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 172);  // DIV.DW D1, (R4 + 1)
  EXPECT_EQ(state.d1(), 0x100);
  EXPECT_EQ(state.st, 0);
}

TEST_F(MulDivTest, DIVS_W) { RunWordCases("DIVS.W", kDivsWordCases); }

TEST_F(MulDivTest, DIVS_W_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(7).AddValue(-1);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, static_cast<uint16_t>(-100)},
                      {CpuCore::R1, 100},
                      {CpuCore::R2, 100},
                      {CpuCore::R3, 0x8000}});

  state.code.AddValue(Encode("DIVS.W", CpuCore::R0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DIVS.W", CpuCore::R1, "$v")).AddValue(-7);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DIVS.W", CpuCore::R1, {"($r)", CpuCore::R2}));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DIVS.W", CpuCore::R3, {"($r + $v)", CpuCore::R2}))
      .AddValue(1);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 93);  // DIVS.W R0, R0
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip2), 94);  // DIVS.W R1, -7
  EXPECT_EQ(state.r1, static_cast<uint16_t>(-14));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(CyclesUntilIp(ip3), 93);  // DIVS.W R1, (R2)
  EXPECT_EQ(state.r1, static_cast<uint16_t>(-2));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(CyclesUntilIp(ip4), 98);  // DIVS.W R3, (R2 + 1)
  EXPECT_EQ(state.r3, 0x8000);
  EXPECT_EQ(state.st, CpuCore::O);
}

TEST_F(MulDivTest, MOD_W) { RunWordCases("MOD.W", kModWordCases); }

TEST_F(MulDivTest, MOD_W_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(0).AddValue(30);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 100},
                      {CpuCore::R1, 100},
                      {CpuCore::R2, 100},
                      {CpuCore::R3, 100}});

  state.code.AddValue(Encode("MOD.W", CpuCore::R0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOD.W", CpuCore::R1, "$v")).AddValue(7);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOD.W", CpuCore::R3, {"($r)", CpuCore::R2}));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOD.W", CpuCore::R3, {"($r + $v)", CpuCore::R2}))
      .AddValue(1);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 86);  // MOD.W R0, R0
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, CpuCore::Z);
  EXPECT_EQ(CyclesUntilIp(ip2), 87);  // MOD.W R1, 7
  EXPECT_EQ(state.r1, 2);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 7);  // MOD.W R3, (R2)
  EXPECT_EQ(state.r3, 100);
  EXPECT_EQ(state.st, CpuCore::O);
  EXPECT_EQ(CyclesUntilIp(ip4), 90);  // MOD.W R3, (R2 + 1)
  EXPECT_EQ(state.r3, 10);
  EXPECT_EQ(state.st, 0);
}

TEST_F(MulDivTest, MOD_DW) { RunDwordCases("MOD.DW", kModDwordCases); }

TEST_F(MulDivTest, MOD_DW_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.extra.SetAddress(state.be + 100).AddValue(0x100);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 100},
                      {CpuCore::R1, 1},
                      {CpuCore::R2, 0x1234},
                      {CpuCore::R3, 7},
                      {CpuCore::R4, 99}});

  state.code.AddValue(Encode("MOD.DW", 0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOD.DW", 1, {"($r + $v)", CpuCore::R4}))
      .AddValue(1);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOD.DW", 1, "$v")).AddValue(7);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 167);  // MOD.DW D0, R0
  EXPECT_EQ(state.d0(), 0x10064 % 100);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip2), 171);  // MOD.DW D1, (R4 + 1)
  EXPECT_EQ(state.d1(), 0x34);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 168);  // MOD.DW D1, 7
  EXPECT_EQ(state.d1(), 0x34 % 7);
  EXPECT_EQ(state.st, 0);
}

TEST_F(MulDivTest, DVMD_W) { RunDwordCases("DVMD.W", kDvmdWordCases); }

TEST_F(MulDivTest, DVMD_W_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.extra.SetAddress(state.be + 100).AddValue(10);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 100},
                      {CpuCore::R1, 7},
                      {CpuCore::R2, 100},
                      {CpuCore::R3, 0xFFFF},
                      {CpuCore::R4, 99}});

  state.code.AddValue(Encode("DVMD.W", 0, {"$r", CpuCore::R1}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DVMD.W", 1, "$v")).AddValue(7);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("DVMD.W", 0, {"($r + $v)", CpuCore::R4}))
      .AddValue(1);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 87);  // DVMD.W D0, R1
  EXPECT_EQ(state.d0(), 0x2000E);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip2), 88);  // DVMD.W D1, 7
  EXPECT_EQ(state.d1(), 0x2000E);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 91);  // DVMD.W D0, (R4 + 1)
  EXPECT_EQ(state.d0(), 0x00040001);
  EXPECT_EQ(state.st, 0);
}

}  // namespace
}  // namespace oz3
