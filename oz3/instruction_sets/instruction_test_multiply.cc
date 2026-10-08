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

// MUL.W cases, from the fewest cycles to the most.
constexpr WordCase kMulWordCases[] = {
    {3, 5, 15, 0, 71},
    {0, 1234, 0, CpuCore::Z, 71},
    {300, 200, 60000, CpuCore::S, 71},
    {0xFFFF, 1, 0xFFFF, CpuCore::S, 71},
    {0x100, 0x100, 0, kZCO, 72},
    {0x8000, 2, 0, kZCO, 72},
    {0xFFFF, 0xFFFF, 1, kCO, 72},
};

// MULS.W cases, from the fewest cycles to the most.
constexpr WordCase kMulsWordCases[] = {
    {0, 0, 0, CpuCore::Z, 77},
    {static_cast<uint16_t>(-3), static_cast<uint16_t>(-5), 15, 0, 77},
    {static_cast<uint16_t>(-1), static_cast<uint16_t>(-1), 1, 0, 77},
    {0, static_cast<uint16_t>(-5), 0, CpuCore::Z, 77},
    {1000, 1000, 0x4240, kCO, 78},
    {256, static_cast<uint16_t>(-129), 0x7F00, kCO, 78},
    {3, static_cast<uint16_t>(-5), static_cast<uint16_t>(-15), CpuCore::S, 78},
    {0x8000, 1, 0x8000, CpuCore::S, 78},
    {256, static_cast<uint16_t>(-128), 0x8000, CpuCore::S, 78},
    {200, 200, 40000, kSCO, 79},
    {0x8000, static_cast<uint16_t>(-1), 0x8000, kSCO, 79},
    {static_cast<uint16_t>(-1000), 1000, 0xBDC0, kSCO, 79},
};

// MUL.DW cases, from the fewest cycles to the most.
constexpr DwordCase kMulDwordCases[] = {
    {3, 5, 15, 0, 137},
    {0, 0xFFFF, 0, CpuCore::Z, 137},
    {0x12345, 0x100, 0x1234500, 0, 137},
    {0x10000, 0x8000, 0x80000000, CpuCore::S, 137},
    {0x80000000, 2, 0, kZCO, 138},
    {0xFFFFFFFF, 0xFFFF, 0xFFFF0001, kSCO, 138},
};

class MultiplyTest : public InstructionTest {
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

TEST_F(MultiplyTest, MUL_W) { RunWordCases("MUL.W", kMulWordCases); }

TEST_F(MultiplyTest, MUL_W_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(7).AddValue(0x2000);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 300},
                      {CpuCore::R1, 3},
                      {CpuCore::R2, 100},
                      {CpuCore::R3, 2}});

  state.code.AddValue(Encode("MUL.W", CpuCore::R0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.W", CpuCore::R1, "$v")).AddValue(5);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.W", CpuCore::R1, "$v")).AddValue(0x8000);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.W", CpuCore::R3, {"($r)", CpuCore::R2}));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.W", CpuCore::R3, {"($r + $v)", CpuCore::R2}))
      .AddValue(1);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 72);  // MUL.W R0, R0
  EXPECT_EQ(state.r0, static_cast<uint16_t>(300 * 300));
  EXPECT_EQ(state.st, kCO);
  EXPECT_EQ(CyclesUntilIp(ip2), 72);  // MUL.W R1, 5
  EXPECT_EQ(state.r1, 15);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 73);  // MUL.W R1, 0x8000
  EXPECT_EQ(state.r1, 0x8000);
  EXPECT_EQ(state.st, kSCO);
  EXPECT_EQ(CyclesUntilIp(ip4), 73);  // MUL.W R3, (R2)
  EXPECT_EQ(state.r3, 14);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip5), 76);  // MUL.W R3, (R2 + 1)
  EXPECT_EQ(state.r3, 0xC000);
  EXPECT_EQ(state.st, kSCO);
}

TEST_F(MultiplyTest, MUL_DW) { RunDwordCases("MUL.DW", kMulDwordCases); }

TEST_F(MultiplyTest, MUL_DW_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.extra.SetAddress(state.be + 100).AddValue(0x8000);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 3},
                      {CpuCore::R1, 1},
                      {CpuCore::R2, 0},
                      {CpuCore::R3, 0x8000},
                      {CpuCore::R4, 99}});

  state.code.AddValue(Encode("MUL.DW", 0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.DW", 1, "$v")).AddValue(2);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MUL.DW", 0, {"($r + $v)", CpuCore::R4}))
      .AddValue(1);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 137);  // MUL.DW D0, R0
  EXPECT_EQ(state.d0(), 0x30009);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip2), 139);  // MUL.DW D1, 2
  EXPECT_EQ(state.d1(), 0);
  EXPECT_EQ(state.st, kZCO);
  EXPECT_EQ(CyclesUntilIp(ip3), 142);  // MUL.DW D0, (R4 + 1)
  EXPECT_EQ(state.d0(), 0x80048000);
  EXPECT_EQ(state.st, kSCO);
}

TEST_F(MultiplyTest, MULS_W) { RunWordCases("MULS.W", kMulsWordCases); }

TEST_F(MultiplyTest, MULS_W_Operands) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(7).AddValue(3000);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, static_cast<uint16_t>(-200)},
                      {CpuCore::R1, 3},
                      {CpuCore::R2, 100},
                      {CpuCore::R3, 2},
                      {CpuCore::R4, 256}});

  state.code.AddValue(Encode("MULS.W", CpuCore::R0, {"$r", CpuCore::R0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MULS.W", CpuCore::R1, "$v")).AddValue(5);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MULS.W", CpuCore::R4, "$v")).AddValue(200);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MULS.W", CpuCore::R3, {"($r)", CpuCore::R2}));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MULS.W", CpuCore::R3, {"($r + $v)", CpuCore::R2}))
      .AddValue(1);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("HALT"));

  EXPECT_EQ(CyclesUntilIp(ip1), 79);  // MULS.W R0, R0
  EXPECT_EQ(state.r0, 40000);
  EXPECT_EQ(state.st, kSCO);
  EXPECT_EQ(CyclesUntilIp(ip2), 78);  // MULS.W R1, 5
  EXPECT_EQ(state.r1, 15);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip3), 80);  // MULS.W R4, 200
  EXPECT_EQ(state.r4, 51200);
  EXPECT_EQ(state.st, kSCO);
  EXPECT_EQ(CyclesUntilIp(ip4), 79);  // MULS.W R3, (R2)
  EXPECT_EQ(state.r3, 14);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(CyclesUntilIp(ip5), 83);  // MULS.W R3, (R2 + 1)
  EXPECT_EQ(state.r3, 42000);
  EXPECT_EQ(state.st, kSCO);
}

}  // namespace
}  // namespace oz3
