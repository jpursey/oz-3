// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdint>

#include "oz3/instruction_sets/instruction_test.h"

namespace oz3 {
namespace {

constexpr uint16_t kSC = CpuCore::S | CpuCore::C;
constexpr uint16_t kSCO = CpuCore::S | CpuCore::C | CpuCore::O;

TEST_F(InstructionTest, CPI) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(5).AddValue(6);
  state.data.SetAddress(state.bd + 110).AddValue(110);
  state.extra.SetAddress(state.be + 200).AddValue(3);
  state.stack.SetAddress(state.bs + 300).AddValue(0x8000);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 5},
                      {CpuCore::R1, 100},
                      {CpuCore::R2, 110},
                      {CpuCore::R4, 200},
                      {CpuCore::R7, 300}});

  state.code.AddValue(Encode("CPI", CpuCore::R0, CpuCore::R1));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPI", CpuCore::R0, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPI", CpuCore::R0, CpuCore::R4));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPI", CpuCore::R0, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPI", CpuCore::R2, CpuCore::R2));
  const uint16_t ip5 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 6);  // CPI R0, (R1)
  EXPECT_EQ(state.r1, 101);
  EXPECT_EQ(state.st, CpuCore::Z);

  EXPECT_EQ(CyclesUntilIp(ip2), 6);  // CPI R0, (R1)
  EXPECT_EQ(state.r1, 102);
  EXPECT_EQ(state.st, kSC);

  EXPECT_EQ(CyclesUntilIp(ip3), 6);  // CPI R0, (R4)
  EXPECT_EQ(state.r4, 201);
  EXPECT_EQ(state.st, 0);

  // R7 is an address like any other.
  EXPECT_EQ(CyclesUntilIp(ip4), 6);  // CPI R0, (R7)
  EXPECT_EQ(state.r7, 301);
  EXPECT_EQ(state.st, kSCO);

  // The register is compared before the address is stepped.
  EXPECT_EQ(CyclesUntilIp(ip5), 6);  // CPI R2, (R2)
  EXPECT_EQ(state.r2, 111);
  EXPECT_EQ(state.st, CpuCore::Z);
  EXPECT_EQ(state.r0, 5);
}

TEST_F(InstructionTest, CPD) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(6).AddValue(5);
  state.data.SetAddress(state.bd + 110).AddValue(110);
  state.extra.SetAddress(state.be + 200).AddValue(3);
  state.stack.SetAddress(state.bs + 300).AddValue(0x8000);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 5},
                      {CpuCore::R1, 101},
                      {CpuCore::R2, 110},
                      {CpuCore::R4, 200},
                      {CpuCore::R7, 300}});

  state.code.AddValue(Encode("CPD", CpuCore::R0, CpuCore::R1));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPD", CpuCore::R0, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPD", CpuCore::R0, CpuCore::R4));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPD", CpuCore::R0, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPD", CpuCore::R2, CpuCore::R2));
  const uint16_t ip5 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 7);  // CPD R0, (R1)
  EXPECT_EQ(state.r1, 100);
  EXPECT_EQ(state.st, CpuCore::Z);

  EXPECT_EQ(CyclesUntilIp(ip2), 7);  // CPD R0, (R1)
  EXPECT_EQ(state.r1, 99);
  EXPECT_EQ(state.st, kSC);

  EXPECT_EQ(CyclesUntilIp(ip3), 7);  // CPD R0, (R4)
  EXPECT_EQ(state.r4, 199);
  EXPECT_EQ(state.st, 0);

  // R7 is an address like any other.
  EXPECT_EQ(CyclesUntilIp(ip4), 7);  // CPD R0, (R7)
  EXPECT_EQ(state.r7, 299);
  EXPECT_EQ(state.st, kSCO);

  // The register is compared before the address is stepped.
  EXPECT_EQ(CyclesUntilIp(ip5), 7);  // CPD R2, (R2)
  EXPECT_EQ(state.r2, 109);
  EXPECT_EQ(state.st, CpuCore::Z);
  EXPECT_EQ(state.r0, 5);
}

TEST_F(InstructionTest, CPIR_CPDR_R7) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  // The stack is moved away from the code, so R7 can address low words in it.
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 5},
                      {CpuCore::R1, 100},
                      {CpuCore::R7, 3},
                      {CpuCore::BS, 3000}});
  state.data.SetAddress(state.bd + 100).AddValue(3);
  state.stack.SetAddress(state.bs + 2).AddValue(1);
  state.stack.SetAddress(state.bs + 300).AddValue(5);

  state.code.AddValue(Encode("CPIR", CpuCore::R7, CpuCore::R1));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MOV.LW", CpuCore::R7, "$v")).AddValue(300);
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPIR", CpuCore::R0, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPDR", CpuCore::R0, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();

  // R7 is compared before it is decremented.
  EXPECT_EQ(CyclesUntilIp(ip1), 9);  // CPIR R7, (R1)
  EXPECT_EQ(state.r1, 101);
  EXPECT_EQ(state.r7, 2);
  EXPECT_EQ(state.st, CpuCore::Z);

  // As the address, R7 is stepped up and then decremented, so CPIR only stops
  // on a match.
  ASSERT_TRUE(ExecuteUntilIp(setup2));  // MOV R7, 300
  EXPECT_EQ(CyclesUntilIp(ip2), 9);     // CPIR R0, (R7)
  EXPECT_EQ(state.r7, 300);
  EXPECT_EQ(state.st, CpuCore::Z);

  // CPDR steps R7 down before decrementing it, so from 2 it stops after one
  // word.
  ASSERT_TRUE(ExecuteUntilIp(setup3));  // MVQ R7, 2
  EXPECT_EQ(CyclesUntilIp(ip3), 9);     // CPDR R0, (R7)
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, 0);
}

TEST_F(InstructionTest, CPIR_CPDR_Wrap) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 0xFFFE).AddValue(1).AddValue(2);
  state.data.SetAddress(state.bd).AddValue(3).AddValue(4);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 3},
                      {CpuCore::R1, 1},
                      {CpuCore::R2, 0xFFFE},
                      {CpuCore::R3, 2},
                      {CpuCore::R7, 3}});

  state.code.AddValue(Encode("CPIR", CpuCore::R0, CpuCore::R2));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPDR", CpuCore::R3, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();

  // The address wraps from 0xFFFF to 0.
  EXPECT_EQ(CyclesUntilIp(ip1), 9 + 9 + 8);  // CPIR R0, (R2)
  EXPECT_EQ(state.r2, 1);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::Z);

  // The address steps from 1 to 0, and wraps from 0 to 0xFFFF.
  ASSERT_TRUE(ExecuteUntilIp(setup2));         // MVQ R7, 3
  EXPECT_EQ(CyclesUntilIp(ip2), 10 + 10 + 9);  // CPDR R3, (R1)
  EXPECT_EQ(state.r1, 0xFFFE);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::Z);
}

TEST_F(InstructionTest, CPIR) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(1).AddValue(2).AddValue(3);
  state.extra.SetAddress(state.be + 200).AddValue(1).AddValue(5).AddValue(7);
  state.stack.SetAddress(state.bs + 300).AddValue(1).AddValue(5);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 5},
                      {CpuCore::R1, 100},
                      {CpuCore::R4, 200},
                      {CpuCore::R6, 300},
                      {CpuCore::R7, 3}});

  // Each CPIR after the first is preceded by setting R7, which is not timed.
  state.code.AddValue(Encode("CPIR", CpuCore::R0, CpuCore::R1));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPIR", CpuCore::R0, CpuCore::R4));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPIR", CpuCore::R0, CpuCore::R6));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPIR", CpuCore::R0, CpuCore::R1));
  const uint16_t ip4 = state.code.AddNopGetAddress();

  // No match: two words that repeat, and the last that doesn't.
  EXPECT_EQ(CyclesUntilIp(ip1), 9 + 9 + 8);  // CPIR R0, (R1)
  EXPECT_EQ(state.r1, 103);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, 0);

  // A match stops early, past the match.
  ASSERT_TRUE(ExecuteUntilIp(setup2));   // MVQ R7, 3
  EXPECT_EQ(CyclesUntilIp(ip2), 9 + 9);  // CPIR R0, (R4)
  EXPECT_EQ(state.r4, 202);
  EXPECT_EQ(state.r7, 1);
  EXPECT_EQ(state.st, CpuCore::Z);

  // A match on the last word.
  ASSERT_TRUE(ExecuteUntilIp(setup3));   // MVQ R7, 2
  EXPECT_EQ(CyclesUntilIp(ip3), 9 + 8);  // CPIR R0, (R6)
  EXPECT_EQ(state.r6, 302);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::Z);

  // With R7 of 0, nothing is compared, and the flags are cleared.
  EXPECT_EQ(CyclesUntilIp(ip4), 5);  // CPIR R0, (R1)
  EXPECT_EQ(state.r1, 103);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, 0);
}

TEST_F(InstructionTest, CPDR) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(1).AddValue(2).AddValue(3);
  state.extra.SetAddress(state.be + 200).AddValue(7).AddValue(5).AddValue(1);
  state.stack.SetAddress(state.bs + 300).AddValue(5).AddValue(1);
  state.SetRegisters({{CpuCore::ST, 0},
                      {CpuCore::R0, 5},
                      {CpuCore::R1, 102},
                      {CpuCore::R4, 202},
                      {CpuCore::R6, 301},
                      {CpuCore::R7, 3}});

  // Each CPDR after the first is preceded by setting R7, which is not timed.
  state.code.AddValue(Encode("CPDR", CpuCore::R0, CpuCore::R1));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPDR", CpuCore::R0, CpuCore::R4));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPDR", CpuCore::R0, CpuCore::R6));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPDR", CpuCore::R0, CpuCore::R1));
  const uint16_t ip4 = state.code.AddNopGetAddress();

  // No match: two words that repeat, and the last that doesn't.
  EXPECT_EQ(CyclesUntilIp(ip1), 10 + 10 + 9);  // CPDR R0, (R1)
  EXPECT_EQ(state.r1, 99);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, 0);

  // A match stops early, past the match.
  ASSERT_TRUE(ExecuteUntilIp(setup2));     // MVQ R7, 3
  EXPECT_EQ(CyclesUntilIp(ip2), 10 + 10);  // CPDR R0, (R4)
  EXPECT_EQ(state.r4, 200);
  EXPECT_EQ(state.r7, 1);
  EXPECT_EQ(state.st, CpuCore::Z);

  // A match on the last word.
  ASSERT_TRUE(ExecuteUntilIp(setup3));    // MVQ R7, 2
  EXPECT_EQ(CyclesUntilIp(ip3), 10 + 9);  // CPDR R0, (R6)
  EXPECT_EQ(state.r6, 299);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::Z);

  // With R7 of 0, nothing is compared, and the flags are cleared.
  EXPECT_EQ(CyclesUntilIp(ip4), 5);  // CPDR R0, (R1)
  EXPECT_EQ(state.r1, 99);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, 0);
}

TEST_F(InstructionTest, CPIR_CPDR_Interrupt) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100)
      .AddValue(1)
      .AddValue(2)
      .AddValue(3)
      .AddValue(4);
  state.SetRegisters({{CpuCore::ST, CpuCore::I},
                      {CpuCore::R0, 5},
                      {CpuCore::R1, 100},
                      {CpuCore::R7, 3},
                      {CpuCore::SP, 500}});

  state.code.AddValue(Encode("SETI", "$v")).AddValue(1).AddValue(100);
  const uint16_t ip0 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPIR", CpuCore::R0, CpuCore::R1));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CPDR", CpuCore::R0, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.SetAddress(100);
  const uint16_t handler_ip = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IRT"));

  // Raises the interrupt once, during the first word.
  bool raised = false;
  auto raise_after_first_word = [&] {
    if (!raised && state.r7 == 2) {
      state.core.RaiseInterrupt(1);
      raised = true;
    }
  };

  // The interrupt is handled after the first word, and returns to CPIR.
  ASSERT_TRUE(ExecuteUntilIp(ip0));  // SETI 1, 100
  EXPECT_EQ(CyclesUntilIp(handler_ip, raise_after_first_word),
            9 + kCpuCoreStartInterruptCycles);  // CPIR R0, (R1)
  EXPECT_EQ(state.r1, 101);
  EXPECT_EQ(state.r7, 2);
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), CpuCore::I);
  EXPECT_EQ(state.stack.GetValue(), ip0);
  EXPECT_EQ(CyclesUntilIp(ip1), 6 + 9 + 8);  // IRT, CPIR R0, (R1)
  EXPECT_EQ(state.r1, 103);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::I);

  // The same for CPDR.
  raised = false;
  ASSERT_TRUE(ExecuteUntilIp(setup2));  // MVQ R7, 3
  EXPECT_EQ(CyclesUntilIp(handler_ip, raise_after_first_word),
            10 + kCpuCoreStartInterruptCycles);  // CPDR R0, (R1)
  EXPECT_EQ(state.r1, 102);
  EXPECT_EQ(state.r7, 2);
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), CpuCore::I);
  EXPECT_EQ(state.stack.GetValue(), setup2);
  EXPECT_EQ(CyclesUntilIp(ip2), 6 + 10 + 9);  // IRT, CPDR R0, (R1)
  EXPECT_EQ(state.r1, 100);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::I);
}

}  // namespace
}  // namespace oz3
