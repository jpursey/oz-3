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

TEST_F(InstructionTest, MVI) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(0x1234).AddValue(0x2345);
  state.extra.SetAddress(state.be + 201).AddValue(0x3456).AddValue(0x5678);
  state.stack.SetAddress(state.bs + 301).AddValue(0x4567);
  state.SetRegisters({{CpuCore::ST, CpuCore::ZSCO},
                      {CpuCore::R0, 100},
                      {CpuCore::R1, 110},
                      {CpuCore::R4, 200},
                      {CpuCore::R7, 300}});

  state.code.AddValue(Encode("MVI", CpuCore::R1, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVI", CpuCore::R4, CpuCore::R0));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVI", CpuCore::R7, CpuCore::R4));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVI", CpuCore::R0, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVI", CpuCore::R4, CpuCore::R4));
  const uint16_t ip5 = state.code.AddNopGetAddress();

  // DATA to DATA.
  EXPECT_EQ(CyclesUntilIp(ip1), 7);  // MVI (R1), (R0)
  EXPECT_EQ(state.data.SetAddress(state.bd + 110).GetValue(), 0x1234);
  EXPECT_EQ(state.r0, 101);
  EXPECT_EQ(state.r1, 111);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // DATA to EXTRA.
  EXPECT_EQ(CyclesUntilIp(ip2), 7);  // MVI (R4), (R0)
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 0x2345);
  EXPECT_EQ(state.r0, 102);
  EXPECT_EQ(state.r4, 201);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // EXTRA to STACK. R7 is an address like any other.
  EXPECT_EQ(CyclesUntilIp(ip3), 7);  // MVI (R7), (R4)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 300).GetValue(), 0x3456);
  EXPECT_EQ(state.r4, 202);
  EXPECT_EQ(state.r7, 301);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // STACK to DATA.
  EXPECT_EQ(CyclesUntilIp(ip4), 7);  // MVI (R0), (R7)
  EXPECT_EQ(state.data.SetAddress(state.bd + 102).GetValue(), 0x4567);
  EXPECT_EQ(state.r0, 103);
  EXPECT_EQ(state.r7, 302);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // The same register is stepped after the read, so the word is copied to the
  // next address.
  EXPECT_EQ(CyclesUntilIp(ip5), 7);  // MVI (R4), (R4)
  EXPECT_EQ(state.extra.SetAddress(state.be + 203).GetValue(), 0x5678);
  EXPECT_EQ(state.r4, 204);
  EXPECT_EQ(state.st, CpuCore::ZSCO);
}

TEST_F(InstructionTest, MVD) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 101).AddValue(0x5555).AddValue(0x1111);
  state.extra.SetAddress(state.be + 200).AddValue(0x4444).AddValue(0x3333);
  state.stack.SetAddress(state.bs + 301).AddValue(0x2222);
  state.SetRegisters({{CpuCore::ST, CpuCore::ZSCO},
                      {CpuCore::R0, 102},
                      {CpuCore::R1, 112},
                      {CpuCore::R4, 202},
                      {CpuCore::R5, 210},
                      {CpuCore::R7, 302}});

  state.code.AddValue(Encode("MVD", CpuCore::R7, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVD", CpuCore::R4, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVD", CpuCore::R1, CpuCore::R4));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVD", CpuCore::R5, CpuCore::R4));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVD", CpuCore::R0, CpuCore::R0));
  const uint16_t ip5 = state.code.AddNopGetAddress();

  // DATA to STACK. R7 is an address like any other.
  EXPECT_EQ(CyclesUntilIp(ip1), 9);  // MVD (R7), (R0)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 302).GetValue(), 0x1111);
  EXPECT_EQ(state.r0, 101);
  EXPECT_EQ(state.r7, 301);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // STACK to EXTRA.
  EXPECT_EQ(CyclesUntilIp(ip2), 9);  // MVD (R4), (R7)
  EXPECT_EQ(state.extra.SetAddress(state.be + 202).GetValue(), 0x2222);
  EXPECT_EQ(state.r4, 201);
  EXPECT_EQ(state.r7, 300);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // EXTRA to DATA.
  EXPECT_EQ(CyclesUntilIp(ip3), 9);  // MVD (R1), (R4)
  EXPECT_EQ(state.data.SetAddress(state.bd + 112).GetValue(), 0x3333);
  EXPECT_EQ(state.r1, 111);
  EXPECT_EQ(state.r4, 200);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // EXTRA to EXTRA.
  EXPECT_EQ(CyclesUntilIp(ip4), 9);  // MVD (R5), (R4)
  EXPECT_EQ(state.extra.SetAddress(state.be + 210).GetValue(), 0x4444);
  EXPECT_EQ(state.r4, 199);
  EXPECT_EQ(state.r5, 209);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // The same register is stepped after the read, so the word is copied to the
  // next address down.
  EXPECT_EQ(CyclesUntilIp(ip5), 9);  // MVD (R0), (R0)
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue(), 0x5555);
  EXPECT_EQ(state.r0, 99);
  EXPECT_EQ(state.st, CpuCore::ZSCO);
}

TEST_F(InstructionTest, MVIR) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100)
      .AddValue(1)
      .AddValue(2)
      .AddValue(3)
      .AddValue(8);
  state.data.SetAddress(state.bd + 110)
      .AddValue(4)
      .AddValue(5)
      .AddValue(6)
      .AddValue(7);
  state.SetRegisters({{CpuCore::ST, CpuCore::ZSCO},
                      {CpuCore::R0, 100},
                      {CpuCore::R1, 111},
                      {CpuCore::R2, 110},
                      {CpuCore::R3, 114},
                      {CpuCore::R4, 200},
                      {CpuCore::R7, 3}});

  // Each MVIR after the first is preceded by setting R7, which is not timed.
  state.code.AddValue(Encode("MVIR", CpuCore::R4, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVIR", CpuCore::R2, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVIR", CpuCore::R3, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVIR", CpuCore::R4, CpuCore::R0));
  const uint16_t ip4 = state.code.AddNopGetAddress();

  // Two words that repeat, and the last that doesn't.
  EXPECT_EQ(CyclesUntilIp(ip1), 10 + 10 + 9);  // MVIR (R4), (R0)
  state.extra.SetAddress(state.be + 200);
  EXPECT_EQ(state.extra.GetValue(), 1);
  EXPECT_EQ(state.extra.GetValue(), 2);
  EXPECT_EQ(state.extra.GetValue(), 3);
  EXPECT_EQ(state.r0, 103);
  EXPECT_EQ(state.r4, 203);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // Overlapping, moving the words down by one.
  ASSERT_TRUE(ExecuteUntilIp(setup2));         // MVQ R7, 3
  EXPECT_EQ(CyclesUntilIp(ip2), 10 + 10 + 9);  // MVIR (R2), (R1)
  state.data.SetAddress(state.bd + 110);
  EXPECT_EQ(state.data.GetValue(), 5);
  EXPECT_EQ(state.data.GetValue(), 6);
  EXPECT_EQ(state.data.GetValue(), 7);
  EXPECT_EQ(state.data.GetValue(), 7);
  EXPECT_EQ(state.r1, 114);
  EXPECT_EQ(state.r2, 113);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // Overlapping, moving the words up by one, which repeats the first word.
  ASSERT_TRUE(ExecuteUntilIp(setup3));         // MVQ R7, 3
  EXPECT_EQ(CyclesUntilIp(ip3), 10 + 10 + 9);  // MVIR (R3), (R2)
  state.data.SetAddress(state.bd + 113);
  EXPECT_EQ(state.data.GetValue(), 7);
  EXPECT_EQ(state.data.GetValue(), 7);
  EXPECT_EQ(state.data.GetValue(), 7);
  EXPECT_EQ(state.data.GetValue(), 7);
  EXPECT_EQ(state.r2, 116);
  EXPECT_EQ(state.r3, 117);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // With R7 of 0, nothing is copied.
  EXPECT_EQ(CyclesUntilIp(ip4), 5);  // MVIR (R4), (R0)
  EXPECT_EQ(state.extra.SetAddress(state.be + 203).GetValue(), 0);
  EXPECT_EQ(state.r0, 103);
  EXPECT_EQ(state.r4, 203);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::ZSCO);
}

TEST_F(InstructionTest, MVDR) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.stack.SetAddress(state.bs + 299)
      .AddValue(9)
      .AddValue(1)
      .AddValue(2)
      .AddValue(3);
  state.data.SetAddress(state.bd + 100).AddValue(4).AddValue(5).AddValue(6);
  state.SetRegisters({{CpuCore::ST, CpuCore::ZSCO},
                      {CpuCore::R0, 102},
                      {CpuCore::R1, 103},
                      {CpuCore::R5, 202},
                      {CpuCore::R6, 302},
                      {CpuCore::R7, 3}});

  // Each MVDR after the first is preceded by setting R7, which is not timed.
  state.code.AddValue(Encode("MVDR", CpuCore::R5, CpuCore::R6));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVDR", CpuCore::R1, CpuCore::R0));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVDR", CpuCore::R5, CpuCore::R6));
  const uint16_t ip3 = state.code.AddNopGetAddress();

  // Two words that repeat, and the last that doesn't.
  EXPECT_EQ(CyclesUntilIp(ip1), 12 + 12 + 11);  // MVDR (R5), (R6)
  state.extra.SetAddress(state.be + 200);
  EXPECT_EQ(state.extra.GetValue(), 1);
  EXPECT_EQ(state.extra.GetValue(), 2);
  EXPECT_EQ(state.extra.GetValue(), 3);
  EXPECT_EQ(state.r5, 199);
  EXPECT_EQ(state.r6, 299);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // Overlapping, moving the words up by one.
  ASSERT_TRUE(ExecuteUntilIp(setup2));          // MVQ R7, 3
  EXPECT_EQ(CyclesUntilIp(ip2), 12 + 12 + 11);  // MVDR (R1), (R0)
  state.data.SetAddress(state.bd + 100);
  EXPECT_EQ(state.data.GetValue(), 4);
  EXPECT_EQ(state.data.GetValue(), 4);
  EXPECT_EQ(state.data.GetValue(), 5);
  EXPECT_EQ(state.data.GetValue(), 6);
  EXPECT_EQ(state.r0, 99);
  EXPECT_EQ(state.r1, 100);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  // With R7 of 0, nothing is copied.
  EXPECT_EQ(CyclesUntilIp(ip3), 5);  // MVDR (R5), (R6)
  EXPECT_EQ(state.extra.SetAddress(state.be + 199).GetValue(), 0);
  EXPECT_EQ(state.r5, 199);
  EXPECT_EQ(state.r6, 299);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::ZSCO);
}

TEST_F(InstructionTest, MVDR_R7) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();

  // The stack is moved away from the code, so R7 can address low words in it.
  state.SetRegisters({{CpuCore::ST, CpuCore::ZSCO},
                      {CpuCore::R1, 100},
                      {CpuCore::R7, 2},
                      {CpuCore::BS, 3000}});
  state.stack.SetAddress(state.bs + 1).AddValue(8).AddValue(9);

  state.code.AddValue(Encode("MVDR", CpuCore::R1, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();

  // As the address, R7 is stepped down and then decremented, so from 2 it
  // stops after one word.
  EXPECT_EQ(CyclesUntilIp(ip1), 11);  // MVDR (R1), (R7)
  state.data.SetAddress(state.bd + 99);
  EXPECT_EQ(state.data.GetValue(), 0);
  EXPECT_EQ(state.data.GetValue(), 9);
  EXPECT_EQ(state.r1, 99);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, CpuCore::ZSCO);
}

TEST_F(InstructionTest, MVIR_MVDR_Interrupt) {
  ASSERT_TRUE(InitAndReset());
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(1).AddValue(2).AddValue(3);
  state.SetRegisters({{CpuCore::ST, CpuCore::I},
                      {CpuCore::R0, 100},
                      {CpuCore::R1, 112},
                      {CpuCore::R4, 200},
                      {CpuCore::R5, 202},
                      {CpuCore::R7, 3},
                      {CpuCore::SP, 500}});

  state.code.AddValue(Encode("SETI", "$v")).AddValue(1).AddValue(100);
  const uint16_t ip0 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVIR", CpuCore::R4, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVDR", CpuCore::R1, CpuCore::R5));
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

  // The interrupt is handled after the first word, and returns to MVIR.
  ASSERT_TRUE(ExecuteUntilIp(ip0));  // SETI 1, 100
  EXPECT_EQ(CyclesUntilIp(handler_ip, raise_after_first_word),
            10 + kCpuCoreStartInterruptCycles);  // MVIR (R4), (R0)
  EXPECT_EQ(state.r0, 101);
  EXPECT_EQ(state.r4, 201);
  EXPECT_EQ(state.r7, 2);
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), CpuCore::I);
  EXPECT_EQ(state.stack.GetValue(), ip0);
  EXPECT_EQ(CyclesUntilIp(ip1), 6 + 10 + 9);  // IRT, MVIR (R4), (R0)
  state.extra.SetAddress(state.be + 200);
  EXPECT_EQ(state.extra.GetValue(), 1);
  EXPECT_EQ(state.extra.GetValue(), 2);
  EXPECT_EQ(state.extra.GetValue(), 3);
  EXPECT_EQ(state.r0, 103);
  EXPECT_EQ(state.r4, 203);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::I);

  // The same for MVDR, copying the words back to the DATA bank.
  raised = false;
  ASSERT_TRUE(ExecuteUntilIp(setup2));  // MVQ R7, 3
  EXPECT_EQ(CyclesUntilIp(handler_ip, raise_after_first_word),
            12 + kCpuCoreStartInterruptCycles);  // MVDR (R1), (R5)
  EXPECT_EQ(state.r1, 111);
  EXPECT_EQ(state.r5, 201);
  EXPECT_EQ(state.r7, 2);
  EXPECT_EQ(state.sp, 498);
  EXPECT_EQ(state.stack.SetAddress(state.bs + state.sp).GetValue(), CpuCore::I);
  EXPECT_EQ(state.stack.GetValue(), setup2);
  EXPECT_EQ(CyclesUntilIp(ip2), 6 + 12 + 11);  // IRT, MVDR (R1), (R5)
  state.data.SetAddress(state.bd + 110);
  EXPECT_EQ(state.data.GetValue(), 1);
  EXPECT_EQ(state.data.GetValue(), 2);
  EXPECT_EQ(state.data.GetValue(), 3);
  EXPECT_EQ(state.r1, 109);
  EXPECT_EQ(state.r5, 199);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.sp, 500);
  EXPECT_EQ(state.st, CpuCore::I);
}

}  // namespace
}  // namespace oz3
