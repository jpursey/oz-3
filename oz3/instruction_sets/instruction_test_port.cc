// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "gmock/gmock.h"
#include "oz3/instruction_sets/instruction_test.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;

constexpr uint16_t ZC = CpuCore::Z | CpuCore::C;
constexpr uint16_t ZSC = CpuCore::Z | CpuCore::S | CpuCore::C;

TEST_F(InstructionTest, IN_RW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R0, 1},    // Port
                      {CpuCore::R2, 100},  // For "($r)"
                      {CpuCore::R4, 150},  // For "($r + $v)", v == 50
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 510}});

  state.code.AddValue(Encode("IN.RW", CpuCore::R0, {"$r", CpuCore::R1}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RW", CpuCore::R0, {"($r)", CpuCore::R2}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RW", CpuCore::R0, {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RW", CpuCore::R0, "(SP)"));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RW", CpuCore::R0, "(SP + $v)")).AddValue(2);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RW", CpuCore::R0, "(FP)"));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RW", CpuCore::R0, "(FP + $v)")).AddValue(-2);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RW", CpuCore::R0, "S($v)")).AddValue(520);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RW", CpuCore::R0, "D($v)")).AddValue(300);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RW", CpuCore::R0, "E($v)")).AddValue(400);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RW", CpuCore::R0, {"$r", CpuCore::R3}));
  const uint16_t ip11 = state.code.AddNopGetAddress();

  WritePort(1, 1);
  EXPECT_EQ(CyclesUntilIp(ip1), 4);  // IN.RW R0, R1
  EXPECT_EQ(state.r1, 1);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  WritePort(1, 2);
  EXPECT_EQ(CyclesUntilIp(ip2), 6);  // IN.RW R0, (R2)
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue(), 2);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 3);
  EXPECT_EQ(CyclesUntilIp(ip3), 8);  // IN.RW R0, (R4 + 50)
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 3);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 4);
  EXPECT_EQ(CyclesUntilIp(ip4), 6);  // IN.RW R0, (SP)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 500).GetValue(), 4);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 5);
  EXPECT_EQ(CyclesUntilIp(ip5), 8);  // IN.RW R0, (SP + 2)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 502).GetValue(), 5);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 6);
  EXPECT_EQ(CyclesUntilIp(ip6), 6);  // IN.RW R0, (FP)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 510).GetValue(), 6);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 7);
  EXPECT_EQ(CyclesUntilIp(ip7), 8);  // IN.RW R0, (FP - 2)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 508).GetValue(), 7);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 8);
  EXPECT_EQ(CyclesUntilIp(ip8), 7);  // IN.RW R0, S(520)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 520).GetValue(), 8);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 9);
  EXPECT_EQ(CyclesUntilIp(ip9), 7);  // IN.RW R0, D(300)
  EXPECT_EQ(state.data.SetAddress(state.bd + 300).GetValue(), 9);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 10);
  EXPECT_EQ(CyclesUntilIp(ip10), 7);  // IN.RW R0, E(400)
  EXPECT_EQ(state.extra.SetAddress(state.be + 400).GetValue(), 10);
  EXPECT_EQ(state.st, ZSC);

  // Without the port status set, the port is still read, but S is cleared.
  EXPECT_EQ(CyclesUntilIp(ip11), 4);  // IN.RW R0, R3
  EXPECT_EQ(state.r3, 10);
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);
}

TEST_F(InstructionTest, IN_IW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}});

  state.code.AddValue(Encode("IN.IW", {"$r", CpuCore::R1})).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.IW", {"($r + $v)", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  WritePort(1, 1);
  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // IN.IW 1, R1
  EXPECT_EQ(state.r1, 1);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  WritePort(1, 2);
  EXPECT_EQ(CyclesUntilIp(ip2), 9);  // IN.IW 1, (R4 + 50)
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 2);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);
}

TEST_F(InstructionTest, IN_RD) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R5, 1},    // Port
                      {CpuCore::R2, 100},  // For "[$r]"
                      {CpuCore::R4, 150},  // For "[$r + $v]", v == 50
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 510}});

  state.code.AddValue(Encode("IN.RD", CpuCore::R5, {"$R", 0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RD", CpuCore::R5, {"[$r]", CpuCore::R2}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RD", CpuCore::R5, {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RD", CpuCore::R5, "[SP]"));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RD", CpuCore::R5, "[SP + $v]")).AddValue(2);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RD", CpuCore::R5, "[FP]"));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RD", CpuCore::R5, "[FP + $v]")).AddValue(-4);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RD", CpuCore::R5, "S[$v]")).AddValue(520);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RD", CpuCore::R5, "D[$v]")).AddValue(300);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RD", CpuCore::R5, "E[$v]")).AddValue(400);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.RD", CpuCore::R5, {"$R", 3}));
  const uint16_t ip11 = state.code.AddNopGetAddress();

  WritePort32(1, 0x10002);
  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // IN.RD R5, D0
  EXPECT_EQ(state.d0(), 0x10002);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  WritePort32(1, 0x30004);
  EXPECT_EQ(CyclesUntilIp(ip2), 8);  // IN.RD R5, [R2]
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue32(), 0x30004);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0x50006);
  EXPECT_EQ(CyclesUntilIp(ip3), 10);  // IN.RD R5, [R4 + 50]
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0x50006);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0x70008);
  EXPECT_EQ(CyclesUntilIp(ip4), 8);  // IN.RD R5, [SP]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 500).GetValue32(), 0x70008);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0x9000A);
  EXPECT_EQ(CyclesUntilIp(ip5), 10);  // IN.RD R5, [SP + 2]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 502).GetValue32(), 0x9000A);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0xB000C);
  EXPECT_EQ(CyclesUntilIp(ip6), 8);  // IN.RD R5, [FP]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 510).GetValue32(), 0xB000C);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0xD000E);
  EXPECT_EQ(CyclesUntilIp(ip7), 10);  // IN.RD R5, [FP - 4]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 506).GetValue32(), 0xD000E);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0xF0010);
  EXPECT_EQ(CyclesUntilIp(ip8), 9);  // IN.RD R5, S[520]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 520).GetValue32(), 0xF0010);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0x110012);
  EXPECT_EQ(CyclesUntilIp(ip9), 9);  // IN.RD R5, D[300]
  EXPECT_EQ(state.data.SetAddress(state.bd + 300).GetValue32(), 0x110012);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0x130014);
  EXPECT_EQ(CyclesUntilIp(ip10), 9);  // IN.RD R5, E[400]
  EXPECT_EQ(state.extra.SetAddress(state.be + 400).GetValue32(), 0x130014);
  EXPECT_EQ(state.st, ZSC);

  // Without the port status set, the port is still read, but S is cleared.
  EXPECT_EQ(CyclesUntilIp(ip11), 5);  // IN.RD R5, D3
  EXPECT_EQ(state.d3(), 0x130014);
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);
}

TEST_F(InstructionTest, IN_ID) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}});

  state.code.AddValue(Encode("IN.ID", {"$R", 1})).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("IN.ID", {"[$r + $v]", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  WritePort32(1, 0x10002);
  EXPECT_EQ(CyclesUntilIp(ip1), 6);  // IN.ID 1, D1
  EXPECT_EQ(state.d1(), 0x10002);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  WritePort32(1, 0x30004);
  EXPECT_EQ(CyclesUntilIp(ip2), 11);  // IN.ID 1, [R4 + 50]
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0x30004);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);
}

TEST_F(InstructionTest, INS_RW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R0, 1},    // Port
                      {CpuCore::R2, 100},  // For "($r)"
                      {CpuCore::R3, 55},   // For "$r" when not ready
                      {CpuCore::R4, 150},  // For "($r + $v)", v == 50
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 510}});

  // Port ready
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, {"($r)", CpuCore::R2}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "(SP)"));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "(SP + $v)")).AddValue(2);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "(FP)"));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "(FP + $v)")).AddValue(-2);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "S($v)")).AddValue(520);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "D($v)")).AddValue(300);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "E($v)")).AddValue(400);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, {"$r", CpuCore::R1}));
  const uint16_t ip10 = state.code.AddNopGetAddress();

  // Port not ready
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, {"($r)", CpuCore::R2}));
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "(SP)"));
  const uint16_t ip13 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "(SP + $v)")).AddValue(2);
  const uint16_t ip14 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "(FP)"));
  const uint16_t ip15 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "(FP + $v)")).AddValue(-2);
  const uint16_t ip16 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "S($v)")).AddValue(520);
  const uint16_t ip17 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "D($v)")).AddValue(300);
  const uint16_t ip18 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, "E($v)")).AddValue(400);
  const uint16_t ip19 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RW", CpuCore::R0, {"$r", CpuCore::R3}));
  const uint16_t ip20 = state.code.AddNopGetAddress();

  WritePort(1, 1);
  EXPECT_EQ(CyclesUntilIp(ip1), 7);  // INS.RW R0, (R2)
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue(), 1);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  WritePort(1, 2);
  EXPECT_EQ(CyclesUntilIp(ip2), 9);  // INS.RW R0, (R4 + 50)
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 2);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 3);
  EXPECT_EQ(CyclesUntilIp(ip3), 7);  // INS.RW R0, (SP)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 500).GetValue(), 3);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 4);
  EXPECT_EQ(CyclesUntilIp(ip4), 9);  // INS.RW R0, (SP + 2)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 502).GetValue(), 4);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 5);
  EXPECT_EQ(CyclesUntilIp(ip5), 7);  // INS.RW R0, (FP)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 510).GetValue(), 5);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 6);
  EXPECT_EQ(CyclesUntilIp(ip6), 9);  // INS.RW R0, (FP - 2)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 508).GetValue(), 6);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 7);
  EXPECT_EQ(CyclesUntilIp(ip7), 8);  // INS.RW R0, S(520)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 520).GetValue(), 7);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 8);
  EXPECT_EQ(CyclesUntilIp(ip8), 8);  // INS.RW R0, D(300)
  EXPECT_EQ(state.data.SetAddress(state.bd + 300).GetValue(), 8);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 9);
  EXPECT_EQ(CyclesUntilIp(ip9), 8);  // INS.RW R0, E(400)
  EXPECT_EQ(state.extra.SetAddress(state.be + 400).GetValue(), 9);
  EXPECT_EQ(state.st, ZSC);

  WritePort(1, 10);
  EXPECT_EQ(CyclesUntilIp(ip10), 4);  // INS.RW R0, R1
  EXPECT_EQ(state.r1, 10);
  EXPECT_EQ(state.st, ZSC);

  // Without the port status set, nothing is written, and S is cleared. The
  // port still holds 10, so any write would show.
  EXPECT_EQ(CyclesUntilIp(ip11), 4);  // INS.RW R0, (R2)
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue(), 1);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip12), 5);  // INS.RW R0, (R4 + 50)
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 2);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip13), 4);  // INS.RW R0, (SP)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 500).GetValue(), 3);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip14), 5);  // INS.RW R0, (SP + 2)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 502).GetValue(), 4);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip15), 4);  // INS.RW R0, (FP)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 510).GetValue(), 5);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip16), 5);  // INS.RW R0, (FP - 2)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 508).GetValue(), 6);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip17), 5);  // INS.RW R0, S(520)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 520).GetValue(), 7);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip18), 5);  // INS.RW R0, D(300)
  EXPECT_EQ(state.data.SetAddress(state.bd + 300).GetValue(), 8);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip19), 5);  // INS.RW R0, E(400)
  EXPECT_EQ(state.extra.SetAddress(state.be + 400).GetValue(), 9);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip20), 4);  // INS.RW R0, R3
  EXPECT_EQ(state.r3, 55);
  EXPECT_EQ(state.st, ZC);
}

TEST_F(InstructionTest, INS_IW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}});

  state.code.AddValue(Encode("INS.IW", {"$r", CpuCore::R1})).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.IW", {"($r + $v)", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.IW", {"($r + $v)", CpuCore::R4}))
      .AddValue(1)
      .AddValue(60);
  const uint16_t ip3 = state.code.AddNopGetAddress();

  WritePort(1, 1);
  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // INS.IW 1, R1
  EXPECT_EQ(state.r1, 1);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  WritePort(1, 2);
  EXPECT_EQ(CyclesUntilIp(ip2), 10);  // INS.IW 1, (R4 + 50)
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 2);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  // Not ready, so nothing is written.
  EXPECT_EQ(CyclesUntilIp(ip3), 6);  // INS.IW 1, (R4 + 60)
  EXPECT_EQ(state.extra.SetAddress(state.be + 210).GetValue(), 0);
  EXPECT_EQ(state.st, ZC);
}

TEST_F(InstructionTest, INS_RD) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R5, 1},     // Port
                      {CpuCore::R2, 100},   // For "[$r]"
                      {CpuCore::R4, 150},   // For "[$r + $v]", v == 50
                      {CpuCore::R6, 0x55},  // For "$R" when not ready
                      {CpuCore::R7, 0x66},  // For "$R" when not ready
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 510}});

  // Port ready
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, {"[$r]", CpuCore::R2}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "[SP]"));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "[SP + $v]")).AddValue(2);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "[FP]"));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "[FP + $v]")).AddValue(-4);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "S[$v]")).AddValue(520);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "D[$v]")).AddValue(300);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "E[$v]")).AddValue(400);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, {"$R", 0}));
  const uint16_t ip10 = state.code.AddNopGetAddress();

  // Port not ready
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, {"[$r]", CpuCore::R2}));
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "[SP]"));
  const uint16_t ip13 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "[SP + $v]")).AddValue(2);
  const uint16_t ip14 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "[FP]"));
  const uint16_t ip15 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "[FP + $v]")).AddValue(-4);
  const uint16_t ip16 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "S[$v]")).AddValue(520);
  const uint16_t ip17 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "D[$v]")).AddValue(300);
  const uint16_t ip18 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, "E[$v]")).AddValue(400);
  const uint16_t ip19 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.RD", CpuCore::R5, {"$R", 3}));
  const uint16_t ip20 = state.code.AddNopGetAddress();

  WritePort32(1, 0x10002);
  EXPECT_EQ(CyclesUntilIp(ip1), 9);  // INS.RD R5, [R2]
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue32(), 0x10002);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  WritePort32(1, 0x30004);
  EXPECT_EQ(CyclesUntilIp(ip2), 11);  // INS.RD R5, [R4 + 50]
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0x30004);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0x50006);
  EXPECT_EQ(CyclesUntilIp(ip3), 9);  // INS.RD R5, [SP]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 500).GetValue32(), 0x50006);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0x70008);
  EXPECT_EQ(CyclesUntilIp(ip4), 11);  // INS.RD R5, [SP + 2]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 502).GetValue32(), 0x70008);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0x9000A);
  EXPECT_EQ(CyclesUntilIp(ip5), 9);  // INS.RD R5, [FP]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 510).GetValue32(), 0x9000A);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0xB000C);
  EXPECT_EQ(CyclesUntilIp(ip6), 11);  // INS.RD R5, [FP - 4]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 506).GetValue32(), 0xB000C);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0xD000E);
  EXPECT_EQ(CyclesUntilIp(ip7), 10);  // INS.RD R5, S[520]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 520).GetValue32(), 0xD000E);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0xF0010);
  EXPECT_EQ(CyclesUntilIp(ip8), 10);  // INS.RD R5, D[300]
  EXPECT_EQ(state.data.SetAddress(state.bd + 300).GetValue32(), 0xF0010);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0x110012);
  EXPECT_EQ(CyclesUntilIp(ip9), 10);  // INS.RD R5, E[400]
  EXPECT_EQ(state.extra.SetAddress(state.be + 400).GetValue32(), 0x110012);
  EXPECT_EQ(state.st, ZSC);

  WritePort32(1, 0x130014);
  EXPECT_EQ(CyclesUntilIp(ip10), 5);  // INS.RD R5, D0
  EXPECT_EQ(state.d0(), 0x130014);
  EXPECT_EQ(state.st, ZSC);

  // Without the port status set, nothing is written, and S is cleared. The
  // port still holds 0x130014, so any write would show.
  EXPECT_EQ(CyclesUntilIp(ip11), 5);  // INS.RD R5, [R2]
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue32(), 0x10002);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip12), 6);  // INS.RD R5, [R4 + 50]
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0x30004);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip13), 5);  // INS.RD R5, [SP]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 500).GetValue32(), 0x50006);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip14), 6);  // INS.RD R5, [SP + 2]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 502).GetValue32(), 0x70008);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip15), 5);  // INS.RD R5, [FP]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 510).GetValue32(), 0x9000A);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip16), 6);  // INS.RD R5, [FP - 4]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 506).GetValue32(), 0xB000C);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip17), 6);  // INS.RD R5, S[520]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 520).GetValue32(), 0xD000E);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip18), 6);  // INS.RD R5, D[300]
  EXPECT_EQ(state.data.SetAddress(state.bd + 300).GetValue32(), 0xF0010);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip19), 6);  // INS.RD R5, E[400]
  EXPECT_EQ(state.extra.SetAddress(state.be + 400).GetValue32(), 0x110012);
  EXPECT_EQ(state.st, ZC);

  EXPECT_EQ(CyclesUntilIp(ip20), 5);  // INS.RD R5, D3
  EXPECT_EQ(state.d3(), 0x660055);
  EXPECT_EQ(state.st, ZC);
}

TEST_F(InstructionTest, INS_ID) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}});

  state.code.AddValue(Encode("INS.ID", {"$R", 1})).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.ID", {"[$r + $v]", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INS.ID", {"[$r + $v]", CpuCore::R4}))
      .AddValue(1)
      .AddValue(60);
  const uint16_t ip3 = state.code.AddNopGetAddress();

  WritePort32(1, 0x10002);
  EXPECT_EQ(CyclesUntilIp(ip1), 6);  // INS.ID 1, D1
  EXPECT_EQ(state.d1(), 0x10002);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  WritePort32(1, 0x30004);
  EXPECT_EQ(CyclesUntilIp(ip2), 12);  // INS.ID 1, [R4 + 50]
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0x30004);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  // Not ready, so nothing is written.
  EXPECT_EQ(CyclesUntilIp(ip3), 7);  // INS.ID 1, [R4 + 60]
  EXPECT_EQ(state.extra.SetAddress(state.be + 210).GetValue32(), 0);
  EXPECT_EQ(state.st, ZC);
}

TEST_F(InstructionTest, INR_RW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R0, 1},    // Port
                      {CpuCore::R2, 100},  // For "($r)"
                      {CpuCore::R4, 150},  // For "($r + $v)", v == 50
                      {CpuCore::R7, 2},    // Words to read
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 520}});

  // Each INR after the first is preceded by setting R7, which is not timed.
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, {"($r)", CpuCore::R2}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, "(SP)"));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, "(SP + $v)")).AddValue(4);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, "(FP)"));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, "(FP + $v)")).AddValue(-8);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, "S($v)")).AddValue(530);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, "D($v)")).AddValue(300);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, "E($v)")).AddValue(400);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, "D($v)")).AddValue(600);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  const uint16_t setup11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RW", CpuCore::R0, "D($v)")).AddValue(700);
  const uint16_t ip11 = state.code.AddNopGetAddress();

  PortFeeder feeder1(this, 1, PortSize::kWord, {1, 2});
  EXPECT_EQ(CyclesUntilIp(ip1, [&] { feeder1.Update(); }),
            13);  // INR.RW R0, (R2)
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue(), 1);
  EXPECT_EQ(state.data.GetValue(), 2);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup2));  // MVQ.LW R7, 2
  PortFeeder feeder2(this, 1, PortSize::kWord, {3, 4});
  EXPECT_EQ(CyclesUntilIp(ip2, [&] { feeder2.Update(); }),
            14);  // INR.RW R0, (R4 + 50)
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 3);
  EXPECT_EQ(state.extra.GetValue(), 4);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup3));  // MVQ.LW R7, 2
  PortFeeder feeder3(this, 1, PortSize::kWord, {5, 6});
  EXPECT_EQ(CyclesUntilIp(ip3, [&] { feeder3.Update(); }),
            13);  // INR.RW R0, (SP)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 500).GetValue(), 5);
  EXPECT_EQ(state.stack.GetValue(), 6);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup4));  // MVQ.LW R7, 2
  PortFeeder feeder4(this, 1, PortSize::kWord, {7, 8});
  EXPECT_EQ(CyclesUntilIp(ip4, [&] { feeder4.Update(); }),
            14);  // INR.RW R0, (SP + 4)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 504).GetValue(), 7);
  EXPECT_EQ(state.stack.GetValue(), 8);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup5));  // MVQ.LW R7, 2
  PortFeeder feeder5(this, 1, PortSize::kWord, {9, 10});
  EXPECT_EQ(CyclesUntilIp(ip5, [&] { feeder5.Update(); }),
            13);  // INR.RW R0, (FP)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 520).GetValue(), 9);
  EXPECT_EQ(state.stack.GetValue(), 10);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup6));  // MVQ.LW R7, 2
  PortFeeder feeder6(this, 1, PortSize::kWord, {11, 12});
  EXPECT_EQ(CyclesUntilIp(ip6, [&] { feeder6.Update(); }),
            14);  // INR.RW R0, (FP - 8)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 512).GetValue(), 11);
  EXPECT_EQ(state.stack.GetValue(), 12);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup7));  // MVQ.LW R7, 2
  PortFeeder feeder7(this, 1, PortSize::kWord, {13, 14});
  EXPECT_EQ(CyclesUntilIp(ip7, [&] { feeder7.Update(); }),
            13);  // INR.RW R0, S(530)
  EXPECT_EQ(state.stack.SetAddress(state.bs + 530).GetValue(), 13);
  EXPECT_EQ(state.stack.GetValue(), 14);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup8));  // MVQ.LW R7, 2
  PortFeeder feeder8(this, 1, PortSize::kWord, {15, 16});
  EXPECT_EQ(CyclesUntilIp(ip8, [&] { feeder8.Update(); }),
            13);  // INR.RW R0, D(300)
  EXPECT_EQ(state.data.SetAddress(state.bd + 300).GetValue(), 15);
  EXPECT_EQ(state.data.GetValue(), 16);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup9));  // MVQ.LW R7, 2
  PortFeeder feeder9(this, 1, PortSize::kWord, {17, 18});
  EXPECT_EQ(CyclesUntilIp(ip9, [&] { feeder9.Update(); }),
            13);  // INR.RW R0, E(400)
  EXPECT_EQ(state.extra.SetAddress(state.be + 400).GetValue(), 17);
  EXPECT_EQ(state.extra.GetValue(), 18);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  // The port runs dry after one word, leaving R7 at the words not read.
  ASSERT_TRUE(ExecuteUntilIp(setup10));  // MVQ.LW R7, 3
  PortFeeder feeder10(this, 1, PortSize::kWord, {19});
  EXPECT_EQ(CyclesUntilIp(ip10, [&] { feeder10.Update(); }),
            11);  // INR.RW R0, D(600)
  EXPECT_EQ(state.data.SetAddress(state.bd + 600).GetValue(), 19);
  EXPECT_EQ(state.data.GetValue(), 0);
  EXPECT_EQ(state.r7, 2);
  EXPECT_EQ(state.st, ZC);

  // Nothing is read when R7 is zero, even with the port ready.
  ASSERT_TRUE(ExecuteUntilIp(setup11));  // MVQ.LW R7, 0
  WritePort(1, 20);
  EXPECT_EQ(CyclesUntilIp(ip11), 6);  // INR.RW R0, D(700)
  EXPECT_EQ(state.data.SetAddress(state.bd + 700).GetValue(), 0);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
}

TEST_F(InstructionTest, INR_IW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}, {CpuCore::R7, 2}});

  state.code.AddValue(Encode("INR.IW", {"($r + $v)", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip1 = state.code.AddNopGetAddress();

  PortFeeder feeder(this, 1, PortSize::kWord, {1, 2});
  EXPECT_EQ(CyclesUntilIp(ip1, [&] { feeder.Update(); }),
            15);  // INR.IW 1, (R4 + 50)
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue(), 1);
  EXPECT_EQ(state.extra.GetValue(), 2);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);
}

TEST_F(InstructionTest, INR_RD) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R5, 1},    // Port
                      {CpuCore::R2, 100},  // For "[$r]"
                      {CpuCore::R4, 150},  // For "[$r + $v]", v == 50
                      {CpuCore::R7, 2},    // Dwords to read
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 520}});

  // Each INR after the first is preceded by setting R7, which is not timed.
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, {"[$r]", CpuCore::R2}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, "[SP]"));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, "[SP + $v]")).AddValue(4);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, "[FP]"));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, "[FP + $v]")).AddValue(-8);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, "S[$v]")).AddValue(530);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, "D[$v]")).AddValue(300);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, "E[$v]")).AddValue(400);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, "D[$v]")).AddValue(600);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  const uint16_t setup11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("INR.RD", CpuCore::R5, "D[$v]")).AddValue(700);
  const uint16_t ip11 = state.code.AddNopGetAddress();

  PortFeeder feeder1(this, 1, PortSize::kDword, {0x10002, 0x30004});
  EXPECT_EQ(CyclesUntilIp(ip1, [&] { feeder1.Update(); }),
            17);  // INR.RD R5, [R2]
  EXPECT_EQ(state.data.SetAddress(state.bd + 100).GetValue32(), 0x10002);
  EXPECT_EQ(state.data.GetValue32(), 0x30004);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup2));  // MVQ.LW R7, 2
  PortFeeder feeder2(this, 1, PortSize::kDword, {0x50006, 0x70008});
  EXPECT_EQ(CyclesUntilIp(ip2, [&] { feeder2.Update(); }),
            18);  // INR.RD R5, [R4 + 50]
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0x50006);
  EXPECT_EQ(state.extra.GetValue32(), 0x70008);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup3));  // MVQ.LW R7, 2
  PortFeeder feeder3(this, 1, PortSize::kDword, {0x9000A, 0xB000C});
  EXPECT_EQ(CyclesUntilIp(ip3, [&] { feeder3.Update(); }),
            17);  // INR.RD R5, [SP]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 500).GetValue32(), 0x9000A);
  EXPECT_EQ(state.stack.GetValue32(), 0xB000C);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup4));  // MVQ.LW R7, 2
  PortFeeder feeder4(this, 1, PortSize::kDword, {0xD000E, 0xF0010});
  EXPECT_EQ(CyclesUntilIp(ip4, [&] { feeder4.Update(); }),
            18);  // INR.RD R5, [SP + 4]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 504).GetValue32(), 0xD000E);
  EXPECT_EQ(state.stack.GetValue32(), 0xF0010);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup5));  // MVQ.LW R7, 2
  PortFeeder feeder5(this, 1, PortSize::kDword, {0x110012, 0x130014});
  EXPECT_EQ(CyclesUntilIp(ip5, [&] { feeder5.Update(); }),
            17);  // INR.RD R5, [FP]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 520).GetValue32(), 0x110012);
  EXPECT_EQ(state.stack.GetValue32(), 0x130014);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup6));  // MVQ.LW R7, 2
  PortFeeder feeder6(this, 1, PortSize::kDword, {0x150016, 0x170018});
  EXPECT_EQ(CyclesUntilIp(ip6, [&] { feeder6.Update(); }),
            18);  // INR.RD R5, [FP - 8]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 512).GetValue32(), 0x150016);
  EXPECT_EQ(state.stack.GetValue32(), 0x170018);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup7));  // MVQ.LW R7, 2
  PortFeeder feeder7(this, 1, PortSize::kDword, {0x19001A, 0x1B001C});
  EXPECT_EQ(CyclesUntilIp(ip7, [&] { feeder7.Update(); }),
            17);  // INR.RD R5, S[530]
  EXPECT_EQ(state.stack.SetAddress(state.bs + 530).GetValue32(), 0x19001A);
  EXPECT_EQ(state.stack.GetValue32(), 0x1B001C);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup8));  // MVQ.LW R7, 2
  PortFeeder feeder8(this, 1, PortSize::kDword, {0x1D001E, 0x1F0020});
  EXPECT_EQ(CyclesUntilIp(ip8, [&] { feeder8.Update(); }),
            17);  // INR.RD R5, D[300]
  EXPECT_EQ(state.data.SetAddress(state.bd + 300).GetValue32(), 0x1D001E);
  EXPECT_EQ(state.data.GetValue32(), 0x1F0020);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup9));  // MVQ.LW R7, 2
  PortFeeder feeder9(this, 1, PortSize::kDword, {0x210022, 0x230024});
  EXPECT_EQ(CyclesUntilIp(ip9, [&] { feeder9.Update(); }),
            17);  // INR.RD R5, E[400]
  EXPECT_EQ(state.extra.SetAddress(state.be + 400).GetValue32(), 0x210022);
  EXPECT_EQ(state.extra.GetValue32(), 0x230024);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  // The port runs dry after one dword, leaving R7 at the dwords not read.
  ASSERT_TRUE(ExecuteUntilIp(setup10));  // MVQ.LW R7, 3
  PortFeeder feeder10(this, 1, PortSize::kDword, {0x250026});
  EXPECT_EQ(CyclesUntilIp(ip10, [&] { feeder10.Update(); }),
            14);  // INR.RD R5, D[600]
  EXPECT_EQ(state.data.SetAddress(state.bd + 600).GetValue32(), 0x250026);
  EXPECT_EQ(state.data.GetValue32(), 0);
  EXPECT_EQ(state.r7, 2);
  EXPECT_EQ(state.st, ZC);

  // Nothing is read when R7 is zero, even with the port ready.
  ASSERT_TRUE(ExecuteUntilIp(setup11));  // MVQ.LW R7, 0
  WritePort32(1, 0x270028);
  EXPECT_EQ(CyclesUntilIp(ip11), 6);  // INR.RD R5, D[700]
  EXPECT_EQ(state.data.SetAddress(state.bd + 700).GetValue32(), 0);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
}

TEST_F(InstructionTest, INR_ID) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}, {CpuCore::R7, 2}});

  state.code.AddValue(Encode("INR.ID", {"[$r + $v]", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip1 = state.code.AddNopGetAddress();

  PortFeeder feeder(this, 1, PortSize::kDword, {0x10002, 0x30004});
  EXPECT_EQ(CyclesUntilIp(ip1, [&] { feeder.Update(); }),
            19);  // INR.ID 1, [R4 + 50]
  EXPECT_EQ(state.extra.SetAddress(state.be + 200).GetValue32(), 0x10002);
  EXPECT_EQ(state.extra.GetValue32(), 0x30004);
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);
}

TEST_F(InstructionTest, OUT_RW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(2);
  state.extra.SetAddress(state.be + 200).AddValue(3);
  state.stack.SetAddress(state.bs + 500).AddValue(4);
  state.stack.SetAddress(state.bs + 502).AddValue(5);
  state.stack.SetAddress(state.bs + 510).AddValue(6);
  state.stack.SetAddress(state.bs + 508).AddValue(7);
  state.stack.SetAddress(state.bs + 520).AddValue(9);
  state.data.SetAddress(state.bd + 300).AddValue(10);
  state.extra.SetAddress(state.be + 400).AddValue(11);
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R0, 1},    // Port
                      {CpuCore::R1, 1},    // For "$r"
                      {CpuCore::R2, 100},  // For "($r)"
                      {CpuCore::R4, 150},  // For "($r + $v)", v == 50
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 510}});

  // OUT always sets S, so each OUT after the first is preceded by clearing S,
  // which is not timed.
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, {"$r", CpuCore::R1}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, {"($r)", CpuCore::R2}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, "(SP)"));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, "(SP + $v)")).AddValue(2);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, "(FP)"));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, "(FP + $v)")).AddValue(-2);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, "$v")).AddValue(8);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, "S($v)")).AddValue(520);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, "D($v)")).AddValue(300);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RW", CpuCore::R0, "E($v)")).AddValue(400);
  const uint16_t ip11 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 4);  // OUT.RW R0, R1
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(ReadPort(1), 1);

  ASSERT_TRUE(ExecuteUntilIp(setup2));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip2), 6);     // OUT.RW R0, (R2)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 2);

  ASSERT_TRUE(ExecuteUntilIp(setup3));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip3), 8);     // OUT.RW R0, (R4 + 50)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 3);

  ASSERT_TRUE(ExecuteUntilIp(setup4));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip4), 6);     // OUT.RW R0, (SP)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 4);

  ASSERT_TRUE(ExecuteUntilIp(setup5));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip5), 8);     // OUT.RW R0, (SP + 2)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 5);

  ASSERT_TRUE(ExecuteUntilIp(setup6));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip6), 6);     // OUT.RW R0, (FP)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 6);

  ASSERT_TRUE(ExecuteUntilIp(setup7));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip7), 8);     // OUT.RW R0, (FP - 2)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 7);

  ASSERT_TRUE(ExecuteUntilIp(setup8));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip8), 5);     // OUT.RW R0, 8
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 8);

  ASSERT_TRUE(ExecuteUntilIp(setup9));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip9), 7);     // OUT.RW R0, S(520)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 9);

  ASSERT_TRUE(ExecuteUntilIp(setup10));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip10), 7);     // OUT.RW R0, D(300)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 10);

  ASSERT_TRUE(ExecuteUntilIp(setup11));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip11), 7);     // OUT.RW R0, E(400)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 11);
}

TEST_F(InstructionTest, OUT_IW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.extra.SetAddress(state.be + 200).AddValue(2);
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}});

  state.code.AddValue(Encode("OUT.IW", "$v")).AddValue(1).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.IW", {"($r + $v)", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 6);  // OUT.IW 1, 1
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(ReadPort(1), 1);

  ASSERT_TRUE(ExecuteUntilIp(setup2));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip2), 9);     // OUT.IW 1, (R4 + 50)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(ReadPort(1), 2);
}

TEST_F(InstructionTest, OUT_RD) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue32(0x30004);
  state.extra.SetAddress(state.be + 200).AddValue32(0x50006);
  state.stack.SetAddress(state.bs + 500).AddValue32(0x70008);
  state.stack.SetAddress(state.bs + 502).AddValue32(0x9000A);
  state.stack.SetAddress(state.bs + 510).AddValue32(0xB000C);
  state.stack.SetAddress(state.bs + 506).AddValue32(0xD000E);
  state.stack.SetAddress(state.bs + 520).AddValue32(0x110012);
  state.data.SetAddress(state.bd + 300).AddValue32(0x130014);
  state.extra.SetAddress(state.be + 400).AddValue32(0x150016);
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R5, 1},       // Port
                      {CpuCore::R0, 0x0002},  // For "$R"
                      {CpuCore::R1, 0x0001},  // For "$R"
                      {CpuCore::R2, 100},     // For "[$r]"
                      {CpuCore::R4, 150},     // For "[$r + $v]", v == 50
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 510}});

  // OUT always sets S, so each OUT after the first is preceded by clearing S,
  // which is not timed.
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, {"$R", 0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, {"[$r]", CpuCore::R2}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, "[SP]"));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, "[SP + $v]")).AddValue(2);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, "[FP]"));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, "[FP + $v]")).AddValue(-4);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, "$V")).AddValue32(0xF0010);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, "S[$v]")).AddValue(520);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, "D[$v]")).AddValue(300);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.RD", CpuCore::R5, "E[$v]")).AddValue(400);
  const uint16_t ip11 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // OUT.RD R5, D0
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(ReadPort32(1), 0x10002);

  ASSERT_TRUE(ExecuteUntilIp(setup2));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip2), 8);     // OUT.RD R5, [R2]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x30004);

  ASSERT_TRUE(ExecuteUntilIp(setup3));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip3), 10);    // OUT.RD R5, [R4 + 50]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x50006);

  ASSERT_TRUE(ExecuteUntilIp(setup4));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip4), 8);     // OUT.RD R5, [SP]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x70008);

  ASSERT_TRUE(ExecuteUntilIp(setup5));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip5), 10);    // OUT.RD R5, [SP + 2]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x9000A);

  ASSERT_TRUE(ExecuteUntilIp(setup6));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip6), 8);     // OUT.RD R5, [FP]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0xB000C);

  ASSERT_TRUE(ExecuteUntilIp(setup7));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip7), 10);    // OUT.RD R5, [FP - 4]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0xD000E);

  ASSERT_TRUE(ExecuteUntilIp(setup8));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip8), 7);     // OUT.RD R5, 0xF0010
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0xF0010);

  ASSERT_TRUE(ExecuteUntilIp(setup9));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip9), 9);     // OUT.RD R5, S[520]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x110012);

  ASSERT_TRUE(ExecuteUntilIp(setup10));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip10), 9);     // OUT.RD R5, D[300]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x130014);

  ASSERT_TRUE(ExecuteUntilIp(setup11));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip11), 9);     // OUT.RD R5, E[400]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x150016);
}

TEST_F(InstructionTest, OUT_ID) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.extra.SetAddress(state.be + 200).AddValue32(0x30004);
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}});

  state.code.AddValue(Encode("OUT.ID", "$V")).AddValue(1).AddValue32(0x10002);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("CLRF", CpuCore::S));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUT.ID", {"[$r + $v]", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 8);  // OUT.ID 1, 0x10002
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(ReadPort32(1), 0x10002);

  ASSERT_TRUE(ExecuteUntilIp(setup2));  // CLRF S
  EXPECT_EQ(CyclesUntilIp(ip2), 11);    // OUT.ID 1, [R4 + 50]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(ReadPort32(1), 0x30004);
}

TEST_F(InstructionTest, OUTS_RW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(2);
  state.extra.SetAddress(state.be + 200).AddValue(3);
  state.stack.SetAddress(state.bs + 500).AddValue(4);
  state.stack.SetAddress(state.bs + 502).AddValue(5);
  state.stack.SetAddress(state.bs + 510).AddValue(6);
  state.stack.SetAddress(state.bs + 508).AddValue(7);
  state.stack.SetAddress(state.bs + 520).AddValue(9);
  state.data.SetAddress(state.bd + 300).AddValue(10);
  state.extra.SetAddress(state.be + 400).AddValue(11);
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R0, 1},    // Port
                      {CpuCore::R1, 1},    // For "$r"
                      {CpuCore::R2, 100},  // For "($r)"
                      {CpuCore::R4, 150},  // For "($r + $v)", v == 50
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 510}});

  // Each form is run twice: first with the port ready, and then not ready.
  // This toggles S, so a form that doesn't update it shows.
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, {"$r", CpuCore::R1}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, {"$r", CpuCore::R1}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, {"($r)", CpuCore::R2}));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, {"($r)", CpuCore::R2}));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code
      .AddValue(Encode("OUTS.RW", CpuCore::R0, {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code
      .AddValue(Encode("OUTS.RW", CpuCore::R0, {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "(SP)"));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "(SP)"));
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "(SP + $v)")).AddValue(2);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "(SP + $v)")).AddValue(2);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "(FP)"));
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "(FP)"));
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "(FP + $v)")).AddValue(-2);
  const uint16_t ip13 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "(FP + $v)")).AddValue(-2);
  const uint16_t ip14 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "$v")).AddValue(8);
  const uint16_t ip15 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "$v")).AddValue(8);
  const uint16_t ip16 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "S($v)")).AddValue(520);
  const uint16_t ip17 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "S($v)")).AddValue(520);
  const uint16_t ip18 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "D($v)")).AddValue(300);
  const uint16_t ip19 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "D($v)")).AddValue(300);
  const uint16_t ip20 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "E($v)")).AddValue(400);
  const uint16_t ip21 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RW", CpuCore::R0, "E($v)")).AddValue(400);
  const uint16_t ip22 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 4);  // OUTS.RW R0, R1
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(ReadPort(1), 1);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip2), 4);  // OUTS.RW R0, R1 (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip3), 6);  // OUTS.RW R0, (R2)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 2);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip4), 6);  // OUTS.RW R0, (R2) (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip5), 8);  // OUTS.RW R0, (R4 + 50)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 3);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip6), 8);  // OUTS.RW R0, (R4 + 50) (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip7), 6);  // OUTS.RW R0, (SP)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 4);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip8), 6);  // OUTS.RW R0, (SP) (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip9), 8);  // OUTS.RW R0, (SP + 2)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 5);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip10), 8);  // OUTS.RW R0, (SP + 2) (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip11), 6);  // OUTS.RW R0, (FP)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 6);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip12), 6);  // OUTS.RW R0, (FP) (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip13), 8);  // OUTS.RW R0, (FP - 2)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 7);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip14), 8);  // OUTS.RW R0, (FP - 2) (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip15), 5);  // OUTS.RW R0, 8
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 8);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip16), 5);  // OUTS.RW R0, 8 (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip17), 7);  // OUTS.RW R0, S(520)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 9);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip18), 7);  // OUTS.RW R0, S(520) (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip19), 7);  // OUTS.RW R0, D(300)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 10);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip20), 7);  // OUTS.RW R0, D(300) (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip21), 7);  // OUTS.RW R0, E(400)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 11);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip22), 7);  // OUTS.RW R0, E(400) (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);
}

TEST_F(InstructionTest, OUTS_IW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.extra.SetAddress(state.be + 200).AddValue(2);
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}});

  state.code.AddValue(Encode("OUTS.IW", "$v")).AddValue(1).AddValue(1);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.IW", "$v")).AddValue(1).AddValue(1);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.IW", {"($r + $v)", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.IW", {"($r + $v)", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip4 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 6);  // OUTS.IW 1, 1
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(ReadPort(1), 1);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip2), 6);  // OUTS.IW 1, 1 (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);

  EXPECT_EQ(CyclesUntilIp(ip3), 9);  // OUTS.IW 1, (R4 + 50)
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort(1), 2);
  WritePort(1, 0xBAD);
  EXPECT_EQ(CyclesUntilIp(ip4), 9);  // OUTS.IW 1, (R4 + 50) (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort(1), 0xBAD);
}

TEST_F(InstructionTest, OUTS_RD) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue32(0x30004);
  state.extra.SetAddress(state.be + 200).AddValue32(0x50006);
  state.stack.SetAddress(state.bs + 500).AddValue32(0x70008);
  state.stack.SetAddress(state.bs + 502).AddValue32(0x9000A);
  state.stack.SetAddress(state.bs + 510).AddValue32(0xB000C);
  state.stack.SetAddress(state.bs + 506).AddValue32(0xD000E);
  state.stack.SetAddress(state.bs + 520).AddValue32(0x110012);
  state.data.SetAddress(state.bd + 300).AddValue32(0x130014);
  state.extra.SetAddress(state.be + 400).AddValue32(0x150016);
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R5, 1},       // Port
                      {CpuCore::R0, 0x0002},  // For "$R"
                      {CpuCore::R1, 0x0001},  // For "$R"
                      {CpuCore::R2, 100},     // For "[$r]"
                      {CpuCore::R4, 150},     // For "[$r + $v]", v == 50
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 510}});

  // Each form is run twice: first with the port ready, and then not ready.
  // This toggles S, so a form that doesn't update it shows.
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, {"$R", 0}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, {"$R", 0}));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, {"[$r]", CpuCore::R2}));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, {"[$r]", CpuCore::R2}));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code
      .AddValue(Encode("OUTS.RD", CpuCore::R5, {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code
      .AddValue(Encode("OUTS.RD", CpuCore::R5, {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "[SP]"));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "[SP]"));
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "[SP + $v]")).AddValue(2);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "[SP + $v]")).AddValue(2);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "[FP]"));
  const uint16_t ip11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "[FP]"));
  const uint16_t ip12 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "[FP + $v]")).AddValue(-4);
  const uint16_t ip13 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "[FP + $v]")).AddValue(-4);
  const uint16_t ip14 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "$V")).AddValue32(0xF0010);
  const uint16_t ip15 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "$V")).AddValue32(0xF0010);
  const uint16_t ip16 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "S[$v]")).AddValue(520);
  const uint16_t ip17 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "S[$v]")).AddValue(520);
  const uint16_t ip18 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "D[$v]")).AddValue(300);
  const uint16_t ip19 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "D[$v]")).AddValue(300);
  const uint16_t ip20 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "E[$v]")).AddValue(400);
  const uint16_t ip21 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.RD", CpuCore::R5, "E[$v]")).AddValue(400);
  const uint16_t ip22 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 5);  // OUTS.RD R5, D0
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(ReadPort32(1), 0x10002);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip2), 5);  // OUTS.RD R5, D0 (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip3), 8);  // OUTS.RD R5, [R2]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x30004);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip4), 8);  // OUTS.RD R5, [R2] (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip5), 10);  // OUTS.RD R5, [R4 + 50]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x50006);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip6), 10);  // OUTS.RD R5, [R4 + 50] (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip7), 8);  // OUTS.RD R5, [SP]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x70008);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip8), 8);  // OUTS.RD R5, [SP] (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip9), 10);  // OUTS.RD R5, [SP + 2]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x9000A);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip10), 10);  // OUTS.RD R5, [SP + 2] (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip11), 8);  // OUTS.RD R5, [FP]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0xB000C);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip12), 8);  // OUTS.RD R5, [FP] (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip13), 10);  // OUTS.RD R5, [FP - 4]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0xD000E);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip14), 10);  // OUTS.RD R5, [FP - 4] (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip15), 7);  // OUTS.RD R5, 0xF0010
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0xF0010);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip16), 7);  // OUTS.RD R5, 0xF0010 (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip17), 9);  // OUTS.RD R5, S[520]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x110012);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip18), 9);  // OUTS.RD R5, S[520] (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip19), 9);  // OUTS.RD R5, D[300]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x130014);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip20), 9);  // OUTS.RD R5, D[300] (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip21), 9);  // OUTS.RD R5, E[400]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x150016);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip22), 9);  // OUTS.RD R5, E[400] (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);
}

TEST_F(InstructionTest, OUTS_ID) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.extra.SetAddress(state.be + 200).AddValue32(0x30004);
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}});

  state.code.AddValue(Encode("OUTS.ID", "$V")).AddValue(1).AddValue32(0x10002);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.ID", "$V")).AddValue(1).AddValue32(0x10002);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.ID", {"[$r + $v]", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTS.ID", {"[$r + $v]", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip4 = state.code.AddNopGetAddress();

  EXPECT_EQ(CyclesUntilIp(ip1), 8);  // OUTS.ID 1, 0x10002
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(ReadPort32(1), 0x10002);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip2), 8);  // OUTS.ID 1, 0x10002 (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);

  EXPECT_EQ(CyclesUntilIp(ip3), 11);  // OUTS.ID 1, [R4 + 50]
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(ReadPort32(1), 0x30004);
  WritePort32(1, 0xBAD0BAD);
  EXPECT_EQ(CyclesUntilIp(ip4), 11);  // OUTS.ID 1, [R4 + 50] (not ready)
  EXPECT_EQ(state.st, ZC);
  EXPECT_EQ(ReadPort32(1), 0xBAD0BAD);
}

TEST_F(InstructionTest, OUTR_RW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue(1).AddValue(2);
  state.extra.SetAddress(state.be + 200).AddValue(3).AddValue(4);
  state.stack.SetAddress(state.bs + 500).AddValue(5).AddValue(6);
  state.stack.SetAddress(state.bs + 504).AddValue(7).AddValue(8);
  state.stack.SetAddress(state.bs + 520).AddValue(9).AddValue(10);
  state.stack.SetAddress(state.bs + 512).AddValue(11).AddValue(12);
  state.stack.SetAddress(state.bs + 530).AddValue(13).AddValue(14);
  state.data.SetAddress(state.bd + 300).AddValue(15).AddValue(16);
  state.extra.SetAddress(state.be + 400).AddValue(17).AddValue(18);
  state.data.SetAddress(state.bd + 600).AddValue(19).AddValue(20).AddValue(21);
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R0, 1},    // Port
                      {CpuCore::R2, 100},  // For "($r)"
                      {CpuCore::R4, 150},  // For "($r + $v)", v == 50
                      {CpuCore::R7, 2},    // Words to write
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 520}});

  // Each OUTR after the first is preceded by setting R7, which is not timed.
  state.code.AddValue(Encode("OUTR.RW", CpuCore::R0, {"($r)", CpuCore::R2}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code
      .AddValue(Encode("OUTR.RW", CpuCore::R0, {"($r + $v)", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RW", CpuCore::R0, "(SP)"));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RW", CpuCore::R0, "(SP + $v)")).AddValue(4);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RW", CpuCore::R0, "(FP)"));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RW", CpuCore::R0, "(FP + $v)")).AddValue(-8);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RW", CpuCore::R0, "S($v)")).AddValue(530);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RW", CpuCore::R0, "D($v)")).AddValue(300);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RW", CpuCore::R0, "E($v)")).AddValue(400);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RW", CpuCore::R0, "D($v)")).AddValue(600);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  const uint16_t setup11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RW", CpuCore::R0, "D($v)")).AddValue(700);
  const uint16_t ip11 = state.code.AddNopGetAddress();

  PortDrainer drainer1(this, 1, PortSize::kWord, 2);
  EXPECT_EQ(CyclesUntilIp(ip1, [&] { drainer1.Update(); }),
            13);  // OUTR.RW R0, (R2)
  EXPECT_THAT(drainer1.GetValues(), ElementsAre(1, 2));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup2));  // MVQ.LW R7, 2
  PortDrainer drainer2(this, 1, PortSize::kWord, 2);
  EXPECT_EQ(CyclesUntilIp(ip2, [&] { drainer2.Update(); }),
            14);  // OUTR.RW R0, (R4 + 50)
  EXPECT_THAT(drainer2.GetValues(), ElementsAre(3, 4));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup3));  // MVQ.LW R7, 2
  PortDrainer drainer3(this, 1, PortSize::kWord, 2);
  EXPECT_EQ(CyclesUntilIp(ip3, [&] { drainer3.Update(); }),
            13);  // OUTR.RW R0, (SP)
  EXPECT_THAT(drainer3.GetValues(), ElementsAre(5, 6));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup4));  // MVQ.LW R7, 2
  PortDrainer drainer4(this, 1, PortSize::kWord, 2);
  EXPECT_EQ(CyclesUntilIp(ip4, [&] { drainer4.Update(); }),
            14);  // OUTR.RW R0, (SP + 4)
  EXPECT_THAT(drainer4.GetValues(), ElementsAre(7, 8));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup5));  // MVQ.LW R7, 2
  PortDrainer drainer5(this, 1, PortSize::kWord, 2);
  EXPECT_EQ(CyclesUntilIp(ip5, [&] { drainer5.Update(); }),
            13);  // OUTR.RW R0, (FP)
  EXPECT_THAT(drainer5.GetValues(), ElementsAre(9, 10));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup6));  // MVQ.LW R7, 2
  PortDrainer drainer6(this, 1, PortSize::kWord, 2);
  EXPECT_EQ(CyclesUntilIp(ip6, [&] { drainer6.Update(); }),
            14);  // OUTR.RW R0, (FP - 8)
  EXPECT_THAT(drainer6.GetValues(), ElementsAre(11, 12));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup7));  // MVQ.LW R7, 2
  PortDrainer drainer7(this, 1, PortSize::kWord, 2);
  EXPECT_EQ(CyclesUntilIp(ip7, [&] { drainer7.Update(); }),
            13);  // OUTR.RW R0, S(530)
  EXPECT_THAT(drainer7.GetValues(), ElementsAre(13, 14));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup8));  // MVQ.LW R7, 2
  PortDrainer drainer8(this, 1, PortSize::kWord, 2);
  EXPECT_EQ(CyclesUntilIp(ip8, [&] { drainer8.Update(); }),
            13);  // OUTR.RW R0, D(300)
  EXPECT_THAT(drainer8.GetValues(), ElementsAre(15, 16));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup9));  // MVQ.LW R7, 2
  PortDrainer drainer9(this, 1, PortSize::kWord, 2);
  EXPECT_EQ(CyclesUntilIp(ip9, [&] { drainer9.Update(); }),
            13);  // OUTR.RW R0, E(400)
  EXPECT_THAT(drainer9.GetValues(), ElementsAre(17, 18));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  // The device stops reading after one word, so the second word stays in the
  // port, and the third can't be written. R7 is left at the words not written.
  ASSERT_TRUE(ExecuteUntilIp(setup10));  // MVQ.LW R7, 3
  PortDrainer drainer10(this, 1, PortSize::kWord, 1);
  EXPECT_EQ(CyclesUntilIp(ip10, [&] { drainer10.Update(); }),
            17);  // OUTR.RW R0, D(600)
  EXPECT_THAT(drainer10.GetValues(), ElementsAre(19));
  EXPECT_EQ(state.r7, 1);
  EXPECT_EQ(state.st, ZC);

  // Nothing is written when R7 is zero, even with the port ready.
  ASSERT_TRUE(ExecuteUntilIp(setup11));  // MVQ.LW R7, 0
  EXPECT_EQ(ReadPort(1), 20);
  EXPECT_EQ(CyclesUntilIp(ip11), 6);  // OUTR.RW R0, D(700)
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);
}

TEST_F(InstructionTest, OUTR_IW) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.extra.SetAddress(state.be + 200).AddValue(1).AddValue(2);
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}, {CpuCore::R7, 2}});

  state.code.AddValue(Encode("OUTR.IW", {"($r + $v)", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip1 = state.code.AddNopGetAddress();

  PortDrainer drainer(this, 1, PortSize::kWord, 2);
  EXPECT_EQ(CyclesUntilIp(ip1, [&] { drainer.Update(); }),
            15);  // OUTR.IW 1, (R4 + 50)
  EXPECT_THAT(drainer.GetValues(), ElementsAre(1, 2));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);
}

TEST_F(InstructionTest, OUTR_RD) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.data.SetAddress(state.bd + 100).AddValue32(0x10002).AddValue32(0x30004);
  state.extra.SetAddress(state.be + 200)
      .AddValue32(0x50006)
      .AddValue32(0x70008);
  state.stack.SetAddress(state.bs + 500)
      .AddValue32(0x9000A)
      .AddValue32(0xB000C);
  state.stack.SetAddress(state.bs + 504)
      .AddValue32(0xD000E)
      .AddValue32(0xF0010);
  state.stack.SetAddress(state.bs + 520)
      .AddValue32(0x110012)
      .AddValue32(0x130014);
  state.stack.SetAddress(state.bs + 512)
      .AddValue32(0x150016)
      .AddValue32(0x170018);
  state.stack.SetAddress(state.bs + 530)
      .AddValue32(0x19001A)
      .AddValue32(0x1B001C);
  state.data.SetAddress(state.bd + 300)
      .AddValue32(0x1D001E)
      .AddValue32(0x1F0020);
  state.extra.SetAddress(state.be + 400)
      .AddValue32(0x210022)
      .AddValue32(0x230024);
  state.data.SetAddress(state.bd + 600)
      .AddValue32(0x250026)
      .AddValue32(0x270028)
      .AddValue32(0x29002A);
  state.SetRegisters({{CpuCore::ST, ZC},
                      {CpuCore::R5, 1},    // Port
                      {CpuCore::R2, 100},  // For "[$r]"
                      {CpuCore::R4, 150},  // For "[$r + $v]", v == 50
                      {CpuCore::R7, 2},    // Dwords to write
                      {CpuCore::SP, 500},
                      {CpuCore::FP, 520}});

  // Each OUTR after the first is preceded by setting R7, which is not timed.
  state.code.AddValue(Encode("OUTR.RD", CpuCore::R5, {"[$r]", CpuCore::R2}));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup2 = state.code.AddNopGetAddress();
  state.code
      .AddValue(Encode("OUTR.RD", CpuCore::R5, {"[$r + $v]", CpuCore::R4}))
      .AddValue(50);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RD", CpuCore::R5, "[SP]"));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RD", CpuCore::R5, "[SP + $v]")).AddValue(4);
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RD", CpuCore::R5, "[FP]"));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RD", CpuCore::R5, "[FP + $v]")).AddValue(-8);
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RD", CpuCore::R5, "S[$v]")).AddValue(530);
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RD", CpuCore::R5, "D[$v]")).AddValue(300);
  const uint16_t ip8 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 2));
  const uint16_t setup9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RD", CpuCore::R5, "E[$v]")).AddValue(400);
  const uint16_t ip9 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 3));
  const uint16_t setup10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RD", CpuCore::R5, "D[$v]")).AddValue(600);
  const uint16_t ip10 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("MVQ.LW", CpuCore::R7, 0));
  const uint16_t setup11 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode("OUTR.RD", CpuCore::R5, "D[$v]")).AddValue(700);
  const uint16_t ip11 = state.code.AddNopGetAddress();

  PortDrainer drainer1(this, 1, PortSize::kDword, 2);
  EXPECT_EQ(CyclesUntilIp(ip1, [&] { drainer1.Update(); }),
            17);  // OUTR.RD R5, [R2]
  EXPECT_THAT(drainer1.GetValues(), ElementsAre(0x10002, 0x30004));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup2));  // MVQ.LW R7, 2
  PortDrainer drainer2(this, 1, PortSize::kDword, 2);
  EXPECT_EQ(CyclesUntilIp(ip2, [&] { drainer2.Update(); }),
            18);  // OUTR.RD R5, [R4 + 50]
  EXPECT_THAT(drainer2.GetValues(), ElementsAre(0x50006, 0x70008));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup3));  // MVQ.LW R7, 2
  PortDrainer drainer3(this, 1, PortSize::kDword, 2);
  EXPECT_EQ(CyclesUntilIp(ip3, [&] { drainer3.Update(); }),
            17);  // OUTR.RD R5, [SP]
  EXPECT_THAT(drainer3.GetValues(), ElementsAre(0x9000A, 0xB000C));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup4));  // MVQ.LW R7, 2
  PortDrainer drainer4(this, 1, PortSize::kDword, 2);
  EXPECT_EQ(CyclesUntilIp(ip4, [&] { drainer4.Update(); }),
            18);  // OUTR.RD R5, [SP + 4]
  EXPECT_THAT(drainer4.GetValues(), ElementsAre(0xD000E, 0xF0010));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup5));  // MVQ.LW R7, 2
  PortDrainer drainer5(this, 1, PortSize::kDword, 2);
  EXPECT_EQ(CyclesUntilIp(ip5, [&] { drainer5.Update(); }),
            17);  // OUTR.RD R5, [FP]
  EXPECT_THAT(drainer5.GetValues(), ElementsAre(0x110012, 0x130014));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup6));  // MVQ.LW R7, 2
  PortDrainer drainer6(this, 1, PortSize::kDword, 2);
  EXPECT_EQ(CyclesUntilIp(ip6, [&] { drainer6.Update(); }),
            18);  // OUTR.RD R5, [FP - 8]
  EXPECT_THAT(drainer6.GetValues(), ElementsAre(0x150016, 0x170018));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup7));  // MVQ.LW R7, 2
  PortDrainer drainer7(this, 1, PortSize::kDword, 2);
  EXPECT_EQ(CyclesUntilIp(ip7, [&] { drainer7.Update(); }),
            17);  // OUTR.RD R5, S[530]
  EXPECT_THAT(drainer7.GetValues(), ElementsAre(0x19001A, 0x1B001C));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup8));  // MVQ.LW R7, 2
  PortDrainer drainer8(this, 1, PortSize::kDword, 2);
  EXPECT_EQ(CyclesUntilIp(ip8, [&] { drainer8.Update(); }),
            17);  // OUTR.RD R5, D[300]
  EXPECT_THAT(drainer8.GetValues(), ElementsAre(0x1D001E, 0x1F0020));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  ASSERT_TRUE(ExecuteUntilIp(setup9));  // MVQ.LW R7, 2
  PortDrainer drainer9(this, 1, PortSize::kDword, 2);
  EXPECT_EQ(CyclesUntilIp(ip9, [&] { drainer9.Update(); }),
            17);  // OUTR.RD R5, E[400]
  EXPECT_THAT(drainer9.GetValues(), ElementsAre(0x210022, 0x230024));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);

  // The device stops reading after one dword, so the second dword stays in
  // the port, and the third can't be written. R7 is left at the dwords not
  // written.
  ASSERT_TRUE(ExecuteUntilIp(setup10));  // MVQ.LW R7, 3
  PortDrainer drainer10(this, 1, PortSize::kDword, 1);
  EXPECT_EQ(CyclesUntilIp(ip10, [&] { drainer10.Update(); }),
            23);  // OUTR.RD R5, D[600]
  EXPECT_THAT(drainer10.GetValues(), ElementsAre(0x250026));
  EXPECT_EQ(state.r7, 1);
  EXPECT_EQ(state.st, ZC);

  // Nothing is written when R7 is zero, even with the port ready.
  ASSERT_TRUE(ExecuteUntilIp(setup11));  // MVQ.LW R7, 0
  EXPECT_EQ(ReadPort32(1), 0x270028);
  EXPECT_EQ(CyclesUntilIp(ip11), 6);  // OUTR.RD R5, D[700]
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);
}

TEST_F(InstructionTest, OUTR_ID) {
  ASSERT_TRUE(InitAndReset({.num_ports = 2}));
  auto& state = GetState();
  state.extra.SetAddress(state.be + 200)
      .AddValue32(0x10002)
      .AddValue32(0x30004);
  state.SetRegisters({{CpuCore::ST, ZC}, {CpuCore::R4, 150}, {CpuCore::R7, 2}});

  state.code.AddValue(Encode("OUTR.ID", {"[$r + $v]", CpuCore::R4}))
      .AddValue(1)
      .AddValue(50);
  const uint16_t ip1 = state.code.AddNopGetAddress();

  PortDrainer drainer(this, 1, PortSize::kDword, 2);
  EXPECT_EQ(CyclesUntilIp(ip1, [&] { drainer.Update(); }),
            19);  // OUTR.ID 1, [R4 + 50]
  EXPECT_THAT(drainer.GetValues(), ElementsAre(0x10002, 0x30004));
  EXPECT_EQ(state.r7, 0);
  EXPECT_EQ(state.st, ZSC);
}

}  // namespace
}  // namespace oz3
