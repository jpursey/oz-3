// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_test.h"

namespace oz3 {
namespace {

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

}  // namespace
}  // namespace oz3
