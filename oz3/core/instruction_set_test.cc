// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_set.h"

#include <ostream>
#include <string>
#include <vector>

#include "absl/base/no_destructor.h"
#include "absl/strings/substitute.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/instruction_def.h"
#include "oz3/core/microcode.h"
#include "oz3/core/port.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

constexpr uint8_t kOp_TEST = 128;
constexpr uint8_t kMicro_TEST_NOP = 254;
constexpr uint8_t kMicro_TEST = 255;

InstructionDef MakeDef(std::string_view code) {
  return InstructionDef{.op = kOp_TEST, .op_name = "TEST", .code = code};
}

InstructionDef MakeDef(Argument arg1, std::string_view code = "UL;") {
  return InstructionDef{
      .op = kOp_TEST, .op_name = "TEST", .arg1 = arg1, .code = code};
}

InstructionDef MakeDef(Argument arg1, Argument arg2,
                       std::string_view code = "UL;") {
  return InstructionDef{.op = kOp_TEST,
                        .op_name = "TEST",
                        .arg1 = arg1,
                        .arg2 = arg2,
                        .code = code};
}

TEST(InstructionSetTest, InstructionNotFound) {
  InstructionSet codes;
  DecodedInstruction decoded;
  EXPECT_FALSE(codes.Decode(0, decoded));
  Microcode ul_c = {kMicro_UL};
  DecodedInstruction expected = {};
  expected.size = 1;
  expected.code = absl::MakeConstSpan(&ul_c, 1);
  EXPECT_EQ(decoded, expected);
}

uint16_t MakeCode(uint16_t op, uint16_t args = 0) {
  return (op << 8) | (args & 0xFF);
}

TEST(InstructionSetTest, BankDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kBank},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef("UL;OP(CODE);OP(STACK);OP(DATA);OP(EXTRA)"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::CODE},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::STACK},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::DATA},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::EXTRA}));
}

TEST(InstructionSetTest, StatusDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kStatus},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef("UL;OP(_);"
              "OP(Z);OP(S);OP(C);OP(O);"
              "OP(Z___);OP(_S__);OP(__C_);OP(___O);"
              "OP(ZS);OP(ZC);OP(ZO);OP(SC);OP(SO);OP(CO);"
              "OP(ZSC);OP(ZSO);OP(ZCO);OP(SCO);"
              "OP(ZSCO);OP(CCC_SSS_OZ_OZ_OZ);OP(I);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = 0},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::Z},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::S},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::C},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::O},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::Z},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::S},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::C},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::O},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::Z | CpuCore::S},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::Z | CpuCore::C},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::Z | CpuCore::O},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::S | CpuCore::C},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::S | CpuCore::O},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::C | CpuCore::O},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::Z | CpuCore::S | CpuCore::C},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::Z | CpuCore::S | CpuCore::O},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::Z | CpuCore::C | CpuCore::O},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::S | CpuCore::C | CpuCore::O},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::ZSCO},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::ZSCO},
                  Microcode{.op = kMicro_TEST, .arg1 = CpuCore::I}));
}

TEST(InstructionSetTest, StatusOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kStatus, MicroArgType::kStatus},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kImmediate, 4}, {ArgType::kImmediate, 4},
              "UL;OP(a,b);OP(b,a);"),
  };
  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0x34), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 4);
  EXPECT_EQ(decoded.c[1], 3);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = -2},
                  Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = -1}));
}

TEST(InstructionSetTest, StatusOpMacroArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kStatus, MicroArgType::kStatus},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.prefix = {0, 1}, .arg = {ArgType::kImmediate, 4}, .code = "OP(m,Z);"},
      {.prefix = {1, 1}, .arg = {ArgType::kImmediate, 4}, .code = "OP(Z,m);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 5, .code = macro_code_defs}};
  const InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 5}, "UL;$Macro;"),
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 5}, "UL;$Macro;"),
  };
  InstructionError error;
  DecodedInstruction decoded;
  std::shared_ptr<const InstructionSet> codes;

  codes = CompileInstructionSet(
      {.instructions = absl::Span(&instruction_defs[0], 1),
       .macros = macro_defs},
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());

  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0x04), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 4);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = 1}));

  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0x14), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 4);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST, .arg1 = 1, .arg2 = -1}));

  codes = CompileInstructionSet(
      {.instructions = absl::Span(&instruction_defs[1], 1),
       .macros = macro_defs},
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());

  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0x43), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 8);
  EXPECT_EQ(decoded.r[0], 3);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = 1}));

  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0xC4), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 8);
  EXPECT_EQ(decoded.r[0], 4);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST, .arg1 = 1, .arg2 = -2}));
}

TEST(InstructionSetTest, PortModeDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kPortMode},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef("UL;OP(_);"
              "OP(T);OP(S);OP(A);"
              "OP(TS);OP(TA);OP(SA);"
              "OP(TSA);OP(STA);OP(T_A);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL}, Microcode{.op = kMicro_TEST, .arg1 = 0},
          Microcode{.op = kMicro_TEST, .arg1 = Port::T},
          Microcode{.op = kMicro_TEST, .arg1 = Port::S},
          Microcode{.op = kMicro_TEST, .arg1 = Port::A},
          Microcode{.op = kMicro_TEST, .arg1 = Port::T | Port::S},
          Microcode{.op = kMicro_TEST, .arg1 = Port::T | Port::A},
          Microcode{.op = kMicro_TEST, .arg1 = Port::S | Port::A},
          Microcode{.op = kMicro_TEST, .arg1 = Port::T | Port::S | Port::A},
          Microcode{.op = kMicro_TEST, .arg1 = Port::S | Port::T | Port::A},
          Microcode{.op = kMicro_TEST, .arg1 = Port::T | Port::A}));
}

TEST(InstructionSetTest, ConditionDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kCondition, MicroArgType::kCondition},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef("UL;OP(Z,NZ);OP(S,NS);OP(C,NC);OP(O,NO);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  static_assert(CpuCore::ZShift == 0, "ZShift != 0");
  static_assert(CpuCore::SShift == 1, "SShift != 1");
  static_assert(CpuCore::CShift == 2, "CShift != 2");
  static_assert(CpuCore::OShift == 3, "OShift != 3");
  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST, .arg1 = 4, .arg2 = 0},
                          Microcode{.op = kMicro_TEST, .arg1 = 5, .arg2 = 1},
                          Microcode{.op = kMicro_TEST, .arg1 = 6, .arg2 = 2},
                          Microcode{.op = kMicro_TEST, .arg1 = 7, .arg2 = 3}));
}

TEST(InstructionSetTest, ConditionOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kCondition, MicroArgType::kCondition},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kImmediate, 4}, {ArgType::kImmediate, 4},
              "UL;OP(a,b);OP(b,a);"),
  };
  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0x34), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 4);
  EXPECT_EQ(decoded.c[1], 3);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = -2},
                  Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = -1}));
}

TEST(InstructionSetTest, ConditionOpMacroArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kCondition, MicroArgType::kCondition},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.prefix = {0, 1}, .arg = {ArgType::kImmediate, 3}, .code = "OP(m,Z);"},
      {.prefix = {1, 1}, .arg = {ArgType::kImmediate, 3}, .code = "OP(Z,m);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 4, .code = macro_code_defs}};
  const InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 4}, "UL;$Macro;"),
      MakeDef({ArgType::kWordReg, 4}, {ArgType::kMacro, 4}, "UL;$Macro;"),
  };
  InstructionError error;
  DecodedInstruction decoded;
  std::shared_ptr<const InstructionSet> codes;

  codes = CompileInstructionSet(
      {.instructions = absl::Span(&instruction_defs[0], 1),
       .macros = macro_defs},
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());

  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0x03), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 3);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = 4}));

  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0x0B), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 3);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);

  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST, .arg1 = 4, .arg2 = -1}));
  codes = CompileInstructionSet(
      {.instructions = absl::Span(&instruction_defs[1], 1),
       .macros = macro_defs},
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());

  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0x23), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 2);
  EXPECT_EQ(decoded.r[0], 3);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = 4}));

  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0xA3), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 2);
  EXPECT_EQ(decoded.r[0], 3);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST, .arg1 = 4, .arg2 = -2}));
}

TEST(InstructionSetTest, AddressDecodedCorrecty) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "OP", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef("UL;"                     // 0
              "@start:OP(0,1);"         // 1
              "OP(-1,@end);"            // 2
              "OP(@start,@mid);"        // 3
              "@mid:NOP;"               // 4
              "NOP;"                    // 5
              "@end:OP(@start,@end);")  // 6
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = 0, .arg2 = 1},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = 3},
                  Microcode{.op = kMicro_TEST, .arg1 = -3, .arg2 = 0},
                  Microcode{.op = kMicro_TEST_NOP},
                  Microcode{.op = kMicro_TEST_NOP},
                  Microcode{.op = kMicro_TEST, .arg1 = -6, .arg2 = -1}));
}

TEST(InstructionSetTest, AddressDecodedCorrectlyWithinMacro) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "OP", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "Test1",
       .prefix = {0, 1},
       .code = "UL;"
               "@start:OP(0,1);"
               "OP(-1,@end);"
               "OP(@start,@mid);"
               "@mid:NOP;"
               "NOP;"
               "@end:OP(@start,@end);"},
      {.source = "Test2",
       .prefix = {1, 1},
       .code = "UL;"
               "@start:OP(0,1);"
               "NOP;"
               "OP(-1,@end);"
               "OP(@start,@mid);"
               "@mid:NOP;"
               "NOP;"
               "@end:OP(@start,@end);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 1, .code = macro_code_defs}};
  const InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 1}, "NOP;$Macro;")};
  InstructionError error;
  auto codes = CompileInstructionSet(
      {.instructions = instruction_defs, .macros = macro_defs}, &error,
      micro_defs);
  EXPECT_THAT(error.message, IsEmpty());

  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_TEST_NOP}, Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = 0, .arg2 = 1},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = 3},
                  Microcode{.op = kMicro_TEST, .arg1 = -3, .arg2 = 0},
                  Microcode{.op = kMicro_TEST_NOP},
                  Microcode{.op = kMicro_TEST_NOP},
                  Microcode{.op = kMicro_TEST, .arg1 = -6, .arg2 = -1}));

  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 1), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_TEST_NOP}, Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = 0, .arg2 = 1},
                  Microcode{.op = kMicro_TEST_NOP},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = 3},
                  Microcode{.op = kMicro_TEST, .arg1 = -4, .arg2 = 0},
                  Microcode{.op = kMicro_TEST_NOP},
                  Microcode{.op = kMicro_TEST_NOP},
                  Microcode{.op = kMicro_TEST, .arg1 = -7, .arg2 = -1}));
}

TEST(InstructionSetTest, AddressDecodedCorrectlyAcrossMacro) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "OP", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "one", .prefix = {0, 2}, .code = "NOP;"},
      {.source = "two", .prefix = {1, 2}, .code = "NOP;NOP;"},
      {.source = "three", .prefix = {2, 2}, .code = "NOP;NOP;NOP;"},
      {.source = "four", .prefix = {3, 2}, .code = "NOP;NOP;NOP;NOP;"},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 2},
              "UL;"
              "@start:OP(0,1);"
              "OP(@mid,-1);"
              "OP(@start,@mid);"
              "@macro:$Macro;"
              "@mid:NOP;"
              "OP(-1,@macro);"
              "@end:OP(@start,@end);"),
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 2, .code = macro_code_defs}};
  InstructionError error;
  auto codes = CompileInstructionSet(
      {.instructions = instruction_defs, .macros = macro_defs}, &error,
      micro_defs);
  EXPECT_THAT(error.message, IsEmpty());

  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = 0, .arg2 = 1},
                  Microcode{.op = kMicro_TEST, .arg1 = 2, .arg2 = -1},
                  Microcode{.op = kMicro_TEST, .arg1 = -3, .arg2 = 1},
                  Microcode{.op = kMicro_TEST_NOP},
                  Microcode{.op = kMicro_TEST_NOP},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = -3},
                  Microcode{.op = kMicro_TEST, .arg1 = -7, .arg2 = -1}));

  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 3), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = 0, .arg2 = 1},
          Microcode{.op = kMicro_TEST, .arg1 = 5, .arg2 = -1},
          Microcode{.op = kMicro_TEST, .arg1 = -3, .arg2 = 4},
          Microcode{.op = kMicro_TEST_NOP}, Microcode{.op = kMicro_TEST_NOP},
          Microcode{.op = kMicro_TEST_NOP}, Microcode{.op = kMicro_TEST_NOP},
          Microcode{.op = kMicro_TEST_NOP},
          Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = -6},
          Microcode{.op = kMicro_TEST, .arg1 = -10, .arg2 = -1}));
}

TEST(InstructionSetTest, ImmArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kValue, MicroArgType::kValue},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef("UL;OP(0,1);OP(127,-42);OP(-128,97)"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = 0, .arg2 = 1},
                  Microcode{.op = kMicro_TEST, .arg1 = 127, .arg2 = -42},
                  Microcode{.op = kMicro_TEST, .arg1 = -128, .arg2 = 97}));
}

TEST(InstructionSetTest, RegByteArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegByte, MicroArgType::kRegByte},
  };
  for (int8_t i = 0; i < CpuCore::kRegisterCount; ++i) {
    std::string code = absl::Substitute("UL;OP($0:L,$0:H);OP($0:h,$0:l);",
                                        CpuCore::GetWordRegName(i));
    const InstructionDef instruction_defs[] = {
        MakeDef(code),
    };

    InstructionError error;
    auto codes = CompileInstructionSet({.instructions = instruction_defs},
                                       &error, micro_defs);
    EXPECT_THAT(error.message, IsEmpty());
    DecodedInstruction decoded;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
    EXPECT_EQ(decoded.size, 1);
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], 0);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
    EXPECT_THAT(decoded.code,
                ElementsAre(Microcode{.op = kMicro_UL},
                            Microcode{.op = kMicro_TEST,
                                      .arg1 = i | CpuCore::kRegMaskByte0,
                                      .arg2 = i | CpuCore::kRegMaskByte1},
                            Microcode{.op = kMicro_TEST,
                                      .arg1 = i | CpuCore::kRegMaskByte1,
                                      .arg2 = i | CpuCore::kRegMaskByte0}))
        << "i = " << i;
  }
}

TEST(InstructionSetTest, RegNibbleArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
  };
  for (int8_t i = 0; i < CpuCore::kRegisterCount; ++i) {
    std::string code = absl::Substitute("UL;OP($0:0,$0:1);OP($0:2,$0:3);",
                                        CpuCore::GetWordRegName(i));
    const InstructionDef instruction_defs[] = {
        MakeDef(code),
    };

    InstructionError error;
    auto codes = CompileInstructionSet({.instructions = instruction_defs},
                                       &error, micro_defs);
    EXPECT_THAT(error.message, IsEmpty());
    DecodedInstruction decoded;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
    EXPECT_EQ(decoded.size, 1);
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], 0);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
    EXPECT_THAT(decoded.code,
                ElementsAre(Microcode{.op = kMicro_UL},
                            Microcode{.op = kMicro_TEST,
                                      .arg1 = i | CpuCore::kRegMaskNibble0,
                                      .arg2 = i | CpuCore::kRegMaskNibble1},
                            Microcode{.op = kMicro_TEST,
                                      .arg1 = i | CpuCore::kRegMaskNibble2,
                                      .arg2 = i | CpuCore::kRegMaskNibble3}))
        << "i = " << i;
  }
}

TEST(InstructionSetTest, WordRegArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  for (int8_t i = 0; i < CpuCore::kRegisterCount; ++i) {
    std::string code =
        absl::Substitute("UL;OP($0,$0);", CpuCore::GetWordRegName(i));
    const InstructionDef instruction_defs[] = {
        MakeDef(code),
    };

    InstructionError error;
    auto codes = CompileInstructionSet({.instructions = instruction_defs},
                                       &error, micro_defs);
    EXPECT_THAT(error.message, IsEmpty());
    DecodedInstruction decoded;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
    EXPECT_EQ(decoded.size, 1);
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], 0);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
    EXPECT_THAT(
        decoded.code,
        ElementsAre(Microcode{.op = kMicro_UL},
                    Microcode{.op = kMicro_TEST, .arg1 = i, .arg2 = i}));
  }
}

TEST(InstructionSetTest, DwordRegArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  for (int8_t i = 0; i < CpuCore::kRegisterCount; ++i) {
    std::string_view reg_name = CpuCore::GetDwordRegName(i);
    if (reg_name == "invalid") {
      continue;
    }
    std::string context = absl::StrCat("Context: ", reg_name);
    std::string code = absl::Substitute("UL;OP($0,$0);", reg_name);
    const InstructionDef instruction_defs[] = {
        MakeDef(code),
    };

    InstructionError error;
    auto codes = CompileInstructionSet({.instructions = instruction_defs},
                                       &error, micro_defs);
    EXPECT_THAT(error.message, IsEmpty());
    DecodedInstruction decoded;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.size, 1) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], 0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], 0) << context;
    EXPECT_EQ(decoded.r[3], 0) << context;
    EXPECT_THAT(decoded.code,
                ElementsAre(Microcode{.op = kMicro_UL},
                            Microcode{.op = kMicro_TEST, .arg1 = i, .arg2 = i}))
        << context;
  }
}

TEST(InstructionSetTest, ImmOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kImmediate, 3}, {ArgType::kImmediate, 5},
              "UL;OP(C0,C1)"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    uint16_t c0 = (i * i + 1) % 8;   // 3 bits
    uint16_t c1 = (i * i + 2) % 32;  // 5 bits
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, c0 | (c1 << 3)), decoded));
    EXPECT_EQ(decoded.c[0], c0);
    EXPECT_EQ(decoded.c[1], c1);
    EXPECT_EQ(decoded.r[0], 0);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(decoded.code, ElementsAre(Microcode{.op = kMicro_UL},
                                        Microcode{.op = kMicro_TEST,
                                                  .arg1 = CpuCore::C0,
                                                  .arg2 = CpuCore::C1}));
}

TEST(InstructionSetTest, RegByteOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegByte, MicroArgType::kRegByte},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kWordReg, ArgType::kWordReg,
              "UL;OP(a:l,b:h);OP(b:l,a:h);OP(a:H,b:L);OP(b:H,a:L);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 8;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, a | (b << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0 + a);
    EXPECT_EQ(decoded.r[1], CpuCore::R0 + b);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST,
              .arg1 = (-1 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte0,
              .arg2 = (-2 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte1},
          Microcode{
              .op = kMicro_TEST,
              .arg1 = (-2 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte0,
              .arg2 = (-1 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte1},
          Microcode{
              .op = kMicro_TEST,
              .arg1 = (-1 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte1,
              .arg2 = (-2 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte0},
          Microcode{
              .op = kMicro_TEST,
              .arg1 = (-2 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte1,
              .arg2 =
                  (-1 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte0}));
}

TEST(InstructionSetTest, RegNibbleOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kWordReg, ArgType::kWordReg,
              "UL;OP(a:0,b:1);OP(b:2,a:3);OP(a:3,b:2);OP(b:1,a:0);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 8;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, a | (b << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0 + a);
    EXPECT_EQ(decoded.r[1], CpuCore::R0 + b);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST,
                                    .arg1 = (-1 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble0,
                                    .arg2 = (-2 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble1},
                          Microcode{.op = kMicro_TEST,
                                    .arg1 = (-2 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble2,
                                    .arg2 = (-1 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble3},
                          Microcode{.op = kMicro_TEST,
                                    .arg1 = (-1 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble3,
                                    .arg2 = (-2 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble2},
                          Microcode{.op = kMicro_TEST,
                                    .arg1 = (-2 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble1,
                                    .arg2 = (-1 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble0}));
}

TEST(InstructionSetTest, WordOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kWordReg, ArgType::kWordReg, "UL;OP(a,b);OP(b,a);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 8;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, a | (b << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0 + a);
    EXPECT_EQ(decoded.r[1], CpuCore::R0 + b);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = -2},
                  Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = -1}));
}

TEST(InstructionSetTest, RegByteOpFromDwordArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegByte, MicroArgType::kRegByte},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kDwordReg, ArgType::kDwordReg,
              "UL;OP(a0:l,b0:h);OP(b1:l,a1:h);OP(a0:H,b0:L);OP(b1:H,a1:L);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 4;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, a | (b << 2)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + a * 2);
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + b * 2);
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST,
              .arg1 = (-1 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte0,
              .arg2 = (-2 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte1},
          Microcode{
              .op = kMicro_TEST,
              .arg1 = (-4 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte0,
              .arg2 = (-3 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte1},
          Microcode{
              .op = kMicro_TEST,
              .arg1 = (-1 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte1,
              .arg2 = (-2 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte0},
          Microcode{
              .op = kMicro_TEST,
              .arg1 = (-4 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte1,
              .arg2 =
                  (-3 & ~CpuCore::kRegMaskBytes) | CpuCore::kRegMaskByte0}));
}

TEST(InstructionSetTest, RegNibbleOpFromDwordArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kDwordReg, ArgType::kDwordReg,
              "UL;OP(a0:0,b0:1);OP(b1:2,a1:3);OP(a0:3,b0:2);OP(b1:1,a1:0);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 4;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, a | (b << 2)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + a * 2);
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + b * 2);
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  }
  EXPECT_THAT(decoded.code,
              ElementsAre(Microcode{.op = kMicro_UL},
                          Microcode{.op = kMicro_TEST,
                                    .arg1 = (-1 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble0,
                                    .arg2 = (-2 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble1},
                          Microcode{.op = kMicro_TEST,
                                    .arg1 = (-4 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble2,
                                    .arg2 = (-3 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble3},
                          Microcode{.op = kMicro_TEST,
                                    .arg1 = (-1 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble3,
                                    .arg2 = (-2 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble2},
                          Microcode{.op = kMicro_TEST,
                                    .arg1 = (-4 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble1,
                                    .arg2 = (-3 & ~CpuCore::kRegMaskNibbles) |
                                            CpuCore::kRegMaskNibble0}));
}

TEST(InstructionSetTest, WordOpFromDwordArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kDwordReg, ArgType::kDwordReg,
              "UL;OP(a0,b0);OP(b1,a1);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 4;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, a | (b << 2)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + a * 2);
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + b * 2);
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = -2},
                  Microcode{.op = kMicro_TEST, .arg1 = -4, .arg2 = -3}));
}

TEST(InstructionSetTest, MacroWordToRegByteOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegByte, MicroArgType::kRegByte},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "WORD",
       .prefix = {0b00, 2},
       .arg = ArgType::kWordReg,
       .code = "OP(R0:l,m:h);OP(m:l,R1:h);"},
      {.source = "DWORD",
       .prefix = {0b010, 3},
       .arg = ArgType::kDwordReg,
       .code = "OP(R0:l,m0:h);OP(m1:l,R1:h);"},
      {.source = "IMM",
       .prefix = {0b10, 2},
       .arg = {ArgType::kImmediate, 3},
       .code = "OP(R0:l,i:h);OP(i:l,R1:h);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 5, .code = macro_code_defs}};
  InstructionError error;

  const InstructionDef macro_first[] = {
      MakeDef({ArgType::kMacro, 5}, "UL;$Macro"),
  };
  auto codes =
      CompileInstructionSet({macro_first, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0 + i);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (-1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (-1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, (1 << 3) | i), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (-1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (-3 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, (1 << 4) | i), decoded));
    EXPECT_EQ(decoded.c[0], i);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], 0);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::C0 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::C0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  const InstructionDef macro_second[] = {
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 5}, "UL;$Macro"),
  };
  codes = CompileInstructionSet({macro_second, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i << 3), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], CpuCore::R0 + i);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (-2 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (-2 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(
        codes->Decode(MakeCode(kOp_TEST, (1 << 6) | (i << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (-2 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (-4 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(
        codes->Decode(MakeCode(kOp_TEST, (1 << 7) | (i << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], i);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::C1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::C1 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));
}

TEST(InstructionSetTest, MacroWordToRegNibbleOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "WORD",
       .prefix = {0b00, 2},
       .arg = ArgType::kWordReg,
       .code = "OP(R0:0,m:1);OP(m:2,R1:3);"},
      {.source = "DWORD",
       .prefix = {0b010, 3},
       .arg = ArgType::kDwordReg,
       .code = "OP(R0:0,m0:1);OP(m1:2,R1:3);"},
      {.source = "IMM",
       .prefix = {0b10, 2},
       .arg = {ArgType::kImmediate, 3},
       .code = "OP(R0:0,i:1);OP(i:2,R1:3);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 5, .code = macro_code_defs}};
  InstructionError error;

  const InstructionDef macro_first[] = {
      MakeDef({ArgType::kMacro, 5}, "UL;$Macro"),
  };
  auto codes =
      CompileInstructionSet({macro_first, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0 + i);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (-1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (-1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, (1 << 3) | i), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (-1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (-3 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, (1 << 4) | i), decoded));
    EXPECT_EQ(decoded.c[0], i);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], 0);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::C0 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::C0 | CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  const InstructionDef macro_second[] = {
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 5}, "UL;$Macro"),
  };
  codes = CompileInstructionSet({macro_second, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i << 3), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], CpuCore::R0 + i);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (-2 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (-2 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(
        codes->Decode(MakeCode(kOp_TEST, (1 << 6) | (i << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (-2 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (-4 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(
        codes->Decode(MakeCode(kOp_TEST, (1 << 7) | (i << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], i);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::C1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::C1 | CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));
}

TEST(InstructionSetTest, MacroWordOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "WORD",
       .prefix = {0b00, 2},
       .arg = ArgType::kWordReg,
       .code = "OP(R0,m);OP(m,R1);"},
      {.source = "DWORD",
       .prefix = {0b010, 3},
       .arg = ArgType::kDwordReg,
       .code = "OP(R0,m0);OP(m1,R1);"},
      {.source = "IMM",
       .prefix = {0b10, 2},
       .arg = {ArgType::kImmediate, 3},
       .code = "OP(R0,i);OP(i,R1);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 5, .code = macro_code_defs}};
  InstructionError error;

  const InstructionDef macro_first[] = {
      MakeDef({ArgType::kMacro, 5}, "UL;$Macro"),
  };
  auto codes =
      CompileInstructionSet({macro_first, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0 + i);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = -1},
          Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = CpuCore::R1}));

  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, (1 << 3) | i), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = -1},
          Microcode{.op = kMicro_TEST, .arg1 = -3, .arg2 = CpuCore::R1}));

  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, (1 << 4) | i), decoded));
    EXPECT_EQ(decoded.c[0], i);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], 0);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(decoded.code, ElementsAre(Microcode{.op = kMicro_UL},
                                        Microcode{.op = kMicro_TEST,
                                                  .arg1 = CpuCore::R0,
                                                  .arg2 = CpuCore::C0},
                                        Microcode{.op = kMicro_TEST,
                                                  .arg1 = CpuCore::C0,
                                                  .arg2 = CpuCore::R1}));

  const InstructionDef macro_second[] = {
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 5}, "UL;$Macro"),
  };
  codes = CompileInstructionSet({macro_second, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i << 3), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], CpuCore::R0 + i);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = -2},
          Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = CpuCore::R1}));

  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(
        codes->Decode(MakeCode(kOp_TEST, (1 << 6) | (i << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = -2},
          Microcode{.op = kMicro_TEST, .arg1 = -4, .arg2 = CpuCore::R1}));

  for (uint16_t i = 0; i < 8; ++i) {
    EXPECT_TRUE(
        codes->Decode(MakeCode(kOp_TEST, (1 << 7) | (i << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], i);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(decoded.code, ElementsAre(Microcode{.op = kMicro_UL},
                                        Microcode{.op = kMicro_TEST,
                                                  .arg1 = CpuCore::R0,
                                                  .arg2 = CpuCore::C1},
                                        Microcode{.op = kMicro_TEST,
                                                  .arg1 = CpuCore::C1,
                                                  .arg2 = CpuCore::R1}));
}

TEST(InstructionSetTest, MacroWordToRegByteParamDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegByte, MicroArgType::kRegByte},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "WORD",
       .prefix = {0, 1},
       .code = "OP(R0:l,p:h);OP(p:L,R1:H);"},
  };
  const MacroDef macro_defs[] = {{.name = "Macro",
                                  .param = ArgType::kWordReg,
                                  .size = 1,
                                  .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 1}, ArgType::kWordReg),
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 1}),
  };
  std::string code;
  auto MakeInstructionSetDef = [&](int i,
                                   std::string_view reg) -> InstructionSetDef {
    code = absl::StrCat("UL;$Macro(", reg, ");");
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    absl::string_view reg_name = CpuCore::GetWordRegName(reg);
    codes = CompileInstructionSet(MakeInstructionSetDef(0, reg_name), &error,
                                  micro_defs);
    const std::string context = absl::StrCat("Context: $Macro(", reg_name, ")");
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], 0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], 0) << context;
    EXPECT_EQ(decoded.r[3], 0) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(Microcode{.op = kMicro_UL},
                    Microcode{.op = kMicro_TEST,
                              .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                              .arg2 = reg | CpuCore::kRegMaskByte1},
                    Microcode{.op = kMicro_TEST,
                              .arg1 = reg | CpuCore::kRegMaskByte0,
                              .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}))
        << context;
  }
  codes =
      CompileInstructionSet(MakeInstructionSetDef(0, "b"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, CpuCore::R7 << 1), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::R7);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));
  codes =
      CompileInstructionSet(MakeInstructionSetDef(1, "a"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, CpuCore::R7), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::R7);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));
}

TEST(InstructionSetTest, MacroWordToRegNibbleParamDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "WORD",
       .prefix = {0, 1},
       .code = "OP(R0:0,p:1);OP(p:2,R1:3);"},
  };
  const MacroDef macro_defs[] = {{.name = "Macro",
                                  .param = ArgType::kWordReg,
                                  .size = 1,
                                  .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 1}, ArgType::kWordReg),
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 1}),
  };
  std::string code;
  auto MakeInstructionSetDef = [&](int i,
                                   std::string_view reg) -> InstructionSetDef {
    code = absl::StrCat("UL;$Macro(", reg, ");");
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    absl::string_view reg_name = CpuCore::GetWordRegName(reg);
    codes = CompileInstructionSet(MakeInstructionSetDef(0, reg_name), &error,
                                  micro_defs);
    const std::string context = absl::StrCat("Context: $Macro(", reg_name, ")");
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], 0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], 0) << context;
    EXPECT_EQ(decoded.r[3], 0) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(Microcode{.op = kMicro_UL},
                    Microcode{.op = kMicro_TEST,
                              .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                              .arg2 = reg | CpuCore::kRegMaskNibble1},
                    Microcode{.op = kMicro_TEST,
                              .arg1 = reg | CpuCore::kRegMaskNibble2,
                              .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}))
        << context;
  }
  codes =
      CompileInstructionSet(MakeInstructionSetDef(0, "b"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, CpuCore::R7 << 1), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::R7);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));
  codes =
      CompileInstructionSet(MakeInstructionSetDef(1, "a"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, CpuCore::R7), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::R7);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::A & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));
}

TEST(InstructionSetTest, MacroWordParamDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "WORD", .prefix = {0, 1}, .code = "OP(R0,p);OP(p,R1);"},
  };
  const MacroDef macro_defs[] = {{.name = "Macro",
                                  .param = ArgType::kWordReg,
                                  .size = 1,
                                  .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 1}, ArgType::kWordReg),
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 1}),
  };
  std::string code;
  auto MakeInstructionSetDef = [&](int i,
                                   std::string_view reg) -> InstructionSetDef {
    code = absl::StrCat("UL;$Macro(", reg, ");");
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    absl::string_view reg_name = CpuCore::GetWordRegName(reg);
    codes = CompileInstructionSet(MakeInstructionSetDef(0, reg_name), &error,
                                  micro_defs);
    const std::string context = absl::StrCat("Context: $Macro(", reg_name, ")");
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], 0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], 0) << context;
    EXPECT_EQ(decoded.r[3], 0) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(
            Microcode{.op = kMicro_UL},
            Microcode{.op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = reg},
            Microcode{.op = kMicro_TEST, .arg1 = reg, .arg2 = CpuCore::R1}))
        << context;
  }
  codes =
      CompileInstructionSet(MakeInstructionSetDef(0, "b"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, CpuCore::R7 << 1), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::R7);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::B},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::B, .arg2 = CpuCore::R1}));
  codes =
      CompileInstructionSet(MakeInstructionSetDef(1, "a"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, CpuCore::R7), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::R7);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::A},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::A, .arg2 = CpuCore::R1}));
}

TEST(InstructionSetTest, MacroWordToRegByteReturnDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegByte, MicroArgType::kRegByte},
  };
  MacroCodeDef macro_code_defs[] = {
      {.source = "WORD", .prefix = {0, 2}, .code = "OP(R0:L,R1:H);"},
  };
  MacroDef macro_defs[] = {{.name = "Macro",
                            .ret = ArgType::kWordReg,
                            .size = 2,
                            .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 2}),
      MakeDef({ArgType::kMacro, 2}, ArgType::kWordReg),
      MakeDef(ArgType::kDwordReg, {ArgType::kMacro, 2}),
      MakeDef({ArgType::kMacro, 2}, ArgType::kDwordReg),
  };
  auto MakeInstructionSetDef = [&](int i, ArgType param_type, ArgType arg_type,
                                   int ret_value,
                                   std::string_view code) -> InstructionSetDef {
    macro_defs[0].param = param_type;
    if (arg_type != ArgType::kNone) {
      macro_code_defs[0].arg.type = arg_type;
      macro_code_defs[0].arg.size = 2;
      macro_code_defs[0].prefix.size = 0;
    }
    macro_code_defs[0].ret = ret_value;
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    auto reg_name = CpuCore::GetWordRegName(reg);
    const std::string context = absl::StrCat("Context: ", reg_name);
    codes = CompileInstructionSet(
        MakeInstructionSetDef(0, ArgType::kNone, ArgType::kNone, reg,
                              "UL;$Macro;OP(R0:l,r:h);OP(r:L,R1:H);"),
        &error, micro_defs);
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], 0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], 0) << context;
    EXPECT_EQ(decoded.r[3], 0) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(Microcode{.op = kMicro_UL},
                    Microcode{.op = kMicro_TEST,
                              .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                              .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                    Microcode{.op = kMicro_TEST,
                              .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                              .arg2 = reg | CpuCore::kRegMaskByte1},
                    Microcode{.op = kMicro_TEST,
                              .arg1 = reg | CpuCore::kRegMaskByte0,
                              .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}))
        << context;
  }

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kWordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(R4);OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R4 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R4 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP0,
                            "UL;$Macro(D2);OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R4 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R4 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP1,
                            "UL;$Macro(D2);OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R5 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R5 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kWordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(a);OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 4), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::R4);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(2, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP0,
                            "UL;$Macro(A);OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D2 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(2, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP1,
                            "UL;$Macro(A);OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D2 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kWordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(b);OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 4 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::R4);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(3, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP0,
                            "UL;$Macro(B);OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], CpuCore::D2 + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(3, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP1,
                            "UL;$Macro(B);OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], CpuCore::D2 + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kImmediate, CpuCore::MI,
                            "UL;$Macro;OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::C1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::C1 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kImmediate, CpuCore::MI,
                            "UL;$Macro;OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::C0 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::C0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kWordReg, CpuCore::MM,
                            "UL;$Macro;OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM0,
                            "UL;$Macro;OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM1,
                            "UL;$Macro;OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kWordReg, CpuCore::MM,
                            "UL;$Macro;OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM0,
                            "UL;$Macro;OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM1,
                            "UL;$Macro;OP(R0:l,r:h);OP(r:L,R1:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskByte1}));
}

TEST(InstructionSetTest, MacroWordToRegNibbleReturnDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
  };
  MacroCodeDef macro_code_defs[] = {
      {.source = "WORD", .prefix = {0, 2}, .code = "OP(R0:0,R1:1);"},
  };
  MacroDef macro_defs[] = {{.name = "Macro",
                            .ret = ArgType::kWordReg,
                            .size = 2,
                            .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 2}),
      MakeDef({ArgType::kMacro, 2}, ArgType::kWordReg),
      MakeDef(ArgType::kDwordReg, {ArgType::kMacro, 2}),
      MakeDef({ArgType::kMacro, 2}, ArgType::kDwordReg),
  };
  auto MakeInstructionSetDef = [&](int i, ArgType param_type, ArgType arg_type,
                                   int ret_value,
                                   std::string_view code) -> InstructionSetDef {
    macro_defs[0].param = param_type;
    if (arg_type != ArgType::kNone) {
      macro_code_defs[0].arg.type = arg_type;
      macro_code_defs[0].arg.size = 2;
      macro_code_defs[0].prefix.size = 0;
    }
    macro_code_defs[0].ret = ret_value;
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    auto reg_name = CpuCore::GetWordRegName(reg);
    const std::string context = absl::StrCat("Context: ", reg_name);
    codes = CompileInstructionSet(
        MakeInstructionSetDef(0, ArgType::kNone, ArgType::kNone, reg,
                              "UL;$Macro;OP(R0:0,r:1);OP(r:2,R1:3);"),
        &error, micro_defs);
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], 0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], 0) << context;
    EXPECT_EQ(decoded.r[3], 0) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(Microcode{.op = kMicro_UL},
                    Microcode{.op = kMicro_TEST,
                              .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                              .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                    Microcode{.op = kMicro_TEST,
                              .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                              .arg2 = reg | CpuCore::kRegMaskNibble1},
                    Microcode{.op = kMicro_TEST,
                              .arg1 = reg | CpuCore::kRegMaskNibble2,
                              .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}))
        << context;
  }

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kWordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(R4);OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R4 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R4 | CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP0,
                            "UL;$Macro(D2);OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R4 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R4 | CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP1,
                            "UL;$Macro(D2);OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R5 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R5 | CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kWordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(a);OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 4), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::R4);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(2, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP0,
                            "UL;$Macro(A);OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D2 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(2, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP1,
                            "UL;$Macro(A);OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D2 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kWordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(b);OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 4 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::R4);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(3, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP0,
                            "UL;$Macro(B);OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], CpuCore::D2 + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(3, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP1,
                            "UL;$Macro(B);OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], CpuCore::D2 + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kImmediate, CpuCore::MI,
                            "UL;$Macro;OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::C1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::C1 | CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kImmediate, CpuCore::MI,
                            "UL;$Macro;OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::C0 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::C0 | CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kWordReg, CpuCore::MM,
                            "UL;$Macro;OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM0,
                            "UL;$Macro;OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM1,
                            "UL;$Macro;OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kWordReg, CpuCore::MM,
                            "UL;$Macro;OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::A & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM0,
                            "UL;$Macro;OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM1,
                            "UL;$Macro;OP(R0:0,r:1);OP(r:2,R1:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = CpuCore::R0 | CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = CpuCore::R1 | CpuCore::kRegMaskNibble3}));
}

TEST(InstructionSetTest, MacroWordReturnDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  MacroCodeDef macro_code_defs[] = {
      {.source = "WORD", .prefix = {0, 2}, .code = "OP(R0,R1);"},
  };
  MacroDef macro_defs[] = {{.name = "Macro",
                            .ret = ArgType::kWordReg,
                            .size = 2,
                            .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 2}),
      MakeDef({ArgType::kMacro, 2}, ArgType::kWordReg),
      MakeDef(ArgType::kDwordReg, {ArgType::kMacro, 2}),
      MakeDef({ArgType::kMacro, 2}, ArgType::kDwordReg),
  };
  auto MakeInstructionSetDef = [&](int i, ArgType param_type, ArgType arg_type,
                                   int ret_value,
                                   std::string_view code) -> InstructionSetDef {
    macro_defs[0].param = param_type;
    if (arg_type != ArgType::kNone) {
      macro_code_defs[0].arg.type = arg_type;
      macro_code_defs[0].arg.size = 2;
      macro_code_defs[0].prefix.size = 0;
    }
    macro_code_defs[0].ret = ret_value;
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    auto reg_name = CpuCore::GetWordRegName(reg);
    const std::string context = absl::StrCat("Context: ", reg_name);
    codes = CompileInstructionSet(
        MakeInstructionSetDef(0, ArgType::kNone, ArgType::kNone, reg,
                              "UL;$Macro;OP(R0,r);OP(r,R1);"),
        &error, micro_defs);
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], 0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], 0) << context;
    EXPECT_EQ(decoded.r[3], 0) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(
            Microcode{.op = kMicro_UL},
            Microcode{
                .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
            Microcode{.op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = reg},
            Microcode{.op = kMicro_TEST, .arg1 = reg, .arg2 = CpuCore::R1}))
        << context;
  }

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kWordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(R4);OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R4},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R4, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP0,
                            "UL;$Macro(D2);OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R4},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R4, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP1,
                            "UL;$Macro(D2);OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R5},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R5, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kWordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(a);OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 4), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::R4);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::A0},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::A0, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(2, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP0,
                            "UL;$Macro(A);OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D2 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::A0},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::A0, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(2, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP1,
                            "UL;$Macro(A);OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D2 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::A1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::A1, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kWordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(b);OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 4 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::R4);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::B0},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::B0, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(3, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP0,
                            "UL;$Macro(B);OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], CpuCore::D2 + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::B0},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::B0, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(3, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP1,
                            "UL;$Macro(B);OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], CpuCore::D2 + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::B1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::B1, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kImmediate, CpuCore::MI,
                            "UL;$Macro;OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::C1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::C1, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kImmediate, CpuCore::MI,
                            "UL;$Macro;OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::C0},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::C0, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kWordReg, CpuCore::MM,
                            "UL;$Macro;OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::B},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::B, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM0,
                            "UL;$Macro;OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::B0},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::B0, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM1,
                            "UL;$Macro;OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::B1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::B1, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kWordReg, CpuCore::MM,
                            "UL;$Macro;OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::A},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::A, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM0,
                            "UL;$Macro;OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::A0},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::A0, .arg2 = CpuCore::R1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM1,
                            "UL;$Macro;OP(R0,r);OP(r,R1);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::A1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::A1, .arg2 = CpuCore::R1}));
}

TEST(InstructionSetTest, DwordToRegByteOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegByte, MicroArgType::kRegByte},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kDwordReg, ArgType::kDwordReg,
              "UL;OP(a0:l,b0:h);OP(b0:l,a0:h);OP(a1:L,b1:H);OP(b1:L,a1:H);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 4;
    const std::string context = absl::StrCat("Context: a=", a, ", b=", b);
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, a | (b << 2)), decoded))
        << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + a * 2) << context;
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + b * 2) << context;
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1) << context;
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1) << context;
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1}));
}

TEST(InstructionSetTest, DwordToRegNibbleOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kDwordReg, ArgType::kDwordReg,
              "UL;OP(a0:0,b0:1);OP(b0:2,a0:3);OP(a1:0,b1:1);OP(b1:2,a1:3);"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 4;
    const std::string context = absl::StrCat("Context: a=", a, ", b=", b);
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, a | (b << 2)), decoded))
        << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + a * 2) << context;
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + b * 2) << context;
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1) << context;
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1) << context;
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble3},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble3}));
}

TEST(InstructionSetTest, DwordOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  const InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kDwordReg, ArgType::kDwordReg, "UL;OP(A,B);OP(B,A)"),
  };

  InstructionError error;
  auto codes = CompileInstructionSet({.instructions = instruction_defs}, &error,
                                     micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 4;
    const std::string context = absl::StrCat("Context: a=", a, ", b=", b);
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, a | (b << 2)), decoded))
        << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + a * 2) << context;
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + b * 2) << context;
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1) << context;
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1) << context;
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = -2},
                  Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = -1}));
}

TEST(InstructionSetTest, MacroDwordToRegByteOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegByte, MicroArgType::kRegByte},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "DWORD",
       .arg = ArgType::kDwordReg,
       .code = "OP(m0:l,m1:h);OP(m1:L,m0:H);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 2, .code = macro_code_defs}};
  InstructionError error;

  const InstructionDef macro_first[] = {{
      MakeDef({ArgType::kMacro, 2}, "UL;$Macro"),
  }};
  auto codes =
      CompileInstructionSet({macro_first, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1}));

  const InstructionDef macro_second[] = {
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 2}, "UL;$Macro"),
  };
  codes = CompileInstructionSet({macro_second, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i << 3), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1}));
}

TEST(InstructionSetTest, MacroDwordToRegNibbleOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "DWORD",
       .arg = ArgType::kDwordReg,
       .code = "OP(m0:0,m1:1);OP(m1:2,m0:3);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 2, .code = macro_code_defs}};
  InstructionError error;

  const InstructionDef macro_first[] = {{
      MakeDef({ArgType::kMacro, 2}, "UL;$Macro"),
  }};
  auto codes =
      CompileInstructionSet({macro_first, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble3}));

  const InstructionDef macro_second[] = {
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 2}, "UL;$Macro"),
  };
  codes = CompileInstructionSet({macro_second, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i << 3), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_TEST,
                            .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble3}));
}

TEST(InstructionSetTest, MacroDwordOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "DWORD",
       .arg = ArgType::kDwordReg,
       .code = "OP(D0,M);OP(M,D1);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 2, .code = macro_code_defs}};
  InstructionError error;

  const InstructionDef macro_first[] = {{
      MakeDef({ArgType::kMacro, 2}, "UL;$Macro"),
  }};
  auto codes =
      CompileInstructionSet({macro_first, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[1], 0);
    EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
    EXPECT_EQ(decoded.r[3], 0);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = -1},
          Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = CpuCore::D1}));

  const InstructionDef macro_second[] = {
      MakeDef(ArgType::kWordReg, {ArgType::kMacro, 2}, "UL;$Macro"),
  };
  codes = CompileInstructionSet({macro_second, macro_defs}, &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i << 3), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0);
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + i * 2);
    EXPECT_EQ(decoded.r[2], 0);
    EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = -2},
          Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = CpuCore::D1}));
}

TEST(InstructionSetTest, MacroDwordToRegByteParamDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_MVB, "MVB", MicroArgType::kRegByte, MicroArgType::kRegByte},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "DWORD",
       .prefix = {0, 1},
       .code = "MVB(p0:l,p1:h);MVB(p1:l,p0:h);"},
  };
  const MacroDef macro_defs[] = {{.name = "Macro",
                                  .param = ArgType::kDwordReg,
                                  .size = 1,
                                  .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 1}, ArgType::kDwordReg),
      MakeDef(ArgType::kDwordReg, {ArgType::kMacro, 1}),
  };
  std::string code;
  auto MakeInstructionSetDef = [&](int i,
                                   std::string_view reg) -> InstructionSetDef {
    code = absl::StrCat("UL;$Macro(", reg, ");");
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    std::string_view reg_name = CpuCore::GetDwordRegName(reg);
    if (reg_name == "invalid") {
      continue;
    }
    codes = CompileInstructionSet(MakeInstructionSetDef(0, reg_name), &error,
                                  micro_defs);
    const std::string context = absl::StrCat("Context: $Macro(", reg_name, ")");
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], 0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], 0) << context;
    EXPECT_EQ(decoded.r[3], 1) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(Microcode{.op = kMicro_UL},
                    Microcode{.op = kMicro_MVB,
                              .arg1 = reg | CpuCore::kRegMaskByte0,
                              .arg2 = (reg + 1) | CpuCore::kRegMaskByte1},
                    Microcode{.op = kMicro_MVB,
                              .arg1 = (reg + 1) | CpuCore::kRegMaskByte0,
                              .arg2 = reg | CpuCore::kRegMaskByte1}))
        << context;
  }
  codes =
      CompileInstructionSet(MakeInstructionSetDef(0, "B"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 1), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_MVB,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_MVB,
                            .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1}));
  codes =
      CompileInstructionSet(MakeInstructionSetDef(1, "A"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_MVB,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1},
                  Microcode{.op = kMicro_MVB,
                            .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte0,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                                    CpuCore::kRegMaskByte1}));
}

TEST(InstructionSetTest, MacroDwordToRegNibbleParamDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_MVN, "MVN", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "DWORD",
       .prefix = {0, 1},
       .code = "MVN(p0:0,p1:1);MVN(p1:2,p0:3);"},
  };
  const MacroDef macro_defs[] = {{.name = "Macro",
                                  .param = ArgType::kDwordReg,
                                  .size = 1,
                                  .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 1}, ArgType::kDwordReg),
      MakeDef(ArgType::kDwordReg, {ArgType::kMacro, 1}),
  };
  std::string code;
  auto MakeInstructionSetDef = [&](int i,
                                   std::string_view reg) -> InstructionSetDef {
    code = absl::StrCat("UL;$Macro(", reg, ");");
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    std::string_view reg_name = CpuCore::GetDwordRegName(reg);
    if (reg_name == "invalid") {
      continue;
    }
    codes = CompileInstructionSet(MakeInstructionSetDef(0, reg_name), &error,
                                  micro_defs);
    const std::string context = absl::StrCat("Context: $Macro(", reg_name, ")");
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], 0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], 0) << context;
    EXPECT_EQ(decoded.r[3], 1) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(Microcode{.op = kMicro_UL},
                    Microcode{.op = kMicro_MVN,
                              .arg1 = reg | CpuCore::kRegMaskNibble0,
                              .arg2 = (reg + 1) | CpuCore::kRegMaskNibble1},
                    Microcode{.op = kMicro_MVN,
                              .arg1 = (reg + 1) | CpuCore::kRegMaskNibble2,
                              .arg2 = reg | CpuCore::kRegMaskNibble3}))
        << context;
  }
  codes =
      CompileInstructionSet(MakeInstructionSetDef(0, "B"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 1), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_MVN,
                            .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_MVN,
                            .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble3}));
  codes =
      CompileInstructionSet(MakeInstructionSetDef(1, "A"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_MVN,
                            .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble0,
                            .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble1},
                  Microcode{.op = kMicro_MVN,
                            .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble2,
                            .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                                    CpuCore::kRegMaskNibble3}));
}

TEST(InstructionSetTest, MacroDwordParamDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  const MacroCodeDef macro_code_defs[] = {
      {.source = "DWORD",
       .prefix = {0, 1},
       .code = "OP(D0,P);OP(P,D1);"
               "MOV(R4,p0);MOV(R5,p1);MOV(p0,R6);MOV(p1,R7);"},
  };
  const MacroDef macro_defs[] = {{.name = "Macro",
                                  .param = ArgType::kDwordReg,
                                  .size = 1,
                                  .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 1}, ArgType::kDwordReg),
      MakeDef(ArgType::kDwordReg, {ArgType::kMacro, 1}),
  };
  std::string code;
  auto MakeInstructionSetDef = [&](int i,
                                   std::string_view reg) -> InstructionSetDef {
    code = absl::StrCat("UL;$Macro(", reg, ");");
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    std::string_view reg_name = CpuCore::GetDwordRegName(reg);
    if (reg_name == "invalid") {
      continue;
    }
    codes = CompileInstructionSet(MakeInstructionSetDef(0, reg_name), &error,
                                  micro_defs);
    const std::string context = absl::StrCat("Context: $Macro(", reg_name, ")");
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], 0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], 0) << context;
    EXPECT_EQ(decoded.r[3], 1) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(
            Microcode{.op = kMicro_UL},
            Microcode{.op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = reg},
            Microcode{.op = kMicro_TEST, .arg1 = reg, .arg2 = CpuCore::D1},
            Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R4, .arg2 = reg},
            Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R5, .arg2 = reg + 1},
            Microcode{.op = kMicro_MOV, .arg1 = reg, .arg2 = CpuCore::R6},
            Microcode{.op = kMicro_MOV, .arg1 = reg + 1, .arg2 = CpuCore::R7}))
        << context;
  }
  codes =
      CompileInstructionSet(MakeInstructionSetDef(0, "B"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 1), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], decoded.r[1] + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = -2},
          Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R4, .arg2 = -2},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R5, .arg2 = -4},
          Microcode{.op = kMicro_MOV, .arg1 = -2, .arg2 = CpuCore::R6},
          Microcode{.op = kMicro_MOV, .arg1 = -4, .arg2 = CpuCore::R7}));
  codes =
      CompileInstructionSet(MakeInstructionSetDef(1, "A"), &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], decoded.r[0] + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = -1},
          Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R4, .arg2 = -1},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R5, .arg2 = -3},
          Microcode{.op = kMicro_MOV, .arg1 = -1, .arg2 = CpuCore::R6},
          Microcode{.op = kMicro_MOV, .arg1 = -3, .arg2 = CpuCore::R7}));
}

TEST(InstructionSetTest, MacroDwordReturnToRegByteDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_MVB, "MVB", MicroArgType::kRegByte, MicroArgType::kRegByte},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  MacroCodeDef macro_code_defs[] = {
      {.source = "DWORD", .prefix = {0, 2}, .code = "OP(D0,D1);"},
  };
  MacroDef macro_defs[] = {{.name = "Macro",
                            .ret = ArgType::kDwordReg,
                            .size = 2,
                            .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kDwordReg, {ArgType::kMacro, 2}),
      MakeDef({ArgType::kMacro, 2}, ArgType::kDwordReg),
  };
  auto MakeInstructionSetDef = [&](int i, ArgType param_type, ArgType arg_type,
                                   int ret_value,
                                   std::string_view code) -> InstructionSetDef {
    macro_defs[0].param = param_type;
    if (arg_type != ArgType::kNone) {
      macro_code_defs[0].arg.type = arg_type;
      macro_code_defs[0].arg.size = 2;
      macro_code_defs[0].prefix.size = 0;
    }
    macro_code_defs[0].ret = ret_value;
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    if (!CpuCore::IsDwordReg(reg)) {
      continue;
    }
    auto reg_name = CpuCore::GetDwordRegName(reg);
    const std::string context = absl::StrCat("Context: ", reg_name);
    codes = CompileInstructionSet(
        MakeInstructionSetDef(
            0, ArgType::kNone, ArgType::kNone, reg,
            "UL;$Macro;"
            "MVB(R4:l,r0:h);MVB(r0:l,R5:h);MVB(R6:L,r1:H);MVB(r1:L,R7:H);"),
        &error, micro_defs);
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], CpuCore::D0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], CpuCore::D0 + 1) << context;
    EXPECT_EQ(decoded.r[3], 0) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(
            Microcode{.op = kMicro_UL},
            Microcode{
                .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
            Microcode{.op = kMicro_MVB,
                      .arg1 = CpuCore::R4 | CpuCore::kRegMaskByte0,
                      .arg2 = reg | CpuCore::kRegMaskByte1},
            Microcode{.op = kMicro_MVB,
                      .arg1 = reg | CpuCore::kRegMaskByte0,
                      .arg2 = CpuCore::R5 | CpuCore::kRegMaskByte1},
            Microcode{.op = kMicro_MVB,
                      .arg1 = CpuCore::R6 | CpuCore::kRegMaskByte0,
                      .arg2 = (reg + 1) | CpuCore::kRegMaskByte1},
            Microcode{.op = kMicro_MVB,
                      .arg1 = (reg + 1) | CpuCore::kRegMaskByte0,
                      .arg2 = CpuCore::R7 | CpuCore::kRegMaskByte1}))
        << context;
  }

  codes = CompileInstructionSet(
      MakeInstructionSetDef(
          0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP,
          "UL;$Macro(D2);"
          "MVB(R4:l,r0:h);MVB(r0:l,R5:h);MVB(R6:L,r1:H);MVB(r1:L,R7:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D0 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R4 | CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R6 | CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R5 | CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R7 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(
          0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP,
          "UL;$Macro(A);"
          "MVB(R4:l,r0:h);MVB(r0:l,R5:h);MVB(R6:L,r1:H);MVB(r1:L,R7:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D2 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskByte0,
                    .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R6 | CpuCore::kRegMaskByte0,
                    .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R7 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(
          1, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP,
          "UL;$Macro(B);"
          "MVB(R4:l,r0:h);MVB(r0:l,R5:h);MVB(R6:L,r1:H);MVB(r1:L,R7:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], CpuCore::D2 + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskByte0,
                    .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R6 | CpuCore::kRegMaskByte0,
                    .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R7 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(
          0, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM,
          "UL;$Macro;"
          "MVB(R4:l,r0:h);MVB(r0:l,R5:h);MVB(R6:L,r1:H);MVB(r1:L,R7:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskByte0,
                    .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R6 | CpuCore::kRegMaskByte0,
                    .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R7 | CpuCore::kRegMaskByte1}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(
          1, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM,
          "UL;$Macro;"
          "MVB(R4:l,r0:h);MVB(r0:l,R5:h);MVB(R6:L,r1:H);MVB(r1:L,R7:H);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskByte0,
                    .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = CpuCore::R6 | CpuCore::kRegMaskByte0,
                    .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte1},
          Microcode{.op = kMicro_MVB,
                    .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskBytes) |
                            CpuCore::kRegMaskByte0,
                    .arg2 = CpuCore::R7 | CpuCore::kRegMaskByte1}));
}

TEST(InstructionSetTest, MacroDwordReturnToRegNibbleDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_MVN, "MVN", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  MacroCodeDef macro_code_defs[] = {
      {.source = "DWORD", .prefix = {0, 2}, .code = "OP(D0,D1);"},
  };
  MacroDef macro_defs[] = {{.name = "Macro",
                            .ret = ArgType::kDwordReg,
                            .size = 2,
                            .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kDwordReg, {ArgType::kMacro, 2}),
      MakeDef({ArgType::kMacro, 2}, ArgType::kDwordReg),
  };
  auto MakeInstructionSetDef = [&](int i, ArgType param_type, ArgType arg_type,
                                   int ret_value,
                                   std::string_view code) -> InstructionSetDef {
    macro_defs[0].param = param_type;
    if (arg_type != ArgType::kNone) {
      macro_code_defs[0].arg.type = arg_type;
      macro_code_defs[0].arg.size = 2;
      macro_code_defs[0].prefix.size = 0;
    }
    macro_code_defs[0].ret = ret_value;
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    if (!CpuCore::IsDwordReg(reg)) {
      continue;
    }
    auto reg_name = CpuCore::GetDwordRegName(reg);
    const std::string context = absl::StrCat("Context: ", reg_name);
    codes = CompileInstructionSet(
        MakeInstructionSetDef(
            0, ArgType::kNone, ArgType::kNone, reg,
            "UL;$Macro;"
            "MVN(R4:0,r0:1);MVN(r0:2,R5:3);MVN(R6:0,r1:1);MVN(r1:2,R7:3);"),
        &error, micro_defs);
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], CpuCore::D0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], CpuCore::D0 + 1) << context;
    EXPECT_EQ(decoded.r[3], 0) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(
            Microcode{.op = kMicro_UL},
            Microcode{
                .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
            Microcode{.op = kMicro_MVN,
                      .arg1 = CpuCore::R4 | CpuCore::kRegMaskNibble0,
                      .arg2 = reg | CpuCore::kRegMaskNibble1},
            Microcode{.op = kMicro_MVN,
                      .arg1 = reg | CpuCore::kRegMaskNibble2,
                      .arg2 = CpuCore::R5 | CpuCore::kRegMaskNibble3},
            Microcode{.op = kMicro_MVN,
                      .arg1 = CpuCore::R6 | CpuCore::kRegMaskNibble0,
                      .arg2 = (reg + 1) | CpuCore::kRegMaskNibble1},
            Microcode{.op = kMicro_MVN,
                      .arg1 = (reg + 1) | CpuCore::kRegMaskNibble2,
                      .arg2 = CpuCore::R7 | CpuCore::kRegMaskNibble3}))
        << context;
  }

  codes = CompileInstructionSet(
      MakeInstructionSetDef(
          0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP,
          "UL;$Macro(D2);"
          "MVN(R4:0,r0:1);MVN(r0:2,R5:3);MVN(R6:0,r1:1);MVN(r1:2,R7:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D0 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskNibble0,
                    .arg2 = CpuCore::R4 | CpuCore::kRegMaskNibble1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskNibble2,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskNibble3},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R6 | CpuCore::kRegMaskNibble0,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskNibble1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R5 | CpuCore::kRegMaskNibble2,
                    .arg2 = CpuCore::R7 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(
          0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP,
          "UL;$Macro(A);"
          "MVN(R4:0,r0:1);MVN(r0:2,R5:3);MVN(R6:0,r1:1);MVN(r1:2,R7:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D2 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskNibble0,
                    .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble2,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskNibble3},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R6 | CpuCore::kRegMaskNibble0,
                    .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble2,
                    .arg2 = CpuCore::R7 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(
          1, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP,
          "UL;$Macro(B);"
          "MVN(R4:0,r0:1);MVN(r0:2,R5:3);MVN(R6:0,r1:1);MVN(r1:2,R7:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], CpuCore::D2 + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskNibble0,
                    .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble2,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskNibble3},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R6 | CpuCore::kRegMaskNibble0,
                    .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble2,
                    .arg2 = CpuCore::R7 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(
          0, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM,
          "UL;$Macro;"
          "MVN(R4:0,r0:1);MVN(r0:2,R5:3);MVN(R6:0,r1:1);MVN(r1:2,R7:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskNibble0,
                    .arg2 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = (CpuCore::B0 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble2,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskNibble3},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R6 | CpuCore::kRegMaskNibble0,
                    .arg2 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = (CpuCore::B1 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble2,
                    .arg2 = CpuCore::R7 | CpuCore::kRegMaskNibble3}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(
          1, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM,
          "UL;$Macro;"
          "MVN(R4:0,r0:1);MVN(r0:2,R5:3);MVN(R6:0,r1:1);MVN(r1:2,R7:3);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R4 | CpuCore::kRegMaskNibble0,
                    .arg2 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = (CpuCore::A0 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble2,
                    .arg2 = CpuCore::R5 | CpuCore::kRegMaskNibble3},
          Microcode{.op = kMicro_MVN,
                    .arg1 = CpuCore::R6 | CpuCore::kRegMaskNibble0,
                    .arg2 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble1},
          Microcode{.op = kMicro_MVN,
                    .arg1 = (CpuCore::A1 & ~CpuCore::kRegMaskNibbles) |
                            CpuCore::kRegMaskNibble2,
                    .arg2 = CpuCore::R7 | CpuCore::kRegMaskNibble3}));
}

TEST(InstructionSetTest, MacroDwordReturnDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  MacroCodeDef macro_code_defs[] = {
      {.source = "DWORD", .prefix = {0, 2}, .code = "OP(D0,D1);"},
  };
  MacroDef macro_defs[] = {{.name = "Macro",
                            .ret = ArgType::kDwordReg,
                            .size = 2,
                            .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef(ArgType::kDwordReg, {ArgType::kMacro, 2}),
      MakeDef({ArgType::kMacro, 2}, ArgType::kDwordReg),
  };
  auto MakeInstructionSetDef = [&](int i, ArgType param_type, ArgType arg_type,
                                   int ret_value,
                                   std::string_view code) -> InstructionSetDef {
    macro_defs[0].param = param_type;
    if (arg_type != ArgType::kNone) {
      macro_code_defs[0].arg.type = arg_type;
      macro_code_defs[0].arg.size = 2;
      macro_code_defs[0].prefix.size = 0;
    }
    macro_code_defs[0].ret = ret_value;
    instruction_defs[i].code = code;
    return {absl::Span(&instruction_defs[i], 1), macro_defs};
  };
  InstructionError error;
  std::shared_ptr<const InstructionSet> codes;
  DecodedInstruction decoded;

  for (int8_t reg = 0; reg < CpuCore::kRegisterCount; ++reg) {
    if (!CpuCore::IsDwordReg(reg)) {
      continue;
    }
    auto reg_name = CpuCore::GetDwordRegName(reg);
    const std::string context = absl::StrCat("Context: ", reg_name);
    codes = CompileInstructionSet(
        MakeInstructionSetDef(0, ArgType::kNone, ArgType::kNone, reg,
                              "UL;$Macro;"
                              "OP(D0,R);OP(R,D1);"
                              "MOV(R4,r0);MOV(r0,R5);MOV(R6,r1);MOV(r1,R7);"),
        &error, micro_defs);
    EXPECT_THAT(error.message, IsEmpty()) << context;
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded)) << context;
    EXPECT_EQ(decoded.c[0], 0) << context;
    EXPECT_EQ(decoded.c[1], 0) << context;
    EXPECT_EQ(decoded.r[0], CpuCore::D0) << context;
    EXPECT_EQ(decoded.r[1], 0) << context;
    EXPECT_EQ(decoded.r[2], CpuCore::D0 + 1) << context;
    EXPECT_EQ(decoded.r[3], 0) << context;
    EXPECT_THAT(
        decoded.code,
        ElementsAre(
            Microcode{.op = kMicro_UL},
            Microcode{
                .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
            Microcode{.op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = reg},
            Microcode{.op = kMicro_TEST, .arg1 = reg, .arg2 = CpuCore::D1},
            Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R4, .arg2 = reg},
            Microcode{.op = kMicro_MOV, .arg1 = reg, .arg2 = CpuCore::R5},
            Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R6, .arg2 = reg + 1},
            Microcode{.op = kMicro_MOV, .arg1 = reg + 1, .arg2 = CpuCore::R7}))
        << context;
  }

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(D2);"
                            "OP(D0,R);OP(R,D1);"
                            "MOV(R4,r0);MOV(r0,R5);MOV(R6,r1);MOV(r1,R7);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D0 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D2},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D2, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R4, .arg2 = CpuCore::R4},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R4, .arg2 = CpuCore::R5},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R6, .arg2 = CpuCore::R5},
          Microcode{
              .op = kMicro_MOV, .arg1 = CpuCore::R5, .arg2 = CpuCore::R7}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(A);"
                            "OP(D0,R);OP(R,D1);"
                            "MOV(R4,r0);MOV(r0,R5);MOV(R6,r1);MOV(r1,R7);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], CpuCore::D2);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], CpuCore::D2 + 1);
  EXPECT_EQ(decoded.r[3], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = -1},
          Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R4, .arg2 = -1},
          Microcode{.op = kMicro_MOV, .arg1 = -1, .arg2 = CpuCore::R5},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R6, .arg2 = -3},
          Microcode{.op = kMicro_MOV, .arg1 = -3, .arg2 = CpuCore::R7}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kDwordReg, ArgType::kNone, CpuCore::MP,
                            "UL;$Macro(B);"
                            "OP(D0,R);OP(R,D1);"
                            "MOV(R4,r0);MOV(r0,R5);MOV(R6,r1);MOV(r1,R7);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2 << 2), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], CpuCore::D2);
  EXPECT_EQ(decoded.r[2], 0);
  EXPECT_EQ(decoded.r[3], CpuCore::D2 + 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = -2},
          Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R4, .arg2 = -2},
          Microcode{.op = kMicro_MOV, .arg1 = -2, .arg2 = CpuCore::R5},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R6, .arg2 = -4},
          Microcode{.op = kMicro_MOV, .arg1 = -4, .arg2 = CpuCore::R7}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(0, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM,
                            "UL;$Macro;"
                            "OP(D0,R);OP(R,D1);"
                            "MOV(R4,r0);MOV(r0,R5);MOV(R6,r1);MOV(r1,R7);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::B},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::B, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R4, .arg2 = CpuCore::B0},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::B0, .arg2 = CpuCore::R5},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R6, .arg2 = CpuCore::B1},
          Microcode{
              .op = kMicro_MOV, .arg1 = CpuCore::B1, .arg2 = CpuCore::R7}));

  codes = CompileInstructionSet(
      MakeInstructionSetDef(1, ArgType::kNone, ArgType::kDwordReg, CpuCore::MM,
                            "UL;$Macro;"
                            "OP(D0,R);OP(R,D1);"
                            "MOV(R4,r0);MOV(r0,R5);MOV(R6,r1);MOV(r1,R7);"),
      &error, micro_defs);
  EXPECT_THAT(error.message, IsEmpty());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_EQ(decoded.r[2], 1);
  EXPECT_EQ(decoded.r[3], 1);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::A},
          Microcode{.op = kMicro_TEST, .arg1 = CpuCore::A, .arg2 = CpuCore::D1},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R4, .arg2 = CpuCore::A0},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::A0, .arg2 = CpuCore::R5},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R6, .arg2 = CpuCore::A1},
          Microcode{
              .op = kMicro_MOV, .arg1 = CpuCore::A1, .arg2 = CpuCore::R7}));
}

TEST(InstructionSetTest, DefaultCodeSizeIsOne) {
  InstructionDef instruction_def = MakeDef("UL;");
  InstructionError error;
  auto codes =
      CompileInstructionSet({.instructions = {instruction_def}}, &error);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

TEST(InstructionSetTest, LoadDuringFetchIncreasesCodeSize) {
  InstructionDef instruction_def = MakeDef("LD(C0);UL;");
  InstructionError error;
  auto codes =
      CompileInstructionSet({.instructions = {instruction_def}}, &error);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 2);
}

TEST(InstructionSetTest, TwoLoadsDuringFetch) {
  InstructionDef instruction_def = MakeDef("LD(C0);LD(C1);UL;");
  InstructionError error;
  auto codes =
      CompileInstructionSet({.instructions = {instruction_def}}, &error);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 3);
}

TEST(InstructionSetTest, LoadAfterFetchDoesNotIncreaseCodeSize) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);ADR(C0);LD(C0);UL;");
  InstructionError error;
  auto codes =
      CompileInstructionSet({.instructions = {instruction_def}}, &error);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

TEST(InstructionSetTest, LoadAfterAddressDuringFetchDoesNotIncreaseCodeSize) {
  InstructionDef instruction_def = MakeDef("ADR(C0);LD(C0);UL;");
  InstructionError error;
  auto codes =
      CompileInstructionSet({.instructions = {instruction_def}}, &error);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

TEST(InstructionSetTest, EmptyMacroCodeInInstruction) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = ""}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef({ArgType::kMacro, 1},
                                           "UL;"
                                           "@Before:JC(NZ,@After);"
                                           "@At:$Macro;"
                                           "@After:JC(NS,@At);"
                                           "JC(NC,@Before);");
  InstructionError instruction_error;
  auto instruction_set = CompileInstructionSet({{instruction_def}, {macro_def}},
                                               &instruction_error);
  ASSERT_EQ(instruction_error.message, "");
  DecodedInstruction decoded;
  EXPECT_TRUE(instruction_set->Decode(instruction_def.Encode(1), decoded));
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_JC, .arg1 = CpuCore::ZShift, .arg2 = 0},
          Microcode{.op = kMicro_JC, .arg1 = CpuCore::SShift, .arg2 = -1},
          Microcode{.op = kMicro_JC, .arg1 = CpuCore::CShift, .arg2 = -3}));
}

TEST(InstructionSetTest, MacroChangesCodeSize) {
  const MacroCodeDef macro_code_defs[] = {
      {.source = "ONE", .prefix = {0, 2}, .code = "LD(R0);"},
      {.source = "TWO", .prefix = {1, 2}, .code = "LD(R0);LD(R1);"},
      {.source = "THREE", .prefix = {2, 2}, .code = "LD(R0);LD(R1);LD(R2);"},
      {.source = "FOUR",
       .prefix = {3, 2},
       .code = "LD(R0);LD(R1);LD(R2);LD(R3);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 2, .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 2}, "$Macro;UL;")};
  InstructionError error;
  auto codes = CompileInstructionSet({instruction_defs, macro_defs}, &error);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, i), decoded));
    EXPECT_EQ(decoded.size, i + 2);
  }
}

TEST(InstructionSetTest, UndefinedMacroOptionsResultInNop) {
  const MacroCodeDef macro_code_defs[] = {
      {.source = "ONE", .prefix = {0, 2}, .code = "MOV(R0,R1);"},
      {.source = "TWO", .prefix = {1, 2}, .code = "MOV(R0,R2);"},
      {.source = "FOUR", .prefix = {3, 2}, .code = "MOV(R0,R4);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 2, .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 2}, "UL;$Macro;")};
  InstructionError error;
  auto codes = CompileInstructionSet({instruction_defs, macro_defs}, &error);
  EXPECT_THAT(error.message, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_FALSE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_THAT(decoded.code, ElementsAre(Microcode{.op = kMicro_UL}));
}

TEST(InstructionSetTest, MacroArgumentsShareSubInstructions) {
  const MacroCodeDef macro_code_defs[] = {
      {.source = "DREG",
       .prefix = {0, 1},
       .arg = ArgType::kDwordReg,
       .code = "MOV(R0,m0);MOV(R1,m1);"},
      {.source = "VALUE",
       .prefix = {1, 1},
       .arg = {ArgType::kImmediate, 2},
       .code = "MOV(R0,i);MOVI(R1,0);"},
  };
  const MacroDef macro_defs[] = {
      {.name = "Macro", .size = 3, .code = macro_code_defs}};
  InstructionDef instruction_defs[] = {
      MakeDef({ArgType::kMacro, 3}, "UL;$Macro;")};
  InstructionError error;
  auto codes = CompileInstructionSet({instruction_defs, macro_defs}, &error);
  EXPECT_THAT(error.message, IsEmpty());

  DecodedInstruction decoded;

  DecodedInstruction decoded0;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 0), decoded0));
  EXPECT_THAT(
      decoded0.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R0, .arg2 = CpuCore::A0},
          Microcode{
              .op = kMicro_MOV, .arg1 = CpuCore::R1, .arg2 = CpuCore::A1}));
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 1), decoded));
  EXPECT_EQ(decoded.code.data(), decoded0.code.data());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 2), decoded));
  EXPECT_EQ(decoded.code.data(), decoded0.code.data());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 3), decoded));
  EXPECT_EQ(decoded.code.data(), decoded0.code.data());

  DecodedInstruction decoded1;
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 4), decoded1));
  EXPECT_THAT(
      decoded1.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{.op = kMicro_MOV, .arg1 = CpuCore::R0, .arg2 = CpuCore::C0},
          Microcode{.op = kMicro_MOVI, .arg1 = CpuCore::R1, .arg2 = 0}));
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 5), decoded));
  EXPECT_EQ(decoded.code.data(), decoded1.code.data());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 6), decoded));
  EXPECT_EQ(decoded.code.data(), decoded1.code.data());
  EXPECT_TRUE(codes->Decode(MakeCode(kOp_TEST, 7), decoded));
  EXPECT_EQ(decoded.code.data(), decoded1.code.data());
}

}  // namespace
}  // namespace oz3
