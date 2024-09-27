// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_set.h"

#include <ostream>
#include <string>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/microcode.h"
#include "oz3/core/port.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

constexpr uint8_t kOp_TEST = 200;
constexpr uint8_t kMicro_TEST_NOP = 254;
constexpr uint8_t kMicro_TEST = 255;

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
      {.op = kOp_TEST, .code = "UL;OP(CODE);OP(STACK);OP(DATA);OP(EXTRA)"},
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
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
      {.op = kOp_TEST,
       .code = "UL;OP(_);"
               "OP(Z);OP(S);OP(C);OP(O);"
               "OP(Z___);OP(_S__);OP(__C_);OP(___O);"
               "OP(ZS);OP(ZC);OP(ZO);OP(SC);OP(SO);OP(CO);"
               "OP(ZSC);OP(ZSO);OP(ZCO);OP(SCO);"
               "OP(ZSCO);OP(CCC_SSS_OZ_OZ_OZ);OP(I);"},
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
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

TEST(InstructionSetTest, PortModeDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kPortMode},
  };
  const InstructionDef instruction_defs[] = {
      {.op = kOp_TEST,
       .code = "UL;OP(_);"
               "OP(T);OP(S);OP(A);"
               "OP(TS);OP(TA);OP(SA);"
               "OP(TSA);OP(STA);OP(T_A);"},
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
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
      {.op = kOp_TEST, .code = "UL;OP(Z,NZ);OP(S,NS);OP(C,NC);OP(O,NO);"},
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
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

TEST(InstructionSetTest, AddressDecodedCorrecty) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "OP", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  const InstructionDef instruction_defs[] = {
      {
          .op = kOp_TEST,
          .code = "UL;"                    // 0
                  "@start:OP(0,1);"        // 1
                  "OP(-1,@end);"           // 2
                  "OP(@start,@mid);"       // 3
                  "@mid:NOP;"              // 4
                  "NOP;"                   // 5
                  "@end:OP(@start,@end);"  // 6
      },
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
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

TEST(InstructionSetTest, ImmArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kValue, MicroArgType::kValue},
  };
  const InstructionDef instruction_defs[] = {
      {.op = kOp_TEST, .code = "UL;OP(0,1);OP(127,-42);OP(-128,97)"},
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = 0, .arg2 = 1},
                  Microcode{.op = kMicro_TEST, .arg1 = 127, .arg2 = -42},
                  Microcode{.op = kMicro_TEST, .arg1 = -128, .arg2 = 97}));
}

TEST(InstructionSetTest, WordRegArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const InstructionDef instruction_defs[] = {
      {.op = kOp_TEST,
       .code = "UL;"
               "OP(R0,R1);OP(R2,R3);OP(R4,R5);OP(R6,R7);"
               "OP(C0,C1);OP(C2,PC);OP(SP,DP);OP(ST,BM);"},
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R0, .arg2 = CpuCore::R1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R2, .arg2 = CpuCore::R3},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R4, .arg2 = CpuCore::R5},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::R6, .arg2 = CpuCore::R7},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::C0, .arg2 = CpuCore::C1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::C2, .arg2 = CpuCore::PC},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::SP, .arg2 = CpuCore::DP},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::ST, .arg2 = CpuCore::BM}));
}

TEST(InstructionSetTest, DwordRegArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  const InstructionDef instruction_defs[] = {
      {.op = kOp_TEST, .code = "UL;OP(D0,D1);OP(D2,D3);OP(SD,SD)"},
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
  EXPECT_EQ(decoded.c[0], 0);
  EXPECT_EQ(decoded.c[1], 0);
  EXPECT_EQ(decoded.r[0], 0);
  EXPECT_EQ(decoded.r[1], 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          Microcode{.op = kMicro_UL},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D0, .arg2 = CpuCore::D1},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::D2, .arg2 = CpuCore::D3},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::SD, .arg2 = CpuCore::SD}));
}

TEST(InstructionSetTest, ImmOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const InstructionDef instruction_defs[] = {
      {.op = kOp_TEST,
       .arg1 = {ArgType::kImmediate, 3},
       .arg2 = {ArgType::kImmediate, 5},
       .code = "UL;OP(C0,C1)"},
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    uint16_t c0 = (i * i + 1) % 8;   // 3 bits
    uint16_t c1 = (i * i + 2) % 32;  // 5 bits
    EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST, c0 | (c1 << 3)), decoded));
    EXPECT_EQ(decoded.c[0], c0);
    EXPECT_EQ(decoded.c[1], c1);
    EXPECT_EQ(decoded.r[0], 0);
    EXPECT_EQ(decoded.r[1], 0);
  }
  EXPECT_THAT(decoded.code, ElementsAre(Microcode{.op = kMicro_UL},
                                        Microcode{.op = kMicro_TEST,
                                                  .arg1 = CpuCore::C0,
                                                  .arg2 = CpuCore::C1}));
}

TEST(InstructionSetTest, WordOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const InstructionDef instruction_defs[] = {
      {.op = kOp_TEST,
       .arg1 = ArgType::kWordReg,
       .arg2 = ArgType::kWordReg,
       .code = "UL;OP(a,b);OP(b,a);"},
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 8;
    EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST, a | (b << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::R0 + a);
    EXPECT_EQ(decoded.r[1], CpuCore::R0 + b);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = -2},
                  Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = -1}));
}

TEST(InstructionSetTest, DwordOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  const InstructionDef instruction_defs[] = {
      {.op = kOp_TEST,
       .arg1 = ArgType::kDwordReg,
       .arg2 = ArgType::kDwordReg,
       .code = "UL;OP(A,B);OP(B,A)"},
  };

  InstructionCompiler compiler(micro_defs);
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 4;
    EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST, a | (b << 2)), decoded))
        << "a=" << a << ", b=" << b;
    EXPECT_EQ(decoded.c[0], 0) << "a=" << a << ", b=" << b;
    EXPECT_EQ(decoded.c[1], 0) << "a=" << a << ", b=" << b;
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + a * 2) << "a=" << a << ", b=" << b;
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + b * 2) << "a=" << a << ", b=" << b;
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = -2},
                  Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = -1}));
}

TEST(MicrocodeTest, DefaultCodeSizeIsOne) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

TEST(MicrocodeTest, LoadDuringFetchIncreasesCodeSize) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "LD(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 2);
}

TEST(MicrocodeTest, TwoLoadsDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "LD(C0);LD(C1);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 3);
}

TEST(MicrocodeTest, LoadAfterFetchDoesNotIncreaseCodeSize) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;LK(DATA);ADR(C0);LD(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

TEST(MicrocodeTest, LoadAfterAddressDuringFetchDoesNotIncreaseCodeSize) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "ADR(C0);LD(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  InstructionSet codes = std::move(compiler).ToInstructionSet();
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

}  // namespace
}  // namespace oz3
