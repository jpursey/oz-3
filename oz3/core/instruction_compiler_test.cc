// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_compiler.h"

#include <string>

#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction.h"
#include "oz3/core/microcode.h"

namespace oz3 {
namespace {

using ::testing::IsEmpty;
using ::testing::Not;

constexpr uint8_t kOp_TEST = 200;
constexpr uint8_t kMicro_TEST_NOP = 254;
constexpr uint8_t kMicro_TEST = 255;

constexpr MicrocodeDef kMicroNoArgs = {kMicro_TEST, "TEST"};

bool TestCompile(const MicrocodeDef& microcode_def,
                 const InstructionDef& instruction_def, std::string& error) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"}, {kMicro_LK, "LK", MicroArgType::kBank}, microcode_def};
  InstructionCompiler compiler(micros);
  std::string new_code = absl::StrCat("UL;", instruction_def.code);
  InstructionDef instruction = instruction_def;
  instruction.code = new_code;
  instruction.op_name = "TEST";
  return compiler.Compile(instruction, &error);
}

InstructionDef MakeDef(std::string_view code) {
  return InstructionDef{.code = code};
}

InstructionDef MakeDef(std::pair<Argument, Argument> args,
                       std::string_view code) {
  return InstructionDef{.arg1 = args.first, .arg2 = args.second, .code = code};
}

TEST(InstructionCompilerTest, BankArg) {
  const MicrocodeDef kMicroBankArg1 = {kMicro_TEST, "TEST",
                                       MicroArgType::kBank};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroBankArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroBankArg1, MakeDef("TEST(X)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroBankArg1, MakeDef("TEST(1)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroBankArg1, MakeDef("TEST(CODEX)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroBankArg1, MakeDef("TEST(CODE)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroBankArg1, MakeDef("TEST(STACK)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroBankArg1, MakeDef("TEST(DATA)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroBankArg1, MakeDef("TEST(EXTRA)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroNoArgs, MakeDef("TEST(CODE)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, ImmArg) {
  const MicrocodeDef kMicroImmArg1 = {kMicro_TEST, "TEST",
                                      MicroArgType::kValue};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroImmArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroImmArg1, MakeDef({ArgType::kWordReg, ArgType::kNone}, "TEST(a)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroImmArg1, MakeDef({ArgType::kDwordReg, ArgType::kNone}, "TEST(A)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroImmArg1, MakeDef("TEST(-129)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroImmArg1, MakeDef("TEST(-128)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroImmArg1, MakeDef("TEST(0)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroImmArg1, MakeDef("TEST(127)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroImmArg1, MakeDef("TEST(128)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WordRegArg) {
  constexpr MicrocodeDef kMicroWordArg1 = {kMicro_TEST, "TEST",
                                           MicroArgType::kWordReg};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef({ArgType::kNone, ArgType::kWordReg}, "TEST(a)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef({ArgType::kNone, ArgType::kDwordReg}, "TEST(a0)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef({ArgType::kNone, ArgType::kDwordReg}, "TEST(a1)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(b)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef({ArgType::kWordReg, ArgType::kNone}, "TEST(b)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef({ArgType::kDwordReg, ArgType::kNone}, "TEST(b0)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef({ArgType::kDwordReg, ArgType::kNone}, "TEST(b1)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef({ArgType::kDwordReg, ArgType::kNone}, "TEST(A)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(0)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(R0)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(R1)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(R2)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(R3)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(R4)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(R5)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(R6)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(R7)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(R8)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(D0)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(D1)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(D2)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(D3)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(D4)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(C0)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(C1)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(C2)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(C3)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(SP)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(DP)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(SD)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(PC)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(ST)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1, MakeDef("TEST(BM)"), error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, DwordRegArg) {
  const MicrocodeDef kMicroDwordArg1 = {kMicro_TEST, "TEST",
                                        MicroArgType::kDwordReg};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroDwordArg1, MakeDef({ArgType::kWordReg, ArgType::kNone}, "TEST(a)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroDwordArg1,
      MakeDef({{ArgType::kImmediate, 4}, ArgType::kWordReg}, "TEST(b)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(A)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(
      kMicroDwordArg1, MakeDef({ArgType::kDwordReg, ArgType::kNone}, "TEST(A)"),
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(
      kMicroDwordArg1, MakeDef({ArgType::kNone, ArgType::kDwordReg}, "TEST(A)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(B)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroDwordArg1, MakeDef({ArgType::kDwordReg, ArgType::kNone}, "TEST(B)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(
      kMicroDwordArg1,
      MakeDef({{ArgType::kImmediate, 4}, ArgType::kDwordReg}, "TEST(B)"),
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(0)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(R0)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(R1)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(R2)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(R3)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(R4)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(R5)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(R6)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(R7)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(R8)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroDwordArg1, MakeDef("TEST(D0)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroDwordArg1, MakeDef("TEST(D1)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroDwordArg1, MakeDef("TEST(D2)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroDwordArg1, MakeDef("TEST(D3)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(D4)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(C0)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(C1)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(C2)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(C3)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(SP)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(DP)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroDwordArg1, MakeDef("TEST(SD)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(PC)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(ST)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(BM)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StatusArg) {
  const MicrocodeDef kMicroStatusArgs = {
      kMicro_TEST, "TEST", MicroArgType::kStatus, MicroArgType::kStatus};
  std::string error;
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(Z,S)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(C,O)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(ZS,CO)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(ZC,SO)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(ZSC,SCO)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(ZCO,ZSC)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(ZZZ,SSS)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(CCC,OOO)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(_,_)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(S,_)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(_,C)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(Z___,_S__)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(__C_,___O)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroStatusArgs, MakeDef("TEST(I,I)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroStatusArgs, MakeDef("TEST(X,Z)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroStatusArgs, MakeDef("TEST(Z,X)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortModeArg) {
  const MicrocodeDef kMicroPortModeArgs = {
      kMicro_TEST, "TEST", MicroArgType::kPortMode, MicroArgType::kPortMode};
  std::string error;
  EXPECT_TRUE(TestCompile(kMicroPortModeArgs, MakeDef("TEST(T,S)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroPortModeArgs, MakeDef("TEST(T,_)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroPortModeArgs, MakeDef("TEST(TS,TA)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroPortModeArgs, MakeDef("TEST(SA,TSA)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroPortModeArgs, MakeDef("TEST(STA,AST)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroPortModeArgs, MakeDef("TEST(T_A,_S_)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroPortModeArgs, MakeDef("TEST(X,T)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroPortModeArgs, MakeDef("TEST(T,X)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, ConditionArg) {
  const MicrocodeDef kMicroConditionArgs = {
      kMicro_TEST, "TEST", MicroArgType::kCondition, MicroArgType::kCondition};
  std::string error;
  EXPECT_TRUE(TestCompile(kMicroConditionArgs, MakeDef("TEST(Z,NZ)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroConditionArgs, MakeDef("TEST(S,NS)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroConditionArgs, MakeDef("TEST(C,NC)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroConditionArgs, MakeDef("TEST(O,NO)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroConditionArgs, MakeDef("TEST(Z,X)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroConditionArgs, MakeDef("TEST(X,Z)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, InvalidMicrocodeName) {
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroNoArgs, MakeDef("INVALID"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, ProvideExtraArg) {
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroNoArgs, MakeDef("TEST(1)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, Address) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"},
      {kMicro_LK, "LK", MicroArgType::kBank},
      {kMicro_LKR, "LKR", MicroArgType::kWordReg},
      {kMicro_PUL, "PUL"},
      {kMicro_PLK, "PLK", MicroArgType::kWordReg},
      {kMicro_CUL, "CUL"},
      {kMicro_CLK, "CLK", MicroArgType::kWordReg},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "TEST", MicroArgType::kAddress},
  };
  InstructionCompiler compiler(micros);
  std::string error;
  EXPECT_TRUE(compiler.Compile(InstructionDef{.code = "UL;TEST(-1);"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(
      compiler.Compile(InstructionDef{.code = "UL;TEST(-2);"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      compiler.Compile(InstructionDef{.code = "UL;TEST(-3);"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(compiler.Compile(InstructionDef{.code = "UL;TEST(0);"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(compiler.Compile(InstructionDef{.code = "UL;TEST(1);"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(compiler.Compile(InstructionDef{.code = "TEST(0);UL;"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(
      compiler.Compile(InstructionDef{.code = "NOP;TEST(-2);UL;"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(compiler.Compile(InstructionDef{.code = "TEST(1);UL;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "TEST(2);UL;NOP;LK(CODE);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code = "TEST(3);UL;NOP;LK(CODE);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code = "TEST(4);UL;NOP;LK(CODE);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "TEST(5);UL;NOP;LK(CODE);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code = "UL;TEST(4);NOP;LK(CODE);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "TEST(3);UL;NOP;LK(STACK);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "TEST(3);UL;NOP;LK(DATA);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "TEST(3);UL;NOP;LK(EXTRA);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code =
                         "@label:NOP;TEST(@label);UL;NOP;LK(DATA);NOP;UL;NOP;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code =
                         "NOP;TEST(@label);@label:UL;NOP;LK(DATA);NOP;UL;NOP;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code =
                         "NOP;TEST(@label);UL;@label:NOP;LK(DATA);NOP;UL;NOP;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code =
                         "NOP;UL;TEST(@label);NOP;@label:LK(DATA);NOP;UL;NOP;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code =
                         "NOP;UL;TEST(@label);NOP;LK(DATA);@label:NOP;UL;NOP;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code =
                         "NOP;UL;TEST(@label);NOP;LK(DATA);NOP;UL;@label:NOP;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(
      compiler.Compile(InstructionDef{.code = "UL;TEST(@missing);"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "UL;TEST(invalid);@invalid:NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "UL;TEST(@label);PLK(C0);@label:NOP;PUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "UL;PLK(C0);NOP;TEST(@label);PUL;@label:NOP;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{
          .code =
              "UL;PLK(C0);NOP;TEST(@label);PUL;NOP;PLK(C0);@label:NOP;PUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code = "UL;PLK(C0);NOP;TEST(@label);NOP;@label:NOP;PUL;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{
          .code =
              "UL;LK(DATA);NOP;TEST(@label);UL;NOP;PLK(C0);@label:NOP;PUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "UL;TEST(@label);CLK(C0);@label:NOP;CUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "UL;CLK(C0);NOP;TEST(@label);CUL;@label:NOP;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{
          .code =
              "UL;CLK(C0);NOP;TEST(@label);CUL;NOP;CLK(C0);@label:NOP;CUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code = "UL;CLK(C0);NOP;TEST(@label);NOP;@label:NOP;CUL;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{
          .code =
              "UL;LK(DATA);NOP;TEST(@label);UL;NOP;CLK(C0);@label:NOP;CUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code = "UL;"
                             "LKR(R0);TEST(2);UL;LKR(R0);TEST(-4);UL;"
                             "LKR(R1);TEST(2);UL;LKR(R1);TEST(-4);UL;"
                             "LKR(R2);TEST(2);UL;LKR(R2);TEST(-4);UL;"
                             "LKR(R3);TEST(2);UL;LKR(R3);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code = "UL;"
                             "LKR(R4);TEST(2);UL;LKR(R4);TEST(-4);UL;"
                             "LKR(R5);TEST(2);UL;LKR(R5);TEST(-4);UL;"
                             "LKR(R6);TEST(2);UL;LKR(R6);TEST(-4);UL;"
                             "LKR(R7);TEST(2);UL;LKR(R7);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code = "UL;"
                             "LKR(C0);TEST(2);UL;LKR(C0);TEST(-4);UL;"
                             "LKR(C1);TEST(2);UL;LKR(C1);TEST(-4);UL;"
                             "LKR(C2);TEST(2);UL;LKR(C2);TEST(-4);UL;"
                             "LKR(PC);TEST(2);UL;LKR(PC);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(compiler.Compile(
      InstructionDef{.code = "UL;"
                             "LKR(SP);TEST(2);UL;LKR(SP);TEST(-4);UL;"
                             "LKR(DP);TEST(2);UL;LKR(DP);TEST(-4);UL;"
                             "LKR(ST);TEST(2);UL;LKR(ST);TEST(-4);UL;"
                             "LKR(BM);TEST(2);UL;LKR(BM);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "LKR(R0);TEST(2);UL;LKR(R1);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "LKR(R2);TEST(2);UL;LKR(R3);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "LKR(R4);TEST(2);UL;LKR(R5);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "LKR(R6);TEST(2);UL;LKR(R7);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "LKR(C0);TEST(2);UL;LKR(C1);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "LKR(C2);TEST(2);UL;LKR(PC);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "LKR(SP);TEST(2);UL;LKR(DP);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(compiler.Compile(
      InstructionDef{.code = "LKR(ST);TEST(2);UL;LKR(BM);TEST(-4);UL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "LK(STACK);UL;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, RegisterMemoryLockDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "LKR(R7);UL;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLockDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "PLK(C0);PUL;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreLockDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "CLK(C0);CUL;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;LK(DATA);LK(STACK);UL;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockAfterPriorPortLock) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;PLK(C0);LK(STACK);UL;PUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockAfterPriorCoreLock) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;CLK(C0);LK(STACK);UL;CUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, RegisterMemoryLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);LKR(R7);UL;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);PLK(C0);PUL;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);CLK(C0);CUL;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MemoryUnlockWhenNotLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryUnlockWhenPortLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortUnlockWhenPortLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);PUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, PortUnlockWhenNotLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);PUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreUnlockWhenCoreLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);CUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, CoreUnlockWhenNotLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);CUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MaxLocks) {
  std::string code = "UL;";
  for (int i = 0; i < kMaxLocksPerInstruction; ++i) {
    switch (i % 3) {
      case 0:
        absl::StrAppend(&code, "LK(DATA);UL;");
        break;
      case 1:
        absl::StrAppend(&code, "PLK(C0);PUL;");
        break;
      case 2:
        absl::StrAppend(&code, "CLK(C0);CUL;");
        break;
    }
  }
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = code};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, TooManyLocks) {
  std::string code = "UL;";
  for (int i = 0; i <= kMaxLocksPerInstruction; ++i) {
    switch (i % 3) {
      case 0:
        absl::StrAppend(&code, "LK(DATA);UL;");
        break;
      case 1:
        absl::StrAppend(&code, "PLK(C0);PUL;");
        break;
      case 2:
        absl::StrAppend(&code, "CLK(C0);CUL;");
        break;
    }
  }
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = code};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, AddressWhenNotLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;ADR(C0);"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, AddressWhenPortLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);ADR(C0);"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, AddressDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "ADR(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, AddressWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);ADR(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, LoadDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "LD(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, LoadBeforeAddress) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);LD(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, LoadAfterAddress) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;LK(DATA);ADR(C0);LD(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StoreDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "ST(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StoreAfterAddressDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "ADR(C0);ST(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StoreBeforeAddress) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);ST(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StoreAfterAddress) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;LK(DATA);ADR(C0);ST(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StorePushDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "STP(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StorePushAfterAddressDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "ADR(C0);STP(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StorePushBeforeAddress) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);STP(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StorePushAfterAddress) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;LK(DATA);ADR(C0);STP(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, PortLoadWhenUnlocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLD(_,C1);"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLoadWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);PLD(_,C1);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLoadWhenPortLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);PLD(_,C1);PUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, PortStoreWhenUnlocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PST(_,C1);"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortStoreWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);PST(_,C1);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortStoreWhenPortLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);PST(_,C1);PUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_TRUE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, NoFetchUnlock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "MOV(R0,R1);"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, NoMemoryUnlock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);MOV(R0,R1);"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, NoPortUnlock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);MOV(R0,R1);"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, NoCoreUnlock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);MOV(R0,R1);"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "WAIT(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitWithinMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);WAIT(C0);UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitWithinPortLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);WAIT(C0);PUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitWithinCoreLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);WAIT(C0);CUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "HALT;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltWithinMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);HALT;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltWithinPortLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);HALT;PUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltWithinCoreLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);HALT;CUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "IRT;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtWithinMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);IRT;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtWithinPortLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);IRT;PUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtWithinCoreLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);IRT;CUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "END;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndWithinMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);END;UL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndWithinPortLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);END;PUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndWithinCoreLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);END;CUL;"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, LabelMissingColon) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;@start;MOV(C0,C1);"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, InvalidLabelCharacter) {
  // Just test the printable ASCII characters.
  for (char ch = 33; absl::ascii_isgraph(ch); ++ch) {
    std::string code = absl::StrCat("UL;@", std::string(1, ch), ":MOV(C0,C1);");
    InstructionDef instruction_def = {
        .op = kOp_TEST, .op_name = "TEST", .code = code};
    InstructionCompiler compiler;
    std::string error;
    if (!absl::ascii_isalnum(ch) && ch != '_') {
      EXPECT_FALSE(compiler.Compile(instruction_def, &error));
      EXPECT_THAT(error, Not(IsEmpty()));
    } else {
      EXPECT_TRUE(compiler.Compile(instruction_def, &error));
      EXPECT_THAT(error, IsEmpty());
    }
  }
}

TEST(InstructionCompilerTest, DuplicateLabel) {
  InstructionDef instruction_def = {
      .op = kOp_TEST,
      .op_name = "TEST",
      .code = "UL;@start:MOV(C0,C1);@start:MOV(C1,C0);"};
  InstructionCompiler compiler;
  std::string error;
  EXPECT_FALSE(compiler.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

}  // namespace
}  // namespace oz3
