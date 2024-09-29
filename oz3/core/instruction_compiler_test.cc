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

using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Not;

constexpr uint8_t kOp_TEST = 200;
constexpr uint8_t kMicro_TEST_NOP = 254;
constexpr uint8_t kMicro_TEST = 255;

constexpr MicrocodeDef kMicroNoArgs = {kMicro_TEST, "TEST"};

bool CompileForTest(absl::Span<const MicrocodeDef> micros,
                    const InstructionDef& instruction_def, std::string& error) {
  return !CompileInstructionSet({.instructions = {instruction_def}}, &error,
                                micros)
              ->IsEmpty();
}

bool CompileForTest(const InstructionDef& instruction_def, std::string& error) {
  return !CompileInstructionSet({.instructions = {instruction_def}}, &error)
              ->IsEmpty();
}

bool CompileForTest(absl::Span<const MicrocodeDef> micros,
                    const InstructionSetDef& instruction_set_def,
                    std::string& error) {
  return !CompileInstructionSet(instruction_set_def, &error, micros)->IsEmpty();
}

bool CompileForTest(const InstructionSetDef& instruction_set_def,
                    std::string& error) {
  return !CompileInstructionSet(instruction_set_def, &error)->IsEmpty();
}

bool TestCompile(const MicrocodeDef& microcode_def,
                 const InstructionDef& instruction_def, std::string& error) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"}, {kMicro_LK, "LK", MicroArgType::kBank}, microcode_def};
  std::string new_code = absl::StrCat("UL;", instruction_def.code);
  InstructionDef instruction = instruction_def;
  instruction.code = new_code;
  instruction.op_name = "TEST";
  return !CompileInstructionSet(
              {.instructions =
                   absl::Span<const InstructionDef>(&instruction, 1)},
              &error, micros)
              ->IsEmpty();
}

InstructionDef MakeDef(std::string_view code) {
  return InstructionDef{.code = code};
}

InstructionDef MakeDef(std::pair<Argument, Argument> args,
                       std::string_view code) {
  return InstructionDef{.arg1 = args.first, .arg2 = args.second, .code = code};
}

TEST(InstructionCompilerTest, InvalidFirstArg) {
  std::string error;
  auto MakeDef = [](Argument arg) {
    return InstructionDef{
        .op = kOp_TEST, .op_name = "TEST", .arg1 = arg, .code = "UL;"};
  };
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kNone, 1}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("first argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kImmediate, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("first argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kImmediate, 9}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("first argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kWordReg, 5}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("first argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kWordReg, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("first argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kDwordReg, 3}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("first argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kDwordReg, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("first argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kMacro, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("first argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kMacro, 9}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("first argument"));
}

TEST(InstructionCompilerTest, InvalidSecondArg) {
  std::string error;
  auto MakeDef = [](Argument arg) {
    return InstructionDef{.op = kOp_TEST,
                          .op_name = "TEST",
                          .arg1 = ArgType::kWordReg,
                          .arg2 = arg,
                          .code = "UL;"};
  };
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kNone, 1}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kImmediate, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kImmediate, 9}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kWordReg, 5}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kWordReg, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kDwordReg, 3}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kDwordReg, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kMacro, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kMacro, 9}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
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
  std::string error;
  EXPECT_TRUE(
      CompileForTest(micros, InstructionDef{.code = "UL;TEST(-1);"}, error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(
      CompileForTest(micros, InstructionDef{.code = "UL;TEST(-2);"}, error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      CompileForTest(micros, InstructionDef{.code = "UL;TEST(-3);"}, error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(
      CompileForTest(micros, InstructionDef{.code = "UL;TEST(0);"}, error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(
      CompileForTest(micros, InstructionDef{.code = "UL;TEST(1);"}, error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(
      CompileForTest(micros, InstructionDef{.code = "TEST(0);UL;"}, error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(CompileForTest(micros, InstructionDef{.code = "NOP;TEST(-2);UL;"},
                             error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(
      CompileForTest(micros, InstructionDef{.code = "TEST(1);UL;"}, error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "TEST(2);UL;NOP;LK(CODE);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      micros, InstructionDef{.code = "TEST(3);UL;NOP;LK(CODE);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(CompileForTest(
      micros, InstructionDef{.code = "TEST(4);UL;NOP;LK(CODE);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "TEST(5);UL;NOP;LK(CODE);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      micros, InstructionDef{.code = "UL;TEST(4);NOP;LK(CODE);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "TEST(3);UL;NOP;LK(STACK);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "TEST(3);UL;NOP;LK(DATA);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "TEST(3);UL;NOP;LK(EXTRA);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      micros,
      InstructionDef{.code =
                         "@label:NOP;TEST(@label);UL;NOP;LK(DATA);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(CompileForTest(
      micros,
      InstructionDef{.code =
                         "NOP;TEST(@label);@label:UL;NOP;LK(DATA);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(
      micros,
      InstructionDef{.code =
                         "NOP;TEST(@label);UL;@label:NOP;LK(DATA);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      micros,
      InstructionDef{.code =
                         "NOP;UL;TEST(@label);NOP;@label:LK(DATA);NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(
      micros,
      InstructionDef{.code =
                         "NOP;UL;TEST(@label);NOP;LK(DATA);@label:NOP;UL;NOP;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      micros,
      InstructionDef{.code =
                         "NOP;UL;TEST(@label);NOP;LK(DATA);NOP;UL;@label:NOP;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "UL;TEST(@missing);"}, error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "UL;TEST(invalid);@invalid:NOP;"}, error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "UL;TEST(@label);PLK(C0);@label:NOP;PUL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;PLK(C0);NOP;TEST(@label);PUL;@label:NOP;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;PLK(C0);NOP;TEST(@label);PUL;NOP;PLK(C0);@"
                             "label:NOP;PUL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;PLK(C0);NOP;TEST(@label);NOP;@label:NOP;PUL;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;LK(DATA);NOP;TEST(@label);UL;NOP;PLK(C0);@"
                             "label:NOP;PUL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "UL;TEST(@label);CLK(C0);@label:NOP;CUL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;CLK(C0);NOP;TEST(@label);CUL;@label:NOP;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;CLK(C0);NOP;TEST(@label);CUL;NOP;CLK(C0);@"
                             "label:NOP;CUL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;CLK(C0);NOP;TEST(@label);NOP;@label:NOP;CUL;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;LK(DATA);NOP;TEST(@label);UL;NOP;CLK(C0);@"
                             "label:NOP;CUL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;"
                             "LKR(R0);TEST(2);UL;LKR(R0);TEST(-4);UL;"
                             "LKR(R1);TEST(2);UL;LKR(R1);TEST(-4);UL;"
                             "LKR(R2);TEST(2);UL;LKR(R2);TEST(-4);UL;"
                             "LKR(R3);TEST(2);UL;LKR(R3);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;"
                             "LKR(R4);TEST(2);UL;LKR(R4);TEST(-4);UL;"
                             "LKR(R5);TEST(2);UL;LKR(R5);TEST(-4);UL;"
                             "LKR(R6);TEST(2);UL;LKR(R6);TEST(-4);UL;"
                             "LKR(R7);TEST(2);UL;LKR(R7);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;"
                             "LKR(C0);TEST(2);UL;LKR(C0);TEST(-4);UL;"
                             "LKR(C1);TEST(2);UL;LKR(C1);TEST(-4);UL;"
                             "LKR(C2);TEST(2);UL;LKR(C2);TEST(-4);UL;"
                             "LKR(PC);TEST(2);UL;LKR(PC);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(CompileForTest(
      micros,
      InstructionDef{.code = "UL;"
                             "LKR(SP);TEST(2);UL;LKR(SP);TEST(-4);UL;"
                             "LKR(DP);TEST(2);UL;LKR(DP);TEST(-4);UL;"
                             "LKR(ST);TEST(2);UL;LKR(ST);TEST(-4);UL;"
                             "LKR(BM);TEST(2);UL;LKR(BM);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "LKR(R0);TEST(2);UL;LKR(R1);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "LKR(R2);TEST(2);UL;LKR(R3);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "LKR(R4);TEST(2);UL;LKR(R5);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "LKR(R6);TEST(2);UL;LKR(R7);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "LKR(C0);TEST(2);UL;LKR(C1);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "LKR(C2);TEST(2);UL;LKR(PC);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "LKR(SP);TEST(2);UL;LKR(DP);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(
      micros, InstructionDef{.code = "LKR(ST);TEST(2);UL;LKR(BM);TEST(-4);UL;"},
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "LK(STACK);UL;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, RegisterMemoryLockDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "LKR(R7);UL;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLockDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "PLK(C0);PUL;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreLockDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "CLK(C0);CUL;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;LK(DATA);LK(STACK);UL;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockAfterPriorPortLock) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;PLK(C0);LK(STACK);UL;PUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockAfterPriorCoreLock) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;CLK(C0);LK(STACK);UL;CUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, RegisterMemoryLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);LKR(R7);UL;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);PLK(C0);PUL;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);CLK(C0);CUL;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);UL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MemoryUnlockWhenNotLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryUnlockWhenPortLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortUnlockWhenPortLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);PUL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, PortUnlockWhenNotLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);PUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreUnlockWhenCoreLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);CUL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, CoreUnlockWhenNotLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);CUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
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
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
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
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, AddressWhenNotLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;ADR(C0);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, AddressWhenPortLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);ADR(C0);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, AddressDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "ADR(C0);UL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, AddressWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);ADR(C0);UL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, LoadDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "LD(C0);UL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, LoadBeforeAddress) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);LD(C0);UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, LoadAfterAddress) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;LK(DATA);ADR(C0);LD(C0);UL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StoreDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "ST(C0);UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StoreAfterAddressDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "ADR(C0);ST(C0);UL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StoreBeforeAddress) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);ST(C0);UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StoreAfterAddress) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;LK(DATA);ADR(C0);ST(C0);UL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StorePushDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "STP(C0);UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StorePushAfterAddressDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "ADR(C0);STP(C0);UL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StorePushBeforeAddress) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);STP(C0);UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StorePushAfterAddress) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .code = "UL;LK(DATA);ADR(C0);STP(C0);UL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, PortLoadWhenUnlocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLD(_,C1);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLoadWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);PLD(_,C1);UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLoadWhenPortLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);PLD(_,C1);PUL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, PortStoreWhenUnlocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PST(_,C1);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortStoreWhenMemoryLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);PST(_,C1);UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortStoreWhenPortLocked) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);PST(_,C1);PUL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, NoFetchUnlock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "MOV(R0,R1);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, NoMemoryUnlock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);MOV(R0,R1);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, NoPortUnlock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);MOV(R0,R1);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, NoCoreUnlock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);MOV(R0,R1);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "WAIT(C0);UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitWithinMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);WAIT(C0);UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitWithinPortLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);WAIT(C0);PUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitWithinCoreLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);WAIT(C0);CUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "HALT;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltWithinMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);HALT;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltWithinPortLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);HALT;PUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltWithinCoreLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);HALT;CUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "IRT;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtWithinMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);IRT;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtWithinPortLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);IRT;PUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtWithinCoreLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);IRT;CUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndDuringFetch) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "END;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndWithinMemoryLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;LK(DATA);END;UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndWithinPortLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;PLK(C0);END;PUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndWithinCoreLock) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;CLK(C0);END;CUL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, LabelMissingColon) {
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;@start;MOV(C0,C1);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, InvalidLabelCharacter) {
  // Just test the printable ASCII characters.
  for (char ch = 33; absl::ascii_isgraph(ch); ++ch) {
    std::string code = absl::StrCat("UL;@", std::string(1, ch), ":MOV(C0,C1);");
    InstructionDef instruction_def = {
        .op = kOp_TEST, .op_name = "TEST", .code = code};
    std::string error;
    if (!absl::ascii_isalnum(ch) && ch != '_') {
      EXPECT_FALSE(CompileForTest(instruction_def, error));
      EXPECT_THAT(error, Not(IsEmpty()));
    } else {
      EXPECT_TRUE(CompileForTest(instruction_def, error));
      EXPECT_THAT(error, IsEmpty());
    }
  }
}

TEST(InstructionCompilerTest, DuplicateLabel) {
  InstructionDef instruction_def = {
      .op = kOp_TEST,
      .op_name = "TEST",
      .code = "UL;@start:MOV(C0,C1);@start:MOV(C1,C0);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MacroWithMissingName) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro name"));
}

TEST(InstructionCompilerTest, MacroWithInvalidCharacter) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  char name[] = "IsValidX";
  MacroDef macro_def = {.name = name, .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  for (char ch = 32; absl::ascii_isprint(ch); ++ch) {
    name[7] = ch;
    if (absl::ascii_isalnum(ch) || ch == '_') {
      EXPECT_TRUE(CompileForTest({{instruction_def}, {macro_def}}, error))
          << "Character: '" << ch << "'";
      EXPECT_THAT(error, IsEmpty()) << "Character: '" << ch << "'";
    } else {
      EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error))
          << "Character: '" << ch << "'";
      EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro name"))
          << "Character: '" << ch << "'";
      EXPECT_THAT(absl::AsciiStrToLower(error),
                  HasSubstr(absl::StrCat("'", std::string(1, ch), "'")));
    }
  }
}

TEST(InstructionCompilerTest, MacroWithZeroSize) {
  MacroDef macro_def = {.name = "Macro", .size = 0};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
}

TEST(InstructionSetTest, MacroWithSize8) {
  std::array<std::string, 256> source_names;
  std::array<MacroCodeDef, 256> macro_code_defs;
  for (int i = 0; i < 256; ++i) {
    source_names[i] = absl::StrCat("R", i);
    macro_code_defs[i] = {.source = source_names[i],
                          .prefix = {static_cast<uint8_t>(i), 8},
                          .code = "MOV(R0,C0);"};
  }
  MacroDef macro_def = {.name = "Macro", .size = 8, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 8},
                                    .code = "UL;$Macro"};
  std::string error;
  EXPECT_TRUE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionSetTest, MacroWithSize9) {
  std::array<std::string, 512> source_names;
  std::array<MacroCodeDef, 512> macro_code_defs;
  for (int i = 0; i < 512; ++i) {
    source_names[i] = absl::StrCat("R", i);
    macro_code_defs[i] = {.source = source_names[i],
                          .prefix = {static_cast<uint8_t>(i), 9},
                          .code = "MOV(R0,C0);"};
  }
  MacroDef macro_def = {.name = "Macro", .size = 9, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
}

TEST(InstructionCompilerTest, MacroWithInvalidPrefixSize) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 2}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("prefix"));
}

TEST(InstructionCompilerTest, MacroWithInvalidArgumentSize) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .arg = {ArgType::kImmediate, 2}, .code = "MOV(R0,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("argument"));
}

TEST(InstructionCompilerTest, MacroWithInvalidArgumentPlusPrefixSize) {
  MacroCodeDef macro_code_defs[] = {{.source = "R0",
                                     .prefix = {0, 1},
                                     .arg = {ArgType::kImmediate, 1},
                                     .code = "MOV(R0,C0);"},
                                    {.source = "R0",
                                     .prefix = {1, 1},
                                     .arg = {ArgType::kImmediate, 1},
                                     .code = "MOV(R1,C0;"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("prefix"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("argument"));
}

TEST(InstructionCompilerTest, MacroWithPrefixValueOutOfRange) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R2", .prefix = {2, 1}, .code = "MOV(R2,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("prefix value"));
}

TEST(InstructionCompilerTest, MacroWithInvalidLabel) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "@NoColon-MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("label"));
}

TEST(InstructionCompilerTest, MacroWithTooMuchCode) {
  std::string code;
  for (int i = 0; i <= kMaxInstructionMicrocodes; ++i) {
    absl::StrAppend(&code, "MOV(R0,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("microcode"));
}

TEST(InstructionCompilerTest, MacroWithNoCode) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = ""}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("microcode"));
}

TEST(InstructionCompilerTest, MacroWithMaxCode) {
  std::string code = "UL;";
  for (int i = 1; i < kMaxInstructionMicrocodes; ++i) {
    absl::StrAppend(&code, "MOV(R0,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "UL;"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "$Macro;"};
  std::string error;
  EXPECT_TRUE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroPlusInstructionOverMaxCode) {
  std::string code;
  for (int i = 0; i < kMaxInstructionMicrocodes; ++i) {
    absl::StrAppend(&code, "MOV(R0,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;$Macro;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("microcode"));
}

TEST(InstructionCompilerTest, MacroPlusInstructionAtMaxCode) {
  std::string code;
  for (int i = 0; i < kMaxInstructionMicrocodes - 1; ++i) {
    absl::StrAppend(&code, "MOV(R0,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;$Macro;"};
  std::string error;
  EXPECT_TRUE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, InvalidMicrocodeInMacro) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;$Macro;$Macro;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("multiple macros"));
}

TEST(InstructionCompilerTest, MultipleMacrosInInstruction) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("not found"));
}

TEST(InstructionCompilerTest, MacroArgumentWithoutMacroUseInInstruction) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
}

TEST(InstructionCompilerTest, MacroInInstructionWithoutArgument) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {
      .op = kOp_TEST, .op_name = "TEST", .code = "UL;$Macro;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
}

TEST(InstructionCompilerTest, TooManyMacroArgumentsInInstruction) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .arg2 = {ArgType::kMacro, 1},
                                    .code = "UL;$Macro;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro arguments"));
}

TEST(InstructionCompilerTest, UnknownMacroInInstruction) {
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;$UndefinedMacro;"};
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("unknown macro"));
  EXPECT_THAT(error, HasSubstr("UndefinedMacro"));
}

TEST(InstructionCompilerTest, MacroSmallerThanInstructionSpec) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 2},
                                    .code = "UL;$Macro;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("argument size"));
}

TEST(InstructionCompilerTest, MacroBiggerThanInstructionSpec) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 2}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 2}, .code = "MOV(R1,C0);"},
      {.source = "R2", .prefix = {2, 2}, .code = "MOV(R2,C0);"},
      {.source = "R3", .prefix = {3, 2}, .code = "MOV(R3,C0);"},
  };
  MacroDef macro_def = {.name = "Macro", .size = 2, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;$Macro;"};
  std::string error;
  EXPECT_FALSE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("argument size"));
}

TEST(InstructionCompilerTest, MacroMakesArg1AddressTooBig) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "TEST", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  std::string code;
  for (int i = 0; i < 128; ++i) {
    absl::StrAppend(&code, "MOV(R1,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;TEST(@end,0);$Macro;@end:NOP"};
  std::string error;
  EXPECT_FALSE(CompileForTest(micros, {{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("address"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("@end"));
}

TEST(InstructionCompilerTest, MacroMakesArg1AddressMax) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "TEST", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  std::string code;
  for (int i = 0; i < 127; ++i) {
    absl::StrAppend(&code, "MOV(R1,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;TEST(@end,0);$Macro;@end:NOP"};
  std::string error;
  EXPECT_TRUE(CompileForTest(micros, {{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroMakesArg2AddressTooBig) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "TEST", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  std::string code;
  for (int i = 0; i < 128; ++i) {
    absl::StrAppend(&code, "MOV(R1,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;TEST(0,@end);$Macro;@end:NOP"};
  std::string error;
  EXPECT_FALSE(CompileForTest(micros, {{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("address"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("@end"));
}

TEST(InstructionCompilerTest, MacroMakesArg2AddressMax) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "TEST", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  std::string code;
  for (int i = 0; i < 127; ++i) {
    absl::StrAppend(&code, "MOV(R1,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;TEST(0,@end);$Macro;@end:NOP"};
  std::string error;
  EXPECT_TRUE(CompileForTest(micros, {{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroMakesArg1AddressTooSmall) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "TEST", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  std::string code;
  for (int i = 0; i < 128; ++i) {
    absl::StrAppend(&code, "MOV(R1,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;@start:$Macro;TEST(@start,0);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(micros, {{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("address"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("@start"));
}

TEST(InstructionCompilerTest, MacroMakesArg1AddressMin) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "TEST", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  std::string code;
  for (int i = 0; i < 127; ++i) {
    absl::StrAppend(&code, "MOV(R1,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;@start:$Macro;TEST(@start,0);"};
  std::string error;
  EXPECT_TRUE(CompileForTest(micros, {{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroMakesArg2AddressTooSmall) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "TEST", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  std::string code;
  for (int i = 0; i < 128; ++i) {
    absl::StrAppend(&code, "MOV(R1,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;@start:$Macro;TEST(0,@start);"};
  std::string error;
  EXPECT_FALSE(CompileForTest(micros, {{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("address"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("@start"));
}

TEST(InstructionCompilerTest, MacroMakesArg2AddressMin) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "TEST", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  std::string code;
  for (int i = 0; i < 127; ++i) {
    absl::StrAppend(&code, "MOV(R1,C0);");
  }
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = code}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 1},
                                    .code = "UL;@start:$Macro;TEST(0,@start);"};
  std::string error;
  EXPECT_TRUE(CompileForTest(micros, {{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroWithNoParameters) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "CODE", .prefix = {0, 2}, .code = "LK(CODE);"},
      {.source = "STACK", .prefix = {1, 2}, .code = "LK(STACK);"},
      {.source = "DATA", .prefix = {2, 2}, .code = "LK(DATA);"},
      {.source = "EXTRA", .prefix = {3, 2}, .code = "LK(EXTRA);"},
  };
  MacroDef macro_def = {.name = "Lk", .size = 2, .code = macro_code_defs};
  InstructionDef instruction_def = {.op = kOp_TEST,
                                    .op_name = "TEST",
                                    .arg1 = {ArgType::kMacro, 2},
                                    .arg2 = ArgType::kWordReg,
                                    .code = "UL;$Lk;ADR(b);LD(b);UL;"};
  std::string error;
  EXPECT_TRUE(CompileForTest({{instruction_def}, {macro_def}}, error));
  EXPECT_THAT(error, IsEmpty());
}

}  // namespace
}  // namespace oz3
