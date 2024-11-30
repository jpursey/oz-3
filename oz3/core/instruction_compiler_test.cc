// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_compiler.h"

#include <string>

#include "absl/base/no_destructor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/substitute.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_def.h"
#include "oz3/core/microcode.h"

namespace oz3 {
namespace {

using ::testing::AllOf;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Not;

constexpr uint8_t kOp_TEST = 128;
constexpr uint8_t kMicro_TEST_NOP = 254;
constexpr uint8_t kMicro_TEST = 255;

constexpr MicrocodeDef kMicroNoArgs = {kMicro_TEST, "TEST"};

bool CompileForTest(
    const InstructionDef& instruction_def, std::string& error,
    absl::Span<const MicrocodeDef> microcode_defs = GetMicrocodeDefs()) {
  InstructionError instruction_error;
  if (CompileInstructionSet({.instructions = {instruction_def}},
                            &instruction_error, microcode_defs) == nullptr) {
    error = instruction_error.message;
    return false;
  }
  error.clear();
  return true;
}

bool CompileForTest(
    const MacroDef& macro_def, std::string& error,
    absl::Span<const MicrocodeDef> microcode_defs = GetMicrocodeDefs()) {
  InstructionError instruction_error;
  if (CompileInstructionSet({.macros = {macro_def}}, &instruction_error,
                            microcode_defs) == nullptr) {
    error = instruction_error.message;
    return false;
  }
  error.clear();
  return true;
}

bool CompileForTest(
    const InstructionDef& instruction_def, const MacroDef& macro_def,
    std::string& error,
    absl::Span<const MicrocodeDef> microcode_defs = GetMicrocodeDefs()) {
  InstructionError instruction_error;
  if (CompileInstructionSet({{instruction_def}, {macro_def}},
                            &instruction_error, microcode_defs) == nullptr) {
    error = instruction_error.message;
    return false;
  }
  error.clear();
  return true;
}

bool TestCompile(const MicrocodeDef& microcode_def,
                 const InstructionDef& instruction_def, std::string& error) {
  MicrocodeDef microcode_defs[] = {
      {kMicro_UL, "UL"}, {kMicro_LK, "LK", MicroArgType::kBank}, microcode_def};
  std::string new_code = absl::StrCat("UL;", instruction_def.code);
  InstructionDef instruction = instruction_def;
  instruction.code = new_code;
  instruction.op_name = "TEST";
  return CompileForTest(instruction, error, microcode_defs);
}

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

TEST(InstructionCompilerTest, InvalidFirstArg) {
  std::string error;
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
  EXPECT_FALSE(
      CompileForTest(MakeDef(ArgType::kWordReg, {ArgType::kNone, 1}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(
      MakeDef(ArgType::kWordReg, {ArgType::kImmediate, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(
      MakeDef(ArgType::kWordReg, {ArgType::kImmediate, 9}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(
      MakeDef(ArgType::kWordReg, {ArgType::kWordReg, 5}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(
      MakeDef(ArgType::kWordReg, {ArgType::kWordReg, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(
      MakeDef(ArgType::kWordReg, {ArgType::kDwordReg, 3}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(CompileForTest(
      MakeDef(ArgType::kWordReg, {ArgType::kDwordReg, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(
      CompileForTest(MakeDef(ArgType::kWordReg, {ArgType::kMacro, 0}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
  EXPECT_FALSE(
      CompileForTest(MakeDef(ArgType::kWordReg, {ArgType::kMacro, 9}), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("second argument"));
}

TEST(InstructionCompilerTest, InvalidMacroArg) {
  std::string error;
  auto TestCompile = [&](Argument arg) {
    const MacroCodeDef macro_code_defs[] = {
        {.source = "TEST", .arg = arg, .code = "UL;"},
    };
    const MacroDef macro_def = {
        .name = "Macro", .size = arg.size, .code = macro_code_defs};
    return CompileForTest(macro_def, error);
  };
  EXPECT_FALSE(TestCompile({ArgType::kNone, 1}));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
  EXPECT_FALSE(TestCompile({ArgType::kWordReg, 5}));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
  EXPECT_FALSE(TestCompile({ArgType::kDwordReg, 3}));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
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

TEST(InstructionCompilerTest, ImmediateArg) {
  const MicrocodeDef kMicroImmArg1 = {kMicro_TEST, "TEST",
                                      MicroArgType::kValue};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroImmArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroImmArg1, MakeDef(ArgType::kWordReg, "TEST(a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroImmArg1,
                           MakeDef(ArgType::kDwordReg, "TEST(A)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroImmArg1, MakeDef("TEST(-129)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroImmArg1, MakeDef("TEST(-128)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroImmArg1, MakeDef("TEST(0)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroImmArg1, MakeDef("TEST(255)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroImmArg1, MakeDef("TEST(256)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, InstructionRegByteArg) {
  constexpr MicrocodeDef kMicroWordArg1 = {kMicro_TEST, "TEST",
                                           MicroArgType::kRegByte};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(a:h)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(a:l)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1,
                           MakeDef(ArgType::kWordReg, "TEST(a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroWordArg1,
                          MakeDef(ArgType::kWordReg, "TEST(a:h)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1,
                          MakeDef(ArgType::kWordReg, "TEST(a:l)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1,
                          MakeDef(ArgType::kWordReg, "TEST(a:H)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1,
                          MakeDef(ArgType::kWordReg, "TEST(a:L)"), error));
  EXPECT_THAT(error, IsEmpty());
  for (std::string_view reg_name : CpuCore::GetWordRegisterNames()) {
    std::string context = absl::StrCat("Context: ", reg_name);
    EXPECT_FALSE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ")")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty()));
    EXPECT_TRUE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":h)")), error))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
    EXPECT_TRUE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":H)")), error))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
    EXPECT_TRUE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":l)")), error))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
    EXPECT_TRUE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":L)")), error))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
  }
  for (std::string_view reg_name : CpuCore::GetDwordRegisterNames()) {
    std::string context = absl::StrCat("Context: ", reg_name);
    EXPECT_FALSE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ")")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty())) << context;
    EXPECT_FALSE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":H)")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty())) << context;
    EXPECT_FALSE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":L)")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty())) << context;
  }
}

TEST(InstructionCompilerTest, InstructionRegNibbleArg) {
  constexpr MicrocodeDef kMicroWordArg1 = {kMicro_TEST, "TEST",
                                           MicroArgType::kRegNibble};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(a:0)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(a:1)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1,
                           MakeDef(ArgType::kWordReg, "TEST(a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(kMicroWordArg1,
                          MakeDef(ArgType::kWordReg, "TEST(a:0)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1,
                          MakeDef(ArgType::kWordReg, "TEST(a:1)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1,
                          MakeDef(ArgType::kWordReg, "TEST(a:2)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(kMicroWordArg1,
                          MakeDef(ArgType::kWordReg, "TEST(a:3)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroWordArg1,
                          MakeDef(ArgType::kWordReg, "TEST(a:4)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  for (std::string_view reg_name : CpuCore::GetWordRegisterNames()) {
    std::string context = absl::StrCat("Context: ", reg_name);
    EXPECT_FALSE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ")")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty()));
    EXPECT_TRUE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":0)")), error))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
    EXPECT_TRUE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":1)")), error))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
    EXPECT_TRUE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":2)")), error))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
    EXPECT_TRUE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":3)")), error))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
    EXPECT_FALSE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":4)")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty()));
  }
  for (std::string_view reg_name : CpuCore::GetDwordRegisterNames()) {
    std::string context = absl::StrCat("Context: ", reg_name);
    EXPECT_FALSE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ")")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty())) << context;
    EXPECT_FALSE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":0)")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty())) << context;
    EXPECT_FALSE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ":1)")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty())) << context;
  }
}

TEST(InstructionCompilerTest, InstructionWordRegArg) {
  constexpr MicrocodeDef kMicroWordArg1 = {kMicro_TEST, "TEST",
                                           MicroArgType::kWordReg};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef(ArgType::kNone, ArgType::kWordReg, "TEST(a)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef(ArgType::kNone, ArgType::kDwordReg, "TEST(a0)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef(ArgType::kNone, ArgType::kDwordReg, "TEST(a1)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(b)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef(ArgType::kWordReg, ArgType::kNone, "TEST(b)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef(ArgType::kDwordReg, ArgType::kNone, "TEST(b0)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef(ArgType::kDwordReg, ArgType::kNone, "TEST(b1)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(i)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(m)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(m0)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(m1)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(p)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro parameter"));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(p0)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro parameter"));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(p1)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro parameter"));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(r)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro return"));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(r0)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro return"));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(r1)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro return"));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef(ArgType::kDwordReg, ArgType::kNone, "TEST(A)"),
      error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("word argument"));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef(ArgType::kDwordReg, ArgType::kNone, "TEST(B)"),
      error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("word argument"));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef(ArgType::kDwordReg, ArgType::kNone, "TEST(M)"),
      error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("word argument"));
  EXPECT_FALSE(TestCompile(
      kMicroWordArg1, MakeDef(ArgType::kDwordReg, ArgType::kNone, "TEST(P)"),
      error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("word argument"));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(0)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));

  for (std::string_view reg_name : CpuCore::GetWordRegisterNames()) {
    std::string context = absl::StrCat("Context: ", reg_name);
    EXPECT_TRUE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ")")), error))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
  }
  for (std::string_view reg_name : CpuCore::GetDwordRegisterNames()) {
    std::string context = absl::StrCat("Context: ", reg_name);
    EXPECT_FALSE(TestCompile(
        kMicroWordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ")")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty())) << context;
  }
}

TEST(InstructionCompilerTest, MacroWordRegArg) {
  std::string error;
  auto TestCompile = [&](std::string_view code) {
    const MacroCodeDef macro_code_defs[] = {
        {.source = "WORD",
         .prefix = {0b00, 2},
         .arg = ArgType::kWordReg,
         .code = "MOV(R0,m);MOV(m,R1);"},
        {.source = "DWORD",
         .prefix = {0b010, 3},
         .arg = ArgType::kDwordReg,
         .code = "MOV(m0,R0);MOV(R1,m1);MOV(m1,R2);MOV(R3,m1);"},
        {.source = "IMM",
         .prefix = {0b10, 2},
         .arg = {ArgType::kImmediate, 3},
         .code = "MOV(R0,i);MOV(i,R1);"},
        {.source = "NONE", .prefix = {0b11000, 5}, .code = code},
    };
    const MacroDef macro_def = {
        .name = "Macro", .size = 5, .code = macro_code_defs};
    const InstructionDef instruction_def =
        MakeDef({ArgType::kMacro, 5}, "UL;$Macro;");
    return CompileForTest(instruction_def, macro_def, error);
  };
  EXPECT_TRUE(TestCompile("MOV(R0,R1);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile("MOV(a,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("instruction argument"));
  EXPECT_FALSE(TestCompile("MOV(a0,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("instruction argument"));
  EXPECT_FALSE(TestCompile("MOV(a1,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("instruction argument"));
  EXPECT_FALSE(TestCompile("MOV(b,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("instruction argument"));
  EXPECT_FALSE(TestCompile("MOV(b0,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("instruction argument"));
  EXPECT_FALSE(TestCompile("MOV(b1,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("instruction argument"));
  EXPECT_FALSE(TestCompile("MOV(i,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro argument"), HasSubstr("immediate value")));
  EXPECT_FALSE(TestCompile("MOV(m,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro argument"), HasSubstr("word register")));
  EXPECT_FALSE(TestCompile("MOV(m0,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro argument"), HasSubstr("word register")));
  EXPECT_FALSE(TestCompile("MOV(m1,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro argument"), HasSubstr("word register")));
  EXPECT_FALSE(TestCompile("MOV(p,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro parameter"), HasSubstr("word register")));
  EXPECT_FALSE(TestCompile("MOV(p0,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro parameter"), HasSubstr("word register")));
  EXPECT_FALSE(TestCompile("MOV(p1,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro parameter"), HasSubstr("word register")));
  EXPECT_FALSE(TestCompile("MOV(r,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("own return value"));
  EXPECT_FALSE(TestCompile("MOV(r0,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("own return value"));
  EXPECT_FALSE(TestCompile("MOV(r1,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("own return value"));
}

TEST(InstructionCompilerTest, MacroWordRegParam) {
  std::string error;
  auto TestCompile = [&](std::string_view code) {
    const MacroCodeDef macro_code_defs[] = {
        {.source = "WORD", .prefix = {0, 1}, .code = "MOV(R0,p);MOV(p,R1);"},
        {.source = "TEST", .prefix = {1, 1}, .code = code},
    };
    const MacroDef macro_def = {.name = "Macro",
                                .param = ArgType::kWordReg,
                                .size = 1,
                                .code = macro_code_defs};
    const InstructionDef instruction_def =
        MakeDef({ArgType::kMacro, 1}, "UL;$Macro(R0);");
    return CompileForTest(instruction_def, macro_def, error);
  };
  EXPECT_TRUE(TestCompile("MOV(R0,R1);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile("MOV(p0,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro parameter"));
  EXPECT_FALSE(TestCompile("MOV(p1,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro parameter"));
}

TEST(InstructionCompilerTest, MacroWordRegReturn) {
  const MicrocodeDef microcode_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST, "TEST", MicroArgType::kDwordReg, MicroArgType::kDwordReg}};
  const MacroCodeDef macro_code_defs[] = {
      {.source = "R0",
       .prefix = {0, 1},
       .ret = CpuCore::R0,
       .code = "MOV(R0,R7);"},
      {.source = "R1",
       .prefix = {1, 1},
       .ret = CpuCore::R1,
       .code = "MOV(R1,R7);"},
  };
  const MacroDef macro_def = {.name = "Macro",
                              .ret = ArgType::kWordReg,
                              .size = 1,
                              .code = macro_code_defs};
  InstructionDef instruction_def;
  std::string error;
  auto TestCompile = [&](std::string_view code) {
    instruction_def = MakeDef({ArgType::kMacro, 1}, code);
    return CompileForTest(instruction_def, macro_def, error, microcode_defs);
  };
  EXPECT_TRUE(TestCompile("UL;$Macro;MOV(R4,r);MOV(r,R5);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile("UL;$Macro;MOV(R4,r0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is word"));
  EXPECT_FALSE(TestCompile("UL;$Macro;MOV(R4,r1);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is word"));
  EXPECT_FALSE(TestCompile("UL;$Macro;TEST(D2,R);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is word"));
}

TEST(InstructionCompilerTest, MacroWordParamRegReturn) {
  const MicrocodeDef microcode_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST, "TEST", MicroArgType::kDwordReg, MicroArgType::kDwordReg}};
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,R7);"},
  };
  MacroDef macro_def = {.name = "Macro",
                        .ret = ArgType::kWordReg,
                        .size = 1,
                        .code = macro_code_defs};
  InstructionDef instruction_def = {};
  std::string error;
  auto TestCompile = [&](ArgType param_type, int8_t ret_value,
                         std::string_view code) {
    macro_code_defs[0].ret = ret_value;
    macro_def.param = param_type;
    instruction_def = MakeDef({ArgType::kMacro, 1}, code);
    return CompileForTest(instruction_def, macro_def, error, microcode_defs);
  };
  EXPECT_TRUE(TestCompile(ArgType::kWordReg, CpuCore::MP,
                          "UL;$Macro(R1);MOV(R4,r);MOV(r,R5);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(ArgType::kWordReg, CpuCore::MP,
                           "UL;$Macro(R1);MOV(R4,r0);MOV(r0,R5);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is word"));
  EXPECT_FALSE(TestCompile(ArgType::kWordReg, CpuCore::MP,
                           "UL;$Macro(R1);MOV(R4,r1);MOV(r1,R5);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is word"));
  EXPECT_TRUE(TestCompile(ArgType::kDwordReg, CpuCore::MP0,
                          "UL;$Macro(D1);MOV(R4,r);MOV(r,R5);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(ArgType::kDwordReg, CpuCore::MP0,
                           "UL;$Macro(R1);MOV(R4,r0);MOV(r0,R5);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is word"));
  EXPECT_FALSE(TestCompile(ArgType::kDwordReg, CpuCore::MP0,
                           "UL;$Macro(R1);MOV(R4,r1);MOV(r1,R5);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is word"));
  EXPECT_TRUE(TestCompile(ArgType::kDwordReg, CpuCore::MP1,
                          "UL;$Macro(D1);MOV(R4,r);MOV(r,R5);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(ArgType::kDwordReg, CpuCore::MP1,
                           "UL;$Macro(R1);MOV(R4,r0);MOV(r0,R5);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is word"));
  EXPECT_FALSE(TestCompile(ArgType::kDwordReg, CpuCore::MP1,
                           "UL;$Macro(R1);MOV(R4,r1);MOV(r1,R5);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is word"));
}

TEST(InstructionCompilerTest, MacroWordRegReturnTypes) {
  const MicrocodeDef microcode_defs[] = {{kMicro_UL, "UL"},
                                         {kMicro_TEST_NOP, "NOP"}};
  MacroCodeDef macro_code_defs[] = {
      {.source = "TEST", .prefix = {0, 1}, .code = "NOP;"}};
  std::string error;
  auto TestCompile = [&](ArgType param, int8_t reg) {
    macro_code_defs[0].ret = reg;
    const MacroDef macro_def = {.name = "Macro",
                                .param = param,
                                .ret = ArgType::kWordReg,
                                .size = 1,
                                .code = macro_code_defs};
    return CompileForTest(macro_def, error, microcode_defs);
  };
  for (int i = CpuCore::kMinVirtualReg; i < CpuCore::kRegisterCount; ++i) {
    const bool should_succeed = (CpuCore::GetWordRegName(i) != "invalid");
    std::string context = absl::StrCat(
        "Context: ", CpuCore::GetVirtualWordRegName(i), " (", i, ")");
    EXPECT_EQ(TestCompile(ArgType::kNone, i), should_succeed) << context;
    if (should_succeed) {
      EXPECT_THAT(error, IsEmpty()) << context;
    } else {
      EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type word"))
          << context;
    }
  }
  EXPECT_TRUE(TestCompile(ArgType::kWordReg, CpuCore::MP));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(ArgType::kWordReg, CpuCore::MP1));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type word"));
  EXPECT_TRUE(TestCompile(ArgType::kDwordReg, CpuCore::MP0));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile(ArgType::kDwordReg, CpuCore::MP1));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, InstructionDwordRegArg) {
  const MicrocodeDef kMicroDwordArg1 = {kMicro_TEST, "TEST",
                                        MicroArgType::kDwordReg};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroDwordArg1, MakeDef(ArgType::kWordReg, ArgType::kNone, "TEST(a)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroDwordArg1,
      MakeDef({ArgType::kImmediate, 4}, ArgType::kWordReg, "TEST(b)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(A)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(
      kMicroDwordArg1, MakeDef(ArgType::kDwordReg, ArgType::kNone, "TEST(A)"),
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(
      kMicroDwordArg1, MakeDef(ArgType::kNone, ArgType::kDwordReg, "TEST(A)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(B)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroDwordArg1, MakeDef(ArgType::kDwordReg, ArgType::kNone, "TEST(B)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(TestCompile(
      kMicroDwordArg1,
      MakeDef({ArgType::kImmediate, 4}, ArgType::kDwordReg, "TEST(B)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(M)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(P)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro parameter"));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(R)"), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro return type"));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(0)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  for (std::string_view reg_name : CpuCore::GetDwordRegisterNames()) {
    std::string context = absl::StrCat("Context: ", reg_name);
    EXPECT_TRUE(TestCompile(
        kMicroDwordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ")")), error))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
  }
  for (std::string_view reg_name : CpuCore::GetWordRegisterNames()) {
    std::string context = absl::StrCat("Context: ", reg_name);
    EXPECT_FALSE(TestCompile(
        kMicroDwordArg1, MakeDef(absl::StrCat("TEST(", reg_name, ")")), error))
        << context;
    EXPECT_THAT(error, Not(IsEmpty())) << context;
  }
}

TEST(InstructionCompilerTest, MacroDwordRegArg) {
  std::string error;
  auto TestCompile = [&](std::string_view code) {
    const MicrocodeDef microcode_defs[] = {
        {kMicro_UL, "UL"},
        {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
        {kMicro_TEST, "TEST", MicroArgType::kDwordReg,
         MicroArgType::kDwordReg}};
    const MacroCodeDef macro_code_defs[] = {
        {.source = "DWORD",
         .prefix = {0b0, 1},
         .arg = ArgType::kDwordReg,
         .code = "TEST(D0,M);TEST(M,D1);MOV(R0,m0);MOV(m1,R1);"},
        {.source = "NONE", .prefix = {0b100, 3}, .code = code},
    };
    const MacroDef macro_def = {
        .name = "Macro", .size = 3, .code = macro_code_defs};
    const InstructionDef instruction_def =
        MakeDef({ArgType::kMacro, 3}, "UL;$Macro;");
    return CompileForTest(instruction_def, macro_def, error, microcode_defs);
  };
  EXPECT_TRUE(TestCompile("TEST(D0,D1);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile("TEST(A,D0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("instruction argument"));
  EXPECT_FALSE(TestCompile("TEST(B,D0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("instruction argument"));
  EXPECT_FALSE(TestCompile("TEST(M,D0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro argument"), HasSubstr("dword register")));
  EXPECT_FALSE(TestCompile("TEST(P,D0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro parameter"), HasSubstr("dword register")));
  EXPECT_FALSE(TestCompile("TEST(R,D0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("own return value"));
}

TEST(InstructionCompilerTest, MacroDwordRegParam) {
  std::string error;
  auto TestCompile = [&](std::string_view code) {
    const MicrocodeDef microcode_defs[] = {
        {kMicro_UL, "UL"},
        {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
        {kMicro_TEST, "TEST", MicroArgType::kDwordReg,
         MicroArgType::kDwordReg}};
    const MacroCodeDef macro_code_defs[] = {
        {.source = "DWORD",
         .prefix = {0, 1},
         .code = "TEST(D0,P);TEST(P,D1);MOV(R0,p0);MOV(p1,R1);"},
        {.source = "TEST", .prefix = {1, 1}, .code = code},
    };
    const MacroDef macro_def = {.name = "Macro",
                                .param = ArgType::kDwordReg,
                                .size = 1,
                                .code = macro_code_defs};
    const InstructionDef instruction_def =
        MakeDef({ArgType::kMacro, 1}, "UL;$Macro(D0);");
    return CompileForTest(instruction_def, macro_def, error, microcode_defs);
  };
  EXPECT_TRUE(TestCompile("TEST(D0,D1);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile("MOV(p,R0);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro parameter"));
}

TEST(InstructionCompilerTest, MacroDwordRegReturn) {
  const MicrocodeDef microcode_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST, "TEST", MicroArgType::kDwordReg, MicroArgType::kDwordReg}};
  const MacroCodeDef macro_code_defs[] = {
      {.source = "R0",
       .prefix = {0, 1},
       .ret = CpuCore::D0,
       .code = "TEST(D0,D3);"},
      {.source = "R1",
       .prefix = {1, 1},
       .ret = CpuCore::D1,
       .code = "TEST(D1,D3);"},
  };
  const MacroDef macro_def = {.name = "Macro",
                              .ret = ArgType::kDwordReg,
                              .size = 1,
                              .code = macro_code_defs};
  std::string error;
  auto TestCompile = [&](std::string_view code) {
    const InstructionDef instruction_def = MakeDef({ArgType::kMacro, 1}, code);
    return CompileForTest(instruction_def, macro_def, error, microcode_defs);
  };
  EXPECT_FALSE(TestCompile("UL;$Macro;MOV(R4,r);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is dword"));
  EXPECT_TRUE(TestCompile("UL;$Macro;MOV(R4,r0);MOV(r0,R5);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile("UL;$Macro;MOV(R4,r1);MOV(r1,R5);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile("UL;$Macro;TEST(D2,R);TEST(R,D3);"));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroDwordParamRegReturn) {
  const MicrocodeDef microcode_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
      {kMicro_TEST, "TEST", MicroArgType::kDwordReg, MicroArgType::kDwordReg}};
  const MacroCodeDef macro_code_defs[] = {
      {.source = "R0",
       .prefix = {0, 1},
       .ret = CpuCore::MP,
       .code = "TEST(D0,D3);"},
  };
  const MacroDef macro_def = {.name = "Macro",
                              .param = ArgType::kDwordReg,
                              .ret = ArgType::kDwordReg,
                              .size = 1,
                              .code = macro_code_defs};
  std::string error;
  auto TestCompile = [&](std::string_view code) {
    InstructionDef instruction_def = MakeDef({ArgType::kMacro, 1}, code);
    return CompileForTest(instruction_def, macro_def, error, microcode_defs);
  };
  EXPECT_FALSE(TestCompile("UL;$Macro(D0);MOV(R4,r);"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type is dword"));
  EXPECT_TRUE(TestCompile("UL;$Macro(D0);MOV(R4,r0);MOV(r0,R5);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile("UL;$Macro(D0);MOV(R4,r1);MOV(r1,R5);"));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile("UL;$Macro(D0);TEST(D2,R);TEST(R,D3);"));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroDwordRegReturnTypes) {
  const MicrocodeDef microcode_defs[] = {{kMicro_UL, "UL"},
                                         {kMicro_TEST_NOP, "NOP"}};
  MacroCodeDef macro_code_defs[] = {
      {.source = "TEST", .prefix = {0, 1}, .code = "NOP;"}};
  std::string error;
  auto TestCompile = [&](ArgType param, int8_t reg) {
    macro_code_defs[0].ret = reg;
    const MacroDef macro_def = {.name = "Macro",
                                .param = param,
                                .ret = ArgType::kDwordReg,
                                .size = 1,
                                .code = macro_code_defs};
    return CompileForTest(macro_def, error, microcode_defs);
  };
  for (int i = CpuCore::kMinVirtualReg; i < CpuCore::kRegisterCount; ++i) {
    const bool should_succeed = (CpuCore::GetDwordRegName(i) != "invalid");
    std::string context = absl::StrCat(
        "Context: ", CpuCore::GetVirtualDwordRegName(i), " (", i, ")");
    EXPECT_EQ(TestCompile(ArgType::kNone, i), should_succeed) << context;
    if (should_succeed) {
      EXPECT_THAT(error, IsEmpty()) << context;
    } else {
      EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type dword"))
          << context;
    }
  }
  EXPECT_TRUE(TestCompile(ArgType::kDwordReg, CpuCore::MP));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(ArgType::kDwordReg, CpuCore::MP1));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("return type dword"));
}

TEST(InstructionCompilerTest, MacroNoReturnTypeWithSpecifiedRegister) {
  const MicrocodeDef microcode_defs[] = {{kMicro_UL, "UL"},
                                         {kMicro_TEST_NOP, "NOP"}};
  MacroCodeDef macro_code_defs[] = {
      {.source = "TEST", .prefix = {0, 1}, .code = "NOP;"}};
  std::string error;
  auto TestCompile = [&](ArgType param, int8_t reg) {
    macro_code_defs[0].ret = reg;
    const MacroDef macro_def = {.name = "Macro",
                                .param = param,
                                .ret = ArgType::kNone,
                                .size = 1,
                                .code = macro_code_defs};
    return CompileForTest(macro_def, error, microcode_defs);
  };
  EXPECT_TRUE(TestCompile(ArgType::kNone, 0));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(ArgType::kNone, CpuCore::R2));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("no return value"));
  EXPECT_FALSE(TestCompile(ArgType::kWordReg, CpuCore::MP0));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("no return value"));
  EXPECT_FALSE(TestCompile(ArgType::kDwordReg, CpuCore::MP1));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("no return value"));
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

TEST(InstructionCompilerTest, InstructionArgAsStatus) {
  const MicrocodeDef kMicroStatusArgs = {
      kMicro_TEST, "TEST", MicroArgType::kStatus, MicroArgType::kStatus};
  std::string error;
  EXPECT_TRUE(TestCompile(
      kMicroStatusArgs,
      MakeDef({ArgType::kImmediate, 5}, {ArgType::kImmediate, 4}, "TEST(a,b)"),
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroStatusArgs,
                           MakeDef(ArgType::kWordReg, "TEST(a,a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroStatusArgs,
                           MakeDef(ArgType::kDwordReg, "TEST(a,a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroStatusArgs, MakeDef("TEST(a,a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroStatusArgs,
      MakeDef({ArgType::kImmediate, 5}, ArgType::kWordReg, "TEST(b,b)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroStatusArgs,
      MakeDef({ArgType::kImmediate, 5}, ArgType::kDwordReg, "TEST(b,b)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroStatusArgs, MakeDef("TEST(b,b)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroStatusArgs,
      MakeDef({ArgType::kImmediate, 5}, {ArgType::kImmediate, 5}, "TEST(m,m)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroStatusArgs,
      MakeDef({ArgType::kImmediate, 5}, {ArgType::kImmediate, 5}, "TEST(i,i)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MacroArgAsStatus) {
  const MicrocodeDef kMicroStatusArgs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "TEST", MicroArgType::kStatus, MicroArgType::kStatus}};
  MacroCodeDef macro_code_defs[] = {
      {.source = "CONST",
       .prefix = {0, 5},
       .code = "TEST(Z,S);TEST(C,O);TEST(ZC,SO);TEST(ZS,CO);TEST(ZSC,SCO);"},
      {.source = "$#4", .prefix = {1, 1}},
  };
  std::string error;
  auto TestCompile = [&](std::string_view code, Argument arg) {
    macro_code_defs[1].code = code;
    macro_code_defs[1].arg = arg;
    const MacroDef macro_def = {
        .name = "Macro", .size = 5, .code = macro_code_defs};
    const InstructionDef instruction_def =
        MakeDef({ArgType::kMacro, 5}, "UL;$Macro;");
    return CompileForTest(instruction_def, macro_def, error, kMicroStatusArgs);
  };
  EXPECT_TRUE(TestCompile("TEST(Z,S);", {ArgType::kImmediate, 4}));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile("TEST(m,m);", {ArgType::kImmediate, 4}));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile("TEST(i,i);", {ArgType::kImmediate, 4}));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile("TEST(a,a);", {ArgType::kImmediate, 4}));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile("TEST(b,b);", {ArgType::kImmediate, 4}));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile("TEST(m,m);", {ArgType::kWordReg, 4}));
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

TEST(InstructionCompilerTest, InstructionArgAsCondition) {
  const MicrocodeDef kMicroConditionArgs = {
      kMicro_TEST, "TEST", MicroArgType::kCondition, MicroArgType::kCondition};
  std::string error;
  EXPECT_TRUE(TestCompile(
      kMicroConditionArgs,
      MakeDef({ArgType::kImmediate, 3}, {ArgType::kImmediate, 3}, "TEST(a,b)"),
      error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile(kMicroConditionArgs,
                           MakeDef(ArgType::kWordReg, "TEST(a,a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroConditionArgs,
                           MakeDef(ArgType::kDwordReg, "TEST(a,a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroConditionArgs, MakeDef("TEST(a,a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroConditionArgs,
      MakeDef({ArgType::kImmediate, 3}, ArgType::kWordReg, "TEST(b,b)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroConditionArgs,
      MakeDef({ArgType::kImmediate, 3}, ArgType::kDwordReg, "TEST(b,b)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroConditionArgs, MakeDef("TEST(b,b)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroConditionArgs,
      MakeDef({ArgType::kImmediate, 3}, {ArgType::kImmediate, 3}, "TEST(m,m)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(
      kMicroConditionArgs,
      MakeDef({ArgType::kImmediate, 3}, {ArgType::kImmediate, 3}, "TEST(i,i)"),
      error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MacroArgAsCondition) {
  const MicrocodeDef kMicroConditionArgs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "TEST", MicroArgType::kCondition,
       MicroArgType::kCondition}};
  MacroCodeDef macro_code_defs[] = {
      {.source = "CONST",
       .prefix = {0, 4},
       .code = "TEST(Z,NZ);TEST(S,NS);TEST(C,NC);TEST(O,NO);"},
      {.source = "$#3", .prefix = {1, 1}},
  };
  std::string error;
  auto TestCompile = [&](std::string_view code, Argument arg) {
    macro_code_defs[1].code = code;
    macro_code_defs[1].arg = arg;
    const MacroDef macro_def = {
        .name = "Macro", .size = 4, .code = macro_code_defs};
    const InstructionDef instruction_def =
        MakeDef({ArgType::kMacro, 4}, "UL;$Macro;");
    return CompileForTest(instruction_def, macro_def, error,
                          kMicroConditionArgs);
  };
  EXPECT_TRUE(TestCompile("TEST(Z,NZ);", {ArgType::kImmediate, 3}));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(TestCompile("TEST(m,m);", {ArgType::kImmediate, 3}));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(TestCompile("TEST(i,i);", {ArgType::kImmediate, 3}));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile("TEST(a,a);", {ArgType::kImmediate, 3}));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile("TEST(b,b);", {ArgType::kImmediate, 3}));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile("TEST(m,m);", {ArgType::kWordReg, 3}));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, AddressArg) {
  MicrocodeDef microcode_defs[] = {
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
  EXPECT_TRUE(CompileForTest(MakeDef("UL;TEST(-1);"), error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(MakeDef("UL;TEST(-2);"), error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(MakeDef("UL;TEST(-3);"), error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(MakeDef("UL;TEST(0);"), error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(MakeDef("UL;TEST(1);"), error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(MakeDef("TEST(0);UL;"), error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(
      CompileForTest(MakeDef("NOP;TEST(-2);UL;"), error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(MakeDef("TEST(1);UL;"), error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(MakeDef("TEST(2);UL;NOP;LK(CODE);NOP;UL;NOP;"),
                              error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(MakeDef("TEST(3);UL;NOP;LK(CODE);NOP;UL;NOP;"),
                             error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(CompileForTest(MakeDef("TEST(4);UL;NOP;LK(CODE);NOP;UL;NOP;"),
                             error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(MakeDef("TEST(5);UL;NOP;LK(CODE);NOP;UL;NOP;"),
                              error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(MakeDef("UL;TEST(4);NOP;LK(CODE);NOP;UL;NOP;"),
                             error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(MakeDef("TEST(3);UL;NOP;LK(STACK);NOP;UL;NOP;"),
                              error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(MakeDef("TEST(3);UL;NOP;LK(DATA);NOP;UL;NOP;"),
                              error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(MakeDef("TEST(3);UL;NOP;LK(EXTRA);NOP;UL;NOP;"),
                              error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      MakeDef("@label:NOP;TEST(@label);UL;NOP;LK(DATA);NOP;UL;NOP;"), error,
      microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(CompileForTest(
      MakeDef("NOP;TEST(@label);@label:UL;NOP;LK(DATA);NOP;UL;NOP;"), error,
      microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(
      MakeDef("NOP;TEST(@label);UL;@label:NOP;LK(DATA);NOP;UL;NOP;"), error,
      microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      MakeDef("NOP;UL;TEST(@label);NOP;@label:LK(DATA);NOP;UL;NOP;"), error,
      microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(
      MakeDef("NOP;UL;TEST(@label);NOP;LK(DATA);@label:NOP;UL;NOP;"), error,
      microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(CompileForTest(
      MakeDef("NOP;UL;TEST(@label);NOP;LK(DATA);NOP;UL;@label:NOP;"), error,
      microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(
      CompileForTest(MakeDef("UL;TEST(@missing);"), error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(MakeDef("UL;TEST(invalid);@invalid:NOP;"), error,
                              microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      CompileForTest(MakeDef("UL;TEST(@label);PLK(C0);@label:NOP;PUL;"), error,
                     microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(

      MakeDef("UL;PLK(C0);NOP;TEST(@label);PUL;@label:NOP;"), error,
      microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(

      MakeDef("UL;PLK(C0);NOP;TEST(@label);PUL;NOP;PLK(C0);@"
              "label:NOP;PUL;"),
      error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(
      CompileForTest(MakeDef("UL;PLK(C0);NOP;TEST(@label);NOP;@label:NOP;PUL;"),
                     error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(

      MakeDef("UL;LK(DATA);NOP;TEST(@label);UL;NOP;PLK(C0);@"
              "label:NOP;PUL;"),
      error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      CompileForTest(MakeDef("UL;TEST(@label);CLK(C0);@label:NOP;CUL;"), error,
                     microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(

      MakeDef("UL;CLK(C0);NOP;TEST(@label);CUL;@label:NOP;"), error,
      microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(CompileForTest(

      MakeDef("UL;CLK(C0);NOP;TEST(@label);CUL;NOP;CLK(C0);@"
              "label:NOP;CUL;"),
      error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(
      CompileForTest(MakeDef("UL;CLK(C0);NOP;TEST(@label);NOP;@label:NOP;CUL;"),
                     error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(

      MakeDef("UL;LK(DATA);NOP;TEST(@label);UL;NOP;CLK(C0);@"
              "label:NOP;CUL;"),
      error, microcode_defs));
  EXPECT_THAT(error, Not(IsEmpty()));
  for (std::string_view reg_name : CpuCore::GetWordRegisterNames()) {
    std::string context = absl::StrCat("Context: ", reg_name);
    std::string code = absl::Substitute(
        "UL;LKR($0);TEST(2);UL;LKR($0);TEST(-4);UL;", reg_name);
    EXPECT_TRUE(CompileForTest(MakeDef(code), error, microcode_defs))
        << context;
    EXPECT_THAT(error, IsEmpty()) << context;
  }
  for (int i = 0; i < CpuCore::kRegisterCount; ++i) {
    int j = (i + 1) % CpuCore::kRegisterCount;
    std::string context = absl::StrCat("Context: ", CpuCore::GetWordRegName(i),
                                       " and ", CpuCore::GetWordRegName(j));
    std::string code = absl::Substitute(
        "UL;LKR($0);TEST(2);UL;LKR($1);TEST(-4);UL;",
        CpuCore::GetWordRegName(i), CpuCore::GetWordRegName(j));
    EXPECT_FALSE(CompileForTest(MakeDef(code), error, microcode_defs))
        << context;
    EXPECT_THAT(error, Not(IsEmpty())) << context;
  }
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

TEST(InstructionCompilerTest, MemoryLockDuringFetch) {
  InstructionDef instruction_def = MakeDef("LK(STACK);UL;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, RegisterMemoryLockDuringFetch) {
  InstructionDef instruction_def = MakeDef("LKR(R7);UL;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLockDuringFetch) {
  InstructionDef instruction_def = MakeDef("PLK(C0);PUL;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreLockDuringFetch) {
  InstructionDef instruction_def = MakeDef("CLK(C0);CUL;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);LK(STACK);UL;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockAfterPriorPortLock) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);LK(STACK);UL;PUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryLockAfterPriorCoreLock) {
  InstructionDef instruction_def = MakeDef("UL;CLK(C0);LK(STACK);UL;CUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, RegisterMemoryLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);LKR(R7);UL;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);PLK(C0);PUL;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);CLK(C0);CUL;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MemoryUnlockWhenNotLocked) {
  InstructionDef instruction_def = MakeDef("UL;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MemoryUnlockWhenPortLocked) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortUnlockWhenPortLocked) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);PUL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, PortUnlockWhenNotLocked) {
  InstructionDef instruction_def = MakeDef("UL;PUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);PUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreUnlockWhenCoreLocked) {
  InstructionDef instruction_def = MakeDef("UL;CLK(C0);CUL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, CoreUnlockWhenNotLocked) {
  InstructionDef instruction_def = MakeDef("UL;CUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, CoreUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);CUL;");
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
  InstructionDef instruction_def = MakeDef(code);
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
  InstructionDef instruction_def = MakeDef(code);
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, AddressWhenNotLocked) {
  InstructionDef instruction_def = MakeDef("UL;ADR(C0);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, AddressWhenPortLocked) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);ADR(C0);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, AddressDuringFetch) {
  InstructionDef instruction_def = MakeDef("ADR(C0);UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, AddressWhenMemoryLocked) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);ADR(C0);UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, LoadDuringFetch) {
  InstructionDef instruction_def = MakeDef("LD(C0);UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, LoadBeforeAddress) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);LD(C0);UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, LoadAfterAddress) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);ADR(C0);LD(C0);UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StoreDuringFetch) {
  InstructionDef instruction_def = MakeDef("ST(C0);UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StoreAfterAddressDuringFetch) {
  InstructionDef instruction_def = MakeDef("ADR(C0);ST(C0);UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StoreBeforeAddress) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);ST(C0);UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StoreAfterAddress) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);ADR(C0);ST(C0);UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StorePushDuringFetch) {
  InstructionDef instruction_def = MakeDef("STP(C0);UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StorePushAfterAddressDuringFetch) {
  InstructionDef instruction_def = MakeDef("ADR(C0);STP(C0);UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, StorePushBeforeAddress) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);STP(C0);UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, StorePushAfterAddress) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);ADR(C0);STP(C0);UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, PortLoadWhenUnlocked) {
  InstructionDef instruction_def = MakeDef("UL;PLD(_,C1);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLoadWhenMemoryLocked) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);PLD(_,C1);UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortLoadWhenPortLocked) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);PLD(_,C1);PUL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, PortStoreWhenUnlocked) {
  InstructionDef instruction_def = MakeDef("UL;PST(_,C1);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortStoreWhenMemoryLocked) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);PST(_,C1);UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, PortStoreWhenPortLocked) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);PST(_,C1);PUL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, NoFetchUnlock) {
  InstructionDef instruction_def = MakeDef("MOV(R0,R1);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, NoMemoryUnlock) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);MOV(R0,R1);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, NoPortUnlock) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);MOV(R0,R1);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, NoCoreUnlock) {
  InstructionDef instruction_def = MakeDef("UL;CLK(C0);MOV(R0,R1);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitDuringFetch) {
  InstructionDef instruction_def = MakeDef("WAIT(C0);UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitWithinMemoryLock) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);WAIT(C0);UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitWithinPortLock) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);WAIT(C0);PUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, WaitWithinCoreLock) {
  InstructionDef instruction_def = MakeDef("UL;CLK(C0);WAIT(C0);CUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltDuringFetch) {
  InstructionDef instruction_def = MakeDef("HALT;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltWithinMemoryLock) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);HALT;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltWithinPortLock) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);HALT;PUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, HaltWithinCoreLock) {
  InstructionDef instruction_def = MakeDef("UL;CLK(C0);HALT;CUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtDuringFetch) {
  InstructionDef instruction_def = MakeDef("IRT;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtWithinMemoryLock) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);IRT;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtWithinPortLock) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);IRT;PUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, IrtWithinCoreLock) {
  InstructionDef instruction_def = MakeDef("UL;CLK(C0);IRT;CUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndDuringFetch) {
  InstructionDef instruction_def = MakeDef("END;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndWithinMemoryLock) {
  InstructionDef instruction_def = MakeDef("UL;LK(DATA);END;UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndWithinPortLock) {
  InstructionDef instruction_def = MakeDef("UL;PLK(C0);END;PUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, EndWithinCoreLock) {
  InstructionDef instruction_def = MakeDef("UL;CLK(C0);END;CUL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, LabelMissingColon) {
  InstructionDef instruction_def = MakeDef("UL;@start;MOV(C0,C1);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, InvalidLabelCharacter) {
  // Just test the printable ASCII characters.
  for (char ch = 33; absl::ascii_isgraph(ch); ++ch) {
    std::string code = absl::StrCat("UL;@", std::string(1, ch), ":MOV(C0,C1);");
    InstructionDef instruction_def = MakeDef(code);
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
  InstructionDef instruction_def =
      MakeDef("UL;@start:MOV(C0,C1);@start:MOV(C1,C0);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(InstructionCompilerTest, MacroWithMissingName) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro name"));
}

TEST(InstructionCompilerTest, MacroWithInvalidCharacter) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  char name[] = "IsValidX";
  MacroDef macro_def = {.name = name, .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  for (char ch = 32; absl::ascii_isprint(ch); ++ch) {
    name[7] = ch;
    if (absl::ascii_isalnum(ch) || ch == '_') {
      EXPECT_TRUE(CompileForTest(instruction_def, macro_def, error))
          << "Character: '" << ch << "'";
      EXPECT_THAT(error, IsEmpty()) << "Character: '" << ch << "'";
    } else {
      EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error))
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
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
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
  InstructionDef instruction_def = MakeDef({ArgType::kMacro, 8}, "UL;$Macro");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, macro_def, error));
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
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
}

TEST(InstructionCompilerTest, MacroWithInvalidPrefixSize) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 2}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("prefix"));
}

TEST(InstructionCompilerTest, MacroWithInvalidArgumentSize) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .arg = {ArgType::kImmediate, 2}, .code = "MOV(R0,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
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
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("prefix"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("argument"));
}

TEST(InstructionCompilerTest, MacroWithPrefixValueOutOfRange) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R2", .prefix = {2, 1}, .code = "MOV(R2,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("prefix value"));
}

TEST(InstructionCompilerTest, MacroWithInvalidLabel) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "@NoColon-MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
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
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("microcode"));
}

TEST(InstructionCompilerTest, MacroWithNoCode) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = ""}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(error, IsEmpty());
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
  InstructionDef instruction_def = MakeDef({ArgType::kMacro, 1}, "$Macro;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, macro_def, error));
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
  InstructionDef instruction_def = MakeDef({ArgType::kMacro, 1}, "UL;$Macro;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
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
  InstructionDef instruction_def = MakeDef({ArgType::kMacro, 1}, "UL;$Macro;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, InvalidMicrocodeInMacro) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;$Macro;$Macro;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("multiple macros"));
}

TEST(InstructionCompilerTest, MultipleMacrosInInstruction) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef("UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("not found"));
}

TEST(InstructionCompilerTest, MacroArgumentWithoutMacroUseInInstruction) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef({ArgType::kMacro, 1}, "UL;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
}

TEST(InstructionCompilerTest, MacroInInstructionWithoutArgument) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef("UL;$Macro;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro argument"));
}

TEST(InstructionCompilerTest, TooManyMacroArgumentsInInstruction) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"}};
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, {ArgType::kMacro, 1}, "UL;$Macro;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro arguments"));
}

TEST(InstructionCompilerTest, UnknownMacroInInstruction) {
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;$UndefinedMacro;");
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
  InstructionDef instruction_def = MakeDef({ArgType::kMacro, 2}, "UL;$Macro;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
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
  InstructionDef instruction_def = MakeDef({ArgType::kMacro, 1}, "UL;$Macro;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("argument size"));
}

TEST(InstructionCompilerTest, MacroMakesArg1AddressTooBig) {
  MicrocodeDef microcode_defs[] = {
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
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;TEST(@end,0);$Macro;@end:NOP");
  std::string error;
  EXPECT_FALSE(
      CompileForTest(instruction_def, macro_def, error, microcode_defs));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("address"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("@end"));
}

TEST(InstructionCompilerTest, MacroMakesArg1AddressMax) {
  MicrocodeDef microcode_defs[] = {
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
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;TEST(@end,0);$Macro;@end:NOP");
  std::string error;
  EXPECT_TRUE(
      CompileForTest(instruction_def, macro_def, error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroMakesArg2AddressTooBig) {
  MicrocodeDef microcode_defs[] = {
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
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;TEST(0,@end);$Macro;@end:NOP");
  std::string error;
  EXPECT_FALSE(
      CompileForTest(instruction_def, macro_def, error, microcode_defs));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("address"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("@end"));
}

TEST(InstructionCompilerTest, MacroMakesArg2AddressMax) {
  MicrocodeDef microcode_defs[] = {
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
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;TEST(0,@end);$Macro;@end:NOP");
  std::string error;
  EXPECT_TRUE(
      CompileForTest(instruction_def, macro_def, error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroMakesArg1AddressTooSmall) {
  MicrocodeDef microcode_defs[] = {
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
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;@start:$Macro;TEST(@start,0);");
  std::string error;
  EXPECT_FALSE(
      CompileForTest(instruction_def, macro_def, error, microcode_defs));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("address"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("@start"));
}

TEST(InstructionCompilerTest, MacroMakesArg1AddressMin) {
  MicrocodeDef microcode_defs[] = {
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
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;@start:$Macro;TEST(@start,0);");
  std::string error;
  EXPECT_TRUE(
      CompileForTest(instruction_def, macro_def, error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroMakesArg2AddressTooSmall) {
  MicrocodeDef microcode_defs[] = {
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
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;@start:$Macro;TEST(0,@start);");
  std::string error;
  EXPECT_FALSE(
      CompileForTest(instruction_def, macro_def, error, microcode_defs));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro size"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("address"));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("@start"));
}

TEST(InstructionCompilerTest, MacroMakesArg2AddressMin) {
  MicrocodeDef microcode_defs[] = {
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
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;@start:$Macro;TEST(0,@start);");
  std::string error;
  EXPECT_TRUE(
      CompileForTest(instruction_def, macro_def, error, microcode_defs));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstrctuionCompilerTest, CallMacroWithExtraFirstParameter) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"},
  };
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, ArgType::kWordReg, "UL;$Macro(R2);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro"), HasSubstr("first argument")));
}

TEST(InstructionCompilerTest, CallMacroWithNoParameters) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "CODE", .prefix = {0, 2}, .code = "LK(CODE);"},
      {.source = "STACK", .prefix = {1, 2}, .code = "LK(STACK);"},
      {.source = "DATA", .prefix = {2, 2}, .code = "LK(DATA);"},
      {.source = "EXTRA", .prefix = {3, 2}, .code = "LK(EXTRA);"},
  };
  MacroDef macro_def = {.name = "Lk", .size = 2, .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef(
      {ArgType::kMacro, 2}, ArgType::kWordReg, "UL;$Lk;ADR(b);LD(b);UL;");
  std::string error;
  EXPECT_TRUE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroFirstParameterType) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
  };
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  auto MacroWithParam = [&](ArgType param) -> const MacroDef& {
    macro_def.param = param;
    return macro_def;
  };
  std::string error;
  EXPECT_TRUE(CompileForTest(MacroWithParam(ArgType::kNone), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(MacroWithParam(ArgType::kImmediate), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro parameter type"));
  EXPECT_TRUE(CompileForTest(MacroWithParam(ArgType::kWordReg), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(CompileForTest(MacroWithParam(ArgType::kDwordReg), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(MacroWithParam(ArgType::kMacro), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro parameter type"));
}

TEST(InstructionCompilerTest, MacroReturnType) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
  };
  MacroDef macro_def = {.name = "Macro", .size = 1, .code = macro_code_defs};
  auto MacroWithReturn = [&](ArgType return_type) -> const MacroDef& {
    macro_def.ret = return_type;
    return macro_def;
  };
  std::string error;
  EXPECT_TRUE(CompileForTest(MacroWithReturn(ArgType::kNone), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(MacroWithReturn(ArgType::kImmediate), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro return type"));
  EXPECT_TRUE(CompileForTest(MacroWithReturn(ArgType::kWordReg), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(CompileForTest(MacroWithReturn(ArgType::kDwordReg), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(MacroWithReturn(ArgType::kMacro), error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("macro return type"));
}

TEST(InstructionCompilerTest, CallMacroMissingFirstParameter) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"},
  };
  MacroDef macro_def = {.name = "Macro",
                        .param = ArgType::kWordReg,
                        .size = 1,
                        .code = macro_code_defs};
  InstructionDef instruction_def = MakeDef({ArgType::kMacro, 1}, "UL;$Macro;");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro"), HasSubstr("first argument")));
}

TEST(InstructionCompilerTest, CallMacroWithExtraSecondParameter) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "R0", .prefix = {0, 1}, .code = "MOV(R0,C0);"},
      {.source = "R1", .prefix = {1, 1}, .code = "MOV(R1,C0);"},
  };
  MacroDef macro_def = {.name = "Macro",
                        .param = ArgType::kWordReg,
                        .size = 1,
                        .code = macro_code_defs};
  InstructionDef instruction_def =
      MakeDef({ArgType::kMacro, 1}, "UL;$Macro(R0,R1);");
  std::string error;
  EXPECT_FALSE(CompileForTest(instruction_def, macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("macro"), HasSubstr("second argument")));
}

TEST(InstructionCompilerTest, CallMacroWithWordParameter) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "T1", .prefix = {0, 1}, .code = "MOV(R0,p);"},
      {.source = "T2", .prefix = {1, 1}, .code = "MOV(p,R0);"},
  };
  MacroDef macro_def = {.name = "Macro",
                        .param = ArgType::kWordReg,
                        .size = 1,
                        .code = macro_code_defs};
  std::string error;
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kMacro, 1}, "UL;$Macro(1);"),
                              macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("argument"));
  EXPECT_TRUE(CompileForTest(MakeDef({ArgType::kMacro, 1}, "UL;$Macro(R0);"),
                             macro_def, error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kMacro, 1}, "UL;$Macro(D0);"),
                              macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("argument"));
}

TEST(InstructionCompilerTest, CallMacroWithDwordParameter) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "T1", .prefix = {0, 1}, .code = "MOV(R0,p0);MOV(R1,p1);"},
      {.source = "T1", .prefix = {1, 1}, .code = "MOV(p0,R0);MOV(p1,R1);"},
  };
  MacroDef macro_def = {.name = "Macro",
                        .param = ArgType::kDwordReg,
                        .size = 1,
                        .code = macro_code_defs};
  std::string error;
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kMacro, 1}, "UL;$Macro(1);"),
                              macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("argument"));
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kMacro, 1}, "UL;$Macro(R0);"),
                              macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error), HasSubstr("argument"));
  EXPECT_TRUE(CompileForTest(MakeDef({ArgType::kMacro, 1}, "UL;$Macro(D0);"),
                             macro_def, error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(InstructionCompilerTest, MacroWithConflictingPrefix) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "Test0", .prefix = {0, 3}, .code = "MOV(R0,R1);"},
      {.source = "Test1", .prefix = {1, 3}, .code = "MOV(R0,R1);"},
      {.source = "Test2", .prefix = {2, 3}, .code = "MOV(R0,R1);"},
      {.source = "Test3", .prefix = {3, 3}, .code = "MOV(R0,R1);"},
      {.source = "Test4", .prefix = {4, 3}, .code = "MOV(R0,R1);"},
      {.source = "Test5", .prefix = {5, 3}, .code = "MOV(R0,R1);"},
      {.source = "Test6", .prefix = {2, 3}, .code = "MOV(R0,R1);"},
      {.source = "Test7", .prefix = {7, 3}, .code = "MOV(R0,R1);"},
      {.source = "Test8", .prefix = {8, 3}, .code = "MOV(R0,R1);"},
  };
  MacroDef macro_def = {.name = "Macro",
                        .param = ArgType::kDwordReg,
                        .size = 3,
                        .code = macro_code_defs};
  std::string error;
  EXPECT_FALSE(CompileForTest(macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("duplicate macro"), HasSubstr("test2"),
                    HasSubstr("test6"), HasSubstr(" 010 ")));
}

TEST(InstructionCompilerTest, MacroWithConflictingArgAndPrefix) {
  MacroCodeDef macro_code_defs[] = {
      {.source = "Test0", .prefix = {0, 3}, .code = "MOV(R0,R1);"},
      {.source = "Test1", .arg = ArgType::kWordReg, .code = "MOV(R0,R1);"},
  };
  MacroDef macro_def = {.name = "Macro",
                        .param = ArgType::kDwordReg,
                        .size = 3,
                        .code = macro_code_defs};
  std::string error;
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kMacro, 1}, "UL;$Macro(2);"),
                              macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("duplicate macro"), HasSubstr(" 000 ")));
}

TEST(InstructionCompilerTest, MacroWithConflictingArgPrefixCombos) {
  MacroCodeDef macro_code_defs[] = {
      {
          .source = "Test0",
          .prefix = {0b11001, 5},
          .code = "MOV(R0,R1);",
      },
      {
          .source = "Test1",
          .prefix = {0b01, 2},
          .arg = ArgType::kWordReg,
          .code = "MOV(R0,R1);",
      },
      {
          .source = "Test2",
          .prefix = {0b1, 1},
          .arg = {ArgType::kImmediate, 4},
          .code = "MOV(R0,R1);",
      },
  };
  MacroDef macro_def = {.name = "Macro",
                        .param = ArgType::kDwordReg,
                        .size = 5,
                        .code = macro_code_defs};
  std::string error;
  EXPECT_FALSE(CompileForTest(MakeDef({ArgType::kMacro, 1}, "UL;$Macro(2);"),
                              macro_def, error));
  EXPECT_THAT(absl::AsciiStrToLower(error),
              AllOf(HasSubstr("duplicate macro"), HasSubstr("test0"),
                    HasSubstr("test2"), HasSubstr(" 11001 ")));
}

}  // namespace
}  // namespace oz3
