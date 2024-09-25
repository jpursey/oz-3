// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/microcode.h"

#include <ostream>
#include <string>

#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction.h"
#include "oz3/core/port.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Not;
using ::testing::TestWithParam;
using ::testing::ValuesIn;

constexpr uint8_t kOp_TEST = 200;
constexpr uint8_t kMicro_TEST_NOP = 254;
constexpr uint8_t kMicro_TEST = 255;

struct CompileTestCase {
  MicrocodeDef micro;
  InstructionDef instruction;
  bool valid;
};

void PrintArg(MicroArgType arg_type, std::ostream* os) {
  switch (arg_type) {
    case MicroArgType::kNone:
      *os << "_";
      break;
    case MicroArgType::kBank:
      *os << "b";
      break;
    case MicroArgType::kStatus:
      *os << "s";
      break;
    case MicroArgType::kCondition:
      *os << "c";
      break;
    case MicroArgType::kAddress:
      *os << "a";
      break;
    case MicroArgType::kValue:
      *os << "v";
      break;
    case MicroArgType::kWordReg:
      *os << "r";
      break;
    case MicroArgType::kDwordReg:
      *os << "d";
      break;
  }
}

void PrintDef(const MicrocodeDef& def, std::ostream* os) {
  *os << def.op_name;
  if (def.arg1 != MicroArgType::kNone) {
    *os << "(";
    PrintArg(def.arg1, os);
    if (def.arg2 != MicroArgType::kNone) {
      *os << ",";
      PrintArg(def.arg2, os);
    }
    *os << ")";
  }
}

void PrintDef(const InstructionDef& def, std::ostream* os) {
  *os << "\"" << (def.decl.op_name.empty() ? "???" : def.decl.op_name);
  if (!def.decl.arg1.empty()) {
    *os << " " << def.decl.arg1;
    if (!def.decl.arg2.empty()) {
      *os << " " << def.decl.arg2;
    }
  }
  *os << ": " << def.code << "\"";
}

void PrintTo(const CompileTestCase& test_case, std::ostream* os) {
  *os << "{micro=";
  PrintDef(test_case.micro, os);
  *os << ",instruction=";
  PrintDef(test_case.instruction, os);
  *os << ",valid=" << (test_case.valid ? "true" : "false") << "}";
}

constexpr MicrocodeDef kMicroNoArgs = {kMicro_TEST, "TEST"};

bool TestCompile(const MicrocodeDef& microcode_def,
                 const InstructionDef& instruction_def, std::string& error) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"}, {kMicro_LK, "LK", MicroArgType::kBank}, microcode_def};
  InstructionMicrocodes codes(micros);
  std::string new_code = absl::StrCat("UL;", instruction_def.code);
  InstructionDef instruction = instruction_def;
  instruction.code = new_code;
  instruction.decl.op_name = "TEST";
  return codes.Compile(instruction, &error);
}
InstructionDef MakeDef(std::string_view code) {
  return InstructionDef{.code = code};
}

InstructionDef MakeDef(std::pair<std::string_view, std::string_view> args,
                       std::string_view code) {
  return InstructionDef{.decl = {.arg1 = args.first, .arg2 = args.second},
                        .code = code};
}

TEST(MicrocodeTest, BankArg) {
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

TEST(MicrocodeTest, ImmArg) {
  const MicrocodeDef kMicroImmArg1 = {kMicro_TEST, "TEST",
                                      MicroArgType::kValue};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroImmArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroImmArg1, MakeDef({"a", ""}, "TEST(a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroImmArg1, MakeDef({"A", ""}, "TEST(A)"), error));
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

TEST(MicrocodeTest, WordRegArg) {
  constexpr MicrocodeDef kMicroWordArg1 = {kMicro_TEST, "TEST",
                                           MicroArgType::kWordReg};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroWordArg1, MakeDef({"", "a"}, "TEST(a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroWordArg1, MakeDef({"", "A"}, "TEST(a0)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroWordArg1, MakeDef({"", "A"}, "TEST(a1)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroWordArg1, MakeDef("TEST(b)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroWordArg1, MakeDef({"b", ""}, "TEST(b)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroWordArg1, MakeDef({"B", ""}, "TEST(b0)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroWordArg1, MakeDef({"B", ""}, "TEST(b1)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroWordArg1, MakeDef({"A", ""}, "TEST(A)"), error));
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

TEST(MicrocodeTest, DwordRegArg) {
  const MicrocodeDef kMicroDwordArg1 = {kMicro_TEST, "TEST",
                                        MicroArgType::kDwordReg};
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroDwordArg1, MakeDef({"a", ""}, "TEST(a)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroDwordArg1, MakeDef({"v", "b"}, "TEST(b)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(A)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(
      TestCompile(kMicroDwordArg1, MakeDef({"A", ""}, "TEST(A)"), error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(
      TestCompile(kMicroDwordArg1, MakeDef({"", "A"}, "TEST(A)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(TestCompile(kMicroDwordArg1, MakeDef("TEST(B)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(
      TestCompile(kMicroDwordArg1, MakeDef({"B", ""}, "TEST(B)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(
      TestCompile(kMicroDwordArg1, MakeDef({"v", "B"}, "TEST(B)"), error));
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

TEST(MicrocodeTest, StatusArg) {
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

TEST(MicrocodeTest, PortModeArg) {
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

TEST(MicrocodeTest, ConditionArg) {
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

TEST(MicrocodeTest, InvalidMicrocodeName) {
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroNoArgs, MakeDef("INVALID"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, ProvideExtraArg) {
  std::string error;
  EXPECT_FALSE(TestCompile(kMicroNoArgs, MakeDef("TEST(1)"), error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, Address) {
  MicrocodeDef micros[] = {
      {kMicro_UL, "UL"},        {kMicro_LK, "LK", MicroArgType::kBank},
      {kMicro_PUL, "PUL"},      {kMicro_PLK, "PLK", MicroArgType::kWordReg},
      {kMicro_CUL, "CUL"},      {kMicro_CLK, "CLK", MicroArgType::kWordReg},
      {kMicro_TEST_NOP, "NOP"}, {kMicro_TEST, "TEST", MicroArgType::kAddress},
  };
  InstructionMicrocodes codes(micros);
  std::string error;
  EXPECT_TRUE(codes.Compile(InstructionDef{.code = "UL;TEST(-1);"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(codes.Compile(InstructionDef{.code = "UL;TEST(-2);"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(InstructionDef{.code = "UL;TEST(-3);"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(codes.Compile(InstructionDef{.code = "UL;TEST(0);"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(codes.Compile(InstructionDef{.code = "UL;TEST(1);"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(codes.Compile(InstructionDef{.code = "TEST(0);UL;"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(
      codes.Compile(InstructionDef{.code = "NOP;TEST(-2);UL;"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(codes.Compile(InstructionDef{.code = "TEST(1);UL;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code = "TEST(2);UL;NOP;LK(CODE);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(codes.Compile(
      InstructionDef{.code = "TEST(3);UL;NOP;LK(CODE);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(codes.Compile(
      InstructionDef{.code = "TEST(4);UL;NOP;LK(CODE);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code = "TEST(5);UL;NOP;LK(CODE);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(codes.Compile(
      InstructionDef{.code = "UL;TEST(4);NOP;LK(CODE);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code = "TEST(3);UL;NOP;LK(STACK);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code = "TEST(3);UL;NOP;LK(DATA);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code = "TEST(3);UL;NOP;LK(EXTRA);NOP;UL;NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(codes.Compile(
      InstructionDef{.code =
                         "@label:NOP;TEST(@label);UL;NOP;LK(DATA);NOP;UL;NOP;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_TRUE(codes.Compile(
      InstructionDef{.code =
                         "NOP;TEST(@label);@label:UL;NOP;LK(DATA);NOP;UL;NOP;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code =
                         "NOP;TEST(@label);UL;@label:NOP;LK(DATA);NOP;UL;NOP;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(codes.Compile(
      InstructionDef{.code =
                         "NOP;UL;TEST(@label);NOP;@label:LK(DATA);NOP;UL;NOP;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code =
                         "NOP;UL;TEST(@label);NOP;LK(DATA);@label:NOP;UL;NOP;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(codes.Compile(
      InstructionDef{.code =
                         "NOP;UL;TEST(@label);NOP;LK(DATA);NOP;UL;@label:NOP;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(
      codes.Compile(InstructionDef{.code = "UL;TEST(@missing);"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code = "UL;TEST(invalid);@invalid:NOP;"}, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code = "UL;TEST(@label);PLK(C0);@label:NOP;PUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code = "UL;PLK(C0);NOP;TEST(@label);PUL;@label:NOP;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(
      InstructionDef{
          .code =
              "UL;PLK(C0);NOP;TEST(@label);PUL;NOP;PLK(C0);@label:NOP;PUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(codes.Compile(
      InstructionDef{.code = "UL;PLK(C0);NOP;TEST(@label);NOP;@label:NOP;PUL;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(codes.Compile(
      InstructionDef{
          .code =
              "UL;LK(DATA);NOP;TEST(@label);UL;NOP;PLK(C0);@label:NOP;PUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code = "UL;TEST(@label);CLK(C0);@label:NOP;CUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(
      InstructionDef{.code = "UL;CLK(C0);NOP;TEST(@label);CUL;@label:NOP;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_FALSE(codes.Compile(
      InstructionDef{
          .code =
              "UL;CLK(C0);NOP;TEST(@label);CUL;NOP;CLK(C0);@label:NOP;CUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
  EXPECT_TRUE(codes.Compile(
      InstructionDef{.code = "UL;CLK(C0);NOP;TEST(@label);NOP;@label:NOP;CUL;"},
      &error));
  EXPECT_THAT(error, IsEmpty());
  EXPECT_FALSE(codes.Compile(
      InstructionDef{
          .code =
              "UL;LK(DATA);NOP;TEST(@label);UL;NOP;CLK(C0);@label:NOP;CUL;"},
      &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, InstructionNotFound) {
  InstructionMicrocodes codes;
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

TEST(MicrocodeTest, BankDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kBank},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST, {}, "UL;OP(CODE);OP(STACK);OP(DATA);OP(EXTRA)"},
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
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

TEST(MicrocodeTest, StatusDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kStatus},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST,
       {},
       "UL;OP(_);"
       "OP(Z);OP(S);OP(C);OP(O);"
       "OP(Z___);OP(_S__);OP(__C_);OP(___O);"
       "OP(ZS);OP(ZC);OP(ZO);OP(SC);OP(SO);OP(CO);"
       "OP(ZSC);OP(ZSO);OP(ZCO);OP(SCO);"
       "OP(ZSCO);OP(CCC_SSS_OZ_OZ_OZ);OP(I);"},
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
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

TEST(MicrocodeTest, PortModeDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kPortMode},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST,
       {},
       "UL;OP(_);"
       "OP(T);OP(S);OP(A);"
       "OP(TS);OP(TA);OP(SA);"
       "OP(TSA);OP(STA);OP(T_A);"},
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
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

TEST(MicrocodeTest, ConditionDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kCondition, MicroArgType::kCondition},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST, {}, "UL;OP(Z,NZ);OP(S,NS);OP(C,NC);OP(O,NO);"},
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
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

TEST(MicrocodeTest, AddressDecodedCorrecty) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST_NOP, "NOP"},
      {kMicro_TEST, "OP", MicroArgType::kAddress, MicroArgType::kAddress},
  };
  const InstructionDef instruction_defs[] = {
      {
          kOp_TEST,
          {},
          "UL;"                    // 0
          "@start:OP(0,1);"        // 1
          "OP(-1,@end);"           // 2
          "OP(@start,@mid);"       // 3
          "@mid:NOP;"              // 4
          "NOP;"                   // 5
          "@end:OP(@start,@end);"  // 6
      },
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
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

TEST(MicrocodeTest, ImmArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kValue, MicroArgType::kValue},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST, {}, "UL;OP(0,1);OP(127,-42);OP(-128,97)"},
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
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

TEST(MicrocodeTest, WordRegArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST,
       {},
       "UL;"
       "OP(R0,R1);OP(R2,R3);OP(R4,R5);OP(R6,R7);"
       "OP(C0,C1);OP(C2,PC);OP(SP,DP);OP(ST,BM);"},
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
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

TEST(MicrocodeTest, DwordRegArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST, {}, "UL;OP(D0,D1);OP(D2,D3);OP(SD,SD)"},
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
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

TEST(MicrocodeTest, ImmOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST, {.arg1 = "#3", .arg2 = "#5"}, "UL;OP(C0,C1)"},
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
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

TEST(MicrocodeTest, WordOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordReg, MicroArgType::kWordReg},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST, {.arg1 = "a", .arg2 = "b"}, "UL;OP(a,b);OP(b,a)"},
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
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

TEST(MicrocodeTest, DwordOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kDwordReg, MicroArgType::kDwordReg},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST, {.arg1 = "A", .arg2 = "B"}, "UL;OP(A,B);OP(B,A)"},
  };

  InstructionMicrocodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 8;
    EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST, a | (b << 3)), decoded));
    EXPECT_EQ(decoded.c[0], 0);
    EXPECT_EQ(decoded.c[1], 0);
    EXPECT_EQ(decoded.r[0], CpuCore::D0 + a * 2);
    EXPECT_EQ(decoded.r[1], CpuCore::D0 + b * 2);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(Microcode{.op = kMicro_UL},
                  Microcode{.op = kMicro_TEST, .arg1 = -1, .arg2 = -2},
                  Microcode{.op = kMicro_TEST, .arg1 = -2, .arg2 = -1}));
}

TEST(MicrocodeTest, MemoryLockDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "LK(STACK);UL;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, PortLockDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "PLK(C0);PUL;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, CoreLockDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "CLK(C0);CUL;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, MemoryLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);LK(STACK);UL;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, MemoryLockAfterPriorPortLock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;PLK(C0);LK(STACK);UL;PUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, PortLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);PLK(C0);PUL;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, CoreLockAfterPriorMemoryLock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);CLK(C0);CUL;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, MemoryUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;LK(DATA);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, MemoryUnlockWhenNotLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, MemoryUnlockWhenPortLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;PLK(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, PortUnlockWhenPortLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;PLK(C0);PUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, PortUnlockWhenNotLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;PUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, PortUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;LK(DATA);PUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, CoreUnlockWhenCoreLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;CLK(C0);CUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, CoreUnlockWhenNotLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;CUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, CoreUnlockWhenMemoryLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;LK(DATA);CUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, MaxLocks) {
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
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, code};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, TooManyLocks) {
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
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, code};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, AddressWhenNotLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;ADR(C0);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, AddressWhenPortLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;PLK(C0);ADR(C0);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, AddressDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "ADR(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, AddressWhenMemoryLocked) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);ADR(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, LoadDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, LoadBeforeAddress) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, LoadAfterAddress) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);ADR(C0);LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, StoreDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "ST(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, StoreAfterAddressDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "ADR(C0);ST(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, StoreBeforeAddress) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);ST(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, StoreAfterAddress) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);ADR(C0);ST(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, StorePushDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "STP(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, StorePushAfterAddressDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "ADR(C0);STP(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, StorePushBeforeAddress) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);STP(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, StorePushAfterAddress) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);ADR(C0);STP(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, PortLoadWhenUnlocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;PLD(_,C1);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, PortLoadWhenMemoryLocked) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);PLD(_,C1);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, PortLoadWhenPortLocked) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;PLK(C0);PLD(_,C1);PUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, PortStoreWhenUnlocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;PST(_,C1);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, PortStoreWhenMemoryLocked) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);PST(_,C1);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, PortStoreWhenPortLocked) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;PLK(C0);PST(_,C1);PUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicrocodeTest, NoFetchUnlock) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "MOV(R0,R1);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, NoMemoryUnlock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);MOV(R0,R1);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, NoPortUnlock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;PLK(C0);MOV(R0,R1);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, NoCoreUnlock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;CLK(C0);MOV(R0,R1);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, WaitDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "WAIT(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, WaitWithinMemoryLock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);WAIT(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, WaitWithinPortLock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;PLK(C0);WAIT(C0);PUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, WaitWithinCoreLock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;CLK(C0);WAIT(C0);CUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, HaltDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "HALT;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, HaltWithinMemoryLock) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;LK(DATA);HALT;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, HaltWithinPortLock) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;PLK(C0);HALT;PUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, HaltWithinCoreLock) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;CLK(C0);HALT;CUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, IrtDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "IRT;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, IrtWithinMemoryLock) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;LK(DATA);IRT;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, IrtWithinPortLock) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;PLK(C0);IRT;PUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, IrtWithinCoreLock) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;CLK(C0);IRT;CUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, EndDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "END;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, EndWithinMemoryLock) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;LK(DATA);END;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, EndWithinPortLock) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;PLK(C0);END;PUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, EndWithinCoreLock) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;CLK(C0);END;CUL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, DefaultCodeSizeIsOne) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

TEST(MicrocodeTest, LoadDuringFetchIncreasesCodeSize) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 2);
}

TEST(MicrocodeTest, TwoLoadsDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "LD(C0);LD(C1);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 3);
}

TEST(MicrocodeTest, LoadAfterFetchDoesNotIncreaseCodeSize) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);ADR(C0);LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

TEST(MicrocodeTest, LoadAfterAddressDuringFetchDoesNotIncreaseCodeSize) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "ADR(C0);LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

TEST(MicrocodeTest, LabelMissingColon) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;@start;MOV(C0,C1);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicrocodeTest, InvalidLabelCharacter) {
  // Just test the printable ASCII characters.
  for (char ch = 33; absl::ascii_isgraph(ch); ++ch) {
    std::string code = absl::StrCat("UL;@", std::string(1, ch), ":MOV(C0,C1);");
    InstructionDef instruction_def = {kOp_TEST, {"TEST"}, code};
    InstructionMicrocodes codes;
    std::string error;
    if (!absl::ascii_isalnum(ch) && ch != '_') {
      EXPECT_FALSE(codes.Compile(instruction_def, &error));
      EXPECT_THAT(error, Not(IsEmpty()));
    } else {
      EXPECT_TRUE(codes.Compile(instruction_def, &error));
      EXPECT_THAT(error, IsEmpty());
    }
  }
}

TEST(MicrocodeTest, DuplicateLabel) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;@start:MOV(C0,C1);@start:MOV(C1,C0);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

}  // namespace
}  // namespace oz3
