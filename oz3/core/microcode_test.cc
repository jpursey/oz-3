// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/microcode.h"

#include <ostream>
#include <string>

#include "absl/strings/str_cat.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Not;
using ::testing::TestWithParam;
using ::testing::ValuesIn;

constexpr uint8_t kOp_TEST = 200;
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
    case MicroArgType::kZsco:
      *os << "z";
      break;
    case MicroArgType::kValue:
      *os << "v";
      break;
    case MicroArgType::kWordRegister:
      *os << "r";
      break;
    case MicroArgType::kDwordRegister:
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

constexpr MicrocodeDef kMicroNoArgs = {kMicro_TEST, "TEST", MicroArgType::kNone,
                                       MicroArgType::kNone};
constexpr MicrocodeDef kMicroBankArg1 = {
    kMicro_TEST, "TEST", MicroArgType::kBank, MicroArgType::kNone};
constexpr MicrocodeDef kMicroImmArg1 = {
    kMicro_TEST, "TEST", MicroArgType::kValue, MicroArgType::kNone};
constexpr MicrocodeDef kMicroWordArg1 = {
    kMicro_TEST, "TEST", MicroArgType::kWordRegister, MicroArgType::kNone};
constexpr MicrocodeDef kMicroDwordArg1 = {
    kMicro_TEST, "TEST", MicroArgType::kDwordRegister, MicroArgType::kNone};
constexpr MicrocodeDef kMicroImmArg2 = {
    kMicro_TEST, "TEST", MicroArgType::kValue, MicroArgType::kValue};
constexpr MicrocodeDef kMicroWordArg2 = {
    kMicro_TEST, "TEST", MicroArgType::kValue, MicroArgType::kWordRegister};
constexpr MicrocodeDef kMicroDwordArg2 = {
    kMicro_TEST, "TEST", MicroArgType::kValue, MicroArgType::kDwordRegister};
constexpr MicrocodeDef kMicroZscoArgs = {
    kMicro_TEST, "TEST", MicroArgType::kZsco, MicroArgType::kZsco};

using CompileTest = TestWithParam<CompileTestCase>;

TEST_P(CompileTest, Test) {
  const auto& test_case = GetParam();

  MicrocodeDef micros[] = {
      {kMicro_UL, "UL", MicroArgType::kNone, MicroArgType::kNone},
      test_case.micro};
  InstructionMicrocodes codes(micros);
  std::string new_code = absl::StrCat("UL;", test_case.instruction.code);
  InstructionDef instruction = test_case.instruction;
  instruction.code = new_code;
  instruction.decl.op_name = "TEST";
  std::string error;
  EXPECT_EQ(codes.Compile(instruction, &error), test_case.valid);
  if (test_case.valid) {
    EXPECT_THAT(error, IsEmpty());
  } else {
    EXPECT_THAT(error, Not(IsEmpty()));
  }
}

constexpr CompileTestCase kBankCompileTestCases[] = {
    {kMicroBankArg1, InstructionDef{.decl = {}, .code = "TEST"}, false},
    {kMicroBankArg1, InstructionDef{.code = "TEST(X)"}, false},
    {kMicroBankArg1, InstructionDef{.code = "TEST(1)"}, false},
    {kMicroBankArg1, InstructionDef{.code = "TEST(CODEX)"}, false},
    {kMicroBankArg1, InstructionDef{.code = "TEST(CODE)"}, true},
    {kMicroBankArg1, InstructionDef{.code = "TEST(STACK)"}, true},
    {kMicroBankArg1, InstructionDef{.code = "TEST(DATA)"}, true},
    {kMicroBankArg1, InstructionDef{.code = "TEST(EXTRA)"}, true},
    {kMicroNoArgs, InstructionDef{.code = "TEST(CODE)"}, false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeBankTest, CompileTest,
                         ValuesIn(kBankCompileTestCases));

constexpr CompileTestCase kImmArg1CompileTestCases[] = {
    {kMicroImmArg1, InstructionDef{.code = "TEST"}, false},
    {kMicroImmArg1, InstructionDef{.decl = {.arg1 = "a"}, .code = "TEST(a)"},
     false},
    {kMicroImmArg1, InstructionDef{.decl = {.arg1 = "A"}, .code = "TEST(A)"},
     false},
    {kMicroImmArg1, InstructionDef{.code = "TEST(-129)"}, false},
    {kMicroImmArg1, InstructionDef{.code = "TEST(-128)"}, true},
    {kMicroImmArg1, InstructionDef{.code = "TEST(0)"}, true},
    {kMicroImmArg1, InstructionDef{.code = "TEST(127)"}, true},
    {kMicroImmArg1, InstructionDef{.code = "TEST(128)"}, false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeImmArg1Test, CompileTest,
                         ValuesIn(kImmArg1CompileTestCases));

constexpr CompileTestCase kWordArg1CompileTestCases[] = {
    {kMicroWordArg1, InstructionDef{.code = "TEST"}, false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(a)"}, false},
    {kMicroWordArg1, InstructionDef{.decl = {.arg2 = "a"}, .code = "TEST(a)"},
     false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(b)"}, false},
    {kMicroWordArg1, InstructionDef{.decl = {.arg1 = "b"}, .code = "TEST(b)"},
     false},
    {kMicroWordArg1, InstructionDef{.decl = {.arg1 = "A"}, .code = "TEST(A)"},
     false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(0)"}, false},
    {kMicroWordArg1, InstructionDef{.decl = {.arg1 = "a"}, .code = "TEST(a)"},
     true},
    {kMicroWordArg1,
     InstructionDef{.decl = {.arg1 = "v", .arg2 = "b"}, .code = "TEST(b)"},
     true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(R0)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(R1)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(R2)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(R3)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(R4)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(R5)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(R6)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(R7)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(R8)"}, false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(D0)"}, false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(D1)"}, false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(D2)"}, false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(D3)"}, false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(D4)"}, false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(C0)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(C1)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(C2)"}, false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(CD)"}, false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(SP)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(DP)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(SD)"}, false},
    {kMicroWordArg1, InstructionDef{.code = "TEST(PC)"}, true},
    {kMicroWordArg1, InstructionDef{.code = "TEST(ST)"}, true},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeWordArg1Test, CompileTest,
                         ValuesIn(kWordArg1CompileTestCases));

constexpr CompileTestCase kDwordArg1CompileTestCases[] = {
    {kMicroDwordArg1, InstructionDef{.code = "TEST"}, false},
    {kMicroDwordArg1, InstructionDef{.decl = {.arg1 = "a"}, .code = "TEST(a)"},
     false},
    {kMicroDwordArg1,
     InstructionDef{.decl = {.arg1 = "v", .arg2 = "b"}, .code = "TEST(b)"},
     false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(A)"}, false},
    {kMicroDwordArg1, InstructionDef{.decl = {.arg1 = "A"}, .code = "TEST(A)"},
     true},
    {kMicroDwordArg1, InstructionDef{.decl = {.arg2 = "A"}, .code = "TEST(A)"},
     false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(B)"}, false},
    {kMicroDwordArg1, InstructionDef{.decl = {.arg1 = "B"}, .code = "TEST(B)"},
     false},
    {kMicroDwordArg1,
     InstructionDef{.decl = {.arg1 = "v", .arg2 = "B"}, .code = "TEST(B)"},
     true},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(0)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(R0)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(R1)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(R2)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(R3)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(R4)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(R5)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(R6)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(R7)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(R8)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(D0)"}, true},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(D1)"}, true},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(D2)"}, true},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(D3)"}, true},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(D4)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(C0)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(C1)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(C2)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(CD)"}, true},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(SP)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(DP)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(SD)"}, true},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(PC)"}, false},
    {kMicroDwordArg1, InstructionDef{.code = "TEST(ST)"}, false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeDwordArg1Test, CompileTest,
                         ValuesIn(kDwordArg1CompileTestCases));

constexpr CompileTestCase kImmArg2CompileTestCases[] = {
    {kMicroImmArg2, InstructionDef{.code = "TEST"}, false},
    {kMicroImmArg2, InstructionDef{.code = "TEST(1)"}, false},
    {kMicroImmArg2, InstructionDef{.code = "TEST(1,C1)"}, false},
    {kMicroImmArg2, InstructionDef{.code = "TEST(1,-129)"}, false},
    {kMicroImmArg2, InstructionDef{.code = "TEST(1,-128)"}, true},
    {kMicroImmArg2, InstructionDef{.code = "TEST(1,0)"}, true},
    {kMicroImmArg2, InstructionDef{.code = "TEST(1,127)"}, true},
    {kMicroImmArg2, InstructionDef{.code = "TEST(1,128)"}, false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeImmArg2Test, CompileTest,
                         ValuesIn(kImmArg2CompileTestCases));

constexpr CompileTestCase kWordArg2CompileTestCases[] = {
    {kMicroWordArg2, InstructionDef{.code = "TEST"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,a)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,b)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,A)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,0)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,R0)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,R1)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,R2)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,R3)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,R4)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,R5)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,R6)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,R7)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,R8)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,D0)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,D1)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,D2)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,D3)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,D4)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,C0)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,C1)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,C2)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,CD)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,SP)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,DP)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,SD)"}, false},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,PC)"}, true},
    {kMicroWordArg2, InstructionDef{.code = "TEST(1,ST)"}, true},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeWordArg2Test, CompileTest,
                         ValuesIn(kWordArg2CompileTestCases));

constexpr CompileTestCase kDwordArg2CompileTestCases[] = {
    {kMicroDwordArg2, InstructionDef{.code = "TEST"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,a)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,b)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,A)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,0)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,R0)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,R1)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,R2)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,R3)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,R4)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,R5)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,R6)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,R7)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,R8)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,D0)"}, true},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,D1)"}, true},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,D2)"}, true},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,D3)"}, true},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,D4)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,C0)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,C1)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,C2)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,CD)"}, true},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,SP)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,DP)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,SD)"}, true},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,PC)"}, false},
    {kMicroDwordArg2, InstructionDef{.code = "TEST(1,ST)"}, false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeDwordArg2Test, CompileTest,
                         ValuesIn(kDwordArg2CompileTestCases));

constexpr CompileTestCase kZscoCompileTestCases[] = {
    {kMicroZscoArgs, InstructionDef{.code = "TEST(Z,S)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(C,O))"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(ZS,CO)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(ZC,SO)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(ZSC,SCO)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(ZCO,ZSC)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(ZZZ,SSS)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(CCC,OOO)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(_,_)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(S,_)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(_,C)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(Z___,_S__)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(__C_,___O)"}, true},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(X,Z)"}, false},
    {kMicroZscoArgs, InstructionDef{.code = "TEST(Z,X)"}, false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeZscoTest, CompileTest,
                         ValuesIn(kZscoCompileTestCases));

constexpr CompileTestCase kMiscCompileTestCases[] = {
    {kMicroNoArgs, InstructionDef{.code = "INVALID"}, false},
    {kMicroNoArgs, InstructionDef{.code = "TEST(1)"}, false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeMiscTest, CompileTest,
                         ValuesIn(kMiscCompileTestCases));

TEST(MicroCodeTest, DecodeInstructionNotFound) {
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

TEST(MicroCodeTest, BankDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kBank, MicroArgType::kNone},
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

TEST(MicroCodeTest, ImmArgsDecodedCorrectly) {
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

TEST(MicroCodeTest, WordRegArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordRegister,
       MicroArgType::kWordRegister},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST,
       {},
       "UL;"
       "OP(R0,R1);OP(R2,R3);OP(R4,R5);OP(R6,R7);"
       "OP(C0,C1);OP(SP,DP);OP(PC,ST)"},
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
              .op = kMicro_TEST, .arg1 = CpuCore::SP, .arg2 = CpuCore::DP},
          Microcode{
              .op = kMicro_TEST, .arg1 = CpuCore::PC, .arg2 = CpuCore::ST}));
}

TEST(MicroCodeTest, DwordRegArgsDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kDwordRegister,
       MicroArgType::kDwordRegister},
  };
  const InstructionDef instruction_defs[] = {
      {kOp_TEST, {}, "UL;OP(D0,D1);OP(D2,D3);OP(CD,SD)"},
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
              .op = kMicro_TEST, .arg1 = CpuCore::CD, .arg2 = CpuCore::SD}));
}

TEST(MicroCodeTest, ImmOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordRegister,
       MicroArgType::kWordRegister},
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

TEST(MicroCodeTest, WordOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kWordRegister,
       MicroArgType::kWordRegister},
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

TEST(MicroCodeTest, DwordOpArgDecodedCorrectly) {
  const MicrocodeDef micro_defs[] = {
      {kMicro_UL, "UL"},
      {kMicro_TEST, "OP", MicroArgType::kDwordRegister,
       MicroArgType::kDwordRegister},
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

TEST(MicroCodeTest, LockDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "LK(STACK);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicroCodeTest, LockAfterPriorLock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);LK(STACK);UL;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicroCodeTest, UnlockWhenNotLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicroCodeTest, AddressWhenNotLocked) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;ADR(C0);"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicroCodeTest, AddressDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "ADR(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicroCodeTest, AddressAfterLock) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);ADR(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicroCodeTest, LoadDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicroCodeTest, LoadBeforeAddress) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicroCodeTest, LoadAfterAddress) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);ADR(C0);LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicroCodeTest, StoreDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "ST(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicroCodeTest, StoreAfterAddressDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "ADR(C0);ST(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicroCodeTest, StoreBeforeAddress) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);ST(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_FALSE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, Not(IsEmpty()));
}

TEST(MicroCodeTest, StoreAfterAddress) {
  InstructionDef instruction_def = {
      kOp_TEST, {"TEST"}, "UL;LK(DATA);ADR(C0);ST(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
}

TEST(MicroCodeTest, DefaultCodeSizeIsOne) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

TEST(MicroCodeTest, LoadDuringFetchIncreasesCodeSize) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 2);
}

TEST(MicroCodeTest, TwoLoadsDuringFetch) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "LD(C0);LD(C1);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 3);
}

TEST(MicroCodeTest, LoadAfterFetchDoesNotIncreaseCodeSize) {
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

TEST(MicroCodeTest, LoadAfterAddressDuringFetchDoesNotIncreaseCodeSize) {
  InstructionDef instruction_def = {kOp_TEST, {"TEST"}, "ADR(C0);LD(C0);UL;"};
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(instruction_def, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(kOp_TEST), decoded));
  EXPECT_EQ(decoded.size, 1);
}

}  // namespace
}  // namespace oz3
