// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/micro_code.h"

#include <ostream>
#include <string>

#include "absl/strings/str_cat.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_set.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Not;
using ::testing::TestWithParam;
using ::testing::ValuesIn;

TEST(MicroCodeTest, InstructionSetCompiles) {
  InstructionMicroCodes codes;
  std::string error;
  EXPECT_TRUE(codes.CompileToMicroCode(GetInstructionSet(), &error));
  EXPECT_THAT(error, IsEmpty());
}

struct CompileTestCase {
  absl::Span<const MicroCodeDef> GetMicros() const {
    unsigned count = 0;
    while (count < ABSL_ARRAYSIZE(micros) &&
           micros[count].op != static_cast<MicroOp>(0)) {
      ++count;
    }
    return {micros, count};
  }

  absl::Span<const InstructionDef> GetInstructions() const {
    unsigned count = 0;
    while (count < ABSL_ARRAYSIZE(instructions) &&
           instructions[count].op != static_cast<Op>(0)) {
      ++count;
    }
    return {instructions, count};
  }

  MicroCodeDef micros[2];
  InstructionDef instructions[2];
  bool valid;
};

void PrintArg(ArgType arg_type, std::ostream* os) {
  switch (arg_type) {
    case ArgType::kNone:
      *os << "_";
      break;
    case ArgType::kImmediate:
      *os << "#";
      break;
    case ArgType::kWordRegister:
      *os << "r";
      break;
    case ArgType::kDwordRegister:
      *os << "D";
      break;
    case ArgType::kValue:
      *os << "v";
      break;
  }
}

void PrintDef(const MicroCodeDef& def, std::ostream* os) {
  *os << def.op_name;
  if (def.has_bank) {
    *os << ".B";
  }
  if (def.arg1 != ArgType::kNone) {
    *os << "(";
    PrintArg(def.arg1, os);
    if (def.arg2 != ArgType::kNone) {
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
  *os << "{micros={";
  bool first_def = true;
  for (const auto& def : test_case.GetMicros()) {
    if (!first_def) *os << ", ";
    first_def = false;
    PrintDef(def, os);
  }
  *os << "},instructions={";
  first_def = true;
  for (const auto& def : test_case.GetInstructions()) {
    if (!first_def) *os << ", ";
    first_def = false;
    PrintDef(def, os);
  }
  *os << "},valid=" << (test_case.valid ? "true" : "false") << "}";
}

constexpr MicroCodeDef kMicroRequireBank = {MicroOp::MICRO_OP, "MICRO_OP", true,
                                            ArgType::kNone, ArgType::kNone};
constexpr MicroCodeDef kMicroNoBank = {MicroOp::MICRO_OP, "MICRO_OP", false,
                                       ArgType::kNone, ArgType::kNone};
constexpr MicroCodeDef kMicroImmArg1 = {MicroOp::MICRO_OP, "MICRO_OP", false,
                                        ArgType::kImmediate, ArgType::kNone};
constexpr MicroCodeDef kMicroWordArg1 = {MicroOp::MICRO_OP, "MICRO_OP", false,
                                         ArgType::kWordRegister,
                                         ArgType::kNone};
constexpr MicroCodeDef kMicroDwordArg1 = {MicroOp::MICRO_OP, "MICRO_OP", false,
                                          ArgType::kDwordRegister,
                                          ArgType::kNone};
constexpr MicroCodeDef kMicroImmArg2 = {MicroOp::MICRO_OP, "MICRO_OP", false,
                                        ArgType::kImmediate,
                                        ArgType::kImmediate};
constexpr MicroCodeDef kMicroWordArg2 = {MicroOp::MICRO_OP, "MICRO_OP", false,
                                         ArgType::kImmediate,
                                         ArgType::kWordRegister};
constexpr MicroCodeDef kMicroDwordArg2 = {MicroOp::MICRO_OP, "MICRO_OP", false,
                                          ArgType::kImmediate,
                                          ArgType::kDwordRegister};

using CompileTest = TestWithParam<CompileTestCase>;

TEST_P(CompileTest, Test) {
  const auto& test_case = GetParam();
  InstructionMicroCodes codes(test_case.GetMicros());
  std::string error;
  EXPECT_EQ(codes.CompileToMicroCode(test_case.GetInstructions(), &error),
            test_case.valid);
  if (test_case.valid) {
    EXPECT_THAT(error, IsEmpty());
  } else {
    EXPECT_THAT(error, Not(IsEmpty()));
  }
}

constexpr CompileTestCase kBankCompileTestCases[] = {
    {{kMicroRequireBank},
     {InstructionDef{.op = Op::ASM_OP, .decl = {}, .code = "MICRO_OP"}},
     false},
    {{kMicroRequireBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP.X"}},
     false},
    {{kMicroRequireBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP.CX"}},
     false},
    {{kMicroRequireBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP.C"}},
     true},
    {{kMicroRequireBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP.S"}},
     true},
    {{kMicroRequireBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP.D"}},
     true},
    {{kMicroRequireBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP.E"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP.C"}},
     false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeBankTest, CompileTest,
                         ValuesIn(kBankCompileTestCases));

constexpr CompileTestCase kImmArg1CompileTestCases[] = {
    {{kMicroImmArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP"}},
     false},
    {{kMicroImmArg1},
     {InstructionDef{
         .op = Op::ASM_OP, .decl = {.arg1 = "a"}, .code = "MICRO_OP(a)"}},
     false},
    {{kMicroImmArg1},
     {InstructionDef{
         .op = Op::ASM_OP, .decl = {.arg1 = "A"}, .code = "MICRO_OP(A)"}},
     false},
    {{kMicroImmArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(-129)"}},
     false},
    {{kMicroImmArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(-128)"}},
     true},
    {{kMicroImmArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(0)"}},
     true},
    {{kMicroImmArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(127)"}},
     true},
    {{kMicroImmArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(128)"}},
     false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeImmArg1Test, CompileTest,
                         ValuesIn(kImmArg1CompileTestCases));

constexpr CompileTestCase kWordArg1CompileTestCases[] = {
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(a)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{
         .op = Op::ASM_OP, .decl = {.arg2 = "a"}, .code = "MICRO_OP(a)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(b)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{
         .op = Op::ASM_OP, .decl = {.arg1 = "b"}, .code = "MICRO_OP(b)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{
         .op = Op::ASM_OP, .decl = {.arg1 = "A"}, .code = "MICRO_OP(A)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(0)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{
         .op = Op::ASM_OP, .decl = {.arg1 = "a"}, .code = "MICRO_OP(a)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP,
                     .decl = {.arg1 = "v", .arg2 = "b"},
                     .code = "MICRO_OP(b)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R0)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R1)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R2)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R3)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R4)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R5)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R6)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R7)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R8)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(D0)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(D1)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(D2)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(D3)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(D4)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(C0)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(C1)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(C2)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(CD)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(SP)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(DP)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(SD)"}},
     false},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(PC)"}},
     true},
    {{kMicroWordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(ST)"}},
     true},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeWordArg1Test, CompileTest,
                         ValuesIn(kWordArg1CompileTestCases));

constexpr CompileTestCase kDwordArg1CompileTestCases[] = {
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{
         .op = Op::ASM_OP, .decl = {.arg1 = "a"}, .code = "MICRO_OP(a)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP,
                     .decl = {.arg1 = "v", .arg2 = "b"},
                     .code = "MICRO_OP(b)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(A)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{
         .op = Op::ASM_OP, .decl = {.arg1 = "A"}, .code = "MICRO_OP(A)"}},
     true},
    {{kMicroDwordArg1},
     {InstructionDef{
         .op = Op::ASM_OP, .decl = {.arg2 = "A"}, .code = "MICRO_OP(A)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(B)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{
         .op = Op::ASM_OP, .decl = {.arg1 = "B"}, .code = "MICRO_OP(B)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP,
                     .decl = {.arg1 = "v", .arg2 = "B"},
                     .code = "MICRO_OP(B)"}},
     true},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(0)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R0)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R1)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R2)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R3)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R4)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R5)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R6)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R7)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(R8)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(D0)"}},
     true},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(D1)"}},
     true},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(D2)"}},
     true},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(D3)"}},
     true},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(D4)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(C0)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(C1)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(C2)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(CD)"}},
     true},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(SP)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(DP)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(SD)"}},
     true},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(PC)"}},
     false},
    {{kMicroDwordArg1},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(ST)"}},
     false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeDwordArg1Test, CompileTest,
                         ValuesIn(kDwordArg1CompileTestCases));

constexpr CompileTestCase kImmArg2CompileTestCases[] = {
    {{kMicroImmArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP"}},
     false},
    {{kMicroImmArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1)"}},
     false},
    {{kMicroImmArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,C1)"}},
     false},
    {{kMicroImmArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,-129)"}},
     false},
    {{kMicroImmArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,-128)"}},
     true},
    {{kMicroImmArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,0)"}},
     true},
    {{kMicroImmArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,127)"}},
     true},
    {{kMicroImmArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,128)"}},
     false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeImmArg2Test, CompileTest,
                         ValuesIn(kImmArg2CompileTestCases));

constexpr CompileTestCase kWordArg2CompileTestCases[] = {
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,a)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,b)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,A)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,0)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R0)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R1)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R2)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R3)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R4)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R5)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R6)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R7)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R8)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,D0)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,D1)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,D2)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,D3)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,D4)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,C0)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,C1)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,C2)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,CD)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,SP)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,DP)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,SD)"}},
     false},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,PC)"}},
     true},
    {{kMicroWordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,ST)"}},
     true},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeWordArg2Test, CompileTest,
                         ValuesIn(kWordArg2CompileTestCases));

constexpr CompileTestCase kDwordArg2CompileTestCases[] = {
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,a)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,b)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,A)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,0)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R0)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R1)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R2)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R3)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R4)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R5)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R6)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R7)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,R8)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,D0)"}},
     true},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,D1)"}},
     true},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,D2)"}},
     true},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,D3)"}},
     true},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,D4)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,C0)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,C1)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,C2)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,CD)"}},
     true},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,SP)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,DP)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,SD)"}},
     true},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,PC)"}},
     false},
    {{kMicroDwordArg2},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1,ST)"}},
     false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeDwordArg2Test, CompileTest,
                         ValuesIn(kDwordArg2CompileTestCases));

constexpr CompileTestCase kZscoCompileTestCases[] = {
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:zscoZSCO"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:scoZSCO"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:zcoZSCO"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:zcoZSCO"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:zscZSCO"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:zscoSCO"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:zscoZCO"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:zscoZSO"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:zscoZSC"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:zZ"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:Zz"}},
     false},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:zZz"}},
     false},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:sS"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:Ss"}},
     false},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:sSs"}},
     false},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:cC"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:Cc"}},
     false},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:cCc"}},
     false},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:oO"}},
     true},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:Oo"}},
     false},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP:oOo"}},
     false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeZscoTest, CompileTest,
                         ValuesIn(kZscoCompileTestCases));

constexpr CompileTestCase kMiscCompileTestCases[] = {
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "INVALID"}},
     false},
    {{kMicroNoBank},
     {InstructionDef{.op = Op::ASM_OP, .code = "MICRO_OP(1)"}},
     false},
};
INSTANTIATE_TEST_SUITE_P(MicroCodeMiscTest, CompileTest,
                         ValuesIn(kMiscCompileTestCases));

TEST(MicroCodeTest, DecodeInstructionNotFound) {
  InstructionMicroCodes codes;
  DecodedInstruction decoded;
  EXPECT_FALSE(codes.Decode(0, decoded));
  MicroCode ul_c = {MicroOp::UL};
  DecodedInstruction expected = {};
  expected.code = absl::MakeConstSpan(&ul_c, 1);
  EXPECT_EQ(decoded, expected);
}

uint16_t MakeCode(Op op, uint16_t args = 0) {
  return (static_cast<uint16_t>(op) << 8) | (args & 0xFF);
}

TEST(MicroCodeTest, BankDecodedCorrectly) {
  const MicroCodeDef micro_defs[] = {
      {MicroOp::MICRO_OP, "OP", true, ArgType::kNone, ArgType::kNone},
  };
  const InstructionDef instruction_defs[] = {
      {Op::ASM_OP, {}, "OP.C;OP.S;OP.D;OP.E"},
  };

  InstructionMicroCodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.CompileToMicroCode(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(Op::ASM_OP), decoded));
  EXPECT_EQ(decoded.c0, 0);
  EXPECT_EQ(decoded.c1, 0);
  EXPECT_EQ(decoded.reg1, 0);
  EXPECT_EQ(decoded.reg2, 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(MicroCode{.op = MicroOp::MICRO_OP, .bank = CpuCore::CODE},
                  MicroCode{.op = MicroOp::MICRO_OP, .bank = CpuCore::STACK},
                  MicroCode{.op = MicroOp::MICRO_OP, .bank = CpuCore::DATA},
                  MicroCode{.op = MicroOp::MICRO_OP, .bank = CpuCore::EXTRA}));
}

TEST(MicroCodeTest, ImmArgsDecodedCorrectly) {
  const MicroCodeDef micro_defs[] = {
      {MicroOp::MICRO_OP, "OP", false, ArgType::kImmediate,
       ArgType::kImmediate},
  };
  const InstructionDef instruction_defs[] = {
      {Op::ASM_OP, {}, "OP(0,1);OP(127,-42);OP(-128,97)"},
  };

  InstructionMicroCodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.CompileToMicroCode(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(Op::ASM_OP), decoded));
  EXPECT_EQ(decoded.c0, 0);
  EXPECT_EQ(decoded.c1, 0);
  EXPECT_EQ(decoded.reg1, 0);
  EXPECT_EQ(decoded.reg2, 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(
          MicroCode{.op = MicroOp::MICRO_OP, .arg1 = 0, .arg2 = 1},
          MicroCode{.op = MicroOp::MICRO_OP, .arg1 = 127, .arg2 = -42},
          MicroCode{.op = MicroOp::MICRO_OP, .arg1 = -128, .arg2 = 97}));
}

TEST(MicroCodeTest, WordRegArgsDecodedCorrectly) {
  const MicroCodeDef micro_defs[] = {
      {MicroOp::MICRO_OP, "OP", false, ArgType::kWordRegister,
       ArgType::kWordRegister},
  };
  const InstructionDef instruction_defs[] = {
      {Op::ASM_OP,
       {},
       "OP(R0,R1);OP(R2,R3);OP(R4,R5);OP(R6,R7);OP(C0,C1);OP(SP,DP);OP(PC,ST)"},
  };

  InstructionMicroCodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.CompileToMicroCode(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(Op::ASM_OP), decoded));
  EXPECT_EQ(decoded.c0, 0);
  EXPECT_EQ(decoded.c1, 0);
  EXPECT_EQ(decoded.reg1, 0);
  EXPECT_EQ(decoded.reg2, 0);
  EXPECT_THAT(decoded.code, ElementsAre(MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::R0,
                                                  .arg2 = CpuCore::R1},
                                        MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::R2,
                                                  .arg2 = CpuCore::R3},
                                        MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::R4,
                                                  .arg2 = CpuCore::R5},
                                        MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::R6,
                                                  .arg2 = CpuCore::R7},
                                        MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::C0,
                                                  .arg2 = CpuCore::C1},
                                        MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::SP,
                                                  .arg2 = CpuCore::DP},
                                        MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::PC,
                                                  .arg2 = CpuCore::ST}));
}

TEST(MicroCodeTest, DwordRegArgsDecodedCorrectly) {
  const MicroCodeDef micro_defs[] = {
      {MicroOp::MICRO_OP, "OP", false, ArgType::kDwordRegister,
       ArgType::kDwordRegister},
  };
  const InstructionDef instruction_defs[] = {
      {Op::ASM_OP, {}, "OP(D0,D1);OP(D2,D3);OP(CD,SD)"},
  };

  InstructionMicroCodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.CompileToMicroCode(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(Op::ASM_OP), decoded));
  EXPECT_EQ(decoded.c0, 0);
  EXPECT_EQ(decoded.c1, 0);
  EXPECT_EQ(decoded.reg1, 0);
  EXPECT_EQ(decoded.reg2, 0);
  EXPECT_THAT(decoded.code, ElementsAre(MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::D0,
                                                  .arg2 = CpuCore::D1},
                                        MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::D2,
                                                  .arg2 = CpuCore::D3},
                                        MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::CD,
                                                  .arg2 = CpuCore::SD}));
}

TEST(MicroCodeTest, ZscoDecodedCorrectly) {
  const MicroCodeDef micro_defs[] = {
      {MicroOp::MICRO_OP, "OP", false, ArgType::kNone, ArgType::kNone},
  };
  const InstructionDef instruction_defs[] = {
      {Op::ASM_OP,
       {},
       "OP:z;OP:s;OP:c;OP:o;OP:Z;OP:S;OP:C;OP:O;OP:zcSO;OP:soZC"},
  };

  InstructionMicroCodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.CompileToMicroCode(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  EXPECT_TRUE(codes.Decode(MakeCode(Op::ASM_OP), decoded));
  EXPECT_EQ(decoded.c0, 0);
  EXPECT_EQ(decoded.c1, 0);
  EXPECT_EQ(decoded.reg1, 0);
  EXPECT_EQ(decoded.reg2, 0);
  EXPECT_THAT(
      decoded.code,
      ElementsAre(MicroCode{.op = MicroOp::MICRO_OP, .st_clear = CpuCore::Z},
                  MicroCode{.op = MicroOp::MICRO_OP, .st_clear = CpuCore::S},
                  MicroCode{.op = MicroOp::MICRO_OP, .st_clear = CpuCore::C},
                  MicroCode{.op = MicroOp::MICRO_OP, .st_clear = CpuCore::O},
                  MicroCode{.op = MicroOp::MICRO_OP, .st_set = CpuCore::Z},
                  MicroCode{.op = MicroOp::MICRO_OP, .st_set = CpuCore::S},
                  MicroCode{.op = MicroOp::MICRO_OP, .st_set = CpuCore::C},
                  MicroCode{.op = MicroOp::MICRO_OP, .st_set = CpuCore::O},
                  MicroCode{.op = MicroOp::MICRO_OP,
                            .st_clear = CpuCore::Z | CpuCore::C,
                            .st_set = CpuCore::S | CpuCore::O},
                  MicroCode{.op = MicroOp::MICRO_OP,
                            .st_clear = CpuCore::S | CpuCore::O,
                            .st_set = CpuCore::Z | CpuCore::C}));
}

TEST(MicroCodeTest, ImmOpArgDecodedCorrectly) {
  const MicroCodeDef micro_defs[] = {
      {MicroOp::MICRO_OP, "OP", false, ArgType::kWordRegister,
       ArgType::kWordRegister},
  };
  const InstructionDef instruction_defs[] = {
      {Op::ASM_OP, {.arg1 = "#3", .arg2 = "#5"}, "OP(C0,C1)"},
  };

  InstructionMicroCodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.CompileToMicroCode(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    uint16_t c0 = (i * i + 1) % 8;   // 3 bits
    uint16_t c1 = (i * i + 2) % 32;  // 5 bits
    EXPECT_TRUE(codes.Decode(MakeCode(Op::ASM_OP, c0 | (c1 << 3)), decoded));
    EXPECT_EQ(decoded.c0, c0);
    EXPECT_EQ(decoded.c1, c1);
    EXPECT_EQ(decoded.reg1, 0);
    EXPECT_EQ(decoded.reg2, 0);
  }
  EXPECT_THAT(decoded.code, ElementsAre(MicroCode{.op = MicroOp::MICRO_OP,
                                                  .arg1 = CpuCore::C0,
                                                  .arg2 = CpuCore::C1}));
}

TEST(MicroCodeTest, WordOpArgDecodedCorrectly) {
  const MicroCodeDef micro_defs[] = {
      {MicroOp::MICRO_OP, "OP", false, ArgType::kWordRegister,
       ArgType::kWordRegister},
  };
  const InstructionDef instruction_defs[] = {
      {Op::ASM_OP, {.arg1 = "a", .arg2 = "b"}, "OP(a,b);OP(b,a)"},
  };

  InstructionMicroCodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.CompileToMicroCode(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 8; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 8;
    EXPECT_TRUE(codes.Decode(MakeCode(Op::ASM_OP, a | (b << 3)), decoded));
    EXPECT_EQ(decoded.c0, 0);
    EXPECT_EQ(decoded.c1, 0);
    EXPECT_EQ(decoded.reg1, CpuCore::R0 + a);
    EXPECT_EQ(decoded.reg2, CpuCore::R0 + b);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(MicroCode{.op = MicroOp::MICRO_OP, .arg1 = -1, .arg2 = -2},
                  MicroCode{.op = MicroOp::MICRO_OP, .arg1 = -2, .arg2 = -1}));
}

TEST(MicroCodeTest, DwordOpArgDecodedCorrectly) {
  const MicroCodeDef micro_defs[] = {
      {MicroOp::MICRO_OP, "OP", false, ArgType::kDwordRegister,
       ArgType::kDwordRegister},
  };
  const InstructionDef instruction_defs[] = {
      {Op::ASM_OP, {.arg1 = "A", .arg2 = "B"}, "OP(A,B);OP(B,A)"},
  };

  InstructionMicroCodes codes(micro_defs);
  std::string error;
  EXPECT_TRUE(codes.CompileToMicroCode(instruction_defs, &error));
  EXPECT_THAT(error, IsEmpty());
  DecodedInstruction decoded;
  for (uint16_t i = 0; i < 4; ++i) {
    uint16_t a = i;
    uint16_t b = (i + 1) % 8;
    EXPECT_TRUE(codes.Decode(MakeCode(Op::ASM_OP, a | (b << 3)), decoded));
    EXPECT_EQ(decoded.c0, 0);
    EXPECT_EQ(decoded.c1, 0);
    EXPECT_EQ(decoded.reg1, CpuCore::D0 + a * 2);
    EXPECT_EQ(decoded.reg2, CpuCore::D0 + b * 2);
  }
  EXPECT_THAT(
      decoded.code,
      ElementsAre(MicroCode{.op = MicroOp::MICRO_OP, .arg1 = -1, .arg2 = -2},
                  MicroCode{.op = MicroOp::MICRO_OP, .arg1 = -2, .arg2 = -1}));
}

}  // namespace
}  // namespace oz3
