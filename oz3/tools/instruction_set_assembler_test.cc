// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_set_assembler.h"

#include "absl/strings/ascii.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

using ::testing::AllOf;
using ::testing::HasSubstr;

TEST(InstructionSetAssemblerTest, BasicSuccess) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:0) NOP { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_NE(asm_set->GetInstructionSet(), nullptr);
  EXPECT_EQ(asm_set->GetInstructionSet()->GetInstructionCount(), 1);
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 1);
  const auto& instruction = asm_set->GetInstructionSetDef().instructions[0];
  EXPECT_EQ(instruction.op, 0);
  EXPECT_EQ(instruction.op_name, "NOP");
  EXPECT_EQ(instruction.arg_source, "");
  EXPECT_EQ(instruction.arg1.type, ArgType::kNone);
  EXPECT_EQ(instruction.arg2.type, ArgType::kNone);
  EXPECT_EQ(instruction.code, "UL;");
}

TEST(InstructionSetAssemblerTest, BasicSuccessFromFile) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:0) NOP { 
      UL;
    } 
  )---";
  gb::FileSystem file_system;
  file_system.Register(std::make_unique<gb::MemoryFileProtocol>());
  ASSERT_TRUE(file_system.WriteFile("mem:/test.oz3ism", source));
  auto asm_set =
      AssembleInstructionSetFile(file_system, "mem:/test.oz3ism", &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_NE(asm_set->GetInstructionSet(), nullptr);
  EXPECT_EQ(asm_set->GetInstructionSet()->GetInstructionCount(), 1);
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 1);
  const auto& instruction = asm_set->GetInstructionSetDef().instructions[0];
  EXPECT_EQ(instruction.op, 0);
  EXPECT_EQ(instruction.op_name, "NOP");
  EXPECT_EQ(instruction.arg_source, "");
  EXPECT_EQ(instruction.arg1.type, ArgType::kNone);
  EXPECT_EQ(instruction.arg2.type, ArgType::kNone);
  EXPECT_EQ(instruction.code, "UL;");
}

TEST(InstructionSetAssemblerTest, FailToOpenFile) {
  gb::ParseError error;
  gb::FileSystem file_system;
  auto asm_set =
      AssembleInstructionSetFile(file_system, "mem:/test.oz3ism", &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              HasSubstr("test.oz3ism"));
}

TEST(InstructionSetAssemblerTest, AutoAssignOpcodes) {
  gb::ParseError error;
  std::string source = R"---(
    instruction ZERO { 
      UL;
    } 
    instruction TWO { 
      UL;
    } 
    instruction(opcode:1) ONE { 
      UL;
    }
    instruction THREE {
      UL;
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_NE(asm_set->GetInstructionSet(), nullptr);
  EXPECT_EQ(asm_set->GetInstructionSet()->GetInstructionCount(), 4);
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 4);
  const auto& instruction0 = asm_set->GetInstructionSetDef().instructions[0];
  EXPECT_EQ(instruction0.op_name, "ZERO");
  EXPECT_EQ(instruction0.op, 0);
  const auto& instruction1 = asm_set->GetInstructionSetDef().instructions[1];
  EXPECT_EQ(instruction1.op_name, "TWO");
  EXPECT_EQ(instruction1.op, 2);
  const auto& instruction2 = asm_set->GetInstructionSetDef().instructions[2];
  EXPECT_EQ(instruction2.op_name, "ONE");
  EXPECT_EQ(instruction2.op, 1);
  const auto& instruction3 = asm_set->GetInstructionSetDef().instructions[3];
  EXPECT_EQ(instruction3.op_name, "THREE");
  EXPECT_EQ(instruction3.op, 3);
}

TEST(InstructionSetAssemblerTest, MaxInstructions) {
  gb::ParseError error;
  std::string source;
  for (int i = 0; i < 256; ++i) {
    absl::StrAppend(&source, "instruction NOP", i, " { UL; }\n");
  }
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr);
  EXPECT_EQ(asm_set->GetInstructionSet()->GetInstructionCount(), 256);
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 256);
  for (int i = 0; i < 256; ++i) {
    const auto& instruction = asm_set->GetInstructionSetDef().instructions[i];
    EXPECT_EQ(instruction.op, i);
    EXPECT_EQ(instruction.op_name, absl::StrCat("NOP", i));
  }
}

TEST(InstructionSetAssemblerTest, TooManyInstructions) {
  gb::ParseError error;
  std::string source;
  for (int i = 0; i < 257; ++i) {
    absl::StrAppend(&source, "instruction NOP", i, " { UL; }\n");
  }
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              HasSubstr("instructions"));
}

TEST(InstructionSetAssemblerTest, DuplicateOpcode) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:42) ALPHA { 
      UL;
    } 
    instruction(opcode:42) BETA { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("already defined"), HasSubstr("42")));
}

TEST(InstructionSetAssemblerTest, OpcodeNegative) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:-1) ALPHA { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("opcode"), HasSubstr("-1")));
}

TEST(InstructionSetAssemblerTest, OpcodeTooBig) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:256) ALPHA { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("opcode"), HasSubstr("256")));
}

TEST(InstructionSetAssemblerTest, InstructionValidArgSource) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:0) ALPHA "$v" { 
      LD(R0);
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_NE(asm_set->GetInstructionSet(), nullptr);
  EXPECT_EQ(asm_set->GetInstructionSet()->GetInstructionCount(), 1);
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 1);
  const auto& instruction = asm_set->GetInstructionSetDef().instructions[0];
  EXPECT_EQ(instruction.op, 0);
  EXPECT_EQ(instruction.op_name, "ALPHA");
  EXPECT_EQ(instruction.arg_source, "$v");
  EXPECT_EQ(instruction.arg1.type, ArgType::kNone);
  EXPECT_EQ(instruction.arg2.type, ArgType::kNone);
  EXPECT_EQ(instruction.code, "LD(R0);UL;");
}

TEST(InstructionSetAssemblerTest, InstructionInvalidArgSource) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:0) ALPHA "$x" { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("invalid"), HasSubstr("$x")));
}

TEST(InstructionSetAssemblerTest, InstructionEmbeddedArgumentTypes) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:0) WORD "$r, $r4" { 
      UL;
    } 
    instruction(opcode:1) DWORD "$R, $R1" { 
      UL;
    } 
    instruction(opcode:2) IMM "$#3, $#5" { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_NE(asm_set->GetInstructionSet(), nullptr);
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 3);
  const auto& instruction0 = asm_set->GetInstructionSetDef().instructions[0];
  EXPECT_EQ(instruction0.op_name, "WORD");
  EXPECT_EQ(instruction0.arg1.type, ArgType::kWordReg);
  EXPECT_EQ(instruction0.arg1.size, GetDefaultArgTypeSize(ArgType::kWordReg));
  EXPECT_EQ(instruction0.arg2.type, ArgType::kWordReg);
  EXPECT_EQ(instruction0.arg2.size, 4);
  const auto& instruction1 = asm_set->GetInstructionSetDef().instructions[1];
  EXPECT_EQ(instruction1.op_name, "DWORD");
  EXPECT_EQ(instruction1.arg1.type, ArgType::kDwordReg);
  EXPECT_EQ(instruction1.arg1.size, GetDefaultArgTypeSize(ArgType::kDwordReg));
  EXPECT_EQ(instruction1.arg2.type, ArgType::kDwordReg);
  EXPECT_EQ(instruction1.arg2.size, 1);
  const auto& instruction2 = asm_set->GetInstructionSetDef().instructions[2];
  EXPECT_EQ(instruction2.op_name, "IMM");
  EXPECT_EQ(instruction2.arg1.type, ArgType::kImmediate);
  EXPECT_EQ(instruction2.arg1.size, 3);
  EXPECT_EQ(instruction2.arg2.type, ArgType::kImmediate);
  EXPECT_EQ(instruction2.arg2.size, 5);
}

TEST(InstructionSetAssemblerTest, InstructionTooManyEmbeddedArgTypes) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:0) WORDS "$r1, $r2, $r3" { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("invalid"), HasSubstr("$r1, $r2, $r3")));
}

TEST(InstructionSetAssemblerTest, OpcodeNameWithExtension) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:0) ALPHA.BETA { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_NE(asm_set->GetInstructionSet(), nullptr);
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 1);
  const auto& instruction = asm_set->GetInstructionSetDef().instructions[0];
  EXPECT_EQ(instruction.op, 0);
  EXPECT_EQ(instruction.op_name, "ALPHA.BETA");
  EXPECT_EQ(instruction.arg_source, "");
  EXPECT_EQ(instruction.arg1.type, ArgType::kNone);
  EXPECT_EQ(instruction.arg2.type, ArgType::kNone);
  EXPECT_EQ(instruction.code, "UL;");
}

TEST(InstructionSetAssemblerTest, DuplicateOpcodeNames) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:0) ALPHA { 
      UL;
    } 
    instruction(opcode:1) ALPHA { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("already defined"), HasSubstr("alpha")));
}

TEST(InstructionSetAssemblerTest, OpcodeNamesDifferByExtension) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:0) ALPHA { 
      UL;
    } 
    instruction(opcode:1) ALPHA.BETA { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_NE(asm_set->GetInstructionSet(), nullptr);
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 2);
  const auto& instruction0 = asm_set->GetInstructionSetDef().instructions[0];
  EXPECT_EQ(instruction0.op, 0);
  EXPECT_EQ(instruction0.op_name, "ALPHA");
  const auto& instruction1 = asm_set->GetInstructionSetDef().instructions[1];
  EXPECT_EQ(instruction1.op, 1);
  EXPECT_EQ(instruction1.op_name, "ALPHA.BETA");
}

TEST(InstructionSetAssemblerTest, DuplicateOpcodeNamesWithExtensions) {
  gb::ParseError error;
  std::string source = R"---(
    instruction(opcode:0) ALPHA.BETA { 
      UL;
    } 
    instruction(opcode:1) ALPHA.BETA { 
      UL;
    } 
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("already defined"), HasSubstr("alpha.beta")));
}

TEST(InstructionSetAssemblerTest, InstructionFirstMacroArgument) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "$r" { MOV(R0,m); }
    }
    instruction(opcode:0) TEST "$m" {
      UL;
      $Macro;
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_NE(asm_set->GetInstructionSet(), nullptr);
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 1);
  const auto& instruction = asm_set->GetInstructionSetDef().instructions[0];
  EXPECT_EQ(instruction.op_name, "TEST");
  EXPECT_EQ(instruction.arg1.type, ArgType::kMacro);
  EXPECT_EQ(instruction.arg1.size, GetDefaultArgTypeSize(ArgType::kWordReg));
  EXPECT_EQ(instruction.code, "UL;$Macro;");
}

TEST(InstructionSetAssemblerTest, InstructionSecondMacroArgument) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "$r" { MOV(R0,m); }
    }
    instruction(opcode:0) TEST "$r, $m" {
      UL;
      $Macro;
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_NE(asm_set->GetInstructionSet(), nullptr);
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 1);
  const auto& instruction = asm_set->GetInstructionSetDef().instructions[0];
  EXPECT_EQ(instruction.op_name, "TEST");
  EXPECT_EQ(instruction.arg1.type, ArgType::kWordReg);
  EXPECT_EQ(instruction.arg1.size, GetDefaultArgTypeSize(ArgType::kWordReg));
  EXPECT_EQ(instruction.arg2.type, ArgType::kMacro);
  EXPECT_EQ(instruction.arg2.size, GetDefaultArgTypeSize(ArgType::kWordReg));
  EXPECT_EQ(instruction.code, "UL;$Macro;");
}

TEST(InstructionSetAssemblerTest, InstructionTwoMacroArgumentsAreInvalid) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "$r" { MOV(R0,m); }
    }
    instruction(opcode:0) TEST "$m, $m" {
      UL;
      $Macro;
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("invalid"), HasSubstr("$m, $m")));
}

}  // namespace
}  // namespace oz3
