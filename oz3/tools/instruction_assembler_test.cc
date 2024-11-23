// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_assembler.h"

#include "absl/strings/ascii.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_compiler.h"

namespace oz3 {
namespace {

using ::testing::AllOf;
using ::testing::HasSubstr;

MATCHER_P2(AtLineCol, line, column, "") {
  return arg.line == line && arg.column == column;
}

TEST(InstructionAssemblerTest, BasicSuccess) {
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
  ASSERT_EQ(instruction.code.size(), 1);
  EXPECT_EQ(instruction.code[0].source, "");
  EXPECT_EQ(instruction.code[0].arg1.type, ArgType::kNone);
  EXPECT_EQ(instruction.code[0].arg2.type, ArgType::kNone);
  EXPECT_EQ(instruction.code[0].code, "UL;");
}

TEST(InstructionAssemblerTest, BasicSuccessFromFile) {
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
  ASSERT_EQ(instruction.code.size(), 1);
  EXPECT_EQ(instruction.code[0].source, "");
  EXPECT_EQ(instruction.code[0].arg1.type, ArgType::kNone);
  EXPECT_EQ(instruction.code[0].arg2.type, ArgType::kNone);
  EXPECT_EQ(instruction.code[0].code, "UL;");
}

TEST(InstructionAssemblerTest, FailToOpenFile) {
  gb::ParseError error;
  gb::FileSystem file_system;
  auto asm_set =
      AssembleInstructionSetFile(file_system, "mem:/test.oz3ism", &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              HasSubstr("test.oz3ism"));
  EXPECT_EQ(error.GetLocation(), gb::LexerLocation());
}

TEST(InstructionAssemblerTest, AutoAssignOpcodes) {
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

TEST(InstructionAssemblerTest, MaxInstructions) {
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

TEST(InstructionAssemblerTest, TooManyInstructions) {
  gb::ParseError error;
  std::string source;
  for (int i = 0; i < 257; ++i) {
    absl::StrAppend(&source, "instruction NOP", i, " { UL; }\n");
  }
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              HasSubstr("instructions"));
  EXPECT_EQ(error.GetLocation(), gb::LexerLocation());
}

TEST(InstructionAssemblerTest, DuplicateOpcode) {
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
  EXPECT_THAT(error.GetLocation(), AtLineCol(4, 4));
}

TEST(InstructionAssemblerTest, OpcodeNegative) {
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
  EXPECT_THAT(error.GetLocation(), AtLineCol(1, 4));
}

TEST(InstructionAssemblerTest, OpcodeTooBig) {
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
  EXPECT_THAT(error.GetLocation(), AtLineCol(1, 4));
}

TEST(InstructionAssemblerTest, InstructionValidArgSource) {
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
  ASSERT_EQ(instruction.code.size(), 1);
  EXPECT_EQ(instruction.code[0].source, "$v");
  EXPECT_EQ(instruction.code[0].arg1.type, ArgType::kNone);
  EXPECT_EQ(instruction.code[0].arg2.type, ArgType::kNone);
  EXPECT_EQ(instruction.code[0].code, "LD(R0);UL;");
}

TEST(InstructionAssemblerTest, InstructionInvalidArgSource) {
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
  EXPECT_THAT(error.GetLocation(), AtLineCol(1, 4));
}

TEST(InstructionAssemblerTest, InstructionEmbeddedArgumentTypes) {
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
  auto instruction = asm_set->GetInstructionSetDef().instructions[0];
  EXPECT_EQ(instruction.op_name, "WORD");
  ASSERT_EQ(instruction.code.size(), 1);
  EXPECT_EQ(instruction.code[0].arg1.type, ArgType::kWordReg);
  EXPECT_EQ(instruction.code[0].arg1.size,
            GetDefaultArgTypeSize(ArgType::kWordReg));
  EXPECT_EQ(instruction.code[0].arg2.type, ArgType::kWordReg);
  EXPECT_EQ(instruction.code[0].arg2.size, 4);
  instruction = asm_set->GetInstructionSetDef().instructions[1];
  EXPECT_EQ(instruction.op_name, "DWORD");
  ASSERT_EQ(instruction.code.size(), 1);
  EXPECT_EQ(instruction.code[0].arg1.type, ArgType::kDwordReg);
  EXPECT_EQ(instruction.code[0].arg1.size,
            GetDefaultArgTypeSize(ArgType::kDwordReg));
  EXPECT_EQ(instruction.code[0].arg2.type, ArgType::kDwordReg);
  EXPECT_EQ(instruction.code[0].arg2.size, 1);
  instruction = asm_set->GetInstructionSetDef().instructions[2];
  EXPECT_EQ(instruction.op_name, "IMM");
  ASSERT_EQ(instruction.code.size(), 1);
  EXPECT_EQ(instruction.code[0].arg1.type, ArgType::kImmediate);
  EXPECT_EQ(instruction.code[0].arg1.size, 3);
  EXPECT_EQ(instruction.code[0].arg2.type, ArgType::kImmediate);
  EXPECT_EQ(instruction.code[0].arg2.size, 5);
}

TEST(InstructionAssemblerTest, InstructionTooManyEmbeddedArgTypes) {
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
  EXPECT_THAT(error.GetLocation(), AtLineCol(1, 4));
}

TEST(InstructionAssemblerTest, OpcodeNameWithExtension) {
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
  ASSERT_EQ(instruction.code.size(), 1);
  EXPECT_EQ(instruction.code[0].source, "");
  EXPECT_EQ(instruction.code[0].arg1.type, ArgType::kNone);
  EXPECT_EQ(instruction.code[0].arg2.type, ArgType::kNone);
  EXPECT_EQ(instruction.code[0].code, "UL;");
}

TEST(InstructionAssemblerTest, DuplicateOpcodeNames) {
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
  EXPECT_THAT(error.GetLocation(), AtLineCol(4, 4));
}

TEST(InstructionAssemblerTest, OpcodeNamesDifferByExtension) {
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

TEST(InstructionAssemblerTest, DuplicateOpcodeNamesWithExtensions) {
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
  EXPECT_THAT(error.GetLocation(), AtLineCol(4, 4));
}

TEST(InstructionAssemblerTest, InstructionFirstMacroArgument) {
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
  ASSERT_EQ(instruction.code.size(), 1);
  EXPECT_EQ(instruction.code[0].arg1.type, ArgType::kMacro);
  EXPECT_EQ(instruction.code[0].arg1.size,
            GetDefaultArgTypeSize(ArgType::kWordReg));
  EXPECT_EQ(instruction.code[0].code, "UL;$Macro;");
}

TEST(InstructionAssemblerTest, InstructionSecondMacroArgument) {
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
  ASSERT_EQ(instruction.code.size(), 1);
  EXPECT_EQ(instruction.code[0].arg1.type, ArgType::kWordReg);
  EXPECT_EQ(instruction.code[0].arg1.size,
            GetDefaultArgTypeSize(ArgType::kWordReg));
  EXPECT_EQ(instruction.code[0].arg2.type, ArgType::kMacro);
  EXPECT_EQ(instruction.code[0].arg2.size,
            GetDefaultArgTypeSize(ArgType::kWordReg));
  EXPECT_EQ(instruction.code[0].code, "UL;$Macro;");
}

TEST(InstructionAssemblerTest, InstructionTwoMacroArgumentsAreInvalid) {
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
  EXPECT_THAT(error.GetLocation(), AtLineCol(4, 4));
}

TEST(InstructionAssemblerTest, InstructionMacroArgumentSizeMismatch) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "$r" { MOV(R0,m); }
    }
    instruction TEST "$m4" {
      UL;
      $Macro;
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(
      absl::AsciiStrToLower(error.GetMessage()),
      AllOf(HasSubstr("macro argument size"), HasSubstr("3"), HasSubstr("4")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(6, 6));
}

TEST(InstructionAssemblerTest, MinimumValidMacro) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "X" { MOV(R0,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_NE(asm_set->GetInstructionSet(), nullptr);
  EXPECT_EQ(asm_set->GetInstructionSet()->GetInstructionCount(), 0);
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  EXPECT_EQ(macro.name, "Macro");
  EXPECT_EQ(macro.param, ArgType::kNone);
  EXPECT_EQ(macro.ret, ArgType::kNone);
  EXPECT_EQ(macro.size, 1);
  ASSERT_EQ(macro.code.size(), 1);
  auto code = macro.code[0];
  EXPECT_EQ(code.arg.type, ArgType::kNone);
  EXPECT_EQ(code.arg.size, 0);
  EXPECT_EQ(code.ret, 0);
  EXPECT_EQ(code.source, "X");
  EXPECT_EQ(code.prefix.value, 0);
  EXPECT_EQ(code.prefix.size, 1);
  EXPECT_EQ(code.code, "MOV(R0,R1);");
}

TEST(InstructionAssemblerTest, DuplicateMacroName) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "X" { MOV(R0,R1); }
    }
    macro Macro {
      code "Y" { MOV(R1,R0); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("already defined"), HasSubstr("macro")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(4, 4));
}

TEST(InstructionAssemblerTest, MacroWithWordParameter) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro(p) {
      code "X" { MOV(R0,p); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  EXPECT_EQ(macro.name, "Macro");
  EXPECT_EQ(macro.param, ArgType::kWordReg);
}

TEST(InstructionAssemblerTest, MacroWithDwordParameter) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro(P) {
      code "X" { MOV(R0,p0); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  EXPECT_EQ(macro.name, "Macro");
  EXPECT_EQ(macro.param, ArgType::kDwordReg);
}

TEST(InstructionAssemblerTest, MacroWithWordReturn) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro:r {
      code:R4 "X" { MOV(R4,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  EXPECT_EQ(macro.name, "Macro");
  EXPECT_EQ(macro.ret, ArgType::kWordReg);
  ASSERT_EQ(macro.code.size(), 1);
  auto code = macro.code[0];
  EXPECT_EQ(code.ret, CpuCore::R4);
}

TEST(InstructionAssemblerTest, MacroWithDwordReturn) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro:R {
      code:D1 "X" { MOV(R2,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  EXPECT_EQ(macro.name, "Macro");
  EXPECT_EQ(macro.ret, ArgType::kDwordReg);
  ASSERT_EQ(macro.code.size(), 1);
  auto code = macro.code[0];
  EXPECT_EQ(code.ret, CpuCore::D1);
}

TEST(InstructionAssemblerTest, MacroCodeRetWithoutMacroRet) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code:R0 "X" { MOV(R0,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("macro"), HasSubstr("return")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(2, 6));
}

TEST(InstructionAssemblerTest, MacroCodeNoRetWithMacroRet) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro:r {
      code "X" { MOV(R0,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("macro"), HasSubstr("return")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(2, 6));
}

struct RetParamTest {
  template <typename Sink>
  friend void AbslStringify(Sink& sink, const RetParamTest& test) {
    absl::Format(&sink, "RetParamTest(%s, %s, %s, %s)", test.param_type,
                 test.ret_type, test.arg_type, test.ret);
  }

  std::string_view param_type;
  std::string_view ret_type;
  std::string_view arg_type;
  std::string_view ret;
};

TEST(InstructionAssemblerTest, MacroCodeRetParamValid) {
  const RetParamTest kTests[] = {
      {"p", "r", "X", "p"},   {"P", "r", "X", "p0"},  {"P", "r", "X", "p1"},
      {"P", "R", "X", "P"},   {"p", "r", "$r", "m"},  {"p", "r", "$R", "m0"},
      {"p", "r", "$R", "m1"}, {"p", "r", "$#3", "i"}, {"p", "R", "$R", "M"},
  };

  for (const auto& test : kTests) {
    gb::ParseError error;
    std::string source = absl::StrCat(
        "macro Macro(", test.param_type, "):", test.ret_type,
        " { code:", test.ret, " \"", test.arg_type, "\" { MOV(R0,R1); } }");
    auto asm_set = AssembleInstructionSet(source, &error);
    ASSERT_NE(asm_set, nullptr) << test << " Error: " << error.FormatMessage();
    ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1) << test;
    auto macro = asm_set->GetInstructionSetDef().macros[0];
    ASSERT_EQ(macro.code.size(), 1) << test;
    auto code = macro.code[0];
    if (CpuCore::GetVirtualDwordRegFromName(test.ret) != CpuCore::kInvalidReg) {
      EXPECT_EQ(code.ret, CpuCore::GetVirtualDwordRegFromName(test.ret))
          << test;
    } else {
      EXPECT_EQ(code.ret, CpuCore::GetVirtualWordRegFromName(test.ret)) << test;
    }
  }
}

TEST(InstructionAssemblerTest, MacroCodeRetParamInvalid) {
  const RetParamTest kTests[] = {
      {"p", "r", "X", "P"},    {"p", "r", "X", "p0"},   {"p", "r", "X", "p1"},
      {"p", "R", "X", "p"},    {"p", "R", "X", "P"},    {"p", "R", "X", "p0"},
      {"p", "R", "X", "p1"},   {"P", "r", "X", "p"},    {"P", "r", "X", "P"},
      {"P", "R", "X", "p"},    {"P", "R", "X", "p0"},   {"P", "R", "X", "p1"},
      {"p", "r", "$r", "M"},   {"p", "r", "$r", "m0"},  {"p", "r", "$r", "m0"},
      {"p", "r", "$R", "M"},   {"p", "R", "$r", "m"},   {"p", "R", "$r", "m0"},
      {"p", "R", "$r", "m1"},  {"p", "R", "$r", "M"},   {"p", "R", "$R", "m"},
      {"p", "R", "$R", "m0"},  {"p", "R", "$R", "m1"},  {"p", "r", "$r", "i"},
      {"p", "r", "$R", "i"},   {"p", "R", "$#3", "i"},  {"p", "r", "$#3", "m"},
      {"p", "r", "$#3", "m0"}, {"p", "r", "$#3", "m1"}, {"p", "R", "$#3", "M"},
  };
  for (const auto& test : kTests) {
    gb::ParseError error;
    std::string source =
        absl::StrCat("macro Macro(", test.param_type, "):", test.ret_type,
                     " { code:", test.ret, " \"X\" { MOV(R0,R1); } }");
    auto asm_set = AssembleInstructionSet(source, &error);
    EXPECT_EQ(asm_set, nullptr) << test;
    ASSERT_THAT(absl::AsciiStrToLower(error.GetMessage()),
                AllOf(HasSubstr("macro"), HasSubstr("return")))
        << test;
    EXPECT_THAT(error.GetLocation(), AtLineCol(0, 19));
  }
}

TEST(InstructionAssemblerTest, MacroCodeInvalidWordRegReturn) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro:r {
      code:D0 "X" { MOV(R0,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("invalid"), HasSubstr("d0")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(2, 6));
}

TEST(InstructionAssemblerTest, MacroCodeInvalidDwordRegReturn) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro:R {
      code:R4 "X" { MOV(R4,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("invalid"), HasSubstr("r4")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(2, 6));
}

TEST(InstructionAssemblerTest, DuplicateMacroCodeSource) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "XYZ" { MOV(R0,R1); }
      code "XYZ" { MOV(R1,R0); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("already defined"), HasSubstr("xyz")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(3, 6));
}

TEST(InstructionAssemblerTest, DuplicateMacroCodeSourceIgnoreSpaces) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "[ X + Y-Z ]" { MOV(R0,R1); }
      code "[X+Y -   Z] " { MOV(R1,R0); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("already defined"), HasSubstr("[x+y -   z] ")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(3, 6));
}

TEST(InstructionAssemblerTest, MacroCodeSourceInvalid) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "$x" { MOV(R0,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("invalid"), HasSubstr("$x")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(2, 6));
}

TEST(InstructionAssemblerTest, MacroCodeSourceMultipleArgsValid) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "A,$r3" { MOV(R0,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  EXPECT_EQ(macro.size, 3);
  ASSERT_EQ(macro.code.size(), 1);
  auto code = macro.code[0];
  EXPECT_EQ(code.arg.type, ArgType::kWordReg);
  EXPECT_EQ(code.arg.size, 3);
}

TEST(InstructionAssemblerTest, MacroCodeSourceWithCodeValues) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "$V + $v" { MOV(R0,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  ASSERT_EQ(macro.code.size(), 1);
  auto code = macro.code[0];
  EXPECT_EQ(code.source, "$V + $v");
}

TEST(InstructionAssemblerTest, MacroCodeSourceWithMacroArgInvalid) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "$m" { MOV(R0,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("invalid"), HasSubstr("$m")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(2, 6));
}

TEST(InstructionAssemblerTest,
     MacroCodeSourceWithMultipleNonCodeValueArgsInvalid) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "$r + $#3" { MOV(R0,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("invalid"), HasSubstr("$r + $#3")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(2, 6));
}

TEST(InstructionAssemblerTest, MacroBitsSetExplicitly) {
  gb::ParseError error;
  std::string source = R"---(
    macro(bits:2) Macro {
      code "X" { MOV(R0,R1); }
      code "Y" { MOV(R1,R0); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  ASSERT_EQ(macro.size, 2);
  ASSERT_EQ(macro.code.size(), 2);
  auto code = macro.code[0];
  EXPECT_EQ(code.prefix.value, 0);
  EXPECT_EQ(code.prefix.size, 2);
  code = macro.code[1];
  EXPECT_EQ(code.prefix.value, 1);
  EXPECT_EQ(code.prefix.size, 2);
}

TEST(InstructionAssemblerTest, MacroBitsMinMax) {
  gb::ParseError error;
  std::string source = R"---(
    macro(bits:0) Macro {
      code "X" { MOV(R0,R1); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("bits"), HasSubstr("0")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(1, 4));

  source = R"---(
    macro(bits:1) Macro {
      code "X" { MOV(R0,R1); }
    }
  )---";
  asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr);
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  auto macro = asm_set->GetInstructionSetDef().macros[0];
  ASSERT_EQ(macro.size, 1);
  ASSERT_EQ(macro.code.size(), 1);
  auto code = macro.code[0];
  EXPECT_EQ(code.prefix.value, 0);
  EXPECT_EQ(code.prefix.size, 1);

  source = R"---(
    macro(bits:8) Macro {
      code "X" { MOV(R0,R1); }
    }
  )---";
  asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr);
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  macro = asm_set->GetInstructionSetDef().macros[0];
  ASSERT_EQ(macro.size, 8);
  ASSERT_EQ(macro.code.size(), 1);
  code = macro.code[0];
  EXPECT_EQ(code.prefix.value, 0);
  EXPECT_EQ(code.prefix.size, 8);

  source = R"---(
    macro(bits:9) Macro {
      code "X" { MOV(R0,R1); }
    }
  )---";
  asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("bits"), HasSubstr("9")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(1, 4));
}

TEST(InstructionAssemblerTest, MacroBitsAutoSourceArgSizes) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "$r3" { MOV(R0,m); }
      code "$R2" { MOV(R0,m0); }
      code "$#2" { MOV(R0,i); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  ASSERT_EQ(macro.size, 4);
  ASSERT_EQ(macro.code.size(), 3);
  auto code = macro.code[0];
  EXPECT_EQ(code.prefix.value, 0);
  EXPECT_EQ(code.prefix.size, 1);
  code = macro.code[1];
  EXPECT_EQ(code.prefix.value, 0b10);
  EXPECT_EQ(code.prefix.size, 2);
  code = macro.code[2];
  EXPECT_EQ(code.prefix.value, 0b11);
  EXPECT_EQ(code.prefix.size, 2);
}

TEST(InstructionAssemblerTest, MacroBitsAutoArgSizesSmallToLarge) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "$r1" { MOV(R1,m); }
      code "$r2" { MOV(R2,m); }
      code "$r3" { MOV(R3,m); }
      code "$r4" { MOV(R4,m); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  ASSERT_EQ(macro.size, 5);
  ASSERT_EQ(macro.code.size(), 4);
  auto code = macro.code[0];
  EXPECT_EQ(code.source, "$r4");
  EXPECT_EQ(code.prefix.value, 0);
  EXPECT_EQ(code.prefix.size, 1);
  code = macro.code[1];
  EXPECT_EQ(code.source, "$r3");
  EXPECT_EQ(code.prefix.value, 0b10);
  EXPECT_EQ(code.prefix.size, 2);
  code = macro.code[2];
  EXPECT_EQ(code.source, "$r2");
  EXPECT_EQ(code.prefix.value, 0b110);
  EXPECT_EQ(code.prefix.size, 3);
  code = macro.code[3];
  EXPECT_EQ(code.source, "$r1");
  EXPECT_EQ(code.prefix.value, 0b1110);
  EXPECT_EQ(code.prefix.size, 4);
}

TEST(InstructionAssemblerTest, MacroCodeSourceExceedsSetBits) {
  gb::ParseError error;
  std::string source = R"---(
    macro(bits:2) Macro {
      code "$r3" { MOV(R1,m); }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("exceeds"), HasSubstr("2")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(1, 4));

  source = R"---(
    macro(bits:2) Macro {
      code "$r2" { MOV(R1,m); }
      code "$#2" { MOV(R1,m); }
    }
  )---";
  asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("exceeds"), HasSubstr("2")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(1, 4));
}

TEST(InstructionAssemblerTest, AllMicrocodeSyntaxValid) {
  gb::ParseError error;
  std::string source = R"---(
    macro NoArg {
      code "$r" { MOV(R0,m); }
    }
    macro WithArg(p) {
      code "$r" { MOV(m,p); }
    }
    instruction TEST "$r, $m" { 
      LD(C0);             # Load value into C0
      UL;                 # End fetch phase
      @label:ADDI(a,1);   # Increment argument by 1
      JC(NZ,@label);      # Continue looping if argument is not zero
      $NoArg;             # Call macro with no argument
    }
    instruction TEST2 "$m" {
      UL;
      $WithArg(R0);
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().instructions.size(), 2);
  auto instruction = asm_set->GetInstructionSetDef().instructions[0];
  ASSERT_EQ(instruction.code.size(), 1);
  EXPECT_EQ(instruction.code[0].code,
            "LD(C0);UL;@label:ADDI(a,1);JC(NZ,@label);$NoArg;");
  instruction = asm_set->GetInstructionSetDef().instructions[1];
  EXPECT_EQ(instruction.code[0].code, "UL;$WithArg(R0);");
}

TEST(InstructionAssemblerTest, CallMacroInMacroInvalid) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "X" { $Macro; }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()), HasSubstr("macro"));
  EXPECT_THAT(error.GetLocation(), AtLineCol(2, 17));
}

TEST(InstructionAssemblerTest, CallMacroWithNoMacroParameterInvalid) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "X" { MOV(R0,R1); }
    }
    instruction TEST {
      UL;
      $Macro;
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()), HasSubstr("macro"));
  EXPECT_THAT(error.GetLocation(), AtLineCol(6, 6));
}

TEST(InstructionAssemblerTest, CallMultipleMacrosInvalid) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro1 {
      code "X" { MOV(R0,R1); }
    }
    macro Macro2 {
      code "Y" { MOV(R1,R0); }
    }
    instruction TEST "$m" {
      UL;
      $Macro1;
      $Macro2;
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("multiple"), HasSubstr("macro")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(10, 6));
}

TEST(InstructionAssemblerTest, CallUndefinedMacro) {
  gb::ParseError error;
  std::string source = R"---(
    instruction TEST "$m" {
      UL;
      $Macro;
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(absl::AsciiStrToLower(error.GetMessage()),
              AllOf(HasSubstr("undefined"), HasSubstr("macro")));
  EXPECT_THAT(error.GetLocation(), AtLineCol(3, 6));
}

TEST(InstructionAssemblerTest, NoCodeForMacroCode) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "X" { }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  ASSERT_NE(asm_set, nullptr) << "Error: " << error.FormatMessage();
  ASSERT_EQ(asm_set->GetInstructionSetDef().macros.size(), 1);
  const auto& macro = asm_set->GetInstructionSetDef().macros[0];
  ASSERT_EQ(macro.code.size(), 1);
  auto code = macro.code[0];
  EXPECT_EQ(code.source, "X");
  EXPECT_EQ(code.code, "");
}

TEST(InstructionAssemblerTest, ErrorInMacroCodeCode) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "X" { INVALID; }
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(error.GetMessage(), HasSubstr("INVALID"));
  EXPECT_THAT(error.GetLocation(), AtLineCol(2, 17));
}

TEST(InstructionAssemblerTest, NoULForInstruction) {
  gb::ParseError error;
  std::string source = R"---(
    instruction TEST { MOV(R0,R1); }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(error.GetMessage(), HasSubstr("UL"));
  EXPECT_THAT(error.GetLocation(), AtLineCol(1, 4));
}

TEST(InstructionAssemblerTest, ErrorInInstructionCode) {
  gb::ParseError error;
  std::string source = R"---(
    instruction TEST { UL; INVALID; }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(error.GetMessage(), HasSubstr("INVALID"));
  EXPECT_THAT(error.GetLocation(), AtLineCol(1, 27));
}

TEST(InstructionAssemblerTest, ErrorLocationAfterMacroCorrect) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "X" { 
        MOV(R0,R1);
        MOV(R2,R3);
        MOV(R4,R5);
        MOV(R6,R7);
      }
    }
    instruction TEST "$m" {
      UL;
      $Macro;
      INVALID;
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(error.GetMessage(), HasSubstr("INVALID"));
  EXPECT_THAT(error.GetLocation(), AtLineCol(12, 6));
}

TEST(InstructionAssemblerTest, ErrorInsideMacroExpansionLocationAtMacroCall) {
  gb::ParseError error;
  std::string source = R"---(
    macro Macro {
      code "X" { 
        MOV(R0,R1);
        MOV(R2,R3);
        LD(R5);
        MOV(R4,R5);
        MOV(R6,R7);
      }
    }
    instruction TEST "$m" {
      UL;
      $Macro;
      MOV(R6,R7);
    }
  )---";
  auto asm_set = AssembleInstructionSet(source, &error);
  EXPECT_EQ(asm_set, nullptr);
  EXPECT_THAT(error.GetMessage(), HasSubstr("LD(R5)"));
  EXPECT_THAT(error.GetLocation(), AtLineCol(12, 6));
}

}  // namespace
}  // namespace oz3
