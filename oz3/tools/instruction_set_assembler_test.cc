// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_set_assembler.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

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

}  // namespace
}  // namespace oz3
