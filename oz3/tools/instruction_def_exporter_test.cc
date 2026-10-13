// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_def_exporter.h"

#include <memory>
#include <string>
#include <string_view>

#include "gb/file/file_system.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/instruction_def.h"

namespace oz3 {
namespace {

using ::testing::HasSubstr;
using ::testing::Not;

// Exports the instruction set to text.
std::string Export(const InstructionSetDef& instruction_set) {
  gb::FileSystem file_system;
  file_system.Register(std::make_unique<gb::MemoryFileProtocol>());
  EXPECT_TRUE(ExportInstructionSetDef(file_system, "mem:/test.inc",
                                      instruction_set, "kTest"));
  std::string text;
  EXPECT_TRUE(file_system.ReadFile("mem:/test.inc", &text));
  return text;
}

TEST(InstructionDefExporterTest, ValueSetsAndRelativeInstructions) {
  const NamedValueDef conditions[] = {{"Z", 4}, {"NZ", 0}};
  const NamedValueDef flags[] = {{"C", 4}};
  const ValueSetDef value_sets[] = {
      {.name = "Condition", .size = 3, .values = conditions},
      {.name = "Flags", .size = 4, .values = flags},
  };
  const InstructionDef instructions[] = {
      {.op = 1,
       .op_name = "JR",
       .source = "$Condition, $v",
       .relative = true,
       .arg1 = {ArgType::kImmediate, 3},
       .code = "LD(C0);UL;"},
      {.op = 2,
       .op_name = "SETF",
       .source = "$Flags",
       .arg1 = {ArgType::kImmediate, 4},
       .code = "UL;"},
  };
  EXPECT_EQ(Export({.instructions = instructions, .value_sets = value_sets}),
            R"---(constexpr oz3::NamedValueDef kTestValues_Condition[] = {
    {.name = "Z", .value = 4},
    {.name = "NZ", .value = 0},
};

constexpr oz3::NamedValueDef kTestValues_Flags[] = {
    {.name = "C", .value = 4},
};

constexpr oz3::ValueSetDef kTestValueSets[] = {
    {.name = "Condition",
     .size = 3,
     .values = kTestValues_Condition},
    {.name = "Flags",
     .size = 4,
     .values = kTestValues_Flags},
};

constexpr oz3::InstructionDef kTestInstructions[] = {
    {.op = 1,
     .op_name = "JR",
     .source = "$Condition, $v",
     .relative = true,
     .arg1 = {oz3::ArgType::kImmediate, 3},
     .code = "LD(C0);UL;"},
    {.op = 2,
     .op_name = "SETF",
     .source = "$Flags",
     .arg1 = {oz3::ArgType::kImmediate, 4},
     .code = "UL;"},
};

constexpr oz3::InstructionSetDef kTestInstructionSet = {
    .instructions=kTestInstructions,
    .value_sets=kTestValueSets,
};
)---");
}

TEST(InstructionDefExporterTest, NoValueSetsOrRelativeInstructions) {
  const InstructionDef instructions[] = {
      {.op = 0, .op_name = "NOP", .code = "UL;"},
  };
  const std::string text = Export({.instructions = instructions});
  EXPECT_THAT(text, HasSubstr("kTestInstructions[]"));
  EXPECT_THAT(text, Not(HasSubstr("NamedValueDef")));
  EXPECT_THAT(text, Not(HasSubstr("ValueSet")));
  EXPECT_THAT(text, Not(HasSubstr("value_sets")));
  EXPECT_THAT(text, Not(HasSubstr("relative")));
}

}  // namespace
}  // namespace oz3
