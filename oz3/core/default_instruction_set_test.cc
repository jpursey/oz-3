// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/default_instruction_set.h"

#include <string>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/instruction_compiler.h"

namespace oz3 {
namespace {

using ::testing::IsEmpty;

TEST(DefaultInstructionSetTest, InstructionSetCompiles) {
  InstructionCompiler codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(GetDefaultInstructionSet(), &error));
  EXPECT_THAT(error, IsEmpty());
}

}  // namespace
}  // namespace oz3
