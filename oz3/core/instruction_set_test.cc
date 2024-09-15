// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_set.h"

#include <string>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/microcode.h"

namespace oz3 {
namespace {

using ::testing::IsEmpty;

TEST(InstructionSetTest, InstructionSetCompiles) {
  InstructionMicrocodes codes;
  std::string error;
  EXPECT_TRUE(codes.Compile(GetInstructionSet(), &error));
  EXPECT_THAT(error, IsEmpty());
}

}  // namespace
}  // namespace oz3