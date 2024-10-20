// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_set_parser.h"

#include "gtest/gtest.h"

namespace oz3 {
namespace {

TEST(InstructionSetParserTest, EmptyText) {
  EXPECT_FALSE(ParseInstructionSet(""));
}

}  // namespace
}  // namespace oz3
