// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_def.h"

#include <string_view>

#include "gtest/gtest.h"

namespace oz3 {
namespace {

TEST(InstructionDefTest, IsPlaceholderName) {
  for (std::string_view name :
       {"r", "r1", "r2", "r3", "r4", "R", "R1", "R2", "m", "m1", "m2", "m3",
        "m4", "m5", "m6", "m7", "m8", "v", "V"}) {
    EXPECT_TRUE(IsPlaceholderName(name)) << name;
  }
  for (std::string_view name :
       {"", "#", "#3", "r0", "r5", "R0", "R3", "m0", "m9", "M", "v1", "V2",
        "rr", "r12", "x", "Condition"}) {
    EXPECT_FALSE(IsPlaceholderName(name)) << name;
  }
}

}  // namespace
}  // namespace oz3
