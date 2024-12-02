// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/base_core_test.h"

namespace oz3 {
namespace {

using InstructionTest = BaseCoreTest;

TEST_F(InstructionTest, NOP_ValueIsZero) {
  ASSERT_TRUE(Init());
  EXPECT_EQ(Encode("NOP"), 0);
}

TEST_F(InstructionTest, NOP_Cycles) {
  ASSERT_TRUE(Init());
  auto& state = GetState();
  state.ResetCore();
  for (int i = 0; i < 10; ++i) {
    state.code.AddValue(Encode("NOP"));
  }
  Execute(kCpuCoreFetchAndDecodeCycles * 10);
  EXPECT_EQ(GetCycles(), kCpuCoreFetchAndDecodeCycles * 10);
  state.Update();
  EXPECT_EQ(state.ip, 10);
}

}  // namespace
}  // namespace oz3
