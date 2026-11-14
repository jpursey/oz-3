// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "absl/status/status.h"
#include "gtest/gtest.h"
#include "oz3/ozzy/console.h"

namespace oz3 {
namespace {

TEST(GenConsoleTest, CreateIsUnimplemented) {
  EXPECT_EQ(Console::Create(ConsoleMode::kScreen).status().code(),
            absl::StatusCode::kUnimplemented);
  EXPECT_EQ(Console::Create(ConsoleMode::kLines).status().code(),
            absl::StatusCode::kUnimplemented);
}

}  // namespace
}  // namespace oz3
