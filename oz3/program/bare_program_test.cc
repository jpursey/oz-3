// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/program/bare_program.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"

namespace oz3 {
namespace {

using ::testing::HasSubstr;

constexpr int kAlignment = Program::Segment::Data::kAlignment;

TEST(BareProgramTest, EmptyProgramIsValid) {
  EXPECT_TRUE(ValidateBareProgram({}).ok());
}

TEST(BareProgramTest, BlocksWithGapsAreValid) {
  const BareProgram program = {
      .contents_words = 10,
      .stack_words = 4,
      .data = {{.offset = 0, .data = {1, 2}}, {.offset = 6, .data = {3, 4}}}};
  absl::Status status = ValidateBareProgram(program);
  EXPECT_TRUE(status.ok()) << status;
}

TEST(BareProgramTest, BlockEndingAtContentsIsValid) {
  const BareProgram program = {
      .contents_words = 4,
      .data = {{.offset = 0, .data = {1, 2}}, {.offset = 2, .data = {3, 4}}}};
  absl::Status status = ValidateBareProgram(program);
  EXPECT_TRUE(status.ok()) << status;
}

TEST(BareProgramTest, WholeBankIsValid) {
  const BareProgram program = {
      .contents_words = kMemoryBankMaxSize - kAlignment,
      .stack_words = kAlignment,
  };
  absl::Status status = ValidateBareProgram(program);
  EXPECT_TRUE(status.ok()) << status;
}

TEST(BareProgramTest, InvalidContents) {
  EXPECT_FALSE(ValidateBareProgram({.contents_words = -kAlignment}).ok());
  absl::Status status = ValidateBareProgram({.contents_words = kAlignment + 1});
  EXPECT_FALSE(status.ok());
  EXPECT_THAT(status.message(), HasSubstr("multiple of"));
}

TEST(BareProgramTest, InvalidStack) {
  EXPECT_FALSE(ValidateBareProgram({.stack_words = -1}).ok());
  EXPECT_FALSE(ValidateBareProgram({.stack_words = kMemoryBankMaxSize}).ok());
}

TEST(BareProgramTest, ContentsAndStackPastBank) {
  absl::Status status = ValidateBareProgram({
      .contents_words = kMemoryBankMaxSize - kAlignment,
      .stack_words = kAlignment + 1,
  });
  EXPECT_FALSE(status.ok());
  EXPECT_THAT(status.message(), HasSubstr("pass the end of a bank"));
}

TEST(BareProgramTest, EmptyBlock) {
  absl::Status status = ValidateBareProgram(
      {.contents_words = 4, .data = {{.offset = 0, .data = {}}}});
  EXPECT_FALSE(status.ok());
  EXPECT_THAT(status.message(), HasSubstr("size can't be 0"));
}

TEST(BareProgramTest, UnalignedBlock) {
  EXPECT_FALSE(ValidateBareProgram({.contents_words = 4,
                                    .data = {{.offset = 1, .data = {1, 2}}}})
                   .ok());
  EXPECT_FALSE(ValidateBareProgram(
                   {.contents_words = 4, .data = {{.offset = 0, .data = {1}}}})
                   .ok());
}

TEST(BareProgramTest, BlocksOutOfOrder) {
  absl::Status status = ValidateBareProgram(
      {.contents_words = 8,
       .data = {{.offset = 4, .data = {1, 2}}, {.offset = 0, .data = {3, 4}}}});
  EXPECT_FALSE(status.ok());
  EXPECT_THAT(status.message(), HasSubstr("before the end of the block"));
}

TEST(BareProgramTest, BlocksOverlap) {
  absl::Status status =
      ValidateBareProgram({.contents_words = 8,
                           .data = {{.offset = 0, .data = {1, 2, 3, 4}},
                                    {.offset = 2, .data = {5, 6}}}});
  EXPECT_FALSE(status.ok());
  EXPECT_THAT(status.message(), HasSubstr("before the end of the block"));
}

TEST(BareProgramTest, BlockPastContents) {
  absl::Status status =
      ValidateBareProgram({.contents_words = 2,
                           .stack_words = 64,
                           .data = {{.offset = 0, .data = {1, 2, 3, 4}}}});
  EXPECT_FALSE(status.ok());
  EXPECT_THAT(status.message(), HasSubstr("end of the contents"));
}

}  // namespace
}  // namespace oz3
