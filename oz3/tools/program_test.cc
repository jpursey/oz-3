// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program.h"

#include <string_view>
#include <utility>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"

namespace oz3 {
namespace {

using ::testing::HasSubstr;

using Access = Program::Segment::Access;
using Data = Program::Segment::Data;

// A valid program with two banks, three segments, and two instances.
Program MakeValidProgram() {
  Program program;
  program.banks.resize(2);
  program.segments = {
      {.bank = 0,
       .access = Access::kReadOnly,
       .page_start = 0,
       .page_count = 2,
       .data = {{.offset = 0, .data = {1, 2}}, {.offset = 2, .data = {3, 4}}}},
      {.bank = 0, .page_count = 14},
      {.bank = 1,
       .page_count = 1,
       .data = {{.offset = kMemoryBankPageSize - 2, .data = {5, 6}}}},
  };
  program.code_segment = 0;
  program.stack_segment = 1;
  program.data_segment = 2;
  program.extra_segment = 2;
  program.instances = {
      {.code = 0, .stack = 0, .data = 0, .extra = 0},
      {.code = 2 * kMemoryBankPageSize,
       .stack = 14 * kMemoryBankPageSize,
       .data = kMemoryBankPageSize,
       .extra = kMemoryBankPageSize},
  };
  return program;
}

void ExpectInvalid(const Program& program, std::string_view message) {
  absl::Status status = ValidateProgram(program);
  EXPECT_FALSE(status.ok());
  EXPECT_THAT(status.message(), HasSubstr(message));
}

TEST(ProgramTest, ValidPrograms) {
  EXPECT_TRUE(ValidateProgram({}).ok());
  absl::Status status = ValidateProgram(MakeValidProgram());
  EXPECT_TRUE(status.ok()) << status;
}

TEST(ProgramTest, TooManyBanks) {
  Program program;
  program.banks.resize(kMaxMemoryBanks + 1);
  ExpectInvalid(program, "17 banks");
}

TEST(ProgramTest, TooManySegments) {
  Program program;
  program.banks.resize(kMaxMemoryBanks);
  program.segments.resize(kMaxMemoryBanks * kMemoryBankPageCount + 1);
  ExpectInvalid(program, "257 segments");
}

TEST(ProgramTest, TooManyInstances) {
  Program program;
  program.instances.resize(kMaxCores + 1);
  ExpectInvalid(program, "9 instances");
}

TEST(ProgramTest, SegmentBankNotInProgram) {
  Program program = MakeValidProgram();
  program.segments[2].bank = 2;
  ExpectInvalid(program, "Segment 2 is in bank 2");
}

TEST(ProgramTest, SegmentAccessInvalid) {
  Program program = MakeValidProgram();
  program.segments[1].access = static_cast<Access>(2);
  ExpectInvalid(program, "Segment 1 has access 2");
}

TEST(ProgramTest, SegmentPageCount) {
  Program program = MakeValidProgram();
  program.segments[2].page_count = 0;
  ExpectInvalid(program, "Segment 2 has 0 pages");
  program.segments[2].page_count = kMemoryBankPageCount + 1;
  ExpectInvalid(program, "Segment 2 has 17 pages");
}

TEST(ProgramTest, SegmentPastEndOfBank) {
  Program program = MakeValidProgram();
  program.segments[2].page_start = kMemoryBankPageCount - 1;
  program.segments[2].page_count = 2;
  ExpectInvalid(program, "past the end of its bank");
}

TEST(ProgramTest, BankHasTooManyPages) {
  Program program = MakeValidProgram();
  program.segments[1].page_count = 15;
  ExpectInvalid(program, "Bank 0 has more than 16 pages");
}

TEST(ProgramTest, FixedSegmentsOverlap) {
  Program program = MakeValidProgram();
  program.segments[1].page_start = 1;
  program.segments[1].page_count = 2;
  ExpectInvalid(program, "Segment 1 overlaps");
}

TEST(ProgramTest, DataBlockNotAligned) {
  const std::vector<Data> blocks = {
      {.offset = 0},
      {.offset = 1, .data = {1, 2}},
      {.offset = 2, .data = {1, 2, 3}},
  };
  for (const Data& block : blocks) {
    Program program = MakeValidProgram();
    program.segments[1].data = {block};
    ExpectInvalid(program, "must be multiples of 2");
  }
}

TEST(ProgramTest, DataBlocksOutOfOrder) {
  Program program = MakeValidProgram();
  std::swap(program.segments[0].data[0], program.segments[0].data[1]);
  ExpectInvalid(program, "before the end of the block before it");
}

TEST(ProgramTest, DataBlocksOverlap) {
  Program program = MakeValidProgram();
  program.segments[0].data[1].offset = 0;
  ExpectInvalid(program, "before the end of the block before it");
}

TEST(ProgramTest, DataBlockPastEndOfSegment) {
  Program program = MakeValidProgram();
  program.segments[2].data[0].offset = kMemoryBankPageSize;
  ExpectInvalid(program, "past the end of the segment");
}

TEST(ProgramTest, RoleSegmentNotInProgram) {
  Program program = MakeValidProgram();
  program.code_segment = 3;
  ExpectInvalid(program, "The code segment is 3");

  program = MakeValidProgram();
  program.stack_segment = 3;
  ExpectInvalid(program, "The stack segment is 3");

  program = MakeValidProgram();
  program.data_segment = 3;
  ExpectInvalid(program, "The data segment is 3");

  program = MakeValidProgram();
  program.extra_segment = 3;
  ExpectInvalid(program, "The extra segment is 3");
}

TEST(ProgramTest, InstanceOffsetPastEndOfSegment) {
  // Instance 1's offsets are each at the end of their segment.
  Program program = MakeValidProgram();
  ++program.instances[1].code;
  ExpectInvalid(program, "Instance 1's code offset");

  program = MakeValidProgram();
  ++program.instances[1].stack;
  ExpectInvalid(program, "Instance 1's stack offset");

  program = MakeValidProgram();
  ++program.instances[1].data;
  ExpectInvalid(program, "Instance 1's data offset");

  program = MakeValidProgram();
  ++program.instances[1].extra;
  ExpectInvalid(program, "Instance 1's extra offset");
}

}  // namespace
}  // namespace oz3
