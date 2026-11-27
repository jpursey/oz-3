// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/program/program_file.h"

#include <cstddef>
#include <memory>
#include <vector>

#include "gb/file/file_system.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/program/bare_program.h"
#include "oz3/program/program.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::HasSubstr;

using Access = Program::Segment::Access;

// A program that sets every field of a Program, and the words of its program
// file.
Program MakeTestProgram() {
  Program program;
  program.banks = {{.assign_mask = 0x0003}, {}};
  program.segments = {
      {.bank = 0,
       .access = Access::kReadOnly,
       .page_start = 2,
       .page_count = 1,
       .data = {{.offset = 0x10, .data = {1, 2, 3, 4}}}},
      {.bank = 1,
       .access = Access::kReadWrite,
       .page_count = 2,
       .data = {{.offset = 0, .data = {5, 6}}, {.offset = 6, .data = {7, 8}}}},
  };
  program.code_segment = 0;
  program.stack_segment = 1;
  program.data_segment = 1;
  program.instances = {{.data = 0x100}, {.stack = 0x80, .data = 0x200}};
  return program;
}

// clang-format off
const std::vector<uint16_t> kTestProgramWords = {
    0x5A4F, 1,             // Header: identifier, version
    2,                     // Banks
    0x0003,                //   Bank 0: assign mask
    0xFFFF,                //   Bank 1
    2,                     // Segments
    0, 0, 2, 1, 1,         //   Segment 0: bank, access, page start and count,
                           //              block count
    1, 1, 0xFFFF, 2, 2,    //   Segment 1
    0, 1, 1, 0xFFFF,       // Roles: code, stack, data, extra
    2,                     // Instances
    0, 0, 0x100, 0,        //   Instance 0: code, stack, data, extra
    0, 0x80, 0x200, 0,     //   Instance 1
    8, 2, 1, 2, 3, 4,      // Segment 0 data: offset and size in units, words
    0, 1, 5, 6,            // Segment 1 data
    3, 1, 7, 8,            //
};
// clang-format on

TEST(ProgramFileTest, WriteWords) {
  absl::StatusOr<std::vector<uint16_t>> words =
      WriteProgramWords(MakeTestProgram());
  ASSERT_TRUE(words.ok()) << words.status();
  EXPECT_THAT(*words, ElementsAreArray(kTestProgramWords));
}

TEST(ProgramFileTest, ReadWords) {
  absl::StatusOr<Program> program = ReadProgramWords(kTestProgramWords);
  ASSERT_TRUE(program.ok()) << program.status();
  EXPECT_EQ(*program, MakeTestProgram());
}

TEST(ProgramFileTest, EmptyProgram) {
  absl::StatusOr<std::vector<uint16_t>> words = WriteProgramWords({});
  ASSERT_TRUE(words.ok()) << words.status();
  EXPECT_THAT(*words,
              ElementsAre(0x5A4F, 1, 0, 0, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0));
  absl::StatusOr<Program> program = ReadProgramWords(*words);
  ASSERT_TRUE(program.ok()) << program.status();
  EXPECT_EQ(*program, Program{});
}

TEST(ProgramFileTest, WholeBankBlock) {
  Program program;
  program.banks.resize(1);
  Program::Segment& segment = program.segments.emplace_back();
  segment.page_start = 0;
  segment.page_count = 16;
  std::vector<uint16_t>& data = segment.data.emplace_back().data;
  data.resize(0x10000);
  for (int i = 0; i < 0x10000; ++i) {
    data[i] = static_cast<uint16_t>(i * 3);
  }

  absl::StatusOr<std::vector<uint16_t>> words = WriteProgramWords(program);
  ASSERT_TRUE(words.ok()) << words.status();
  absl::StatusOr<Program> read_program = ReadProgramWords(*words);
  ASSERT_TRUE(read_program.ok()) << read_program.status();
  EXPECT_EQ(*read_program, program);
}

TEST(ProgramFileTest, LastUnitOfBank) {
  Program program;
  program.banks.resize(1);
  program.segments.push_back({.page_start = 0,
                              .page_count = 16,
                              .data = {{.offset = 0xFFFE, .data = {1, 2}}}});
  absl::StatusOr<std::vector<uint16_t>> words = WriteProgramWords(program);
  ASSERT_TRUE(words.ok()) << words.status();
  absl::StatusOr<Program> read_program = ReadProgramWords(*words);
  ASSERT_TRUE(read_program.ok()) << read_program.status();
  EXPECT_EQ(*read_program, program);
}

TEST(ProgramFileTest, WriteFailsForInvalidProgram) {
  Program program = MakeTestProgram();
  program.segments[0].data[0].offset = 0x11;
  absl::StatusOr<std::vector<uint16_t>> words = WriteProgramWords(program);
  EXPECT_FALSE(words.ok());
  EXPECT_THAT(words.status().message(), HasSubstr("multiples of 2"));
}

TEST(ProgramFileTest, ReadFailsForNotProgramFile) {
  EXPECT_FALSE(ReadProgramWords({}).ok());
  std::vector<uint16_t> words = kTestProgramWords;
  words[0] = 0x4F5A;
  absl::StatusOr<Program> program = ReadProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("Not a program file"));
}

TEST(ProgramFileTest, ReadFailsForOtherVersion) {
  std::vector<uint16_t> words = kTestProgramWords;
  words[1] = 2;
  absl::StatusOr<Program> program = ReadProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("version 2"));
}

TEST(ProgramFileTest, ReadFailsForEveryTruncation) {
  for (int size = 0; size < static_cast<int>(kTestProgramWords.size());
       ++size) {
    EXPECT_FALSE(
        ReadProgramWords(absl::MakeConstSpan(kTestProgramWords).first(size))
            .ok())
        << "Size: " << size;
  }
}

TEST(ProgramFileTest, ReadFailsForCountsPastEnd) {
  // Too many segments for the words left.
  EXPECT_FALSE(ReadProgramWords({0x5A4F, 1, 0, 0xFFFF, 0, 0, 0, 0, 0}).ok());

  // Too many data blocks for the words left.
  absl::StatusOr<Program> program =
      ReadProgramWords({0x5A4F, 1, 1, 0xFFFF, 1, 0, 1, 0xFFFF, 1, 0xFFFF, 0, 0,
                        0, 0, 0, 0, 1, 5, 6});
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("ends before"));
}

TEST(ProgramFileTest, ReadFailsForOffsetPastBank) {
  std::vector<uint16_t> words = kTestProgramWords;
  words[words.size() - 14] = 0x8000;  // Segment 0's first block's offset.
  absl::StatusOr<Program> program = ReadProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("past the end of a bank"));
}

TEST(ProgramFileTest, ReadFailsForInvalidProgram) {
  // Segment 0's first block, emptied.
  std::vector<uint16_t> words = kTestProgramWords;
  const int size_index = static_cast<int>(words.size()) - 13;
  words[size_index] = 0;
  words.erase(words.begin() + size_index + 1, words.begin() + size_index + 5);
  absl::StatusOr<Program> program = ReadProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("size can't be 0"));
}

TEST(ProgramFileTest, ReadFailsForWordsAfterProgram) {
  std::vector<uint16_t> words = kTestProgramWords;
  words.push_back(0);
  absl::StatusOr<Program> program = ReadProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("1 words after"));
}

TEST(ProgramFileTest, ReadFailsForInvalidAccess) {
  std::vector<uint16_t> words = kTestProgramWords;
  words[7] = 2;  // Segment 0's access.
  absl::StatusOr<Program> program = ReadProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("access 2"));
}

TEST(ProgramFileTest, WriteAndReadFile) {
  gb::FileSystem file_system;
  file_system.Register(std::make_unique<gb::MemoryFileProtocol>());
  absl::Status status =
      WriteProgramFile(file_system, "mem:/test.oz3", MakeTestProgram());
  ASSERT_TRUE(status.ok()) << status;

  std::vector<std::byte> bytes;
  ASSERT_TRUE(file_system.ReadFile("mem:/test.oz3", &bytes));
  ASSERT_EQ(bytes.size(), kTestProgramWords.size() * 2);

  // "OZ", version 1, and 2 banks.
  const std::byte kHeaderBytes[] = {std::byte{'O'}, std::byte{'Z'},
                                    std::byte{1},   std::byte{0},
                                    std::byte{2},   std::byte{0}};
  EXPECT_THAT(absl::MakeConstSpan(bytes).first(6),
              ElementsAreArray(kHeaderBytes));

  absl::StatusOr<Program> program =
      ReadProgramFile(file_system, "mem:/test.oz3");
  ASSERT_TRUE(program.ok()) << program.status();
  EXPECT_EQ(*program, MakeTestProgram());
}

TEST(ProgramFileTest, ReadFileFailsForMissingFile) {
  gb::FileSystem file_system;
  file_system.Register(std::make_unique<gb::MemoryFileProtocol>());
  EXPECT_FALSE(ReadProgramFile(file_system, "mem:/missing.oz3").ok());
}

TEST(ProgramFileTest, ReadFileFailsForOddByteCount) {
  gb::FileSystem file_system;
  file_system.Register(std::make_unique<gb::MemoryFileProtocol>());
  const std::vector<std::byte> bytes = {std::byte{'O'}, std::byte{'Z'},
                                        std::byte{1}};
  ASSERT_TRUE(file_system.WriteFile("mem:/test.oz3", bytes));
  absl::StatusOr<Program> program =
      ReadProgramFile(file_system, "mem:/test.oz3");
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("odd number of bytes"));
}

TEST(ProgramFileTest, ReadFailsForBareProgramFile) {
  absl::StatusOr<Program> program = ReadProgramWords(
      {kBareProgramFileId, kBareProgramFileVersion, 0, 0, 0, 0});
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(),
              HasSubstr("Not a program file, but a bare program file"));
}

//==============================================================================
// Bare program files
//==============================================================================

// A bare program that sets every field of a BareProgram, with two blocks and a
// gap between them, and the words of its bare program file.
BareProgram MakeTestBareProgram() {
  return {.contents_words = 12,
          .stack_words = 64,
          .data = {{.offset = 0, .data = {1, 2, 3, 4}},
                   {.offset = 8, .data = {5, 6}}}};
}

// clang-format off
const std::vector<uint16_t> kTestBareProgramWords = {
    0x424F, 1,             // Header: identifier, version
    12, 64,                // Contents and stack, in words
    2, 0,                  // Block count, reserved
    0, 2, 1, 2, 3, 4,      // Blocks: offset and size in units, words
    4, 1, 5, 6,            //
};
// clang-format on

TEST(BareProgramFileTest, WriteWords) {
  absl::StatusOr<std::vector<uint16_t>> words =
      WriteBareProgramWords(MakeTestBareProgram());
  ASSERT_TRUE(words.ok()) << words.status();
  EXPECT_THAT(*words, ElementsAreArray(kTestBareProgramWords));
}

TEST(BareProgramFileTest, ReadWords) {
  absl::StatusOr<BareProgram> program =
      ReadBareProgramWords(kTestBareProgramWords);
  ASSERT_TRUE(program.ok()) << program.status();
  EXPECT_EQ(*program, MakeTestBareProgram());
}

TEST(BareProgramFileTest, EmptyProgram) {
  absl::StatusOr<std::vector<uint16_t>> words = WriteBareProgramWords({});
  ASSERT_TRUE(words.ok()) << words.status();
  EXPECT_THAT(*words, ElementsAre(0x424F, 1, 0, 0, 0, 0));
  absl::StatusOr<BareProgram> program = ReadBareProgramWords(*words);
  ASSERT_TRUE(program.ok()) << program.status();
  EXPECT_EQ(*program, BareProgram{});
}

TEST(BareProgramFileTest, LargestProgram) {
  BareProgram program = {.contents_words = 0xFFFE, .stack_words = 2};
  std::vector<uint16_t>& data = program.data.emplace_back().data;
  data.resize(0xFFFE);
  for (int i = 0; i < static_cast<int>(data.size()); ++i) {
    data[i] = static_cast<uint16_t>(i * 3);
  }

  absl::StatusOr<std::vector<uint16_t>> words = WriteBareProgramWords(program);
  ASSERT_TRUE(words.ok()) << words.status();
  absl::StatusOr<BareProgram> read_program = ReadBareProgramWords(*words);
  ASSERT_TRUE(read_program.ok()) << read_program.status();
  EXPECT_EQ(*read_program, program);
}

TEST(BareProgramFileTest, WriteFailsForInvalidProgram) {
  BareProgram program = MakeTestBareProgram();
  program.contents_words = 8;
  absl::StatusOr<std::vector<uint16_t>> words = WriteBareProgramWords(program);
  EXPECT_FALSE(words.ok());
  EXPECT_THAT(words.status().message(), HasSubstr("end of the contents"));
}

TEST(BareProgramFileTest, ReadFailsForProgramFile) {
  absl::StatusOr<BareProgram> program = ReadBareProgramWords(kTestProgramWords);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(),
              HasSubstr("Not a bare program file, but a program file"));
}

TEST(BareProgramFileTest, ReadFailsForNotBareProgramFile) {
  EXPECT_FALSE(ReadBareProgramWords({}).ok());
  std::vector<uint16_t> words = kTestBareProgramWords;
  words[0] = 0x4F42;
  absl::StatusOr<BareProgram> program = ReadBareProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_EQ(program.status().message(), "Not a bare program file");
}

TEST(BareProgramFileTest, ReadFailsForOtherVersion) {
  std::vector<uint16_t> words = kTestBareProgramWords;
  words[1] = 2;
  absl::StatusOr<BareProgram> program = ReadBareProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("version 2"));
}

TEST(BareProgramFileTest, ReadFailsForReservedWord) {
  std::vector<uint16_t> words = kTestBareProgramWords;
  words[5] = 1;
  absl::StatusOr<BareProgram> program = ReadBareProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("reserved word"));
}

TEST(BareProgramFileTest, ReadFailsForEveryTruncation) {
  for (int size = 0; size < static_cast<int>(kTestBareProgramWords.size());
       ++size) {
    absl::StatusOr<BareProgram> program = ReadBareProgramWords(
        absl::MakeConstSpan(kTestBareProgramWords).first(size));
    EXPECT_FALSE(program.ok()) << "Size: " << size;
  }
}

TEST(BareProgramFileTest, ReadFailsForBlockCountPastEnd) {
  std::vector<uint16_t> words = kTestBareProgramWords;
  words[4] = 0xFFFF;
  absl::StatusOr<BareProgram> program = ReadBareProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("ends before"));
}

TEST(BareProgramFileTest, ReadFailsForOffsetPastBank) {
  std::vector<uint16_t> words = kTestBareProgramWords;
  words[12] = 0x8000;  // The second block's offset.
  absl::StatusOr<BareProgram> program = ReadBareProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("past the end of a bank"));
}

TEST(BareProgramFileTest, ReadFailsForInvalidProgram) {
  // The blocks swapped, so they are out of order.
  const std::vector<uint16_t> words = {0x424F, 1, 12, 64, 2, 0, 4, 1,
                                       5,      6, 0,  2,  1, 2, 3, 4};
  absl::StatusOr<BareProgram> program = ReadBareProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(),
              HasSubstr("before the end of the block before it"));
}

TEST(BareProgramFileTest, ReadFailsForWordsAfterProgram) {
  std::vector<uint16_t> words = kTestBareProgramWords;
  words.push_back(0);
  absl::StatusOr<BareProgram> program = ReadBareProgramWords(words);
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("1 words after"));
}

TEST(BareProgramFileTest, WriteAndReadFile) {
  gb::FileSystem file_system;
  file_system.Register(std::make_unique<gb::MemoryFileProtocol>());
  absl::Status status =
      WriteBareProgramFile(file_system, "mem:/test.oz3", MakeTestBareProgram());
  ASSERT_TRUE(status.ok()) << status;

  std::vector<std::byte> bytes;
  ASSERT_TRUE(file_system.ReadFile("mem:/test.oz3", &bytes));
  ASSERT_EQ(bytes.size(), kTestBareProgramWords.size() * 2);

  // "OB", version 1, and 12 words of contents.
  const std::byte kHeaderBytes[] = {std::byte{'O'}, std::byte{'B'},
                                    std::byte{1},   std::byte{0},
                                    std::byte{12},  std::byte{0}};
  EXPECT_THAT(absl::MakeConstSpan(bytes).first(6),
              ElementsAreArray(kHeaderBytes));

  absl::StatusOr<BareProgram> program =
      ReadBareProgramFile(file_system, "mem:/test.oz3");
  ASSERT_TRUE(program.ok()) << program.status();
  EXPECT_EQ(*program, MakeTestBareProgram());
}

TEST(BareProgramFileTest, ReadFileFailsForMissingFile) {
  gb::FileSystem file_system;
  file_system.Register(std::make_unique<gb::MemoryFileProtocol>());
  EXPECT_FALSE(ReadBareProgramFile(file_system, "mem:/missing.oz3").ok());
}

TEST(BareProgramFileTest, ReadFileFailsForOddByteCount) {
  gb::FileSystem file_system;
  file_system.Register(std::make_unique<gb::MemoryFileProtocol>());
  const std::vector<std::byte> bytes = {std::byte{'O'}, std::byte{'B'},
                                        std::byte{1}};
  ASSERT_TRUE(file_system.WriteFile("mem:/test.oz3", bytes));
  absl::StatusOr<BareProgram> program =
      ReadBareProgramFile(file_system, "mem:/test.oz3");
  EXPECT_FALSE(program.ok());
  EXPECT_THAT(program.status().message(), HasSubstr("odd number of bytes"));
}

}  // namespace
}  // namespace oz3
