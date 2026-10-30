// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_listing.h"

#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "gb/file/file_system.h"
#include "gb/file/memory_file_protocol.h"
#include "gb/parse/parse_error.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/tools/instruction_assembler.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program.h"
#include "oz3/tools/program_assembler.h"
#include "oz3/tools/program_map.h"

namespace oz3 {
namespace {

using ::testing::HasSubstr;
using ::testing::IsEmpty;

constexpr std::string_view kInstructionSet = R"---(
  instruction(opcode:0) NOP { UL; }
  instruction(opcode:2) PUSH.W "$v" { LD(C0); UL; }
  instruction(opcode:3) PUSH.D "$V" { LD(C0); LD(C1); UL; }
)---";

class ProgramListingTest : public ::testing::Test {
 protected:
  ProgramListingTest() {
    gb::ParseError error;
    asm_set_ = AssembleInstructionSet(std::string(kInstructionSet), &error);
    CHECK(asm_set_ != nullptr) << error.FormatMessage();
    InstructionError syntax_error;
    syntax_ = InstructionSetSyntax::Create(asm_set_->GetInstructionSetDef(),
                                           asm_set_->GetInstructionSet().get(),
                                           &syntax_error);
    CHECK(syntax_ != nullptr) << syntax_error.message;
    file_system_.Register(std::make_unique<gb::MemoryFileProtocol>());
  }

  // Assembles `text`, which must assemble, and returns it, or null if it
  // doesn't.
  std::unique_ptr<AsmProgram> Assemble(std::string_view text) {
    std::vector<gb::ParseError> errors;
    std::unique_ptr<AsmProgram> program =
        AssembleProgram(*syntax_, std::string(text), &errors);
    for (const gb::ParseError& error : errors) {
      ADD_FAILURE() << error.FormatMessage();
    }
    return program;
  }

  // Returns the listing of the program assembled from `text`, which must
  // assemble and list.
  std::string List(std::string_view text) {
    std::unique_ptr<AsmProgram> program = Assemble(text);
    if (program == nullptr) {
      return "";
    }
    absl::StatusOr<std::string> listing = FormatProgramListing(
        program->GetProgram(), program->GetMap(), {std::string(text)});
    if (!listing.ok()) {
      ADD_FAILURE() << listing.status();
      return "";
    }
    return *listing;
  }

  std::unique_ptr<AsmInstructionSet> asm_set_;
  std::unique_ptr<InstructionSetSyntax> syntax_;
  gb::FileSystem file_system_;
};

TEST_F(ProgramListingTest, DefaultProgram) {
  // Line breaks can be CRLF or CR, as the lexer reads them, and the last line
  // needs none. A label on a line of its own has its location.
  EXPECT_EQ(List("; A comment.\r\n"
                 ".STACK 4\r\n"
                 "start:\r"
                 "  NOP\r\n"
                 "  PUSH.W 5    ; Five.\r\n"
                 "\r\n"
                 "data: .DW 1, 2"),
            "program\n"
            "                           ; A comment.\n"
            "                           .STACK 4\n"
            "0000                       start:\n"
            "0000  0000                   NOP\n"
            "0001  0200 0005              PUSH.W 5    ; Five.\n"
            "\n"
            "0003  0001 0002            data: .DW 1, 2\n");
}

TEST_F(ProgramListingTest, Segments) {
  // Locations name their segments, and a line of more than four words carries
  // on in more rows.
  EXPECT_EQ(List(".STACK 4\n"
                 ".SEGMENT main CODE\n"
                 "  PUSH.D 12345678h\n"
                 ".SEGMENT text DATA ROM\n"
                 "msg: .DW 'Hello', 0\n"
                 ".SEGMENT stack STACK\n"),
            "program\n"
            "                                 .STACK 4\n"
            "                                 .SEGMENT main CODE\n"
            "MAIN:0000   0300 5678 1234         PUSH.D 12345678h\n"
            "                                 .SEGMENT text DATA ROM\n"
            "TEXT:0000   0048 0065 006C 006C  msg: .DW 'Hello', 0\n"
            "TEXT:0004   006F 0000\n"
            "                                 .SEGMENT stack STACK\n");
}

TEST_F(ProgramListingTest, LongSegmentNames) {
  // A name of more than eight characters is cut short, and locations are as
  // wide as the widest.
  EXPECT_EQ(List(".SEGMENT a\n"
                 "NOP\n"
                 ".SEGMENT interrupts\n"
                 "NOP\n"
                 ".SEGMENT eightchr\n"
                 "NOP\n"),
            "program\n"
            "                                    .SEGMENT a\n"
            "A:0000         0000                 NOP\n"
            "                                    .SEGMENT interrupts\n"
            "INTERRU~:0000  0000                 NOP\n"
            "                                    .SEGMENT eightchr\n"
            "EIGHTCHR:0000  0000                 NOP\n");
}

TEST_F(ProgramListingTest, LabelsWithoutWords) {
  // A label on a line that only reserves words has its location, and one at
  // the end of a full segment widens every location.
  EXPECT_EQ(List(".SEGMENT big\n"
                 "buf: .DS 0FFFFh\n"
                 "NOP\n"
                 "end:\n"),
            "program\n"
            "                                .SEGMENT big\n"
            "BIG:0000                        buf: .DS 0FFFFh\n"
            "BIG:FFFF   0000                 NOP\n"
            "BIG:10000                       end:\n");
}

TEST_F(ProgramListingTest, Files) {
  // Each file is listed, and a line included twice has a row for each time.
  // A listing file reads the files by the map's paths.
  ASSERT_TRUE(file_system_.WriteFile("mem:/main.oza",
                                     ".STACK 1\n"
                                     ".INCLUDE \"nop.oza\"\n"
                                     ".INCLUDE \"nop.oza\"\n"));
  ASSERT_TRUE(file_system_.WriteFile("mem:/nop.oza", "NOP\n"));
  std::vector<gb::ParseError> errors;
  std::unique_ptr<AsmProgram> program =
      AssembleProgramFile(*syntax_, file_system_, "mem:/main.oza", &errors);
  EXPECT_THAT(errors, IsEmpty());
  ASSERT_NE(program, nullptr);
  const absl::Status status =
      WriteProgramListingFile(file_system_, "mem:/main.oz3lst",
                              program->GetProgram(), program->GetMap());
  ASSERT_TRUE(status.ok()) << status;
  std::string listing;
  ASSERT_TRUE(file_system_.ReadFile("mem:/main.oz3lst", &listing));
  EXPECT_EQ(listing,
            "/main.oza\n"
            "                           .STACK 1\n"
            "                           .INCLUDE \"nop.oza\"\n"
            "                           .INCLUDE \"nop.oza\"\n"
            "\n"
            "/nop.oza\n"
            "0000  0000                 NOP\n"
            "0001  0000\n");
}

TEST_F(ProgramListingTest, FileErrors) {
  // A program assembled from text has no file to read.
  std::unique_ptr<AsmProgram> program = Assemble(".STACK 1\nNOP\n");
  ASSERT_NE(program, nullptr);
  absl::Status status =
      WriteProgramListingFile(file_system_, "mem:/program.oz3lst",
                              program->GetProgram(), program->GetMap());
  EXPECT_EQ(status.code(), absl::StatusCode::kNotFound);
  EXPECT_THAT(status.message(), HasSubstr("Could not read file: program"));

  ASSERT_TRUE(file_system_.WriteFile("mem:/main.oza", ".STACK 1\nNOP\n"));
  program = AssembleProgramFile(*syntax_, file_system_, "mem:/main.oza");
  ASSERT_NE(program, nullptr);
  status = WriteProgramListingFile(file_system_, "mem:/missing/main.oz3lst",
                                   program->GetProgram(), program->GetMap());
  EXPECT_EQ(status.code(), absl::StatusCode::kUnavailable);
  EXPECT_THAT(status.message(),
              HasSubstr("Could not write file: mem:/missing/main.oz3lst"));
}

TEST_F(ProgramListingTest, WrongSourceCount) {
  std::unique_ptr<AsmProgram> program = Assemble(".STACK 1\nNOP\n");
  ASSERT_NE(program, nullptr);
  absl::StatusOr<std::string> listing =
      FormatProgramListing(program->GetProgram(), program->GetMap(), {});
  EXPECT_EQ(listing.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(listing.status().message(), HasSubstr("0 sources for 1 files"));
}

TEST_F(ProgramListingTest, MapOfAnotherProgram) {
  std::unique_ptr<AsmProgram> program = Assemble(".STACK 1\nNOP\n");
  ASSERT_NE(program, nullptr);
  absl::StatusOr<std::string> listing =
      FormatProgramListing(Program{}, program->GetMap(), {".STACK 1\nNOP\n"});
  EXPECT_EQ(listing.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(listing.status().message(),
              HasSubstr("1 segments for a program with 0"));

  // The words of a statement must be in the program's data.
  Program other = program->GetProgram();
  other.segments[0].data.clear();
  listing = FormatProgramListing(other, program->GetMap(), {".STACK 1\nNOP\n"});
  EXPECT_EQ(listing.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(listing.status().message(),
              HasSubstr("no data for the statement at program:2"));
}

TEST_F(ProgramListingTest, LinesPastSource) {
  std::unique_ptr<AsmProgram> program = Assemble(".STACK 1\nNOP\nend:\n");
  ASSERT_NE(program, nullptr);
  absl::StatusOr<std::string> listing =
      FormatProgramListing(program->GetProgram(), program->GetMap(), {""});
  EXPECT_EQ(listing.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(listing.status().message(),
              HasSubstr("No source line for the statement at program:2"));

  listing = FormatProgramListing(program->GetProgram(), program->GetMap(),
                                 {".STACK 1\nNOP\n"});
  EXPECT_EQ(listing.status().code(), absl::StatusCode::kInvalidArgument);
  EXPECT_THAT(listing.status().message(),
              HasSubstr("No source line for label END at program:3"));
}

}  // namespace
}  // namespace oz3
