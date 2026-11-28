// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/cli/asm/oz3asm.h"

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/statusor.h"
#include "gb/base/context_builder.h"
#include "gb/cli/cli_writer.h"
#include "gb/cli/fake_cli_writer.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/program/bare_program.h"
#include "oz3/program/program.h"
#include "oz3/program/program_file.h"
#include "oz3/program/program_map.h"
#include "oz3/program/program_map_file.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::StartsWith;

// main()'s exit codes, as gb::RunCommandLine returns them.
constexpr int kExitSuccess = 0;
constexpr int kExitFailure = 1;
constexpr int kExitInvalidCommandLine = 2;

// An instruction set small enough that a program's code words are plain: NOP
// is 0000h, and PUSH.W v is 0200h then v.
constexpr std::string_view kInstructionSet =
    "instruction(opcode:0) NOP { UL; }\n"
    "instruction(opcode:2) PUSH.W \"$v\" { LD(C0); UL; }\n";

// A program for kInstructionSet, and the words of its one data block: its
// code, padded to Program::Segment::Data::kAlignment.
constexpr std::string_view kSource =
    ".STACK 4\n"
    "start:\n"
    "  NOP\n"
    "  PUSH.W 5\n";
constexpr uint16_t kSourceWords[] = {0x0000, 0x0200, 0x0005, 0};

class Oz3asmTest : public ::testing::Test {
 protected:
  void SetUp() override {
    ASSERT_TRUE(file_system_.Register(
        std::make_unique<gb::MemoryFileProtocol>(), "mem"));
    ASSERT_TRUE(file_system_.SetDefaultProtocol("mem"));
    WriteHost("mem:/set.izm", kInstructionSet);
    WriteHost("mem:/prog.oza", kSource);
  }

  // Runs oz3asm with `args`, as main() would, and returns its exit code. What
  // it writes is in writer_.
  int Run(std::vector<std::string_view> args) {
    return gb::RunCommandLine(gb::ContextBuilder()
                                  .SetPtr<gb::FileSystem>(&file_system_)
                                  .SetValue<gb::CliWriter>(writer_.GetWriter())
                                  .Build(),
                              &GetOz3asmCommand(), args);
  }

  // Runs oz3asm with `args`, expecting it to succeed and write nothing.
  void RunOk(std::vector<std::string_view> args) {
    EXPECT_EQ(Run(args), kExitSuccess) << writer_.TakeError();
    EXPECT_THAT(writer_.TakeError(), IsEmpty());
    EXPECT_THAT(writer_.TakeHelp(), IsEmpty());
  }

  // Runs oz3asm with `args`, expecting it to exit with `exit_code` and write
  // no help, and returns the error it writes.
  std::string RunError(int exit_code, std::vector<std::string_view> args) {
    EXPECT_EQ(Run(args), exit_code);
    EXPECT_THAT(writer_.TakeHelp(), IsEmpty());
    return writer_.TakeError();
  }

  // Returns the words of the one data block of the program file at `path`,
  // whose program has one segment.
  std::vector<uint16_t> ReadCode(std::string_view path) {
    absl::StatusOr<Program> program = ReadProgramFile(file_system_, path);
    if (!program.ok()) {
      ADD_FAILURE() << program.status();
      return {};
    }
    if (program->segments.size() != 1 ||
        program->segments[0].data.size() != 1) {
      ADD_FAILURE() << "Expected one segment with one data block";
      return {};
    }
    return program->segments[0].data[0].data;
  }

  // Returns the bytes of the host file at `path`.
  std::string ReadHost(std::string_view path) {
    std::string bytes;
    EXPECT_TRUE(file_system_.ReadFile(path, &bytes)) << path;
    return bytes;
  }

  // Writes `bytes` to the host file at `path`.
  void WriteHost(std::string_view path, std::string_view bytes) {
    ASSERT_TRUE(file_system_.WriteFile(path, bytes)) << path;
  }

  // Returns whether the host file at `path` exists.
  bool HostExists(std::string_view path) {
    return file_system_.IsValidFile(path);
  }

  gb::FileSystem file_system_;
  gb::FakeCliWriter writer_;
};

//==============================================================================
// Help and usage
//==============================================================================

TEST_F(Oz3asmTest, Help) {
  EXPECT_EQ(Run({"--help"}), kExitSuccess);
  const std::string help = writer_.TakeHelp();
  EXPECT_THAT(help, StartsWith("Usage: oz3asm [<flags>] <source file> "
                               "[<program file>]"));
  for (std::string_view flag :
       {"--instruction-set", "--define", "--bare", "--map", "--listing"}) {
    EXPECT_THAT(help, HasSubstr(flag));
  }
  EXPECT_THAT(writer_.TakeError(), IsEmpty());
}

TEST_F(Oz3asmTest, NoSourceFile) {
  EXPECT_THAT(RunError(kExitInvalidCommandLine, {}),
              HasSubstr("<source file>"));
}

TEST_F(Oz3asmTest, SourceFileMissing) {
  EXPECT_THAT(RunError(kExitInvalidCommandLine, {"mem:/missing.oza"}),
              HasSubstr("doesn't exist"));
}

TEST_F(Oz3asmTest, InstructionSetMissing) {
  EXPECT_THAT(RunError(kExitInvalidCommandLine,
                       {"-i", "mem:/missing.izm", "mem:/prog.oza"}),
              HasSubstr("doesn't exist"));
}

//==============================================================================
// Assembling
//==============================================================================

TEST_F(Oz3asmTest, DefaultInstructionSet) {
  WriteHost("mem:/nop.oza", ".STACK 4\nNOP\n");
  RunOk({"mem:/nop.oza"});
  EXPECT_TRUE(ReadProgramFile(file_system_, "mem:/nop.oz3").ok());
  EXPECT_FALSE(HostExists("mem:/nop.oz3map"));
  EXPECT_FALSE(HostExists("mem:/nop.oz3lst"));
}

TEST_F(Oz3asmTest, ProgramFileBesideSource) {
  RunOk({"-i", "mem:/set.izm", "mem:/prog.oza"});
  EXPECT_THAT(ReadCode("mem:/prog.oz3"), ElementsAreArray(kSourceWords));
}

TEST_F(Oz3asmTest, ProgramFileNamed) {
  RunOk({"-i", "mem:/set.izm", "mem:/prog.oza", "mem:/named.bin"});
  EXPECT_THAT(ReadCode("mem:/named.bin"), ElementsAreArray(kSourceWords));
  EXPECT_FALSE(HostExists("mem:/prog.oz3"));
}

TEST_F(Oz3asmTest, ProgramFolderMissing) {
  EXPECT_THAT(
      RunError(kExitInvalidCommandLine,
               {"-i", "mem:/set.izm", "mem:/prog.oza", "mem:/out/named.oz3"}),
      HasSubstr("folder doesn't exist"));
}

TEST_F(Oz3asmTest, BareProgram) {
  RunOk({"--instruction-set", "mem:/set.izm", "--bare", "mem:/prog.oza"});
  absl::StatusOr<BareProgram> program =
      ReadBareProgramFile(file_system_, "mem:/prog.oz3");
  ASSERT_TRUE(program.ok()) << program.status();
  EXPECT_EQ(program->stack_words, 4);
  ASSERT_EQ(program->data.size(), 1);
  EXPECT_THAT(program->data[0].data, ElementsAreArray(kSourceWords));
}

TEST_F(Oz3asmTest, BareProgramWithSegments) {
  WriteHost("mem:/segs.oza", ".SEGMENT main CODE\nNOP\n");
  EXPECT_THAT(
      RunError(kExitFailure, {"-i", "mem:/set.izm", "-b", "mem:/segs.oza"}),
      StartsWith("mem:/segs.oza: "));
  EXPECT_FALSE(HostExists("mem:/segs.oz3"));
}

TEST_F(Oz3asmTest, MapAndListing) {
  RunOk({"-i", "mem:/set.izm", "-m", "-l", "mem:/prog.oza", "mem:/p.oz3"});
  EXPECT_THAT(ReadCode("mem:/p.oz3"), ElementsAreArray(kSourceWords));
  absl::StatusOr<ProgramMap> map =
      ReadProgramMapFile(file_system_, "mem:/p.oz3map");
  ASSERT_TRUE(map.ok()) << map.status();
  ASSERT_EQ(map->GetLabels().size(), 1);
  EXPECT_EQ(map->GetLabels()[0].name, "START");
  EXPECT_EQ(ReadHost("mem:/p.oz3lst"),
            "mem:/prog.oza\n"
            "                           .STACK 4\n"
            "0000                       start:\n"
            "0000  0000                   NOP\n"
            "0001  0200 0005              PUSH.W 5\n");
}

TEST_F(Oz3asmTest, Define) {
  WriteHost("mem:/define.oza", ".STACK 4\nPUSH.W A\nPUSH.W B\nPUSH.W C\n");
  RunOk({"-i", "mem:/set.izm", "-D", "A=12h", "--define", "B", "-D", "C=0FFFFh",
         "mem:/define.oza"});
  EXPECT_THAT(ReadCode("mem:/define.oz3"),
              ElementsAre(0x0200, 0x0012, 0x0200, 0x0001, 0x0200, 0xFFFF));
}

TEST_F(Oz3asmTest, DefineInvalid) {
  EXPECT_THAT(RunError(kExitInvalidCommandLine,
                       {"-i", "mem:/set.izm", "-D", "A=zz", "mem:/prog.oza"}),
              HasSubstr("The value of A isn't a number"));
}

TEST_F(Oz3asmTest, DefineNotAName) {
  EXPECT_THAT(RunError(kExitFailure,
                       {"-i", "mem:/set.izm", "-D", "1A", "mem:/prog.oza"}),
              HasSubstr("\"1A\" isn't a name"));
  EXPECT_FALSE(HostExists("mem:/prog.oz3"));
}

//==============================================================================
// Paths that collide
//==============================================================================

TEST_F(Oz3asmTest, ProgramFileIsSourceFile) {
  EXPECT_THAT(
      RunError(kExitInvalidCommandLine,
               {"-i", "mem:/set.izm", "mem:/prog.oza", "mem:/prog.oza"}),
      HasSubstr("\"mem:/prog.oza\" is both the source file and the "
                "program file"));
  EXPECT_EQ(ReadHost("mem:/prog.oza"), kSource);
}

TEST_F(Oz3asmTest, MapIsProgramFile) {
  EXPECT_THAT(
      RunError(kExitInvalidCommandLine,
               {"-i", "mem:/set.izm", "-m", "mem:/prog.oza", "mem:/p.oz3map"}),
      HasSubstr("\"mem:/p.oz3map\" is both the program file and the map"));
  EXPECT_FALSE(HostExists("mem:/p.oz3map"));
}

TEST_F(Oz3asmTest, ListingIsSourceFile) {
  WriteHost("mem:/p.oz3lst", kSource);
  EXPECT_THAT(
      RunError(kExitInvalidCommandLine,
               {"-i", "mem:/set.izm", "-l", "mem:/p.oz3lst", "mem:/p.oz3"}),
      HasSubstr("\"mem:/p.oz3lst\" is both the source file and the listing"));
  EXPECT_FALSE(HostExists("mem:/p.oz3"));
}

//==============================================================================
// Errors in writing
//==============================================================================

TEST_F(Oz3asmTest, ListingWriteFails) {
  // The listing's path is a folder, so only writing it fails, after the
  // program file and map are written.
  ASSERT_TRUE(file_system_.CreateFolder("mem:/p.oz3lst"));
  EXPECT_THAT(RunError(kExitFailure, {"-i", "mem:/set.izm", "-m", "-l",
                                      "mem:/prog.oza", "mem:/p.oz3"}),
              StartsWith("Failed to write listing: "));
  EXPECT_THAT(ReadCode("mem:/p.oz3"), ElementsAreArray(kSourceWords));
  EXPECT_TRUE(HostExists("mem:/p.oz3map"));
}

//==============================================================================
// Errors in source
//==============================================================================

TEST_F(Oz3asmTest, ProgramErrors) {
  // Each error is written, in order.
  WriteHost("mem:/bad.oza", "  BOGUS R9\n");
  const std::string error =
      RunError(kExitFailure, {"-i", "mem:/set.izm", "mem:/bad.oza"});
  EXPECT_THAT(error, HasSubstr("mem:/bad.oza(1:3): Unknown instruction BOGUS"));
  EXPECT_THAT(error, HasSubstr("mem:/bad.oza(1:1): The program has a stack"));
  EXPECT_LT(error.find("(1:3)"), error.find("(1:1)"));
  EXPECT_FALSE(HostExists("mem:/bad.oz3"));
}

TEST_F(Oz3asmTest, InstructionSetParseError) {
  WriteHost("mem:/bad.izm", "instruction(opcode:0) NOP { UL;\n");
  EXPECT_THAT(RunError(kExitFailure, {"-i", "mem:/bad.izm", "mem:/prog.oza"}),
              HasSubstr("mem:/bad.izm(1:"));
  EXPECT_FALSE(HostExists("mem:/prog.oz3"));
}

TEST_F(Oz3asmTest, InstructionSetInvalid) {
  WriteHost("mem:/twice.izm",
            "instruction(opcode:0) NOP { UL; }\n"
            "instruction(opcode:1) nop { UL; }\n");
  EXPECT_THAT(RunError(kExitFailure, {"-i", "mem:/twice.izm", "mem:/prog.oza"}),
              HasSubstr("mem:/twice.izm(2:1): Instruction 'NOP' is already "
                        "defined"));
  EXPECT_FALSE(HostExists("mem:/prog.oz3"));
}

}  // namespace
}  // namespace oz3
