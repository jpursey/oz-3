// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/cli/microasm/oz3ism.h"

#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "gb/base/context_builder.h"
#include "gb/cli/cli_writer.h"
#include "gb/cli/fake_cli_writer.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/microasm/instruction_assembler.h"
#include "oz3/microasm/instruction_def_exporter.h"

namespace oz3 {
namespace {

using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::StartsWith;

// main()'s exit codes, as gb::RunCommandLine returns them.
constexpr int kExitSuccess = 0;
constexpr int kExitFailure = 1;
constexpr int kExitInvalidCommandLine = 2;

constexpr std::string_view kInstructionSet =
    "instruction(opcode:0) NOP { UL; }\n"
    "instruction(opcode:2) PUSH.W \"$v\" { LD(C0); UL; }\n";

class Oz3ismTest : public ::testing::Test {
 protected:
  void SetUp() override {
    ASSERT_TRUE(file_system_.Register(
        std::make_unique<gb::MemoryFileProtocol>(), "mem"));
    ASSERT_TRUE(file_system_.SetDefaultProtocol("mem"));
    WriteHost("mem:/set.izm", kInstructionSet);
  }

  // Runs oz3ism with `args`, as main() would, and returns its exit code. What
  // it writes is in writer_.
  int Run(std::vector<std::string_view> args) {
    return gb::RunCommandLine(gb::ContextBuilder()
                                  .SetPtr<gb::FileSystem>(&file_system_)
                                  .SetValue<gb::CliWriter>(writer_.GetWriter())
                                  .Build(),
                              &GetOz3ismCommand(), args);
  }

  // Runs oz3ism with `args`, expecting it to exit with `exit_code` and write
  // no help, and returns the error it writes.
  std::string RunError(int exit_code, std::vector<std::string_view> args) {
    EXPECT_EQ(Run(args), exit_code);
    EXPECT_THAT(writer_.TakeHelp(), IsEmpty());
    return writer_.TakeError();
  }

  // Returns the C++ that the instruction set assembler and exporter write for
  // the instruction set source at `path`, as oz3ism should.
  std::string ExpectedCpp(std::string_view path) {
    std::unique_ptr<AsmInstructionSet> instruction_set =
        AssembleInstructionSetFile(file_system_, path);
    if (instruction_set == nullptr) {
      ADD_FAILURE() << "Failed to assemble " << path;
      return {};
    }
    if (!ExportInstructionSetDef(file_system_, "mem:/expected.inc",
                                 instruction_set->GetInstructionSetDef())) {
      ADD_FAILURE() << "Failed to export " << path;
      return {};
    }
    return ReadHost("mem:/expected.inc");
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

TEST_F(Oz3ismTest, Help) {
  EXPECT_EQ(Run({"--help"}), kExitSuccess);
  const std::string help = writer_.TakeHelp();
  EXPECT_THAT(help,
              StartsWith("Usage: oz3ism [<flags>] <input file> <output file>"));
  EXPECT_THAT(help, HasSubstr("InstructionSetDef"));
  EXPECT_THAT(writer_.TakeError(), IsEmpty());
}

TEST_F(Oz3ismTest, NoOutputFile) {
  EXPECT_THAT(RunError(kExitInvalidCommandLine, {"mem:/set.izm"}),
              HasSubstr("<output file>"));
}

TEST_F(Oz3ismTest, InputFileMissing) {
  EXPECT_THAT(
      RunError(kExitInvalidCommandLine, {"mem:/missing.izm", "mem:/set.inc"}),
      HasSubstr("doesn't exist"));
  EXPECT_FALSE(HostExists("mem:/set.inc"));
}

TEST_F(Oz3ismTest, OutputFolderMissing) {
  EXPECT_THAT(
      RunError(kExitInvalidCommandLine, {"mem:/set.izm", "mem:/out/set.inc"}),
      HasSubstr("folder doesn't exist"));
}

//==============================================================================
// Assembling
//==============================================================================

TEST_F(Oz3ismTest, WritesInstructionSetDef) {
  EXPECT_EQ(Run({"mem:/set.izm", "mem:/set.inc"}), kExitSuccess);
  EXPECT_THAT(writer_.TakeError(), IsEmpty());
  EXPECT_THAT(writer_.TakeHelp(), IsEmpty());
  const std::string cpp = ReadHost("mem:/set.inc");
  EXPECT_THAT(cpp, HasSubstr("\"PUSH.W\""));
  EXPECT_EQ(cpp, ExpectedCpp("mem:/set.izm"));
}

TEST_F(Oz3ismTest, OverwritesOutputFile) {
  WriteHost("mem:/set.inc", "old");
  EXPECT_EQ(Run({"mem:/set.izm", "mem:/set.inc"}), kExitSuccess);
  EXPECT_EQ(ReadHost("mem:/set.inc"), ExpectedCpp("mem:/set.izm"));
}

TEST_F(Oz3ismTest, OutputFileIsInputFile) {
  // The paths are compared as normalized.
  EXPECT_THAT(
      RunError(kExitInvalidCommandLine, {"mem:/set.izm", "mem:/x/../set.izm"}),
      HasSubstr("\"mem:/set.izm\" is both the input file and the output file"));
  EXPECT_EQ(ReadHost("mem:/set.izm"), kInstructionSet);
}

//==============================================================================
// Errors
//==============================================================================

TEST_F(Oz3ismTest, ParseError) {
  WriteHost("mem:/bad.izm", "instruction(opcode:0) NOP { UL;\n");
  const std::string error =
      RunError(kExitFailure, {"mem:/bad.izm", "mem:/bad.inc"});
  EXPECT_THAT(error, StartsWith("Failed to assemble:\nmem:/bad.izm(1:"));
  EXPECT_FALSE(HostExists("mem:/bad.inc"));
}

TEST_F(Oz3ismTest, InstructionSetInvalid) {
  WriteHost("mem:/twice.izm",
            "instruction(opcode:0) NOP { UL; }\n"
            "instruction(opcode:1) nop { UL; }\n");
  EXPECT_THAT(RunError(kExitFailure, {"mem:/twice.izm", "mem:/twice.inc"}),
              HasSubstr("mem:/twice.izm(2:1): Instruction 'NOP' is already "
                        "defined"));
  EXPECT_FALSE(HostExists("mem:/twice.inc"));
}

TEST_F(Oz3ismTest, WriteFails) {
  // A read-only output file passes the command line's validators, so only
  // writing it fails.
  WriteHost("mem:/set.inc", "old");
  ASSERT_TRUE(
      file_system_.SetAttributes("mem:/set.inc", gb::FileAttribute::kReadOnly));
  EXPECT_EQ(RunError(kExitFailure, {"mem:/set.izm", "mem:/set.inc"}),
            "Failed to export instruction set to: mem:/set.inc");
  EXPECT_EQ(ReadHost("mem:/set.inc"), "old");
}

}  // namespace
}  // namespace oz3
