// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <memory>
#include <string>
#include <string_view>

#include "absl/strings/str_cat.h"
#include "gb/base/context.h"
#include "gb/base/validated_context.h"
#include "gb/cli/cli.h"
#include "gb/cli/cli_command_line.h"
#include "gb/cli/cli_path.h"
#include "gb/cli/cli_writer.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "gb/file/local_file_protocol.h"
#include "gb/file/path.h"
#include "gb/parse/parse_error.h"
#include "oz3/tools/instruction_assembler.h"
#include "oz3/tools/instruction_def_exporter.h"

namespace oz3 {
namespace {

//==============================================================================
// Command line
//==============================================================================

const gb::CliArg<std::string_view> kInputFile = {
    .name = "input file",
    .help = "The instruction set source file to assemble (.izm).",
    .validators = {&gb::kCliFileExists},
};
const gb::CliArg<std::string_view> kOutputFile = {
    .name = "output file",
    .help =
        "Where to write the C++ for its InstructionSetDef, a snippet to "
        "include in a source file.",
    .validators = {&gb::kCliFileWritable},
};

bool Run(gb::ValidatedContext& context, const gb::CliCommandLine& command_line);

const gb::CliCommand kProgram = {
    .name = "oz3ism",
    .help =
        "Assembles an instruction set, and writes it as C++ for an "
        "InstructionSetDef. Paths are written the operating system's way, "
        "absolute or relative to the current directory.",
    .args = {&kInputFile, &kOutputFile},
    .constraints = {&gb::kConstraintCliFileSystem},
    .run = Run,
};

//==============================================================================
// Running
//==============================================================================

bool Run(gb::ValidatedContext& context,
         const gb::CliCommandLine& command_line) {
  gb::FileSystem* file_system = context.GetPtr<gb::FileSystem>();
  const gb::CliWriter writer = *context.GetPtr<gb::CliWriter>();
  const std::string_view input_path = *command_line.Get(kInputFile);
  const std::string_view output_path = *command_line.Get(kOutputFile);

  // The validators of both files check that their paths normalize.
  if (const std::string normalized_path = gb::NormalizePath(output_path);
      normalized_path == gb::NormalizePath(input_path)) {
    context.SetValue<std::string>(
        gb::kKeyCliUsageError,
        absl::StrCat("\"", normalized_path,
                     "\" is both the input file and the output file"));
    return false;
  }

  gb::ParseError error;
  std::unique_ptr<AsmInstructionSet> asm_instruction_set =
      AssembleInstructionSetFile(*file_system, input_path, &error);
  if (asm_instruction_set == nullptr) {
    writer(gb::CliText::kError,
           absl::StrCat("Failed to assemble:\n", error.FormatMessage()));
    return false;
  }

  if (!ExportInstructionSetDef(*file_system, output_path,
                               asm_instruction_set->GetInstructionSetDef())) {
    writer(gb::CliText::kError,
           absl::StrCat("Failed to export instruction set to: ", output_path));
    return false;
  }
  return true;
}

}  // namespace
}  // namespace oz3

int main(int argc, char* argv[]) {
  gb::FileSystem file_system;
  file_system.Register(gb::LocalFileProtocol::CreateOsRoot());
  file_system.SetDefaultProtocol("file");
  gb::Context context;
  context.SetPtr<gb::FileSystem>(&file_system);
  return gb::RunCommandLine(&context, &oz3::kProgram,
                            gb::GetMainArgs(argc, argv));
}
