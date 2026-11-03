// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
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
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/instruction_set.h"
#include "oz3/instruction_sets/default_instruction_set.h"
#include "oz3/tools/instruction_assembler.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program_assembler.h"
#include "oz3/tools/program_file.h"
#include "oz3/tools/program_listing.h"
#include "oz3/tools/program_map_file.h"

namespace oz3 {
namespace {

//==============================================================================
// Command line
//==============================================================================

const gb::CliFlag<std::string_view> kInstructionSet = {
    .name = "instruction-set",
    .short_name = 'i',
    .value_name = "instruction set",
    .help =
        "Assembles for the instruction set in this source file (.izm), "
        "rather than the default one.",
    .validators = {&gb::kCliFileExists},
};
const gb::CliFlag<bool> kMap = {
    .name = "map",
    .short_name = 'm',
    .help = "Writes the program's map beside the program file (.oz3map).",
};
const gb::CliFlag<bool> kListing = {
    .name = "listing",
    .short_name = 'l',
    .help = "Writes the program's listing beside the program file (.oz3lst).",
};
const gb::CliArg<std::string_view> kSourceFile = {
    .name = "source file",
    .help = "The program to assemble (.oza).",
    .validators = {&gb::kCliFileExists},
};
const gb::CliArg<std::string_view> kProgramFile = {
    .name = "program file",
    .help =
        "Where to write the program. Defaults to the source file's path "
        "with the extension .oz3.",
    .optional = true,
    .validators = {&gb::kCliFileWritable},
};

bool Run(gb::ValidatedContext& context, const gb::CliCommandLine& command_line);

const gb::CliCommand kProgram = {
    .name = "oz3asm",
    .help =
        "Assembles a program, and writes its program file. Paths are "
        "written the operating system's way, absolute or relative to the "
        "current directory.",
    .flags = {&kInstructionSet, &kMap, &kListing},
    .args = {&kSourceFile, &kProgramFile},
    .constraints = {&gb::kConstraintCliFileSystem},
    .run = Run,
};

//==============================================================================
// Running
//==============================================================================

// What the command line asks for. The paths of the source file and the files
// written are normalized (see gb::NormalizePath()).
struct Options {
  std::string_view instruction_set_path;  // Empty for the default one.
  std::string source_path;
  std::string program_path;
  std::string map_path;      // Empty unless the map is written.
  std::string listing_path;  // Empty unless the listing is written.
};

// Returns what the command line asks for.
Options ReadOptions(const gb::CliCommandLine& command_line) {
  Options options;
  options.instruction_set_path = command_line.Get(kInstructionSet).value_or("");

  // The validators of the source and program files check that their paths
  // normalize.
  options.source_path = gb::NormalizePath(*command_line.Get(kSourceFile));
  if (const std::optional<std::string_view> program_path =
          command_line.Get(kProgramFile);
      program_path.has_value()) {
    options.program_path = gb::NormalizePath(*program_path);
  } else {
    options.program_path =
        absl::StrCat(gb::RemoveExtension(options.source_path), ".oz3");
  }
  if (command_line.Get(kMap)) {
    options.map_path =
        absl::StrCat(gb::RemoveExtension(options.program_path), ".oz3map");
  }
  if (command_line.Get(kListing)) {
    options.listing_path =
        absl::StrCat(gb::RemoveExtension(options.program_path), ".oz3lst");
  }
  return options;
}

// Returns whether the paths of the files the options write are each different,
// and not the source file's. If they aren't, this sets the command line's usage
// error (gb::kKeyCliUsageError), as it is the command line's mistake.
bool ValidateOutputPaths(gb::ValidatedContext& context,
                         const Options& options) {
  struct File {
    std::string_view name;
    std::string_view path;
  };
  std::vector<File> files = {{"the source file", options.source_path}};
  for (const File& file : {File{"the program file", options.program_path},
                           File{"the map", options.map_path},
                           File{"the listing", options.listing_path}}) {
    if (file.path.empty()) {
      continue;
    }
    if (const auto it = absl::c_find_if(
            files, [&](const File& other) { return other.path == file.path; });
        it != files.end()) {
      context.SetValue<std::string>(gb::kKeyCliUsageError,
                                    absl::StrCat("\"", file.path, "\" is both ",
                                                 it->name, " and ", file.name));
      return false;
    }
    files.push_back(file);
  }
  return true;
}

// Returns the syntax of the instruction set the options ask for. `asm_set`
// is set to the instruction set assembled from source, which the syntax
// needs, if there is one. Returns null after writing the error if it fails.
std::unique_ptr<InstructionSetSyntax> CreateSyntax(
    const gb::CliWriter& writer, gb::FileSystem& file_system,
    const Options& options, std::unique_ptr<AsmInstructionSet>* asm_set) {
  const InstructionSetDef* def = nullptr;
  std::shared_ptr<const InstructionSet> instruction_set;
  if (options.instruction_set_path.empty()) {
    def = &GetDefaultInstructionSetDef();
    instruction_set = GetDefaultInstructionSet();
  } else {
    gb::ParseError error;
    *asm_set = AssembleInstructionSetFile(file_system,
                                          options.instruction_set_path, &error);
    if (*asm_set == nullptr) {
      writer(gb::CliText::kError, error.FormatMessage());
      return nullptr;
    }
    def = &(*asm_set)->GetInstructionSetDef();
    instruction_set = (*asm_set)->GetInstructionSet();
  }
  InstructionError error;
  std::unique_ptr<InstructionSetSyntax> syntax =
      InstructionSetSyntax::Create(*def, instruction_set.get(), &error);
  if (syntax == nullptr) {
    writer(gb::CliText::kError,
           absl::StrCat("Instruction set can't be used: ", error.message));
  }
  return syntax;
}

// Returns whether writing a file of the assembled program succeeded, from its
// `status`, writing the error if it didn't.
bool ReportWrite(const gb::CliWriter& writer, std::string_view kind,
                 const absl::Status& status) {
  if (!status.ok()) {
    writer(gb::CliText::kError,
           absl::StrCat("Failed to write ", kind, ": ", status.message()));
    return false;
  }
  return true;
}

bool Run(gb::ValidatedContext& context,
         const gb::CliCommandLine& command_line) {
  gb::FileSystem* file_system = context.GetPtr<gb::FileSystem>();
  const gb::CliWriter writer = *context.GetPtr<gb::CliWriter>();
  const Options options = ReadOptions(command_line);
  if (!ValidateOutputPaths(context, options)) {
    return false;
  }

  std::unique_ptr<AsmInstructionSet> asm_set;
  std::unique_ptr<InstructionSetSyntax> syntax =
      CreateSyntax(writer, *file_system, options, &asm_set);
  if (syntax == nullptr) {
    return false;
  }

  std::vector<gb::ParseError> errors;
  std::unique_ptr<AsmProgram> program =
      AssembleProgramFile(*syntax, *file_system, options.source_path, &errors);
  if (program == nullptr) {
    for (const gb::ParseError& error : errors) {
      writer(gb::CliText::kError, error.FormatMessage());
    }
    return false;
  }

  if (!ReportWrite(writer, "program",
                   WriteProgramFile(*file_system, options.program_path,
                                    program->GetProgram()))) {
    return false;
  }
  if (!options.map_path.empty() &&
      !ReportWrite(writer, "map",
                   WriteProgramMapFile(*file_system, options.map_path,
                                       program->GetMap()))) {
    return false;
  }
  if (!options.listing_path.empty() &&
      !ReportWrite(
          writer, "listing",
          WriteProgramListingFile(*file_system, options.listing_path,
                                  program->GetProgram(), program->GetMap()))) {
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
