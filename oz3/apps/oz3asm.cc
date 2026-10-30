// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdio>
#include <filesystem>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/strings/str_format.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gb/file/local_file_protocol.h"
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

constexpr std::string_view kUsage =
    "Usage: oz3asm [-i <instruction set>] [-m] [-l] <source file> "
    "[<program file>]\n"
    "\n"
    "Assembles a program, and writes its program file, by default the source\n"
    "file's path with the extension .oz3.\n"
    "\n"
    "  -i <instruction set>  Assembles for the instruction set in this source\n"
    "                        file (.izm), rather than the default one.\n"
    "  -m                    Writes the program's map beside the program file\n"
    "                        (.oz3map).\n"
    "  -l                    Writes the program's listing beside the program\n"
    "                        file (.oz3lst).\n";

// What the command line asks for.
struct Options {
  std::string_view instruction_set_path;  // Empty for the default one.
  std::string_view source_path;
  std::string program_path;
  std::string map_path;      // Empty unless the map is written.
  std::string listing_path;  // Empty unless the listing is written.
};

// Returns `path` with its file's extension, if it has one, replaced by
// `extension` (which starts with a ".").
std::string ReplaceExtension(std::string_view path,
                             std::string_view extension) {
  return std::filesystem::path(path).replace_extension(extension).string();
}

// Reads the command line into `options`, or returns false if it isn't valid.
bool ReadOptions(absl::Span<const std::string_view> args, Options* options) {
  const int arg_count = static_cast<int>(args.size());
  std::vector<std::string_view> paths;
  bool write_map = false;
  bool write_listing = false;
  for (int i = 0; i < arg_count; ++i) {
    const std::string_view arg = args[i];
    if (arg == "-i") {
      if (i + 1 == arg_count || args[i + 1].empty() ||
          !options->instruction_set_path.empty()) {
        return false;
      }
      options->instruction_set_path = args[++i];
    } else if (arg == "-m") {
      write_map = true;
    } else if (arg == "-l") {
      write_listing = true;
    } else if (arg.starts_with('-')) {
      return false;
    } else {
      paths.push_back(arg);
    }
  }
  if (paths.empty() || paths.size() > 2) {
    return false;
  }
  options->source_path = paths[0];
  options->program_path = paths.size() == 2
                              ? std::string(paths[1])
                              : ReplaceExtension(paths[0], ".oz3");
  if (write_map) {
    options->map_path = ReplaceExtension(options->program_path, ".oz3map");
  }
  if (write_listing) {
    options->listing_path = ReplaceExtension(options->program_path, ".oz3lst");
  }
  return true;
}

// Returns whether the files the options write are each a different file, and
// not the source file, printing the error if they aren't.
bool ValidateOutputPaths(const Options& options) {
  std::vector<std::filesystem::path> paths = {
      std::filesystem::path(options.source_path).lexically_normal()};
  for (const std::string_view output :
       {std::string_view(options.program_path),
        std::string_view(options.map_path),
        std::string_view(options.listing_path)}) {
    if (output.empty()) {
      continue;
    }
    const std::filesystem::path path =
        std::filesystem::path(output).lexically_normal();
    if (absl::c_linear_search(paths, path)) {
      absl::FPrintF(stderr, "Writing %s would overwrite another file\n",
                    output);
      return false;
    }
    paths.push_back(path);
  }
  return true;
}

// Returns the syntax of the instruction set the options ask for. `asm_set`
// is set to the instruction set assembled from source, which the syntax
// needs, if there is one. Returns null after printing the error if it fails.
std::unique_ptr<InstructionSetSyntax> CreateSyntax(
    gb::FileSystem& file_system, const Options& options,
    std::unique_ptr<AsmInstructionSet>* asm_set) {
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
      absl::FPrintF(stderr, "%s\n", error.FormatMessage());
      return nullptr;
    }
    def = &(*asm_set)->GetInstructionSetDef();
    instruction_set = (*asm_set)->GetInstructionSet();
  }
  InstructionError error;
  std::unique_ptr<InstructionSetSyntax> syntax =
      InstructionSetSyntax::Create(*def, instruction_set.get(), &error);
  if (syntax == nullptr) {
    absl::FPrintF(stderr, "Instruction set can't be used: %s\n", error.message);
  }
  return syntax;
}

// Returns whether writing a file of the assembled program succeeded, from its
// `status`, printing the error if it didn't.
bool ReportWrite(std::string_view kind, const absl::Status& status) {
  if (!status.ok()) {
    absl::FPrintF(stderr, "Failed to write %s: %s\n", kind, status.message());
    return false;
  }
  return true;
}

bool Main(absl::Span<const std::string_view> args) {
  Options options;
  if (!ReadOptions(args, &options)) {
    absl::FPrintF(stderr, "%s", kUsage);
    return false;
  }
  if (!ValidateOutputPaths(options)) {
    return false;
  }

  gb::FileSystem file_system;
  file_system.Register(gb::LocalFileProtocol::Create("/"));
  file_system.SetDefaultProtocol("file");

  std::unique_ptr<AsmInstructionSet> asm_set;
  std::unique_ptr<InstructionSetSyntax> syntax =
      CreateSyntax(file_system, options, &asm_set);
  if (syntax == nullptr) {
    return false;
  }

  std::vector<gb::ParseError> errors;
  std::unique_ptr<AsmProgram> program =
      AssembleProgramFile(*syntax, file_system, options.source_path, &errors);
  if (program == nullptr) {
    for (const gb::ParseError& error : errors) {
      absl::FPrintF(stderr, "%s\n", error.FormatMessage());
    }
    return false;
  }

  if (!ReportWrite("program",
                   WriteProgramFile(file_system, options.program_path,
                                    program->GetProgram()))) {
    return false;
  }
  if (!options.map_path.empty() &&
      !ReportWrite("map", WriteProgramMapFile(file_system, options.map_path,
                                              program->GetMap()))) {
    return false;
  }
  if (!options.listing_path.empty() &&
      !ReportWrite("listing", WriteProgramListingFile(
                                  file_system, options.listing_path,
                                  program->GetProgram(), program->GetMap()))) {
    return false;
  }
  return true;
}

}  // namespace
}  // namespace oz3

int main(int argc, const char* argv[]) {
  std::vector<std::string_view> args(argv + 1, argv + argc);
  return oz3::Main(args) ? 0 : 1;
}
