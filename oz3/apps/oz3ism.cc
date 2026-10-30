// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <iostream>
#include <string_view>
#include <vector>

#include "absl/types/span.h"
#include "gb/base/context_builder.h"
#include "gb/file/file_system.h"
#include "gb/file/local_file_protocol.h"
#include "oz3/tools/instruction_assembler.h"
#include "oz3/tools/instruction_def_exporter.h"

namespace oz3 {

bool Main(absl::Span<const std::string_view> args) {
  if (args.size() != 2) {
    std::cerr << "Usage: oz3ism <input file> <output file>" << std::endl;
    return false;
  }

  gb::FileSystem file_system;
  file_system.Register(gb::LocalFileProtocol::Create(
      gb::ContextBuilder().SetValue<std::string>("root", "/").Build()));
  file_system.SetDefaultProtocol("file");

  gb::ParseError error;
  auto asm_instruction_set =
      AssembleInstructionSetFile(file_system, args[0], &error);
  if (asm_instruction_set == nullptr) {
    std::cerr << "Failed to assemble:\n" << error.FormatMessage() << std::endl;
    return false;
  }

  if (!ExportInstructionSetDef(file_system, args[1],
                               asm_instruction_set->GetInstructionSetDef())) {
    std::cerr << "Failed to export instruction set to: " << args[1]
              << std::endl;
    return false;
  }

  return true;
}

}  // namespace oz3

int main(int argc, const char* argv[]) {
  std::vector<std::string_view> args(argc - 1);
  for (int i = 1; i < argc; ++i) {
    args[i - 1] = argv[i];
  }
  return oz3::Main(args) ? 0 : -1;
}
