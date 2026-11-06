// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/boot_rom.h"

#include <memory>
#include <string>
#include <vector>

#include "absl/log/check.h"
#include "absl/strings/str_cat.h"
#include "gb/parse/parse_error.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/instruction_sets/default_instruction_set.h"
#include "oz3/ozzy/boot_rom_source.h"
#include "oz3/ozzy/machine_table.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program.h"

namespace oz3 {

namespace {

// Returns the boot ROM assembled from its source. It is a bug for it not to
// assemble, or for its code to reach the machine table.
std::unique_ptr<AsmProgram> AssembleBootRom() {
  InstructionError error;
  std::unique_ptr<InstructionSetSyntax> syntax = InstructionSetSyntax::Create(
      GetDefaultInstructionSetDef(), GetDefaultInstructionSet().get(), &error);
  CHECK(syntax != nullptr) << error.message;
  std::vector<gb::ParseError> errors;
  std::unique_ptr<AsmProgram> boot_rom =
      AssembleProgram(*syntax, std::string(kBootRomSource.GetText()), &errors);
  std::string messages;
  for (const gb::ParseError& parse_error : errors) {
    absl::StrAppend(&messages, "\n", parse_error.FormatMessage());
  }
  CHECK(boot_rom != nullptr) << "The boot ROM doesn't assemble:" << messages;
  const Program& program = boot_rom->GetProgram();
  for (const Program::Segment::Data& data :
       program.segments[program.code_segment].data) {
    CHECK_LE(data.offset + data.data.size(), kMachineTableAddress)
        << "The boot ROM's code reaches the machine table";
  }
  return boot_rom;
}

}  // namespace

const AsmProgram& GetBootRom() {
  static const AsmProgram* const boot_rom = AssembleBootRom().release();
  return *boot_rom;
}

}  // namespace oz3
