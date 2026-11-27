// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/machine/boot_rom.h"

#include <memory>
#include <string>
#include <vector>

#include "absl/log/check.h"
#include "absl/strings/str_cat.h"
#include "gb/parse/parse_error.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/instructions/default_instruction_set.h"
#include "oz3/ozzy/machine/boot_rom_source.h"
#include "oz3/tools/instruction_set_syntax.h"

namespace oz3 {

namespace {

// Returns the boot ROM assembled from its source. It is a bug for it not to
// assemble, which it doesn't if its code reaches the machine table.
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
  return boot_rom;
}

}  // namespace

const AsmProgram& GetBootRom() {
  static const AsmProgram* const boot_rom = AssembleBootRom().release();
  return *boot_rom;
}

}  // namespace oz3
