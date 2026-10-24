// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_ASSEMBLER_H_
#define OZ3_TOOLS_PROGRAM_ASSEMBLER_H_

#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "gb/file/file_system.h"
#include "gb/parse/parse_error.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program.h"

namespace oz3 {

// The most errors a pass of the assembler reports. It stops after this many,
// with a last error saying so.
inline constexpr int kMaxProgramErrors = 100;

// An assembled program.
class AsmProgram final {
 public:
  AsmProgram(const AsmProgram&) = delete;
  AsmProgram& operator=(const AsmProgram&) = delete;
  ~AsmProgram() = default;

  // Returns the program, which is valid (see ValidateProgram).
  const Program& GetProgram() const { return program_; }

 private:
  friend class ProgramAssembler;

  AsmProgram() = default;

  Program program_;
};

// Assembles the program whose main source file is at `path`, for the
// instruction set `syntax` describes, as the wiki's Program Source page
// describes. Returns null if the source has errors. If `errors` isn't null, it
// is set to the errors, which are none if the program assembles.
std::unique_ptr<AsmProgram> AssembleProgramFile(
    const InstructionSetSyntax& syntax, gb::FileSystem& file_system,
    std::string_view path, std::vector<gb::ParseError>* errors = nullptr);

// Assembles a program from source text, as AssembleProgramFile does, except
// that an .INCLUDE is an error. Errors are in the file "program".
std::unique_ptr<AsmProgram> AssembleProgram(
    const InstructionSetSyntax& syntax, std::string text,
    std::vector<gb::ParseError>* errors = nullptr);

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_ASSEMBLER_H_
