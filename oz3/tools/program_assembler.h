// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_ASSEMBLER_H_
#define OZ3_TOOLS_PROGRAM_ASSEMBLER_H_

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gb/parse/parse_error.h"
#include "oz3/program/bare_program.h"
#include "oz3/program/program.h"
#include "oz3/program/program_map.h"
#include "oz3/tools/instruction_set_syntax.h"

namespace oz3 {

// The most errors a pass of the assembler reports. It stops after this many,
// with a last error saying so. An error in a macro's use counts once, with the
// errors at its uses that follow it.
inline constexpr int kMaxProgramErrors = 100;

// An assembled program, and its map.
class AsmProgram final {
 public:
  AsmProgram(const AsmProgram&) = delete;
  AsmProgram& operator=(const AsmProgram&) = delete;
  ~AsmProgram() = default;

  // Returns the program, which is valid (see ValidateProgram).
  const Program& GetProgram() const { return program_; }

  // Returns the program as a bare program, which is valid (see
  // ValidateBareProgram), or an error if it can't be one: if it declares
  // segments, has a label in an address from BS or BE (which a bare program
  // starts elsewhere), or its contents, rounded up to a multiple of
  // Program::Segment::Data::kAlignment, and its stack pass the end of a bank.
  const absl::StatusOr<BareProgram>& GetBareProgram() const {
    return bare_program_;
  }

  // Returns the program's map: its source files, where each statement's words
  // are, and its labels and constants.
  const ProgramMap& GetMap() const { return map_; }

 private:
  friend class ProgramAssembler;

  AsmProgram() = default;

  Program program_;
  absl::StatusOr<BareProgram> bare_program_;
  ProgramMap map_;
};

// A constant the host gives a program, which the program sees as if it were
// defined above its first line, so .IF and .IFDEF can test it. The name is
// read as the source reads one, so it is case-insensitive.
struct AsmConstant {
  std::string name;
  uint32_t value = 0;
};

// Returns the constant `text` gives, written as "NAME" or "NAME=value", with
// the value a number as the source writes one (15, 0Fh, or 1111b), or 1 if
// it is left out. Returns an error if the value isn't a number that fits a
// dword. The name is only validated when the constant is assembled with.
absl::StatusOr<AsmConstant> ParseAsmConstant(std::string_view text);

// Assembles the program whose main source file is at `path`, for the
// instruction set `syntax` describes, as the manual's Program Source page
// describes. Returns null if the source has errors. If `errors` isn't null, it
// is set to the errors, which are none if the program assembles.
//
// `constants` are the host's constants for the program (see AsmConstant).
// Each must have a name, which isn't reserved (as a register's is), and that
// no other one has, or the program isn't assembled, with an error for each
// that has no location. A label or constant the program defines with one's
// name is an error there.
std::unique_ptr<AsmProgram> AssembleProgramFile(
    const InstructionSetSyntax& syntax, gb::FileSystem& file_system,
    std::string_view path, std::vector<gb::ParseError>* errors = nullptr);
std::unique_ptr<AsmProgram> AssembleProgramFile(
    const InstructionSetSyntax& syntax, gb::FileSystem& file_system,
    std::string_view path, absl::Span<const AsmConstant> constants,
    std::vector<gb::ParseError>* errors = nullptr);

// Assembles a program from source text, as AssembleProgramFile does, except
// that an .INCLUDE is an error. Errors are in the file "program".
std::unique_ptr<AsmProgram> AssembleProgram(
    const InstructionSetSyntax& syntax, std::string text,
    std::vector<gb::ParseError>* errors = nullptr);
std::unique_ptr<AsmProgram> AssembleProgram(
    const InstructionSetSyntax& syntax, std::string text,
    absl::Span<const AsmConstant> constants,
    std::vector<gb::ParseError>* errors = nullptr);

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_ASSEMBLER_H_
