// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/bare_program.h"

#include "absl/strings/str_cat.h"
#include "oz3/core/core_types.h"

namespace oz3 {

absl::Status ValidateBareProgram(const BareProgram& program) {
  constexpr int kAlignment = Program::Segment::Data::kAlignment;
  if (program.contents_words < 0 || program.contents_words > 0xFFFF ||
      program.contents_words % kAlignment != 0) {
    return absl::InvalidArgumentError(
        absl::StrCat("The contents are ", program.contents_words,
                     " words, which must be from 0 to ", 0xFFFF,
                     ", and a multiple of ", kAlignment));
  }
  if (program.stack_words < 0 || program.stack_words > 0xFFFF) {
    return absl::InvalidArgumentError(
        absl::StrCat("The stack is ", program.stack_words,
                     " words, which must be from 0 to ", 0xFFFF));
  }
  if (program.contents_words + program.stack_words > kMemoryBankMaxSize) {
    return absl::InvalidArgumentError(absl::StrCat(
        "The contents (", program.contents_words, " words) and stack (",
        program.stack_words, " words) pass the end of a bank, at ",
        kMemoryBankMaxSize, " words"));
  }

  return ValidateSegmentData(program.data, program.contents_words,
                             "The program", "contents");
}

}  // namespace oz3
