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

  int data_end = 0;
  for (const Program::Segment::Data& data : program.data) {
    const int size = static_cast<int>(data.data.size());
    if (size == 0 || data.offset % kAlignment != 0 || size % kAlignment != 0) {
      return absl::InvalidArgumentError(
          absl::StrCat("A data block at offset ", data.offset, " has ", size,
                       " words, but both must be multiples of ", kAlignment,
                       ", and the size can't be 0"));
    }
    if (data.offset < data_end) {
      return absl::InvalidArgumentError(absl::StrCat(
          "A data block at offset ", data.offset,
          " is before the end of the block before it (", data_end, ")"));
    }
    data_end = data.offset + size;
    if (data_end > program.contents_words) {
      return absl::InvalidArgumentError(
          absl::StrCat("A data block at offset ", data.offset, " of ", size,
                       " words passes the end of the contents (",
                       program.contents_words, " words)"));
    }
  }
  return absl::OkStatus();
}

}  // namespace oz3
