// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_BARE_PROGRAM_H_
#define OZ3_TOOLS_BARE_PROGRAM_H_

#include <vector>

#include "absl/status/status.h"
#include "oz3/tools/program.h"

namespace oz3 {

// A bare program is a default program (one that declares no segments) with
// none of a Program's page structure: one block of memory, holding its
// contents (code and data) from its start and its stack after them, which
// loads at any word of a bank, as the manual's Program Files page describes.
//
// Loading it at an address copies each data block's words to that address
// plus the block's offset, and leaves every other word as it is.
struct BareProgram {
  // The size of its contents, in words, which is a multiple of
  // Program::Segment::Data::kAlignment.
  int contents_words = 0;

  // The size of its stack, in words, after its contents. Each size fits in a
  // word, and contents and stack together are at most a bank,
  // kMemoryBankMaxSize words.
  int stack_words = 0;

  // The words it sets, in blocks in order of their offsets, which each follow
  // the rules for a Program::Segment's data, within its contents.
  std::vector<Program::Segment::Data> data;

  bool operator==(const BareProgram&) const = default;
};

// Returns an error naming the first rule above that `program` breaks, or OK if
// it is a valid bare program, which a bare program file can hold.
absl::Status ValidateBareProgram(const BareProgram& program);

}  // namespace oz3

#endif  // OZ3_TOOLS_BARE_PROGRAM_H_
