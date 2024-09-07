// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_set.h"

#include "absl/types/span.h"
#include "glog/logging.h"

namespace oz3 {

namespace {

const InstructionDef kInstructionSet[] = {
    {
        Op::NOP,
        {"NOP", kArgNone, kArgNone, "____"},
        "UL.C;",
    },
    {
        Op::HALT,
        {"HALT", kArgNone, kArgNone, "____"},
        "UL.C;"
        "HALT;",
    },
    {
        Op::WAIT,
        {"WAIT", kArgWordRegA, kArgNone, "____"},
        "UL.C;"
        "WAIT(a);",
    },
};

}  // namespace

ArgTypeBits::ArgTypeBits(std::string_view arg_def) {
  if (arg_def == kArgWordRegA) {
    type = ArgType::kWordRegister;
    size = 3;
  } else if (arg_def == kArgWordRegB) {
    type = ArgType::kWordRegister;
    size = 3;
  } else if (arg_def == kArgDwordRegA) {
    type = ArgType::kDwordRegister;
    size = 3;
  } else if (arg_def == kArgDwordRegB) {
    type = ArgType::kDwordRegister;
    size = 3;
  } else if (arg_def.size() == 2 && arg_def[0] == '#') {
    type = ArgType::kImmediate;
    size = arg_def[1] - '0';
    DCHECK(size > 0 && size < 9);
  }
}

absl::Span<const InstructionDef> GetInstructionSet() { return kInstructionSet; }

}  // namespace oz3
