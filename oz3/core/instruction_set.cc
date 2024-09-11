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
    {kOp_NOP, {"NOP", kArgNone, kArgNone, "____"}, "UL.C;"},
    {kOp_HALT,
     {"HALT", kArgNone, kArgNone, "____"},
     "UL.C;"
     "HALT;"},
    {kOp_WAIT,
     {"WAIT", kArgWordRegA, kArgNone, "____"},
     "UL.C;"
     "WAIT(a);"},
};

}  // namespace

ArgTypeBits::ArgTypeBits(std::string_view arg_def) {
  if (arg_def == kArgWordRegA || arg_def == kArgWordRegB) {
    type = ArgType::kWordRegister;
    size = 3;
  } else if (arg_def == kArgDwordRegA || arg_def == kArgDwordRegB) {
    type = ArgType::kDwordRegister;
    size = 3;
  } else if (arg_def.size() == 2 && arg_def[0] == '#') {
    type = ArgType::kImmediate;
    size = arg_def[1] - '0';
    DCHECK(size > 0 && size < 9);
  }
}

uint16_t InstructionDef::Encode(uint16_t a, uint16_t b) const {
  uint16_t code = static_cast<uint16_t>(op) << 8;
  uint16_t args = 0;
  ArgTypeBits arg1bits(decl.arg1);
  if (arg1bits.size > 0) {
    args = a & ((1 << arg1bits.size) - 1);
  }
  ArgTypeBits arg2bits(decl.arg2);
  if (arg2bits.size > 0) {
    args |= (b & ((1 << arg2bits.size) - 1)) << arg1bits.size;
  }
  return code | args;
}

absl::Span<const InstructionDef> GetInstructionSet() { return kInstructionSet; }

}  // namespace oz3
