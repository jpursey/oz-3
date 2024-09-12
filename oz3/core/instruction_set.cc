// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_set.h"

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

absl::Span<const InstructionDef> GetInstructionSet() { return kInstructionSet; }

}  // namespace oz3
