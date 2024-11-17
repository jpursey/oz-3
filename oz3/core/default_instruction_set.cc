// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/default_instruction_set.h"

#include "absl/log/log.h"
#include "oz3/core/instruction_compiler.h"

namespace oz3 {

namespace {

constexpr oz3::MacroCodeDef kMacroCode_LoadWord[] = {
    {.source = "$r",
     .prefix = {0, 2},
     .arg = {oz3::ArgType::kWordReg, 3},
     .code = "UL;MOV(p,m);"},
    {.source = "($r)",
     .prefix = {1, 2},
     .arg = {oz3::ArgType::kWordReg, 3},
     .code = "UL;LKR(m);ADR(m);LD(p);UL;"},
    {.source = "($r + $v)",
     .prefix = {2, 2},
     .arg = {oz3::ArgType::kWordReg, 3},
     .code = "LD(C0);UL;ADD(C0,m);LKR(m);ADR(C0);LD(p);UL;"},
    {.source = "$v", .prefix = {24, 5}, .code = "LD(p);UL;"},
    {.source = "C($v)",
     .prefix = {25, 5},
     .code = "LD(C0);UL;LK(CODE);ADR(C0);LD(p);UL;"},
    {.source = "S($v)",
     .prefix = {26, 5},
     .code = "LD(C0);UL;LK(STACK);ADR(C0);LD(p);UL;"},
    {.source = "D($v)",
     .prefix = {27, 5},
     .code = "LD(C0);UL;LK(DATA);ADR(C0);LD(p);UL;"},
    {.source = "E($v)",
     .prefix = {28, 5},
     .code = "LD(C0);UL;LK(EXTRA);ADR(C0);LD(p);UL;"},
    {.source = "(SP)",
     .prefix = {29, 5},
     .code = "UL;LKR(SP);ADR(SP);LD(p);UL;"},
    {.source = "(SP + $v)",
     .prefix = {30, 5},
     .code = "LD(C0);UL;ADD(C0,SP);LKR(SP);ADR(C0);LD(p);UL;"},
    {.source = "(FP + $v)",
     .prefix = {31, 5},
     .code = "LD(C0);UL;ADD(C0,FP);LKR(FP);ADR(C0);LD(p);UL;"},
};

constexpr oz3::MacroDef kMacros[] = {
    {.name = "LoadWord",
     .param = oz3::ArgType::kWordReg,
     .size = 5,
     .code = kMacroCode_LoadWord},
};

constexpr oz3::InstructionDef kInstructions[] = {
    {.op = 0, .op_name = "NOP", .code = "UL;"},
    {.op = 1, .op_name = "HALT", .code = "UL;HALT;"},
    {.op = 2,
     .op_name = "WAIT",
     .arg_source = "$r",
     .arg1 = {oz3::ArgType::kWordReg, 3},
     .code = "UL;WAIT(a);"},
    {.op = 3,
     .op_name = "MOV.LW",
     .arg_source = "$r, $m",
     .arg1 = {oz3::ArgType::kWordReg, 3},
     .arg2 = {oz3::ArgType::kMacro, 5},
     .code = "$LoadWord(a);"},
};

constexpr oz3::InstructionSetDef kInstructionSet = {
    .instructions = kInstructions, .macros = kMacros};

}  // namespace

const InstructionSetDef& GetDefaultInstructionSetDef() {
  return kInstructionSet;
}

std::shared_ptr<const InstructionSet> GetDefaultInstructionSet() {
  static std::shared_ptr<const InstructionSet> instruction_set =
      CompileInstructionSet(kInstructionSet);
  return instruction_set;
}

}  // namespace oz3
