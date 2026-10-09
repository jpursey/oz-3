// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/instruction_sets/default_instruction_set.h"

#include <cstdint>
#include <string>
#include <string_view>

#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/instruction_def.h"

namespace oz3 {
namespace {

//==============================================================================
// Pinned code words
//
// Programs and their source depend on the default instruction set's code words
// and syntax, so neither ever changes for an instruction that has been pinned.
// They are pinned by a fingerprint of everything that determines them (see
// EncodingLayout), which leaves out the microcode, so it may change freely.
//
// New instructions go after the last pinned opcode, and don't change the
// fingerprint. They are pinned too by raising kPinnedInstructionCount and
// updating kPinnedFingerprint.
//==============================================================================

// Instructions with opcodes below this have pinned code words.
constexpr int kPinnedInstructionCount = 0x8A;

constexpr uint64_t kPinnedFingerprint = 0x01146DE15F191B06;

std::string ArgumentLayout(const Argument& arg) {
  return absl::StrCat(ArgTypeToString(arg.type), ":",
                      static_cast<int>(arg.size));
}

// Returns everything about an instruction that determines its code words and
// syntax, including the codes of a macro argument. The macro's name is left
// out, as it doesn't affect either.
std::string EncodingLayout(const InstructionSetDef& def,
                           const InstructionDef& instruction) {
  std::string layout = absl::StrCat(
      static_cast<int>(instruction.op), " ", instruction.op_name, " \"",
      instruction.source, "\" ", ArgumentLayout(instruction.arg1), " ",
      ArgumentLayout(instruction.arg2));
  if (instruction.arg1.type != ArgType::kMacro &&
      instruction.arg2.type != ArgType::kMacro) {
    return layout;
  }
  for (const MacroDef& macro : def.macros) {
    if (macro.name != instruction.arg_macro_name) {
      continue;
    }
    absl::StrAppend(&layout, " macro:", macro.size);
    for (const MacroCodeDef& code : macro.code) {
      absl::StrAppend(&layout, " \"", code.source, "\" ",
                      static_cast<int>(code.prefix.value), "/",
                      static_cast<int>(code.prefix.size), " ",
                      ArgumentLayout(code.arg));
    }
  }
  return layout;
}

// Returns the 64-bit FNV-1a hash of the text, which is the same in every build.
uint64_t Fingerprint(std::string_view text) {
  uint64_t hash = 0xCBF29CE484222325;
  for (char c : text) {
    hash ^= static_cast<uint8_t>(c);
    hash *= 0x100000001B3;
  }
  return hash;
}

const InstructionDef* FindInstruction(
    absl::Span<const InstructionDef> instructions, int op) {
  for (const InstructionDef& instruction : instructions) {
    if (instruction.op == op) {
      return &instruction;
    }
  }
  return nullptr;
}

//==============================================================================
// Tests
//==============================================================================

TEST(DefaultInstructionSetTest, InstructionSetCompiles) {
  InstructionError error;
  EXPECT_TRUE(CompileInstructionSet(GetDefaultInstructionSetDef(), &error))
      << error.message;
}

TEST(DefaultInstructionSetTest, PinnedCodeWordsNeverChange) {
  const InstructionSetDef& def = GetDefaultInstructionSetDef();
  std::string layout;
  for (int op = 0; op < kPinnedInstructionCount; ++op) {
    const InstructionDef* instruction = FindInstruction(def.instructions, op);
    ASSERT_NE(instruction, nullptr) << "Opcode " << op << " was removed";
    absl::StrAppend(&layout, EncodingLayout(def, *instruction), "\n");
  }
  EXPECT_EQ(Fingerprint(layout), kPinnedFingerprint)
      << "A pinned code word or its syntax changed. Programs depend on them, "
         "so they never change: see `git diff` of default_instruction_set.inc "
         "for what moved. New instructions go after the last pinned opcode, "
      << kPinnedInstructionCount - 1 << ". Fingerprint: "
      << absl::StrCat("0x", absl::Hex(Fingerprint(layout), absl::kZeroPad16));
}

}  // namespace
}  // namespace oz3
