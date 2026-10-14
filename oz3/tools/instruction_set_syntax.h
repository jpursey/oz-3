// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_INSTRUCTION_SET_SYNTAX_H_
#define OZ3_TOOLS_INSTRUCTION_SET_SYNTAX_H_

#include <array>
#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/instruction_def.h"
#include "oz3/core/instruction_set.h"
#include "oz3/tools/source_pattern.h"

namespace oz3 {

//==============================================================================
// InstructionForm
//==============================================================================

// One form of an instruction: the instruction with one of its macro's codes,
// or the instruction alone if it has no macro. This is one way a program can
// write the instruction.
class InstructionForm final {
 public:
  // An argument of the form: one of its placeholders, in the order its pattern
  // lists them.
  struct Arg {
    enum class Kind {
      kWordReg,    // $r, in the code word.
      kDwordReg,   // $R, in the code word.
      kCodeValue,  // $#, or a value set, in the code word.
      kWord,       // $v, in the word after the code word.
      kDword,      // $V, in the two words after the code word.
    };

    Kind kind = Kind::kWord;

    // For registers and code values, the number of bits it takes in the code
    // word, and the bit they start at.
    int bits = 0;
    int shift = 0;

    // For a code value, the value set that names its values, if it has one.
    const ValueSetDef* value_set = nullptr;

    // For a $v, whether it is a target address, which is encoded as its offset
    // from the next instruction. This is a $v that is a whole argument of a
    // relative instruction.
    bool relative = false;
  };

  InstructionForm(const InstructionForm&) = default;
  InstructionForm(InstructionForm&&) = default;
  InstructionForm& operator=(const InstructionForm&) = default;
  InstructionForm& operator=(InstructionForm&&) = default;
  ~InstructionForm() = default;

  // Returns the instruction, and the macro code if it has a macro.
  const InstructionDef& GetInstruction() const { return *instruction_; }
  const MacroCodeDef* GetMacroCode() const { return macro_code_; }

  // Returns the form's pattern: the instruction's source, with the macro
  // code's source in place of its $m (such as "$r, ($r + $v)").
  std::string_view GetPattern() const { return pattern_; }

  // Returns the parts of the pattern, with the macro code's parts in place of
  // the $m. A placeholder is a whole argument if it is one of the form's
  // comma-separated arguments, and each part's offset is in GetPattern().
  absl::Span<const SourcePattern::Part> GetParts() const { return parts_; }

  // Returns the form's arguments, one for each placeholder in GetParts(), in
  // the same order.
  absl::Span<const Arg> GetArgs() const { return args_; }

  // Returns the form's code word with every argument 0, and the mask of the
  // bits that say which form a code word is: its opcode, and its macro code's
  // prefix.
  uint16_t GetCodeWord() const { return code_word_; }
  uint16_t GetCodeWordMask() const { return code_word_mask_; }

  // Returns the size of the instruction in words: the code word, and the words
  // after it for each $v and $V.
  int GetSize() const { return size_; }

 private:
  friend class InstructionSetSyntaxBuilder;

  InstructionForm() = default;

  const InstructionDef* instruction_ = nullptr;
  const MacroCodeDef* macro_code_ = nullptr;
  std::string pattern_;
  std::vector<SourcePattern::Part> parts_;
  std::vector<Arg> args_;
  uint16_t code_word_ = 0;
  uint16_t code_word_mask_ = 0;
  int size_ = 0;
};

//==============================================================================
// InstructionSetSyntax
//==============================================================================

// Every form of every instruction in an instruction set, for matching
// statements to code words, and code words back to text.
//
// Creating it checks that the OZ-3 tools can use the instruction set, which
// CompileInstructionSet doesn't (see InstructionSetDef).
//
// This class is thread-safe, as it can't change once created.
class InstructionSetSyntax final {
 public:
  // Creates the syntax of `def`, which must outlive it.
  //
  // Checking `def` needs it compiled, which this does unless `instruction_set`
  // gives it already compiled from `def` (it is only used for the check, and
  // isn't kept). Returns null, and sets `error` if it isn't null, if `def`
  // doesn't compile, or the tools can't use it.
  static std::unique_ptr<InstructionSetSyntax> Create(
      const InstructionSetDef& def,
      const InstructionSet* instruction_set = nullptr,
      InstructionError* error = nullptr);

  InstructionSetSyntax(const InstructionSetSyntax&) = delete;
  InstructionSetSyntax& operator=(const InstructionSetSyntax&) = delete;
  ~InstructionSetSyntax() = default;

  // Returns every form, by instruction in the order of the def, then by macro
  // code in the order of the macro.
  absl::Span<const InstructionForm> GetForms() const { return forms_; }

  // Returns the forms of an instruction by its full name ("ADD.W"), or of
  // every instruction with a name without an extension ("ADD" for "ADD",
  // "ADD.W", and "ADD.D"), ignoring case. Returns none for an unknown name.
  absl::Span<const InstructionForm* const> GetForms(
      std::string_view name) const;

  // Returns the form of a code word, or null if it is no form (a code word no
  // instruction or macro code uses).
  const InstructionForm* FindForm(uint16_t code_word) const;

 private:
  friend class InstructionSetSyntaxBuilder;

  InstructionSetSyntax() = default;

  std::vector<InstructionForm> forms_;

  // Forms by upper case name (both full names, and names without extensions),
  // and by opcode.
  absl::flat_hash_map<std::string, std::vector<const InstructionForm*>>
      forms_by_name_;
  std::array<std::vector<const InstructionForm*>, 256> forms_by_op_;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_INSTRUCTION_SET_SYNTAX_H_
