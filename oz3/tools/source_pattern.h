// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_SOURCE_PATTERN_H_
#define OZ3_TOOLS_SOURCE_PATTERN_H_

#include <string_view>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "gb/parse/symbol.h"
#include "gb/parse/token.h"
#include "oz3/core/instruction_def.h"

namespace oz3 {

// A source pattern, parsed: how the arguments of an instruction, or of a macro
// code, are written in a program (InstructionDef::source and
// MacroCodeDef::source).
//
// Parsing checks the rules every pattern follows: its syntax, that each
// placeholder is a valid one or names a value set, how many code word
// arguments it has, and that no value is followed by a binary operator (see
// CheckValueEnds()).
class SourcePattern final {
 public:
  // One part of a pattern: literal text, or a placeholder.
  struct Part {
    enum class Kind {
      kLiteral,   // An identifier, number, string, or symbol, written as is.
      kArgument,  // A code word argument: $r, $R, $#, $m, or a value set.
      kWord,      // $v
      kDword,     // $V
    };

    Kind kind = Kind::kLiteral;

    // Where the part is in the source text, as an offset and size in
    // characters.
    int offset = 0;
    int size = 0;

    // For kLiteral, the token's type and value, such as kTokenSymbol and ','
    // for the commas between arguments.
    gb::TokenType literal_type = gb::kTokenNone;
    gb::TokenValue literal;

    // For kArgument, the argument it is. A register without a size has its
    // type's default size, and a $m without one has size 0, as its size is
    // its macro's.
    Argument arg;

    // For kArgument, the value set it names, if it is one. The set is one of
    // those passed when parsing.
    const ValueSetDef* value_set = nullptr;

    // For a placeholder, whether it is all of one of the pattern's
    // comma-separated arguments, such as the "$v" in "$r, $v", but not in
    // "D($v)".
    bool whole_arg = false;

    // Returns true if a program writes the part as an expression: a $#, a
    // value set, $v, $V, or a literal number.
    bool IsValue() const {
      return kind == Kind::kWord || kind == Kind::kDword ||
             (kind == Kind::kArgument && arg.type == ArgType::kImmediate) ||
             (kind == Kind::kLiteral && literal_type == gb::kTokenInt);
    }
  };

  // Parses an instruction's source, which has at most two code word arguments,
  // of which at most one is a $m. Its placeholders may name any of
  // `value_sets`, which must outlive the result. Returns an error if `source`
  // isn't a valid pattern.
  static absl::StatusOr<SourcePattern> ParseInstruction(
      std::string_view source, absl::Span<const ValueSetDef> value_sets);

  // Parses a macro code's source, which has at most one code word argument,
  // which isn't a $m. Otherwise, as ParseInstruction().
  static absl::StatusOr<SourcePattern> ParseMacroCode(
      std::string_view source, absl::Span<const ValueSetDef> value_sets);

  // Returns the symbols a pattern can write as literal text. A program's lexer
  // must read each of them as the same symbol, or no statement could match a
  // pattern that writes it.
  static absl::Span<const gb::Symbol> GetSymbols();

  // Returns an error if a value in `parts` (a $#, a value set, $v, $V, or a
  // literal number) is followed by a binary operator, as no statement could
  // match it: a program writes the value as an expression, which would go on
  // past the operator. `pattern` is the text the parts' offsets are in.
  // Parsing checks each pattern, and this checks patterns put together, such
  // as an instruction's with a macro code's in place of its $m.
  static absl::Status CheckValueEnds(std::string_view pattern,
                                     absl::Span<const Part> parts);

  SourcePattern(const SourcePattern&) = default;
  SourcePattern(SourcePattern&&) = default;
  SourcePattern& operator=(const SourcePattern&) = default;
  SourcePattern& operator=(SourcePattern&&) = default;
  ~SourcePattern() = default;

  // Returns the parts of the pattern, in order.
  absl::Span<const Part> GetParts() const { return parts_; }

  // Returns the code word arguments of the pattern's kArgument parts, in order.
  absl::Span<const Argument> GetArgs() const { return args_; }

  // Returns the number of words the pattern's $v and $V add after the code
  // word: one for each $v, and two for each $V.
  int GetValueWords() const { return value_words_; }

 private:
  SourcePattern() = default;

  static absl::StatusOr<SourcePattern> Parse(
      std::string_view source, absl::Span<const ValueSetDef> value_sets);

  // Ends the comma-separated argument whose parts start at `arg_start`, marking
  // its placeholder as a whole argument if it is the only part.
  void EndArg(int arg_start);

  std::vector<Part> parts_;
  std::vector<Argument> args_;
  int value_words_ = 0;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_SOURCE_PATTERN_H_
