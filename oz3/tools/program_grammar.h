// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_GRAMMAR_H_
#define OZ3_TOOLS_PROGRAM_GRAMMAR_H_

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/status/statusor.h"
#include "gb/parse/lexer.h"
#include "gb/parse/parse_error.h"
#include "gb/parse/parse_result.h"
#include "gb/parse/parser.h"
#include "gb/parse/parser_program.h"
#include "gb/parse/token.h"
#include "oz3/core/instruction_def.h"
#include "oz3/tools/expression.h"
#include "oz3/tools/instruction_set_syntax.h"

namespace oz3 {

// One argument of a matched form, for one of InstructionForm::GetArgs(), in
// the same order.
struct InstructionArg {
  // The argument's first token, for errors.
  gb::Token token;

  // For a register, its number, as the code word holds it (R1 is 1, D1 is 1).
  int reg = 0;

  // For a value ($#, a value set, $v, or $V), its expression.
  std::optional<Expression> value;

  // For a code word value ($# or a value set), its value as a word, which
  // matching evaluated. It may not fit the argument's bits.
  uint32_t code_value = 0;
};

// A form that a statement matched, with its arguments.
struct InstructionMatch {
  const InstructionForm* form = nullptr;
  std::vector<InstructionArg> args;
};

// A context in which a value set's names are its values, ahead of any symbol
// of the same name in the context it wraps, for a code word argument that
// takes named values ($Condition). Names are matched ignoring case.
class ValueSetContext final : public ExpressionContext {
 public:
  // `value_set` and `context` must outlive this.
  ValueSetContext(const ValueSetDef* value_set,
                  const ExpressionContext* context)
      : value_set_(value_set), context_(context) {}

  uint16_t GetOffset() const override { return context_->GetOffset(); }
  std::optional<ExpressionResult> GetSymbolValue(
      std::string_view name, ExpressionWidth width) const override;

 private:
  const ValueSetDef* const value_set_;
  const ExpressionContext* const context_;
};

// The grammar of programs for one instruction set: a parser program, whose
// kLineRule parses a line of program source, with a rule for each way the
// instruction set's instructions can be written, which MatchInstruction()
// matches a line's instruction with.
//
// This class is thread-safe, as it can't change once created. Parsing needs a
// gb::Parser of its own for each thread.
class ProgramGrammar final {
 public:
  // The rule for a line, which a program is parsed with a line at a time, so
  // that a line with an error doesn't stop the lines after it from being
  // read. A line's items are:
  //   $label        Optional: $name (%ident) or $local_label (%local_label),
  //                 then ':'.
  //   $directive    Optional: '.', then with no space after it, a directive
  //                 (see below).
  //   $instruction  Optional, if there is no directive: $name (%ident), then
  //                 optionally '.' and $extension (%ident) with no space
  //                 around the '.', then any tokens to the end of the line,
  //                 with brackets balanced. $args (%here) is the first of
  //                 them, or the line's end if there are none.
  //   $end          The line break, or the end of the content.
  //
  // A directive's name is $directive, in upper case ("CONST"), and its
  // operands are:
  //   INCLUDE    $path (%string)
  //   CONST      $name (%ident) '=' $value
  //   DW         $items, one or more separated by commas
  //   DD         $items, one or more separated by commas
  //   DS         $count
  //   STACK      $words
  //   INSTANCES  $count
  //   BANK       $name (%ident), then optionally ',' $mask
  //   SEGMENT    $name (%ident), then zero or more $attributes
  // Each value ($value, $items, $count, $words, and $mask) is an Expression.
  // Each segment attribute is one of $role ("CODE", "STACK", "DATA", or
  // "EXTRA"), $rom ("ROM"), $instance ("INSTANCE"), "PAGE" '(' $page ')',
  // "PAGES" '(' $pages ')', or "BANK" '(' $bank (%ident) ')'.
  static constexpr std::string_view kLineRule = "Line";

  // Creates the grammar for `syntax`, which must outlive it. Returns an error
  // if a pattern in `syntax` has literal text the program lexer can't read as
  // one token.
  static absl::StatusOr<std::unique_ptr<ProgramGrammar>> Create(
      const InstructionSetSyntax& syntax);

  ProgramGrammar(const ProgramGrammar&) = delete;
  ProgramGrammar& operator=(const ProgramGrammar&) = delete;
  ~ProgramGrammar() = default;

  // Returns the name of a line's instruction as written ("ADD" or "add.w"),
  // which InstructionSetSyntax::GetForms() looks up ignoring case. `lexer`
  // holds its tokens, and the name is a view of its text.
  static std::string_view GetInstructionName(const gb::Lexer& lexer,
                                             const gb::ParsedItem& instruction);

  // Returns the parser program, for a gb::Parser to parse with.
  std::shared_ptr<const gb::ParserProgram> GetParserProgram() const {
    return program_;
  }

  // Matches `instruction`, a kLineRule match's $instruction, against each of
  // its name's forms, from its first argument to the end of the line. No value
  // matches an expression in brackets as a whole.
  //
  // A form's first pass values (its code word values, and its literal numbers)
  // are evaluated as words in `context`, with a value set's names for a code
  // word value that takes them (see ValueSetContext). A form matches only if
  // each of them has a value, and a literal number's is the number. So with
  // patterns "SELF, $v" and "$#3, $v", "SELF, 1" matches only the first unless
  // SELF is a symbol.
  //
  // Returns true and sets `matches` to every form that matches, which may be
  // none. Returns false and sets `error` if the instruction's name is no
  // instruction's, or if no form matches, but one would have if not for a
  // first pass value with an error or a symbol that isn't defined. Either way,
  // the parser's lexer is left where it was.
  //
  // `parser` must have parsed `instruction` from `content`, with
  // GetParserProgram(), and its lexer must outlive the matches' tokens and
  // expressions.
  bool MatchInstruction(gb::Parser& parser, gb::LexerContentId content,
                        const gb::ParsedItem& instruction,
                        const ExpressionContext& context,
                        std::vector<InstructionMatch>& matches,
                        gb::ParseError& error) const;

 private:
  explicit ProgramGrammar(const InstructionSetSyntax* syntax)
      : syntax_(syntax) {}

  const InstructionSetSyntax* const syntax_;
  std::shared_ptr<const gb::ParserProgram> program_;

  // Each form's rule, as an index into rule_names_, and each rule's name.
  absl::flat_hash_map<const InstructionForm*, int> form_rules_;
  std::vector<std::string> rule_names_;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_GRAMMAR_H_
