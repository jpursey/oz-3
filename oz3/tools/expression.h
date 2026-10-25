// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_EXPRESSION_H_
#define OZ3_TOOLS_EXPRESSION_H_

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "gb/parse/lexer.h"
#include "gb/parse/parse_error.h"
#include "gb/parse/parse_result.h"
#include "gb/parse/token.h"

namespace oz3 {

//==============================================================================
// ExpressionWidth and ExpressionResult
//==============================================================================

// The width of the value an expression is evaluated as, which is where it is
// used. An expression's arithmetic is in its width, as on the machine.
enum class ExpressionWidth {
  kWord,   // 16 bits.
  kDword,  // 32 bits.
};

// The result of evaluating an expression, or a symbol in one.
struct ExpressionResult {
  enum class Kind {
    kValue,      // The value is `value`.
    kUndefined,  // A symbol it uses isn't defined (yet), which `error` names.
    kError,      // The expression is wrong, as `error` says.
  };

  Kind kind = Kind::kValue;

  // The value's bits, which fit the width it was evaluated in, so -1 is
  // 0FFFFh as a word.
  uint32_t value = 0;

  gb::ParseError error;
};

//==============================================================================
// ExpressionContext
//==============================================================================

// What an expression's symbols and "$" are, where it is evaluated.
class ExpressionContext {
 public:
  virtual ~ExpressionContext() = default;

  // Returns the offset of the statement the expression is in, for "$".
  virtual uint16_t GetOffset() const = 0;

  // Returns the value of the symbol `name`, used where a value of `width` is:
  // an upper case name ("LOOP"), or an upper case local label with its "@"
  // ("@LOOP"). Returns nullopt if there is no such symbol (yet), which the
  // expression reports as kUndefined. Any result is the expression's, so a
  // symbol with its own expression (a constant) returns that expression's
  // kUndefined or kError as it is. An error without a location is reported
  // where the symbol is used.
  virtual std::optional<ExpressionResult> GetSymbolValue(
      std::string_view name, ExpressionWidth width) const = 0;
};

//==============================================================================
// Expression
//==============================================================================

// An integer expression in program source: numbers, strings of one word,
// symbols, and "$", with C's unary operators (-, ~, !), and binary operators
// (*, /, %, +, -, <<, >>, &, ^, |) and their precedence, and parentheses.
//
// An expression is parsed by a gb::Parser, with a grammar that includes
// GetParserRules(), and evaluated in the width where it is used, as the
// machine would: values are the width's bits, and arithmetic wraps. Division
// is unsigned, and shifts are logical, giving 0 for a count of the width or
// more. A number or symbol must fit the width, and division by zero is an
// error.
class Expression final {
 public:
  // Returns the parser rules for an expression, as the rule "Expression", for
  // a parser program with the program lexer (see GetProgramLexerConfig) and
  // its tokens (kProgramParserTokens). Its other rules start with
  // "Expression".
  static std::string_view GetParserRules();

  // Creates the expression an "Expression" rule matched, as `item`. `lexer`
  // holds its tokens, and must outlive the expression.
  static Expression Create(const gb::Lexer& lexer, const gb::ParsedItem& item);

  Expression(const Expression&) = default;
  Expression(Expression&&) = default;
  Expression& operator=(const Expression&) = default;
  Expression& operator=(Expression&&) = default;
  ~Expression() = default;

  // Returns the expression's first token, where it starts.
  gb::Token GetToken() const { return token_; }

  // Returns true if the expression is in parentheses as a whole, such as
  // "(5)" or "((A + 1))", but not "(A) + (B)".
  bool IsBracketed() const { return nodes_.back().bracketed; }

  // Returns true if the expression is a string on its own, such as 'Hello',
  // and not in parentheses, which a list of words takes as the string's words
  // rather than as a value.
  bool IsString() const {
    return nodes_.size() == 1 && !nodes_[0].bracketed &&
           token_.GetType() == gb::kTokenString;
  }

  // Evaluates the expression as a value of `width`, in `context`.
  ExpressionResult Evaluate(ExpressionWidth width,
                            const ExpressionContext& context) const;

 private:
  enum class NodeKind {
    kConstant,  // A number or string: `value`, or `error`.
    kSymbol,    // `name`, an upper case name or local label.
    kOffset,    // $
    kUnary,     // `op` applied to `left`.
    kBinary,    // `op` applied to `left` and `right`.
  };

  // One step of the expression. Each node's operands come before it in
  // `nodes_`, so the last node is the whole expression.
  struct Node {
    NodeKind kind = NodeKind::kConstant;
    gb::Token token;  // The node's own token, for errors.
    char op = 0;      // The operator: '<' and '>' are the shifts.
    int left = -1;
    int right = -1;
    uint64_t value = 0;
    std::string name;
    std::string error;
    bool bracketed = false;
  };

  explicit Expression(const gb::Lexer& lexer) : lexer_(&lexer) {}

  // Adds the nodes for a parsed item of any of the expression's rules, and
  // returns the index of its node.
  int AddNodes(const gb::ParsedItem& item);
  int AddValueNode(const gb::ParsedItem& item);
  int AddNode(Node node);

  ExpressionResult EvaluateNode(ExpressionWidth width,
                                const ExpressionContext& context,
                                int index) const;
  ExpressionResult EvaluateUnary(ExpressionWidth width,
                                 const ExpressionContext& context,
                                 const Node& node) const;
  ExpressionResult EvaluateBinary(ExpressionWidth width,
                                  const ExpressionContext& context,
                                  const Node& node) const;
  ExpressionResult EvaluateSymbol(ExpressionWidth width,
                                  const ExpressionContext& context,
                                  const Node& node) const;

  // Returns `value` if it fits `width`, or else an error at `token`.
  ExpressionResult CheckValue(ExpressionWidth width, uint64_t value,
                              const gb::Token& token) const;
  ExpressionResult Error(const gb::Token& token,
                         std::string_view message) const;

  const gb::Lexer* lexer_;
  gb::Token token_;
  std::vector<Node> nodes_;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_EXPRESSION_H_
