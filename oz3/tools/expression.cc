// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/expression.h"

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/parse/lexer.h"
#include "gb/parse/parse_error.h"
#include "gb/parse/parse_result.h"
#include "gb/parse/token.h"
#include "oz3/tools/program_lexer.h"

namespace oz3 {

namespace {

// Each binary precedence level is a rule with a repeated group, as the parser
// doesn't allow left recursion, and Expression::Create folds it left to right.
constexpr std::string_view kParserRules = R"---(
  Expression {
    $args=ExpressionXor ($ops='|' $args=ExpressionXor)*;
  }
  ExpressionXor {
    $args=ExpressionAnd ($ops='^' $args=ExpressionAnd)*;
  }
  ExpressionAnd {
    $args=ExpressionShift ($ops='&' $args=ExpressionShift)*;
  }
  ExpressionShift {
    $args=ExpressionSum ($ops=('<<' | '>>') $args=ExpressionSum)*;
  }
  ExpressionSum {
    $args=ExpressionProduct ($ops=('+' | '-') $args=ExpressionProduct)*;
  }
  ExpressionProduct {
    $args=ExpressionUnary ($ops=('*' | '/' | '%') $args=ExpressionUnary)*;
  }
  ExpressionUnary {
    $op=('-' | '~' | '!') $arg=ExpressionUnary;
    $value=ExpressionValue;
  }
  ExpressionValue {
    $number=%int;
    $string=%string;
    $name=%ident;
    $local_label=%local_label;
    $offset='$';
    '(' $group=Expression ')';
  }
)---";

int GetBits(ExpressionWidth width) {
  return width == ExpressionWidth::kWord ? 16 : 32;
}

// The mask of a width's bits, which is also its largest value.
uint64_t GetMask(ExpressionWidth width) {
  return (uint64_t{1} << GetBits(width)) - 1;
}

std::string_view GetName(ExpressionWidth width) {
  return width == ExpressionWidth::kWord ? "word" : "dword";
}

// Returns the value of a step, wrapped to the width's bits.
ExpressionResult Value(ExpressionWidth width, uint64_t value) {
  return {.kind = ExpressionResult::Kind::kValue,
          .value = static_cast<uint32_t>(value & GetMask(width))};
}

bool IsValue(const ExpressionResult& result) {
  return result.kind == ExpressionResult::Kind::kValue;
}

}  // namespace

std::string_view Expression::GetParserRules() { return kParserRules; }

Expression Expression::Create(const gb::Lexer& lexer,
                              const gb::ParsedItem& item) {
  Expression expression(lexer);
  expression.token_ = item.GetToken();
  expression.AddNodes(item);
  return expression;
}

int Expression::AddNodes(const gb::ParsedItem& item) {
  // The binary operator levels.
  const absl::Span<const gb::ParsedItem> args = item.GetItems("args");
  if (!args.empty()) {
    const absl::Span<const gb::ParsedItem> ops = item.GetItems("ops");
    DCHECK_EQ(ops.size() + 1, args.size());
    int left = AddNodes(args[0]);
    for (int i = 1; i < static_cast<int>(args.size()); ++i) {
      const int right = AddNodes(args[i]);
      const gb::Token op = ops[i - 1].GetToken();
      left = AddNode({.kind = NodeKind::kBinary,
                      .token = op,
                      .op = op.GetSymbol().GetString()[0],
                      .left = left,
                      .right = right});
    }
    return left;
  }

  // A unary operator.
  if (const gb::ParsedItem* arg = item.GetItem("arg"); arg != nullptr) {
    const int operand = AddNodes(*arg);
    const gb::Token op = item.GetToken("op");
    return AddNode({.kind = NodeKind::kUnary,
                    .token = op,
                    .op = op.GetSymbol().GetString()[0],
                    .left = operand});
  }

  const gb::ParsedItem* value = item.GetItem("value");
  DCHECK(value != nullptr);
  return AddValueNode(*value);
}

int Expression::AddValueNode(const gb::ParsedItem& item) {
  if (const gb::ParsedItem* group = item.GetItem("group"); group != nullptr) {
    const int index = AddNodes(*group);
    nodes_[index].bracketed = true;
    return index;
  }

  Node node = {.token = item.GetToken()};
  if (item.GetItem("number") != nullptr) {
    // Numbers have no sign, and hex and binary numbers are read as 64 unsigned
    // bits, so the lexer's int64 is their bits.
    node.kind = NodeKind::kConstant;
    node.value = static_cast<uint64_t>(node.token.GetInt());
  } else if (item.GetItem("string") != nullptr) {
    node.kind = NodeKind::kConstant;
    absl::StatusOr<std::vector<uint16_t>> words =
        GetStringWords(*lexer_, node.token);
    if (!words.ok()) {
      node.error = words.status().message();
    } else if (words->size() != 1) {
      node.error = "A string in an expression must be one word";
    } else {
      node.value = (*words)[0];
    }
  } else if (item.GetItem("name") != nullptr) {
    node.kind = NodeKind::kSymbol;
    node.name = node.token.GetString();
  } else if (item.GetItem("local_label") != nullptr) {
    node.kind = NodeKind::kSymbol;
    node.name = absl::AsciiStrToUpper(node.token.GetString());
  } else {
    DCHECK(item.GetItem("offset") != nullptr);
    node.kind = NodeKind::kOffset;
  }
  return AddNode(std::move(node));
}

int Expression::AddNode(Node node) {
  nodes_.push_back(std::move(node));
  return static_cast<int>(nodes_.size()) - 1;
}

ExpressionResult Expression::Evaluate(ExpressionWidth width,
                                      const ExpressionContext& context) const {
  return EvaluateNode(width, context, static_cast<int>(nodes_.size()) - 1);
}

ExpressionResult Expression::EvaluateNode(ExpressionWidth width,
                                          const ExpressionContext& context,
                                          int index) const {
  const Node& node = nodes_[index];
  switch (node.kind) {
    case NodeKind::kConstant:
      if (!node.error.empty()) {
        return Error(node.token, node.error);
      }
      return CheckValue(width, node.value, node.token);
    case NodeKind::kSymbol:
      return EvaluateSymbol(width, context, node);
    case NodeKind::kOffset:
      return Value(width, context.GetOffset());
    case NodeKind::kUnary:
      return EvaluateUnary(width, context, node);
    case NodeKind::kBinary:
      return EvaluateBinary(width, context, node);
  }
  return Error(node.token, "Internal error: unknown expression node");
}

ExpressionResult Expression::EvaluateUnary(ExpressionWidth width,
                                           const ExpressionContext& context,
                                           const Node& node) const {
  ExpressionResult operand = EvaluateNode(width, context, node.left);
  if (!IsValue(operand)) {
    return operand;
  }
  const uint64_t value = operand.value;
  switch (node.op) {
    case '-':
      return Value(width, 0 - value);
    case '~':
      return Value(width, ~value);
    case '!':
      return Value(width, value == 0 ? 1 : 0);
  }
  return Error(node.token, "Internal error: unknown unary operator");
}

ExpressionResult Expression::EvaluateBinary(ExpressionWidth width,
                                            const ExpressionContext& context,
                                            const Node& node) const {
  ExpressionResult left = EvaluateNode(width, context, node.left);
  if (!IsValue(left)) {
    return left;
  }
  ExpressionResult right = EvaluateNode(width, context, node.right);
  if (!IsValue(right)) {
    return right;
  }
  // Both are the width's bits, so the product fits 64 bits.
  const uint64_t a = left.value;
  const uint64_t b = right.value;
  switch (node.op) {
    case '+':
      return Value(width, a + b);
    case '-':
      return Value(width, a - b);
    case '*':
      return Value(width, a * b);
    case '/':
    case '%':
      if (b == 0) {
        return Error(node.token, "Division by zero");
      }
      return Value(width, node.op == '/' ? a / b : a % b);
    case '&':
      return Value(width, a & b);
    case '|':
      return Value(width, a | b);
    case '^':
      return Value(width, a ^ b);
    case '<':
    case '>':
      if (b >= static_cast<uint64_t>(GetBits(width))) {
        return Value(width, 0);
      }
      return Value(width, node.op == '<' ? a << b : a >> b);
  }
  return Error(node.token, "Internal error: unknown binary operator");
}

ExpressionResult Expression::EvaluateSymbol(ExpressionWidth width,
                                            const ExpressionContext& context,
                                            const Node& node) const {
  std::optional<ExpressionResult> result =
      context.GetSymbolValue(node.name, width);
  if (!result.has_value()) {
    ExpressionResult undefined =
        Error(node.token, absl::StrCat(node.name, " is not defined"));
    undefined.kind = ExpressionResult::Kind::kUndefined;
    return undefined;
  }
  if (IsValue(*result)) {
    return CheckValue(width, result->value, node.token);
  }

  // An error without a location is the use's.
  if (result->error.GetLocation().line < 0) {
    result->error = gb::ParseError(lexer_->GetTokenLocation(node.token),
                                   result->error.GetMessage());
  }
  return *std::move(result);
}

ExpressionResult Expression::CheckValue(ExpressionWidth width, uint64_t value,
                                        const gb::Token& token) const {
  if (value > GetMask(width)) {
    return Error(token, absl::StrCat("Value ", value, " does not fit in a ",
                                     GetName(width)));
  }
  return Value(width, value);
}

ExpressionResult Expression::Error(const gb::Token& token,
                                   std::string_view message) const {
  return {.kind = ExpressionResult::Kind::kError,
          .error = gb::ParseError(lexer_->GetTokenLocation(token), message)};
}

}  // namespace oz3
