// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/expression.h"

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/base/no_destructor.h"
#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_cat.h"
#include "gb/parse/lexer.h"
#include "gb/parse/parse_result.h"
#include "gb/parse/parser.h"
#include "gb/parse/parser_program.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/tools/program_lexer.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using Kind = ExpressionResult::Kind;
using Width = ExpressionWidth;

constexpr Width kWord = Width::kWord;
constexpr Width kDword = Width::kDword;

// Symbols by name, which records the width each was asked for in.
class FakeContext : public ExpressionContext {
 public:
  uint16_t GetOffset() const override { return offset; }

  std::optional<ExpressionResult> GetSymbol(
      std::string_view name, ExpressionWidth width) const override {
    requests.push_back(absl::StrCat(name, width == kWord ? ":W" : ":D"));
    auto it = symbols.find(name);
    if (it == symbols.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  uint16_t offset = 0;
  absl::flat_hash_map<std::string, ExpressionResult> symbols;
  mutable std::vector<std::string> requests;
};

std::shared_ptr<const gb::ParserProgram> GetTestProgram() {
  static const absl::NoDestructor<std::shared_ptr<const gb::ParserProgram>>
      program(gb::ParserProgram::Create(
          GetProgramLexerConfig(),
          absl::StrCat(kProgramParserTokens, Expression::GetParserRules(),
                       "Test { $e=Expression %linebreak; }")));
  return *program;
}

class ExpressionTest : public ::testing::Test {
 protected:
  // Parses `text` as an expression, which must be the whole line.
  std::optional<Expression> Parse(std::string_view text) {
    parser_ = gb::Parser::Create(GetTestProgram());
    if (parser_ == nullptr) {
      ADD_FAILURE() << "Invalid parser program";
      return std::nullopt;
    }
    const gb::LexerContentId content =
        parser_->GetLexer().AddContent(std::string(text));
    result_ =
        std::make_unique<gb::ParseResult>(parser_->Parse(content, "Test"));
    if (!result_->IsOk()) {
      return std::nullopt;
    }
    return Expression::Create(parser_->GetLexer(), *(*result_)->GetItem("e"));
  }

  ExpressionResult Evaluate(std::string_view text, Width width = kWord) {
    std::optional<Expression> expression = Parse(text);
    if (!expression.has_value()) {
      ADD_FAILURE() << "Failed to parse: " << text << ": "
                    << result_->GetError().FormatMessage();
      return {.kind = Kind::kError};
    }
    return expression->Evaluate(width, context_);
  }

  // Returns the value of `text`, which must have one.
  uint32_t Value(std::string_view text, Width width = kWord) {
    ExpressionResult result = Evaluate(text, width);
    EXPECT_EQ(result.kind, Kind::kValue)
        << text << ": " << result.error.FormatMessage();
    return result.value;
  }

  // Returns the error evaluating `text`, which must have one, as "column:
  // message".
  std::string Error(std::string_view text, Width width = kWord) {
    ExpressionResult result = Evaluate(text, width);
    EXPECT_EQ(result.kind, Kind::kError) << text << " is " << result.value;
    return absl::StrCat(result.error.GetLocation().column, ": ",
                        result.error.GetMessage());
  }

  FakeContext context_;
  std::unique_ptr<gb::Parser> parser_;
  std::unique_ptr<gb::ParseResult> result_;
};

TEST_F(ExpressionTest, Numbers) {
  EXPECT_EQ(Value("0"), 0);
  EXPECT_EQ(Value("15"), 15);
  EXPECT_EQ(Value("0FFh"), 255);
  EXPECT_EQ(Value("1011b"), 11);
}

TEST_F(ExpressionTest, UnaryOperators) {
  EXPECT_EQ(Value("-1"), 0xFFFF);
  EXPECT_EQ(Value("-1", kDword), 0xFFFFFFFF);
  EXPECT_EQ(Value("--1"), 1);
  EXPECT_EQ(Value("-0"), 0);
  EXPECT_EQ(Value("~0"), 0xFFFF);
  EXPECT_EQ(Value("~0", kDword), 0xFFFFFFFF);
  EXPECT_EQ(Value("~0FF00h"), 0x00FF);
  EXPECT_EQ(Value("~0FF00h", kDword), 0xFFFF00FF);
  EXPECT_EQ(Value("!0"), 1);
  EXPECT_EQ(Value("!5"), 0);
}

TEST_F(ExpressionTest, BinaryOperators) {
  EXPECT_EQ(Value("6 * 7"), 42);
  EXPECT_EQ(Value("7 / 2"), 3);
  EXPECT_EQ(Value("7 % 2"), 1);
  EXPECT_EQ(Value("5 + 3"), 8);
  EXPECT_EQ(Value("5 - 3"), 2);
  EXPECT_EQ(Value("1 << 4"), 16);
  EXPECT_EQ(Value("16 >> 4"), 1);
  EXPECT_EQ(Value("0Ch & 0Ah"), 8);
  EXPECT_EQ(Value("0Ch ^ 0Ah"), 6);
  EXPECT_EQ(Value("0Ch | 0Ah"), 14);
}

TEST_F(ExpressionTest, ArithmeticWraps) {
  EXPECT_EQ(Value("3 - 5"), 0xFFFE);
  EXPECT_EQ(Value("3 - 5", kDword), 0xFFFFFFFE);
  EXPECT_EQ(Value("65535 + 1"), 0);
  EXPECT_EQ(Value("65535 + 1", kDword), 0x10000);
  EXPECT_EQ(Value("4294967295 + 2", kDword), 1);
  EXPECT_EQ(Value("1000 * 100"), 100000 & 0xFFFF);
  EXPECT_EQ(Value("1000 * 100 / 10"), (100000 & 0xFFFF) / 10);
  EXPECT_EQ(Value("1000 * 100 / 10", kDword), 10000);
  EXPECT_EQ(Value("4294967295 * 4294967295", kDword), 1);
  EXPECT_EQ(Value("-2 * 3"), 0xFFFA);
}

TEST_F(ExpressionTest, DivisionIsUnsigned) {
  EXPECT_EQ(Value("-2 / 2"), 0x7FFF);
  EXPECT_EQ(Value("-2 / 2", kDword), 0x7FFFFFFF);
  EXPECT_EQ(Value("-7 % 2"), 1);
  EXPECT_EQ(Value("7 / -1"), 0);
}

TEST_F(ExpressionTest, Precedence) {
  EXPECT_EQ(Value("2 + 3 * 4"), 14);
  EXPECT_EQ(Value("2 * 3 + 4"), 10);
  EXPECT_EQ(Value("1 + 1 << 2"), 8);
  EXPECT_EQ(Value("1 << 2 + 1"), 8);
  EXPECT_EQ(Value("6 & 3 << 1"), 6);
  EXPECT_EQ(Value("1 | 6 & 3"), 3);
  EXPECT_EQ(Value("1 | 2 ^ 3"), 1);
  EXPECT_EQ(Value("6 ^ 3 & 5"), 7);
  EXPECT_EQ(Value("-2 + 3"), 1);
  EXPECT_EQ(Value("~1 & 3"), 2);
}

TEST_F(ExpressionTest, LeftToRight) {
  EXPECT_EQ(Value("10 - 3 - 2"), 5);
  EXPECT_EQ(Value("100 / 10 / 5"), 2);
  EXPECT_EQ(Value("64 >> 2 >> 1"), 8);
}

TEST_F(ExpressionTest, Parentheses) {
  EXPECT_EQ(Value("(2 + 3) * 4"), 20);
  EXPECT_EQ(Value("2 * (3 + 4)"), 14);
  EXPECT_EQ(Value("((5))"), 5);
  EXPECT_EQ(Value("10 - (3 - 2)"), 9);
}

TEST_F(ExpressionTest, IsBracketed) {
  EXPECT_TRUE(Parse("(5)")->IsBracketed());
  EXPECT_TRUE(Parse("((A + 1))")->IsBracketed());
  EXPECT_TRUE(Parse("(A + 1)")->IsBracketed());
  EXPECT_FALSE(Parse("5")->IsBracketed());
  EXPECT_FALSE(Parse("(A) + (B)")->IsBracketed());
  EXPECT_FALSE(Parse("-(5)")->IsBracketed());
  EXPECT_FALSE(Parse("(A + 1) * 2")->IsBracketed());
}

TEST_F(ExpressionTest, IsString) {
  EXPECT_TRUE(Parse("'Hello'")->IsString());
  EXPECT_TRUE(Parse("\"Hello\"")->IsString());
  EXPECT_TRUE(Parse("''")->IsString());
  EXPECT_FALSE(Parse("('A')")->IsString());
  EXPECT_FALSE(Parse("'A' + 1")->IsString());
  EXPECT_FALSE(Parse("-'A'")->IsString());
  EXPECT_FALSE(Parse("5")->IsString());
  EXPECT_FALSE(Parse("A")->IsString());
}

TEST_F(ExpressionTest, ExpressionEndsWhereTheNextTokenCantContinueIt) {
  for (std::string_view text : {"5 + R1", "N ROM", "5 )", "5 , 6", "5 6"}) {
    EXPECT_FALSE(Parse(text).has_value()) << text;
  }

  // The test grammar needs a line break after the expression, so check what
  // the expression took by parsing just the expression rule.
  parser_ = gb::Parser::Create(GetTestProgram());
  gb::Lexer& lexer = parser_->GetLexer();
  const gb::LexerContentId content = lexer.AddContent("5 + R1");
  gb::ParseResult result = parser_->Parse(content, "Expression");
  ASSERT_TRUE(result.IsOk()) << result.GetError().FormatMessage();
  EXPECT_EQ(Expression::Create(lexer, *result).Evaluate(kWord, context_).value,
            5);
  EXPECT_TRUE(lexer.NextToken(content).IsSymbol('+'));
}

TEST_F(ExpressionTest, RegistersAreNotValues) {
  for (std::string_view text : {"R1", "SP", "1 + D0", "(BC)"}) {
    EXPECT_FALSE(Parse(text).has_value()) << text;
  }
}

TEST_F(ExpressionTest, NumbersMustFitTheWidth) {
  EXPECT_EQ(Value("65535"), 65535);
  EXPECT_EQ(Value("0FFFFh"), 0xFFFF);
  EXPECT_EQ(Value("-32768"), 0x8000);
  EXPECT_EQ(Value("-65535"), 1);
  EXPECT_EQ(Error("65536"), "0: Value 65536 does not fit in a word");
  EXPECT_EQ(Error("-65536"), "1: Value 65536 does not fit in a word");
  EXPECT_EQ(Error("1 + 10000h"), "4: Value 65536 does not fit in a word");
  EXPECT_EQ(Value("4294967295", kDword), 0xFFFFFFFF);
  EXPECT_EQ(Value("65536", kDword), 65536);
  EXPECT_EQ(Error("4294967296", kDword),
            "0: Value 4294967296 does not fit in a dword");
  EXPECT_EQ(Error("0FFFFFFFFFFFFFFFFh", kDword),
            "0: Value 18446744073709551615 does not fit in a dword");
}

TEST_F(ExpressionTest, BitwiseOperators) {
  EXPECT_EQ(Value("~1"), 0xFFFE);
  EXPECT_EQ(Value("-1 & 0FFh"), 0xFF);
  EXPECT_EQ(Value("-1 | 0"), 0xFFFF);
  EXPECT_EQ(Value("-1 | 0", kDword), 0xFFFFFFFF);
  EXPECT_EQ(Value("-1 ^ 0FFh"), 0xFF00);
}

TEST_F(ExpressionTest, ShiftsAreLogical) {
  EXPECT_EQ(Value("1 << 15"), 0x8000);
  EXPECT_EQ(Value("3 << 15"), 0x8000);
  EXPECT_EQ(Value("-1 << 1"), 0xFFFE);
  EXPECT_EQ(Value("1 << 31", kDword), 0x80000000);
  EXPECT_EQ(Value("0FFFFh << 16", kDword), 0xFFFF0000);
  EXPECT_EQ(Value("0FFFFh << 16"), 0);
  EXPECT_EQ(Value("-2 >> 1"), 0x7FFF);
  EXPECT_EQ(Value("-2 >> 1", kDword), 0x7FFFFFFF);
}

TEST_F(ExpressionTest, ShiftsOfTheWidthOrMoreAreZero) {
  EXPECT_EQ(Value("1 << 16"), 0);
  EXPECT_EQ(Value("-1 >> 16"), 0);
  EXPECT_EQ(Value("1 << 16", kDword), 0x10000);
  EXPECT_EQ(Value("1 << 32", kDword), 0);
  EXPECT_EQ(Value("-1 >> 32", kDword), 0);

  // A negative count is a large unsigned one.
  EXPECT_EQ(Value("1 << -1"), 0);
  EXPECT_EQ(Value("-1 >> -1", kDword), 0);
}

TEST_F(ExpressionTest, DivisionByZero) {
  EXPECT_EQ(Error("1 / 0"), "2: Division by zero");
  EXPECT_EQ(Error("1 % (2 - 2)"), "2: Division by zero");
}

TEST_F(ExpressionTest, Strings) {
  EXPECT_EQ(Value("'A'"), 65);
  EXPECT_EQ(Value("\"AB\""), 0x4142);
  EXPECT_EQ(Value("\"A\""), 0x4100);
  EXPECT_EQ(Value("'A' + 1"), 66);
  EXPECT_EQ(Error("1 + 'AB'"), "4: A string in an expression must be one word");
  EXPECT_EQ(Error("''"), "0: A string in an expression must be one word");
  EXPECT_EQ(Error("\"ABC\""), "0: A string in an expression must be one word");
}

TEST_F(ExpressionTest, Offset) {
  context_.offset = 0x1234;
  EXPECT_EQ(Value("$"), 0x1234);
  EXPECT_EQ(Value("$ + 2"), 0x1236);
}

TEST_F(ExpressionTest, Symbols) {
  context_.symbols["LOOP"] = {.value = 10};
  context_.symbols["@NEXT"] = {.value = 20};
  EXPECT_EQ(Value("LOOP"), 10);
  EXPECT_EQ(Value("loop + 1"), 11);
  EXPECT_EQ(Value("@next"), 20);
  EXPECT_EQ(Value("@Next - LOOP"), 10);
}

TEST_F(ExpressionTest, SymbolsAreAskedForInTheWidth) {
  context_.symbols["X"] = {.value = 1};
  Value("X", kWord);
  Value("X", kDword);
  EXPECT_THAT(context_.requests, ElementsAre("X:W", "X:D"));
}

TEST_F(ExpressionTest, SymbolValuesMustFit) {
  context_.symbols["BIG"] = {.value = 0x12345678};
  EXPECT_EQ(Value("BIG", kDword), 0x12345678);
  EXPECT_EQ(Error("1 + BIG"), "4: Value 305419896 does not fit in a word");
  EXPECT_EQ(Error("BIG & 0FFFFh"), "0: Value 305419896 does not fit in a word");
}

TEST_F(ExpressionTest, UndefinedSymbols) {
  context_.symbols["A"] = {.value = 1};
  ExpressionResult result = Evaluate("A + B");
  EXPECT_EQ(result.kind, Kind::kUndefined);
  EXPECT_EQ(result.error.GetMessage(), "B is not defined");
  EXPECT_EQ(result.error.GetLocation().column, 4);

  result = Evaluate("@loop");
  EXPECT_EQ(result.kind, Kind::kUndefined);
  EXPECT_EQ(result.error.GetMessage(), "@LOOP is not defined");
}

TEST_F(ExpressionTest, FirstProblemIsReported) {
  ExpressionResult result = Evaluate("B / 0");
  EXPECT_EQ(result.kind, Kind::kUndefined);
  EXPECT_EQ(result.error.GetMessage(), "B is not defined");

  result = Evaluate("1 / 0 + B");
  EXPECT_EQ(result.kind, Kind::kError);
  EXPECT_EQ(result.error.GetMessage(), "Division by zero");
}

TEST_F(ExpressionTest, SymbolErrorsPassThrough) {
  context_.symbols["BAD"] = {
      .kind = Kind::kError,
      .error = gb::ParseError(gb::LexerLocation{.line = 7, .column = 3},
                              "Bad constant")};
  ExpressionResult result = Evaluate("1 + BAD");
  EXPECT_EQ(result.kind, Kind::kError);
  EXPECT_EQ(result.error.GetMessage(), "Bad constant");
  EXPECT_EQ(result.error.GetLocation().line, 7);
}

TEST_F(ExpressionTest, SymbolErrorsWithoutALocationAreAtTheUse) {
  context_.symbols["BAD"] = {.kind = Kind::kError,
                             .error = gb::ParseError("Bad here")};
  EXPECT_EQ(Error("1 + BAD"), "4: Bad here");
}

TEST_F(ExpressionTest, UndefinedInASymbolsOwnExpressionPassesThrough) {
  // A constant whose expression uses B, which isn't defined yet.
  context_.symbols["C"] = {
      .kind = Kind::kUndefined,
      .error = gb::ParseError(gb::LexerLocation{.line = 2, .column = 11},
                              "B is not defined")};
  ExpressionResult result = Evaluate("C + 1");
  EXPECT_EQ(result.kind, Kind::kUndefined);
  EXPECT_EQ(result.error.GetMessage(), "B is not defined");
  EXPECT_EQ(result.error.GetLocation().line, 2);
}

TEST_F(ExpressionTest, GetToken) {
  std::optional<Expression> expression = Parse("  (A + 1) * 2");
  ASSERT_TRUE(expression.has_value());
  EXPECT_TRUE(expression->GetToken().IsSymbol('('));
}

TEST_F(ExpressionTest, InvalidSyntax) {
  for (std::string_view text : {"", "1 +", "* 2", "(1", "1)", "1 2", "$$"}) {
    EXPECT_FALSE(Parse(text).has_value()) << text;
  }
}

}  // namespace
}  // namespace oz3
