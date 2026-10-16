// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_lexer.h"

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "gb/parse/lexer.h"
#include "gb/parse/symbol.h"
#include "gb/parse/token.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::HasSubstr;
using ::testing::IsEmpty;

class ProgramLexerTest : public ::testing::Test {
 protected:
  // Returns every token in `text`, up to its end, without the line break that
  // ends its last line.
  std::vector<gb::Token> Lex(std::string_view text) {
    lexer_ = gb::Lexer::Create(GetProgramLexerConfig());
    EXPECT_NE(lexer_, nullptr);
    if (lexer_ == nullptr) {
      return {};
    }
    const gb::LexerContentId content = lexer_->AddContent(std::string(text));
    std::vector<gb::Token> tokens;
    for (gb::Token token = lexer_->NextToken(content); !token.IsEnd();
         token = lexer_->NextToken(content)) {
      tokens.push_back(token);
    }
    EXPECT_FALSE(tokens.empty());
    if (!tokens.empty()) {
      EXPECT_EQ(tokens.back().GetType(), gb::kTokenLineBreak);
      tokens.pop_back();
    }
    return tokens;
  }

  // Returns the one token in `text`.
  gb::Token LexOne(std::string_view text) {
    std::vector<gb::Token> tokens = Lex(text);
    EXPECT_EQ(tokens.size(), 1) << text;
    return tokens.empty() ? gb::Token() : tokens[0];
  }

  // Returns the words of the one string token in `text`.
  absl::StatusOr<std::vector<uint16_t>> StringWords(std::string_view text) {
    const gb::Token token = LexOne(text);
    EXPECT_TRUE(token.IsString()) << text;
    if (!token.IsString()) {
      return absl::InvalidArgumentError("Not a string");
    }
    return GetStringWords(*lexer_, token);
  }

  std::unique_ptr<gb::Lexer> lexer_;
};

TEST_F(ProgramLexerTest, DecimalNumbers) {
  EXPECT_TRUE(LexOne("0").IsInt(0));
  EXPECT_TRUE(LexOne("15").IsInt(15));
  EXPECT_TRUE(LexOne("65535").IsInt(65535));
  EXPECT_TRUE(LexOne("4294967295").IsInt(4294967295));
}

TEST_F(ProgramLexerTest, NumbersHaveNoSign) {
  std::vector<gb::Token> tokens = Lex("-1");
  ASSERT_EQ(tokens.size(), 2);
  EXPECT_TRUE(tokens[0].IsSymbol('-'));
  EXPECT_TRUE(tokens[1].IsInt(1));
}

TEST_F(ProgramLexerTest, HexNumbers) {
  EXPECT_TRUE(LexOne("0Fh").IsInt(15));
  EXPECT_TRUE(LexOne("0FFh").IsInt(255));
  EXPECT_TRUE(LexOne("0FFH").IsInt(255));
  EXPECT_TRUE(LexOne("0ffh").IsInt(255));
  EXPECT_TRUE(LexOne("0FFFFh").IsInt(0xFFFF));
  EXPECT_TRUE(LexOne("0FFFFFFFFh").IsInt(0xFFFFFFFF));
  EXPECT_TRUE(LexOne("10h").IsInt(16));
  EXPECT_TRUE(LexOne("10bh").IsInt(0x10B));
  EXPECT_TRUE(LexOne("0Bh").IsInt(11));
}

TEST_F(ProgramLexerTest, HexNumbersStartWithADigit) {
  EXPECT_TRUE(LexOne("FFh").IsIdent("FFH"));
  EXPECT_TRUE(LexOne("BEh").IsIdent("BEH"));
  EXPECT_TRUE(LexOne("ADD").IsIdent("ADD"));
}

TEST_F(ProgramLexerTest, BinaryNumbers) {
  EXPECT_TRUE(LexOne("1011b").IsInt(11));
  EXPECT_TRUE(LexOne("1011B").IsInt(11));
  EXPECT_TRUE(LexOne("0b").IsInt(0));
  EXPECT_TRUE(LexOne("1111111111111111b").IsInt(0xFFFF));
}

TEST_F(ProgramLexerTest, HexNumbersWithTheTopBitSetAreNegative) {
  EXPECT_TRUE(LexOne("7FFFFFFFFFFFFFFFh").IsInt(INT64_MAX));
  EXPECT_LT(LexOne("8000000000000000h").GetInt(), 0);
  EXPECT_TRUE(LexOne("0FFFFFFFFFFFFFFFFh").IsInt(-1));
}

TEST_F(ProgramLexerTest, NumbersTooBig) {
  EXPECT_TRUE(LexOne("99999999999999999999").IsError());
  EXPECT_TRUE(LexOne("10000000000000000h").IsError());
}

TEST_F(ProgramLexerTest, NamesAreUpperCase) {
  EXPECT_TRUE(LexOne("loop").IsIdent("LOOP"));
  EXPECT_TRUE(LexOne("Loop").IsIdent("LOOP"));
  EXPECT_TRUE(LexOne("_value1").IsIdent("_VALUE1"));
  EXPECT_TRUE(LexOne("a_b").IsIdent("A_B"));
}

TEST_F(ProgramLexerTest, RegistersAreKeywords) {
  for (std::string_view name :
       {"R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "BC", "BS",
        "BD", "BE", "MB", "IP", "FP", "SP", "D0", "D1", "D2", "D3"}) {
    EXPECT_EQ(LexOne(name).GetType(), gb::kTokenKeyword) << name;
  }
  EXPECT_TRUE(LexOne("sp").IsKeyword("SP"));
  EXPECT_TRUE(LexOne("Sp").IsKeyword("SP"));
  EXPECT_TRUE(LexOne("r1").IsKeyword("R1"));
  EXPECT_TRUE(LexOne("d3").IsKeyword("D3"));
}

TEST_F(ProgramLexerTest, NamesLikeRegistersAreIdentifiers) {
  EXPECT_TRUE(LexOne("R10").IsIdent("R10"));
  EXPECT_TRUE(LexOne("R8").IsIdent("R8"));
  EXPECT_TRUE(LexOne("D4").IsIdent("D4"));
  EXPECT_TRUE(LexOne("SPX").IsIdent("SPX"));
  EXPECT_TRUE(LexOne("C0").IsIdent("C0"));
  EXPECT_TRUE(LexOne("ST").IsIdent("ST"));
}

TEST_F(ProgramLexerTest, LocalLabels) {
  gb::Token token = LexOne("@loop");
  EXPECT_TRUE(token.IsUser(kTokenLocalLabel, "@loop"));
  EXPECT_EQ(lexer_->GetTokenText(token), "@loop");
}

TEST_F(ProgramLexerTest, Strings) {
  gb::Token token = LexOne("'Hello'");
  EXPECT_TRUE(token.IsString("Hello"));
  EXPECT_EQ(lexer_->GetTokenText(token), "'Hello'");
  token = LexOne("\"Hello\"");
  EXPECT_TRUE(token.IsString("Hello"));
  EXPECT_EQ(lexer_->GetTokenText(token), "\"Hello\"");
  EXPECT_TRUE(LexOne("'It''s'").IsString("It's"));
  EXPECT_TRUE(LexOne("\"Say \"\"hi\"\"\"").IsString("Say \"hi\""));
  EXPECT_TRUE(LexOne("''").IsString(""));
}

TEST_F(ProgramLexerTest, Symbols) {
  std::vector<gb::Token> tokens =
      Lex("( ) [ ] , + - * / % & | ^ ~ ! < > << >>");
  std::vector<std::string> symbols;
  for (const gb::Token& token : tokens) {
    EXPECT_EQ(token.GetType(), gb::kTokenSymbol);
    symbols.emplace_back(token.GetSymbol().GetString());
  }
  EXPECT_THAT(symbols,
              ElementsAre("(", ")", "[", "]", ",", "+", "-", "*", "/", "%", "&",
                          "|", "^", "~", "!", "<", ">", "<<", ">>"));
  EXPECT_TRUE(LexOne("$").IsSymbol('$'));
}

TEST_F(ProgramLexerTest, Statement) {
  std::vector<gb::Token> tokens = Lex("start: mov.lw r1, (sp + 5)");
  ASSERT_EQ(tokens.size(), 12);
  EXPECT_TRUE(tokens[0].IsIdent("START"));
  EXPECT_TRUE(tokens[1].IsSymbol(':'));
  EXPECT_TRUE(tokens[2].IsIdent("MOV"));
  EXPECT_TRUE(tokens[3].IsSymbol('.'));
  EXPECT_TRUE(tokens[4].IsIdent("LW"));
  EXPECT_TRUE(tokens[5].IsKeyword("R1"));
  EXPECT_TRUE(tokens[6].IsSymbol(','));
  EXPECT_TRUE(tokens[7].IsSymbol('('));
  EXPECT_TRUE(tokens[8].IsKeyword("SP"));
  EXPECT_TRUE(tokens[9].IsSymbol('+'));
  EXPECT_TRUE(tokens[10].IsInt(5));
  EXPECT_TRUE(tokens[11].IsSymbol(')'));
}

TEST_F(ProgramLexerTest, Directive) {
  std::vector<gb::Token> tokens = Lex(".dw 1");
  ASSERT_EQ(tokens.size(), 3);
  EXPECT_TRUE(tokens[0].IsSymbol('.'));
  EXPECT_TRUE(tokens[1].IsIdent("DW"));
  EXPECT_TRUE(tokens[2].IsInt(1));
}

TEST_F(ProgramLexerTest, CommentsAndLineBreaks) {
  std::vector<gb::Token> tokens =
      Lex("ADD ; a comment, with 'tokens'\n; only a comment\nSUB");
  ASSERT_EQ(tokens.size(), 4);
  EXPECT_TRUE(tokens[0].IsIdent("ADD"));
  EXPECT_EQ(tokens[1].GetType(), gb::kTokenLineBreak);
  EXPECT_EQ(tokens[2].GetType(), gb::kTokenLineBreak);
  EXPECT_TRUE(tokens[3].IsIdent("SUB"));
}

TEST_F(ProgramLexerTest, StringWordsOneCharacterPerWord) {
  EXPECT_THAT(*StringWords("'Hello'"), ElementsAre('H', 'e', 'l', 'l', 'o'));
  EXPECT_THAT(*StringWords("'A'"), ElementsAre('A'));
  EXPECT_THAT(*StringWords("''"), IsEmpty());
}

TEST_F(ProgramLexerTest, StringWordsTwoCharactersPerWord) {
  EXPECT_THAT(*StringWords("\"Hello\""), ElementsAre(0x4865, 0x6C6C, 0x6F00));
  EXPECT_THAT(*StringWords("\"AB\""), ElementsAre(0x4142));
  EXPECT_THAT(*StringWords("\"A\""), ElementsAre(0x4100));
  EXPECT_THAT(*StringWords("\"\""), IsEmpty());
}

TEST_F(ProgramLexerTest, StringWordsDoubledQuotes) {
  EXPECT_THAT(*StringWords("'It''s'"), ElementsAre('I', 't', '\'', 's'));
  EXPECT_THAT(*StringWords("'\"'"), ElementsAre('"'));
  EXPECT_THAT(*StringWords("\"\"\"\""), ElementsAre(0x2200));
  EXPECT_THAT(*StringWords("\"''\""), ElementsAre(0x2727));
}

TEST_F(ProgramLexerTest, StringWordsUtf8) {
  // U+00E9 (two bytes in UTF-8), and U+20AC (three bytes).
  EXPECT_THAT(*StringWords("'\xC3\xA9\xE2\x82\xAC'"),
              ElementsAre(0x00E9, 0x20AC));
  EXPECT_THAT(*StringWords("\"\xC3\xA9!\""), ElementsAre(0xE921));
}

TEST_F(ProgramLexerTest, StringWordsByteOrderMark) {
  EXPECT_THAT(*StringWords("'\xEF\xBB\xBF"
                           "A'"),
              ElementsAre(0xFEFF, 'A'));
  EXPECT_THAT(*StringWords("'\xEF\xBB\xBF'"), ElementsAre(0xFEFF));
}

TEST_F(ProgramLexerTest, StringWordsCharactersThatDontFit) {
  // U+1F600 (four bytes in UTF-8).
  absl::StatusOr<std::vector<uint16_t>> words =
      StringWords("'a\xF0\x9F\x98\x80'");
  ASSERT_FALSE(words.ok());
  EXPECT_THAT(words.status().message(),
              HasSubstr("U+1F600 does not fit in a word"));

  words = StringWords("\"\xE2\x82\xAC\"");
  ASSERT_FALSE(words.ok());
  EXPECT_THAT(words.status().message(),
              HasSubstr("U+20AC does not fit in a byte"));
}

TEST_F(ProgramLexerTest, InvalidUtf8IsNotAString) {
  for (const gb::Token& token : Lex("'\xFF'")) {
    EXPECT_FALSE(token.IsString());
  }
}

TEST(FormatHexNumberTest, Digits) {
  EXPECT_EQ(FormatHexNumber(0, 4), "0000h");
  EXPECT_EQ(FormatHexNumber(0x1234, 4), "1234h");
  EXPECT_EQ(FormatHexNumber(0x12, 8), "00000012h");
  EXPECT_EQ(FormatHexNumber(0x12345678, 8), "12345678h");
}

TEST(FormatHexNumberTest, LeadingLetter) {
  EXPECT_EQ(FormatHexNumber(0xFFFF, 4), "0FFFFh");
  EXPECT_EQ(FormatHexNumber(0xA000, 4), "0A000h");
  EXPECT_EQ(FormatHexNumber(0x9FFF, 4), "9FFFh");
  EXPECT_EQ(FormatHexNumber(0xFFFFFFFF, 8), "0FFFFFFFFh");
}

}  // namespace
}  // namespace oz3
