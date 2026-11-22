// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_macro.h"

#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "gb/parse/lexer.h"
#include "gb/parse/parse_error.h"
#include "gb/parse/parse_types.h"
#include "gb/parse/token.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/tools/program_lexer.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Pair;

constexpr int kLocalLabel = ProgramMacro::Replace::kLocalLabel;

class ProgramMacroTest : public ::testing::Test {
 protected:
  void SetUp() override {
    lexer_ = gb::Lexer::Create(GetProgramLexerConfig());
    ASSERT_NE(lexer_, nullptr);
  }

  // Adds a file to the lexer, and returns its content.
  gb::LexerContentId AddFile(std::string_view filename, std::string_view text) {
    const gb::LexerContentId content =
        lexer_->AddFileContent(filename, std::string(text));
    EXPECT_NE(content, gb::kNoLexerContent);
    return content;
  }

  // Returns the tokens `lexer_` reads next in `content`, to the end of the
  // line.
  std::vector<gb::Token> ReadTokens(gb::LexerContentId content) {
    std::vector<gb::Token> tokens;
    for (gb::Token token = lexer_->NextToken(content);
         token.GetType() != gb::kTokenLineBreak && !token.IsEnd();
         token = lexer_->NextToken(content)) {
      tokens.push_back(token);
    }
    return tokens;
  }

  // Returns a macro of `kind` with no lines, named and with the parameters as
  // `definition` writes them, as in "M PORT, REG".
  std::unique_ptr<ProgramMacro> CreateMacro(
      std::string_view definition,
      ProgramMacro::Kind kind = ProgramMacro::Kind::kMacro) {
    std::vector<gb::Token> tokens =
        ReadTokens(lexer_->AddContent(std::string(definition)));
    EXPECT_FALSE(tokens.empty());
    std::vector<gb::Token> parameters;
    for (int i = 1; i < static_cast<int>(tokens.size()); ++i) {
      if (!tokens[i].IsSymbol(',')) {
        parameters.push_back(tokens[i]);
      }
    }
    return std::make_unique<ProgramMacro>(tokens[0], std::move(parameters),
                                          kind);
  }

  // Returns a macro of `kind` defined by `definition` (see CreateMacro), whose
  // body is every line of the file `filename`, holding `body`, which must
  // read.
  std::unique_ptr<ProgramMacro> ReadMacro(
      std::string_view filename, std::string_view definition,
      std::string_view body,
      ProgramMacro::Kind kind = ProgramMacro::Kind::kMacro) {
    std::unique_ptr<ProgramMacro> macro = CreateMacro(definition, kind);
    const gb::LexerContentId content = AddFile(filename, body);
    while (lexer_->GetCurrentLine(content) < lexer_->GetLineCount(content)) {
      gb::ParseError error;
      EXPECT_TRUE(macro->ReadLine(*lexer_, content, &error))
          << error.FormatMessage();
    }
    return macro;
  }

  // Returns the text of each of the macro's lines.
  std::vector<std::string_view> GetLineTexts(const ProgramMacro& macro) {
    std::vector<std::string_view> texts;
    for (const ProgramMacro::Line& line : macro.GetLines()) {
      texts.push_back(lexer_->GetTokenRangeText(line.first, line.last));
    }
    return texts;
  }

  // Returns the argument from `first` to `last`, where they were written.
  MacroArg Arg(const gb::Token& first, const gb::Token& last) {
    return {.text = lexer_->GetTokenRangeText(first, last),
            .location = lexer_->GetTokenLocation(first)};
  }

  // Returns the text and parameter of each token the line replaces.
  std::vector<std::pair<std::string_view, int>> GetReplaces(
      const ProgramMacro::Line& line) {
    std::vector<std::pair<std::string_view, int>> replaces;
    for (const ProgramMacro::Replace& replace : line.replaces) {
      replaces.emplace_back(lexer_->GetTokenText(replace.token),
                            replace.parameter);
    }
    return replaces;
  }

  // Returns the location in `content`.
  gb::LexerLocation At(gb::LexerContentId content, int line, int column) {
    gb::LexerLocation location = lexer_->GetLineLocation(content, line);
    location.column = column;
    return location;
  }

  // Returns the location in the file `filename`, which was added.
  gb::LexerLocation At(std::string_view filename, int line, int column) {
    return At(lexer_->GetFileContentId(filename), line, column);
  }

  std::unique_ptr<gb::Lexer> lexer_;
};

//==============================================================================
// ProgramMacro
//==============================================================================

TEST_F(ProgramMacroTest, ReplacesInALine) {
  // A parameter is a word anywhere on the line, in any case: a label, an
  // instruction's name and extension, and an argument, but not a name inside
  // a string or a local label, or one that is only part of a word. Every
  // local label is replaced too. A line is from its first token to its last,
  // so not its comment.
  std::unique_ptr<ProgramMacro> macro =
      ReadMacro("macros.oza", "M REG, SIZE, VALUE, NAME",
                "name:  ADD.size Reg, value+1 ; reg\n"
                "  .DW 'reg', \"value\", @reg, regs, value\n");
  EXPECT_THAT(GetLineTexts(*macro),
              ElementsAre("name:  ADD.size Reg, value+1",
                          ".DW 'reg', \"value\", @reg, regs, value"));
  ASSERT_EQ(macro->GetLines().size(), 2);
  EXPECT_THAT(GetReplaces(macro->GetLines()[0]),
              ElementsAre(Pair("name", 3), Pair("size", 1), Pair("Reg", 0),
                          Pair("value", 2)));
  EXPECT_THAT(GetReplaces(macro->GetLines()[1]),
              ElementsAre(Pair("@reg", kLocalLabel), Pair("value", 2)));
  EXPECT_EQ(lexer_->GetTokenLocation(macro->GetLines()[1].first),
            At("macros.oza", 1, 2));
}

TEST_F(ProgramMacroTest, LinesWithNoTokens) {
  // A line with no tokens, or only a comment, adds no line.
  std::unique_ptr<ProgramMacro> macro = ReadMacro("macros.oza", "M REG",
                                                  "  NOP   ; A comment.\n"
                                                  "; Only a comment.\n"
                                                  "\n"
                                                  "@loop: JP @loop\n"
                                                  "  ; Last.");
  EXPECT_THAT(GetLineTexts(*macro), ElementsAre("NOP", "@loop: JP @loop"));
  ASSERT_EQ(macro->GetLines().size(), 2);
  EXPECT_THAT(GetReplaces(macro->GetLines()[0]), IsEmpty());
  EXPECT_THAT(
      GetReplaces(macro->GetLines()[1]),
      ElementsAre(Pair("@loop", kLocalLabel), Pair("@loop", kLocalLabel)));
}

TEST_F(ProgramMacroTest, Labels) {
  // A label at the start of a line must be a local label or a parameter.
  std::unique_ptr<ProgramMacro> macro = CreateMacro("M NAME");
  const gb::LexerContentId content = AddFile("macros.oza",
                                             "  name :  NOP\n"
                                             "  loop: JP loop\n"
                                             "  @loop: JP loop\n"
                                             "  WAIT#1@loop: NOP\n");
  gb::ParseError error;
  EXPECT_TRUE(macro->ReadLine(*lexer_, content, &error));
  EXPECT_FALSE(macro->ReadLine(*lexer_, content, &error));
  EXPECT_EQ(error.GetLocation(), At("macros.oza", 1, 2));
  EXPECT_EQ(error.GetMessage(),
            "Macro M can't define label loop, only local labels and its "
            "parameters");
  EXPECT_TRUE(macro->ReadLine(*lexer_, content, &error));

  // A macro label is read as any other token, as only the assembler knows
  // whether an expansion wrote it.
  EXPECT_TRUE(macro->ReadLine(*lexer_, content, &error));
  EXPECT_THAT(GetLineTexts(*macro),
              ElementsAre("name :  NOP", "@loop: JP loop", "WAIT#1@loop: NOP"));
  EXPECT_TRUE(lexer_->NextToken(content, /*advance=*/false).IsEnd());
}

TEST_F(ProgramMacroTest, Directives) {
  // A directive can't be named by a parameter, define a constant that isn't
  // one, or define a macro or include a file. An extension right after a name
  // isn't a directive, even after a parameter.
  std::unique_ptr<ProgramMacro> macro = CreateMacro("M OP, SIZE, X");
  const gb::LexerContentId content = AddFile("macros.oza",
                                             "  .DW x\n"
                                             "  .CONST x = 1\n"
                                             "  op.size 1\n"
                                             "  ADD.size R1, 1\n"
                                             "  x: .DD 1\n"
                                             "  .size 1\n"
                                             "  .CONST LIMIT = 5\n"
                                             "  x .MACRO N\n"
                                             "  x .ENDM\n"
                                             "  @l: .INCLUDE 'a.oza'\n");
  gb::ParseError error;
  for (int i = 0; i < 5; ++i) {
    EXPECT_TRUE(macro->ReadLine(*lexer_, content, &error))
        << error.FormatMessage();
  }
  const std::pair<std::pair<int, int>, std::string_view> errors[] = {
      {{5, 3}, "Macro M can't name a directive with its parameter SIZE"},
      {{6, 9}, "Macro M can't define constant LIMIT, only its parameters"},
      {{7, 4}, ".MACRO can't be in a macro's body"},
      {{8, 4}, ".ENDM must be on a line of its own"},
      {{9, 6}, "A macro's body can't include a file"},
  };
  for (const auto& [location, message] : errors) {
    EXPECT_FALSE(macro->ReadLine(*lexer_, content, &error)) << message;
    EXPECT_EQ(error.GetLocation(),
              At("macros.oza", location.first, location.second));
    EXPECT_EQ(error.GetMessage(), message);
  }
  EXPECT_EQ(macro->GetLines().size(), 5);
  EXPECT_TRUE(macro->IsExtended(0));
  EXPECT_FALSE(macro->IsExtended(1));
  EXPECT_FALSE(macro->IsExtended(2));
}

TEST_F(ProgramMacroTest, RepeatLabels) {
  // A repeated block's body can't define a label of any kind, as each
  // repetition would define it again, but it can use them, and macro labels
  // too, for a block in an expansion.
  std::unique_ptr<ProgramMacro> repeat =
      CreateMacro("REPT I", ProgramMacro::Kind::kRepeat);
  const gb::LexerContentId content = AddFile("main.oza",
                                             "  JP @loop, loop, M#1@loop\n"
                                             "  loop: NOP\n"
                                             "  @loop: NOP\n"
                                             "  M#1@loop: NOP\n"
                                             "  i: NOP\n");
  gb::ParseError error;
  EXPECT_TRUE(repeat->ReadLine(*lexer_, content, &error))
      << error.FormatMessage();
  const std::pair<int, std::string_view> errors[] = {
      {1, ".REPT can't define label loop"},
      {2, ".REPT can't define label @loop"},
      {3, ".REPT can't define label M#1@loop"},
      {4, ".REPT can't define label i"},
  };
  for (const auto& [line, message] : errors) {
    EXPECT_FALSE(repeat->ReadLine(*lexer_, content, &error)) << message;
    EXPECT_EQ(error.GetLocation(), At("main.oza", line, 2));
    EXPECT_EQ(error.GetMessage(), message);
  }
  ASSERT_EQ(repeat->GetLines().size(), 1);
  EXPECT_THAT(GetReplaces(repeat->GetLines()[0]), IsEmpty());
}

TEST_F(ProgramMacroTest, RepeatDirectives) {
  // A repeated block's body has the directives a macro's has, and its own
  // messages.
  std::unique_ptr<ProgramMacro> repeat =
      CreateMacro("REPT I", ProgramMacro::Kind::kRepeat);
  const gb::LexerContentId content = AddFile("main.oza",
                                             "  .DW i\n"
                                             "  .REPT i\n"
                                             "  .ENDR\n"
                                             "  .i 1\n"
                                             "  .CONST LIMIT = 5\n"
                                             "  .CONST i = 5\n"
                                             "  x .MACRO N\n"
                                             "  .ENDM\n"
                                             "  .INCLUDE 'a.oza'\n");
  gb::ParseError error;
  for (int i = 0; i < 3; ++i) {
    EXPECT_TRUE(repeat->ReadLine(*lexer_, content, &error))
        << error.FormatMessage();
  }
  const std::pair<std::pair<int, int>, std::string_view> errors[] = {
      {{3, 3}, ".REPT can't name a directive with its index I"},
      {{4, 9}, ".REPT can't define constant LIMIT"},
      {{5, 9}, ".REPT can't define constant I"},
      {{6, 4}, ".MACRO can't be in a .REPT's body"},
      {{7, 2}, ".ENDM can't be in a .REPT's body"},
      {{8, 2}, "A .REPT's body can't include a file"},
  };
  for (const auto& [location, message] : errors) {
    EXPECT_FALSE(repeat->ReadLine(*lexer_, content, &error)) << message;
    EXPECT_EQ(error.GetLocation(),
              At("main.oza", location.first, location.second));
    EXPECT_EQ(error.GetMessage(), message);
  }
  EXPECT_EQ(repeat->GetLines().size(), 3);
}

TEST_F(ProgramMacroTest, LineThatCantBeRead) {
  // A line with a token the lexer can't read isn't added, and the lexer goes
  // on to the next line.
  std::unique_ptr<ProgramMacro> macro = CreateMacro("M REG");
  const gb::LexerContentId content = AddFile("macros.oza",
                                             "  NOP 0FFFFFFFFFFFFFFFFFh reg\n"
                                             "  PUSH reg\n");
  gb::ParseError error;
  EXPECT_FALSE(macro->ReadLine(*lexer_, content, &error));
  EXPECT_EQ(error.GetLocation(), At("macros.oza", 0, 6));
  EXPECT_FALSE(error.GetMessage().empty());
  EXPECT_TRUE(macro->ReadLine(*lexer_, content, &error));
  EXPECT_THAT(GetLineTexts(*macro), ElementsAre("PUSH reg"));
}

//==============================================================================
// MacroExpansion
//==============================================================================

TEST_F(ProgramMacroTest, Expansion) {
  // Each parameter is replaced by its argument's text as the invocation wrote
  // it, each local label is renamed for the invocation, and each part of the
  // text is where it came from.
  std::unique_ptr<ProgramMacro> macro =
      ReadMacro("macros.oza", "M PORT, REG",
                "@wait:  IN reg, port   ; Read.\n"
                "        JZ reg, @wait\n"
                "\n"
                "        NOP\n");
  const std::vector<gb::Token> invocation =
      ReadTokens(AddFile("main.oza", "  M KEYBOARD, r1\n"));
  ASSERT_EQ(invocation.size(), 4);
  const MacroExpansion expansion(
      *lexer_, *macro, 2,
      {Arg(invocation[1], invocation[1]), Arg(invocation[3], invocation[3])});
  EXPECT_EQ(expansion.GetText(),
            "M#2@WAIT:  IN r1, KEYBOARD\n"
            "JZ r1, M#2@WAIT\n"
            "NOP\n");
  EXPECT_EQ(expansion.GetLineCount(), 3);

  // A renamed label starts at the label, and the body is where it came from.
  EXPECT_EQ(expansion.GetLocation(0, 0), At("macros.oza", 0, 0));
  EXPECT_EQ(expansion.GetLocation(0, 8), At("macros.oza", 0, 5));
  EXPECT_EQ(expansion.GetLocation(0, 11), At("macros.oza", 0, 8));
  EXPECT_EQ(expansion.GetLocation(0, 16), At("macros.oza", 0, 14));
  EXPECT_EQ(expansion.GetLocation(1, 0), At("macros.oza", 1, 8));
  EXPECT_EQ(expansion.GetLocation(1, 5), At("macros.oza", 1, 14));
  EXPECT_EQ(expansion.GetLocation(1, 7), At("macros.oza", 1, 16));
  EXPECT_EQ(expansion.GetLocation(2, 0), At("macros.oza", 3, 8));

  // The start, middle, and end of the arguments, where the invocation wrote
  // them.
  EXPECT_EQ(expansion.GetLocation(0, 14), At("main.oza", 0, 14));
  EXPECT_EQ(expansion.GetLocation(0, 15), At("main.oza", 0, 15));
  EXPECT_EQ(expansion.GetLocation(0, 18), At("main.oza", 0, 4));
  EXPECT_EQ(expansion.GetLocation(0, 25), At("main.oza", 0, 11));
  EXPECT_EQ(expansion.GetLocation(1, 3), At("main.oza", 0, 14));

  // Past the end of a line, which is past the end of the body's line, even
  // when it ends with a parameter or a local label.
  EXPECT_EQ(expansion.GetLocation(0, 26), At("macros.oza", 0, 20));
  EXPECT_EQ(expansion.GetLocation(0, 28), At("macros.oza", 0, 22));
  EXPECT_EQ(expansion.GetLocation(1, 15), At("macros.oza", 1, 21));
  EXPECT_EQ(expansion.GetLocation(2, 5), At("macros.oza", 3, 13));
}

TEST_F(ProgramMacroTest, ExpansionWithNoParameters) {
  std::unique_ptr<ProgramMacro> macro =
      ReadMacro("macros.oza", "M", "  NOP\n\n  HALT\n");
  const MacroExpansion expansion(*lexer_, *macro, 1, {});
  EXPECT_EQ(expansion.GetText(), "NOP\nHALT\n");
  EXPECT_EQ(expansion.GetLocation(1, 3), At("macros.oza", 2, 5));
}

TEST_F(ProgramMacroTest, ExpansionThatStartsWithAParameter) {
  // A parameter used as a label is replaced like any other, so the invocation
  // names the label.
  std::unique_ptr<ProgramMacro> macro =
      ReadMacro("macros.oza", "M NAME, X", "name: .DW x,x\n");
  const std::vector<gb::Token> invocation =
      ReadTokens(AddFile("main.oza", "M Table, 1 + 2\n"));
  ASSERT_EQ(invocation.size(), 6);
  const MacroExpansion expansion(
      *lexer_, *macro, 1,
      {Arg(invocation[1], invocation[1]), Arg(invocation[3], invocation[5])});
  EXPECT_EQ(expansion.GetText(), "Table: .DW 1 + 2,1 + 2\n");
  EXPECT_EQ(expansion.GetLocation(0, 0), At("main.oza", 0, 2));
  EXPECT_EQ(expansion.GetLocation(0, 5), At("macros.oza", 0, 4));
  EXPECT_EQ(expansion.GetLocation(0, 16), At("macros.oza", 0, 11));
  EXPECT_EQ(expansion.GetLocation(0, 17), At("main.oza", 0, 9));
}

TEST_F(ProgramMacroTest, ExpansionInAnExpansion) {
  // An invocation in an expansion has its arguments in the expansion's content,
  // so their text in its own expansion is there. A label the outer expansion
  // renamed is an argument like any other.
  std::unique_ptr<ProgramMacro> outer =
      ReadMacro("outer.oza", "M X", "@L: INNER (x), @L\n");
  std::unique_ptr<ProgramMacro> inner =
      ReadMacro("inner.oza", "INNER V, W", "  PUSH v\n  JP w\n");
  const std::vector<gb::Token> invocation =
      ReadTokens(AddFile("main.oza", "OUTER A + B\n"));
  ASSERT_EQ(invocation.size(), 4);
  const MacroExpansion outer_expansion(*lexer_, *outer, 1,
                                       {Arg(invocation[1], invocation[3])});
  ASSERT_EQ(outer_expansion.GetText(), "M#1@L: INNER (A + B), M#1@L\n");
  EXPECT_EQ(outer_expansion.GetLocation(0, 14), At("main.oza", 0, 6));
  EXPECT_EQ(outer_expansion.GetLocation(0, 22), At("outer.oza", 0, 15));

  // The invocation of INNER in the expansion, as the assembler reads it.
  const gb::LexerContentId outer_content =
      lexer_->AddContent(outer_expansion.GetText());
  const std::vector<gb::Token> inner_use = ReadTokens(outer_content);
  ASSERT_EQ(inner_use.size(), 10);
  EXPECT_TRUE(inner_use[0].IsUser(kTokenMacroLabel, "M#1@L"));
  EXPECT_TRUE(inner_use[2].IsIdent("INNER"));
  EXPECT_TRUE(inner_use[9].IsUser(kTokenMacroLabel, "M#1@L"));

  const MacroExpansion inner_expansion(
      *lexer_, *inner, 1,
      {Arg(inner_use[3], inner_use[7]), Arg(inner_use[9], inner_use[9])});
  EXPECT_EQ(inner_expansion.GetText(), "PUSH (A + B)\nJP M#1@L\n");
  EXPECT_EQ(inner_expansion.GetLocation(0, 0), At("inner.oza", 0, 2));
  EXPECT_EQ(inner_expansion.GetLocation(0, 5), At(outer_content, 0, 13));
  EXPECT_EQ(inner_expansion.GetLocation(0, 7), At(outer_content, 0, 15));
  EXPECT_EQ(inner_expansion.GetLocation(1, 3), At(outer_content, 0, 22));
}

TEST_F(ProgramMacroTest, RepeatExpansion) {
  // A repeated block's local labels aren't renamed, and its index, with no
  // location, is where the body names it.
  std::unique_ptr<ProgramMacro> repeat =
      ReadMacro("main.oza", "REPT I", "  JP @next\n  .DW i * 2, 'i'\n",
                ProgramMacro::Kind::kRepeat);
  const MacroExpansion expansion(*lexer_, *repeat, 3, {{.text = "12"}});
  EXPECT_EQ(expansion.GetText(), "JP @next\n.DW 12 * 2, 'i'\n");
  EXPECT_EQ(expansion.GetLocation(1, 4), At("main.oza", 1, 6));
  EXPECT_EQ(expansion.GetLocation(1, 5), At("main.oza", 1, 7));
  EXPECT_EQ(expansion.GetLocation(1, 7), At("main.oza", 1, 8));
}

}  // namespace
}  // namespace oz3
