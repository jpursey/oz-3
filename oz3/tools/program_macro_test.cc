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
using Parameter = ProgramMacro::Line::Parameter;
using Piece = SourceText::Piece;

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

  // Returns a macro whose body is every line of the file `filename`, holding
  // `body`, which must read.
  std::unique_ptr<ProgramMacro> ReadMacro(std::string_view filename,
                                          std::vector<std::string> parameters,
                                          std::string_view body) {
    auto macro = std::make_unique<ProgramMacro>("M", std::move(parameters));
    const gb::LexerContentId content = AddFile(filename, body);
    for (int i = 0; i < lexer_->GetLineCount(content); ++i) {
      gb::Token error;
      EXPECT_TRUE(macro->ReadLine(*lexer_, content, &error))
          << error.GetString();
    }
    return macro;
  }

  // Returns the arguments `lexer_` reads next in `content`, which must read.
  std::vector<MacroArgSpan> ReadArgSpans(gb::LexerContentId content) {
    std::vector<MacroArgSpan> args;
    gb::Token error;
    EXPECT_TRUE(ReadMacroArgs(*lexer_, content, &args, &error))
        << error.GetString();
    return args;
  }

  // Returns the text of each argument after the first token of `line`.
  std::vector<std::string> ReadArgs(std::string_view line) {
    const gb::LexerContentId content = AddFile("args.oza", line);
    lexer_->NextToken(content);
    std::vector<std::string> args;
    const std::string_view text = lexer_->GetLineText(content, 0);
    for (const MacroArgSpan& arg : ReadArgSpans(content)) {
      args.emplace_back(text.substr(arg.column, arg.size));
    }
    return args;
  }

  // Returns the location in the file `filename`, which was added.
  gb::LexerLocation At(std::string_view filename, int line, int column) {
    return {.id = lexer_->GetFileContentId(filename),
            .filename = std::string(filename),
            .line = line,
            .column = column};
  }

  SourceOrigin Origin(std::string_view filename, int line, int column,
                      std::string_view scope) {
    return {.location = At(filename, line, column), .scope = scope};
  }

  std::unique_ptr<gb::Lexer> lexer_;
};

//==============================================================================
// SourceText
//==============================================================================

TEST_F(ProgramMacroTest, SourceText) {
  AddFile("a.oza", "");
  AddFile("b.oza", "");
  SourceText b("XY", Origin("b.oza", 4, 2, "B"));
  b.Append("Z", Origin("b.oza", 9, 0, "B"));
  SourceText text("abc", Origin("a.oza", 1, 5, "A"));
  text.Append(b);
  text.Append("", Origin("a.oza", 1, 9, "A"));
  EXPECT_EQ(text.GetText(), "abcXYZ");
  EXPECT_THAT(text.GetPieces(),
              ElementsAre(Piece{0, Origin("a.oza", 1, 5, "A")},
                          Piece{3, Origin("b.oza", 4, 2, "B")},
                          Piece{5, Origin("b.oza", 9, 0, "B")},
                          Piece{6, Origin("a.oza", 1, 9, "A")}));
  EXPECT_EQ(text.GetOrigin(0), Origin("a.oza", 1, 5, "A"));
  EXPECT_EQ(text.GetOrigin(2), Origin("a.oza", 1, 7, "A"));
  EXPECT_EQ(text.GetOrigin(4), Origin("b.oza", 4, 3, "B"));
  EXPECT_EQ(text.GetOrigin(6), Origin("a.oza", 1, 9, "A"));
  EXPECT_EQ(text.GetOrigin(8), Origin("a.oza", 1, 11, "A"));

  // Text appended where an empty piece is takes its place.
  text.Append("!", Origin("a.oza", 2, 0, "A"));
  EXPECT_EQ(text.GetPieces().back(), (Piece{6, Origin("a.oza", 2, 0, "A")}));

  // A substring starts at its column's origin, and has the pieces after it.
  SourceText sub = text.Substr(1, 3);
  EXPECT_EQ(sub.GetText(), "bcX");
  EXPECT_THAT(sub.GetPieces(),
              ElementsAre(Piece{0, Origin("a.oza", 1, 6, "A")},
                          Piece{2, Origin("b.oza", 4, 2, "B")}));
  sub = text.Substr(4, 10);
  EXPECT_EQ(sub.GetText(), "YZ!");
  EXPECT_THAT(sub.GetPieces(),
              ElementsAre(Piece{0, Origin("b.oza", 4, 3, "B")},
                          Piece{1, Origin("b.oza", 9, 0, "B")},
                          Piece{2, Origin("a.oza", 2, 0, "A")}));
  sub = text.Substr(7, 0);
  EXPECT_EQ(sub.GetText(), "");
  EXPECT_THAT(sub.GetPieces(),
              ElementsAre(Piece{0, Origin("a.oza", 2, 1, "A")}));
}

//==============================================================================
// ProgramMacro
//==============================================================================

TEST_F(ProgramMacroTest, ParametersInALine) {
  // A parameter is a word anywhere on the line, in any case: a label, an
  // instruction's name and extension, and an argument, but not a name inside
  // a string or a local label, or one that is only part of a word. The
  // comment isn't part of the text.
  std::unique_ptr<ProgramMacro> macro =
      ReadMacro("macros.oza", {"REG", "SIZE", "VALUE", "NAME"},
                "name:  ADD.size Reg, value+1 ; reg\n"
                "  .DW 'reg', \"value\", @reg, regs, value\n");
  ASSERT_EQ(macro->GetLines().size(), 2);
  const ProgramMacro::Line& first = macro->GetLines()[0];
  EXPECT_EQ(first.text, "name:  ADD.size Reg, value+1");
  EXPECT_EQ(first.location, At("macros.oza", 0, 0));
  EXPECT_THAT(first.parameters,
              ElementsAre(Parameter{.column = 0, .size = 4, .index = 3},
                          Parameter{.column = 11, .size = 4, .index = 1},
                          Parameter{.column = 16, .size = 3, .index = 0},
                          Parameter{.column = 21, .size = 5, .index = 2}));
  const ProgramMacro::Line& second = macro->GetLines()[1];
  EXPECT_EQ(second.text, "  .DW 'reg', \"value\", @reg, regs, value");
  EXPECT_EQ(second.location, At("macros.oza", 1, 0));
  EXPECT_THAT(second.parameters,
              ElementsAre(Parameter{.column = 34, .size = 5, .index = 2}));
}

TEST_F(ProgramMacroTest, LinesWithNoParameters) {
  std::unique_ptr<ProgramMacro> macro = ReadMacro("macros.oza", {"REG"},
                                                  "  NOP   ; A comment.\n"
                                                  "; Only a comment.\n"
                                                  "\n"
                                                  "@loop: JP @loop");
  ASSERT_EQ(macro->GetLines().size(), 4);
  EXPECT_EQ(macro->GetLines()[0].text, "  NOP");
  EXPECT_EQ(macro->GetLines()[1].text, "");
  EXPECT_EQ(macro->GetLines()[2].text, "");
  EXPECT_EQ(macro->GetLines()[3].text, "@loop: JP @loop");
  for (const ProgramMacro::Line& line : macro->GetLines()) {
    EXPECT_THAT(line.parameters, IsEmpty());
  }
}

TEST_F(ProgramMacroTest, LineThatCantBeRead) {
  // A line with a token the lexer can't read isn't added, and the lexer goes
  // on to the next line.
  ProgramMacro macro("M", {"REG"});
  const gb::LexerContentId content = AddFile("macros.oza",
                                             "  NOP 0FFFFFFFFFFFFFFFFFh reg\n"
                                             "  PUSH reg\n");
  gb::Token error;
  EXPECT_FALSE(macro.ReadLine(*lexer_, content, &error));
  EXPECT_TRUE(error.IsError());
  EXPECT_EQ(lexer_->GetTokenLocation(error).column, 6);
  EXPECT_TRUE(macro.ReadLine(*lexer_, content, &error));
  ASSERT_EQ(macro.GetLines().size(), 1);
  EXPECT_EQ(macro.GetLines()[0].text, "  PUSH reg");
  EXPECT_EQ(macro.GetLines()[0].location.line, 1);
}

TEST_F(ProgramMacroTest, NoLineLeft) {
  // At the end of the content, there is no line to read.
  ProgramMacro macro("M", {});
  const gb::LexerContentId content = AddFile("macros.oza", "  NOP");
  gb::Token error;
  EXPECT_TRUE(macro.ReadLine(*lexer_, content, &error));
  EXPECT_FALSE(macro.ReadLine(*lexer_, content, &error));
  EXPECT_TRUE(error.IsEnd());
  EXPECT_EQ(macro.GetLines().size(), 1);
}

//==============================================================================
// Macro arguments
//==============================================================================

TEST_F(ProgramMacroTest, Args) {
  // Commas inside brackets or strings don't separate arguments.
  EXPECT_THAT(
      ReadArgs("M D(table), (R1), 'a,b', [1, 2], {x, y}, \"c\""),
      ElementsAre("D(table)", "(R1)", "'a,b'", "[1, 2]", "{x, y}", "\"c\""));
  EXPECT_THAT(ReadArgs("M ((1, 2), 3), 4"), ElementsAre("((1, 2), 3)", "4"));

  // An argument is its text from its first token to its last, without the
  // comment.
  EXPECT_THAT(ReadArgs("M   1 +  2  ,x ; a, b"), ElementsAre("1 +  2", "x"));
  EXPECT_THAT(ReadArgs("M ADD.W R1, 2"), ElementsAre("ADD.W R1", "2"));
  EXPECT_THAT(ReadArgs("M"), IsEmpty());
  EXPECT_THAT(ReadArgs("M ; Nothing."), IsEmpty());
}

TEST_F(ProgramMacroTest, EmptyArgs) {
  // An empty argument is at the token after it.
  const gb::LexerContentId content = AddFile("args.oza", "M ,1,,2,\nNOP\n");
  lexer_->NextToken(content);
  const std::vector<MacroArgSpan> args = ReadArgSpans(content);
  std::vector<std::pair<int, int>> spans;
  for (const MacroArgSpan& arg : args) {
    spans.emplace_back(arg.column, arg.size);
  }
  EXPECT_THAT(spans, ElementsAre(Pair(2, 0), Pair(3, 1), Pair(5, 0), Pair(6, 1),
                                 Pair(8, 0)));
  ASSERT_EQ(args.size(), 5);
  EXPECT_TRUE(args[0].token.IsSymbol(','));
  EXPECT_TRUE(args[1].token.IsInt(1));
  EXPECT_EQ(args[4].token.GetType(), gb::kTokenLineBreak);

  // The lexer is past the line.
  EXPECT_TRUE(lexer_->NextToken(content).IsIdent("NOP"));
}

TEST_F(ProgramMacroTest, EmptyLastArgBeforeAComment) {
  // An empty last argument is after its comma, not at the line break, which
  // is after the comment.
  const gb::LexerContentId content = AddFile("args.oza", "M 1, ; Note.\n");
  lexer_->NextToken(content);
  const std::vector<MacroArgSpan> args = ReadArgSpans(content);
  ASSERT_EQ(args.size(), 2);
  EXPECT_EQ(args[1].column, 4);
  EXPECT_EQ(args[1].size, 0);
  EXPECT_EQ(args[1].token.GetType(), gb::kTokenLineBreak);
}

TEST_F(ProgramMacroTest, ArgThatCantBeRead) {
  // A line with a token the lexer can't read has no arguments, and the lexer
  // goes on to the next line.
  const gb::LexerContentId content =
      AddFile("args.oza", "M 1, 0FFFFFFFFFFFFFFFFFh, 2\nNOP\n");
  lexer_->NextToken(content);
  std::vector<MacroArgSpan> args;
  gb::Token error;
  EXPECT_FALSE(ReadMacroArgs(*lexer_, content, &args, &error));
  EXPECT_TRUE(error.IsError());
  EXPECT_EQ(lexer_->GetTokenLocation(error).column, 5);
  EXPECT_THAT(args, IsEmpty());
  EXPECT_TRUE(lexer_->NextToken(content).IsIdent("NOP"));
}

//==============================================================================
// MacroExpansion
//==============================================================================

TEST_F(ProgramMacroTest, Expansion) {
  // Each parameter is replaced by its argument's text as the caller wrote it,
  // and each piece of the text is where it came from.
  std::unique_ptr<ProgramMacro> macro = ReadMacro("macros.oza", {"PORT", "REG"},
                                                  "@wait:  IN reg, port\n"
                                                  "        JZ reg, @wait\n"
                                                  "        NOP\n");
  AddFile("main.oza", "");
  const SourceOrigin use = Origin("main.oza", 5, 2, "START");
  const SourceText port("KEYBOARD", Origin("main.oza", 5, 9, "START"));
  const SourceText reg("r1", Origin("main.oza", 5, 19, "START"));
  const MacroExpansion expansion(*macro, {port, reg}, "M#1", use);
  EXPECT_EQ(expansion.GetUse(), use);
  EXPECT_EQ(expansion.GetText(),
            "@wait:  IN r1, KEYBOARD\n"
            "        JZ r1, @wait\n"
            "        NOP\n");
  EXPECT_EQ(expansion.GetLine(1).GetText(), "        JZ r1, @wait");

  // The start, middle, and end of the body's pieces, in the use's scope.
  EXPECT_EQ(expansion.GetOrigin(0, 0), Origin("macros.oza", 0, 0, "M#1"));
  EXPECT_EQ(expansion.GetOrigin(0, 10), Origin("macros.oza", 0, 10, "M#1"));
  EXPECT_EQ(expansion.GetOrigin(0, 13), Origin("macros.oza", 0, 14, "M#1"));
  EXPECT_EQ(expansion.GetOrigin(0, 14), Origin("macros.oza", 0, 15, "M#1"));
  EXPECT_EQ(expansion.GetOrigin(1, 13), Origin("macros.oza", 1, 14, "M#1"));
  EXPECT_EQ(expansion.GetOrigin(1, 19), Origin("macros.oza", 1, 20, "M#1"));
  EXPECT_EQ(expansion.GetOrigin(2, 8), Origin("macros.oza", 2, 8, "M#1"));

  // The start, middle, and end of the arguments, in the caller's scope.
  EXPECT_EQ(expansion.GetOrigin(0, 11), Origin("main.oza", 5, 19, "START"));
  EXPECT_EQ(expansion.GetOrigin(0, 12), Origin("main.oza", 5, 20, "START"));
  EXPECT_EQ(expansion.GetOrigin(0, 15), Origin("main.oza", 5, 9, "START"));
  EXPECT_EQ(expansion.GetOrigin(0, 18), Origin("main.oza", 5, 12, "START"));
  EXPECT_EQ(expansion.GetOrigin(0, 22), Origin("main.oza", 5, 16, "START"));
  EXPECT_EQ(expansion.GetOrigin(1, 11), Origin("main.oza", 5, 19, "START"));
  EXPECT_EQ(expansion.GetOrigin(1, 12), Origin("main.oza", 5, 20, "START"));

  // Past the end of a line, which is past the end of the body's line, even
  // when it ends with a parameter, and past the end of the text.
  EXPECT_EQ(expansion.GetOrigin(0, 23), Origin("macros.oza", 0, 20, "M#1"));
  EXPECT_EQ(expansion.GetOrigin(0, 25), Origin("macros.oza", 0, 22, "M#1"));
  EXPECT_EQ(expansion.GetOrigin(2, 11), Origin("macros.oza", 2, 11, "M#1"));
  EXPECT_EQ(expansion.GetOrigin(3, 0), use);
}

TEST_F(ProgramMacroTest, ExpansionWithNoParameters) {
  std::unique_ptr<ProgramMacro> macro =
      ReadMacro("macros.oza", {}, "  NOP\n\n  HALT\n");
  const SourceOrigin use = {.location = At("macros.oza", 9, 0)};
  const MacroExpansion expansion(*macro, {}, "M#1", use);
  EXPECT_EQ(expansion.GetText(), "  NOP\n\n  HALT\n");
  EXPECT_EQ(expansion.GetOrigin(1, 0), Origin("macros.oza", 1, 0, "M#1"));
  EXPECT_EQ(expansion.GetOrigin(2, 3), Origin("macros.oza", 2, 3, "M#1"));
}

TEST_F(ProgramMacroTest, ExpansionWithEmptyArgs) {
  // An empty argument has no piece of its own.
  std::unique_ptr<ProgramMacro> macro =
      ReadMacro("macros.oza", {"A", "B"}, "a b,a\n");
  AddFile("main.oza", "");
  const SourceText empty("", Origin("main.oza", 0, 2, "START"));
  const SourceText x("X", Origin("main.oza", 0, 4, "START"));
  const MacroExpansion expansion(*macro, {empty, x}, "M#1", {});
  EXPECT_EQ(expansion.GetText(), " X,\n");
  EXPECT_THAT(expansion.GetLine(0).GetPieces(),
              ElementsAre(Piece{0, Origin("macros.oza", 0, 1, "M#1")},
                          Piece{1, Origin("main.oza", 0, 4, "START")},
                          Piece{2, Origin("macros.oza", 0, 3, "M#1")},
                          Piece{3, Origin("macros.oza", 0, 5, "M#1")}));
}

TEST_F(ProgramMacroTest, ExpansionInAnExpansion) {
  // An argument from an expansion's line keeps that line's origins, so every
  // origin in an expansion of it is in a file.
  std::unique_ptr<ProgramMacro> outer =
      ReadMacro("outer.oza", {"X"}, "  INNER (x), 1\n");
  std::unique_ptr<ProgramMacro> inner =
      ReadMacro("inner.oza", {"V", "W"}, "  PUSH v\n");
  AddFile("main.oza", "");
  const SourceOrigin use = Origin("main.oza", 3, 0, "START");
  const SourceText x("A + B", Origin("main.oza", 3, 6, "START"));
  const MacroExpansion outer_expansion(*outer, {x}, "OUTER#1", use);
  ASSERT_EQ(outer_expansion.GetText(), "  INNER (A + B), 1\n");

  // The use of INNER in the expansion, as the assembler reads it.
  const gb::LexerContentId outer_content =
      lexer_->AddContent(outer_expansion.GetText());
  lexer_->NextToken(outer_content);
  std::vector<SourceText> args;
  for (const MacroArgSpan& span : ReadArgSpans(outer_content)) {
    args.push_back(outer_expansion.GetLine(0).Substr(span.column, span.size));
  }
  ASSERT_EQ(args.size(), 2);
  EXPECT_EQ(args[0].GetText(), "(A + B)");
  EXPECT_THAT(args[0].GetPieces(),
              ElementsAre(Piece{0, Origin("outer.oza", 0, 8, "OUTER#1")},
                          Piece{1, Origin("main.oza", 3, 6, "START")},
                          Piece{6, Origin("outer.oza", 0, 10, "OUTER#1")}));
  EXPECT_EQ(args[1].GetText(), "1");
  EXPECT_THAT(args[1].GetPieces(),
              ElementsAre(Piece{0, Origin("outer.oza", 0, 13, "OUTER#1")}));

  const MacroExpansion inner_expansion(*inner, args, "INNER#2",
                                       outer_expansion.GetUse());
  EXPECT_EQ(inner_expansion.GetText(), "  PUSH (A + B)\n");
  EXPECT_EQ(inner_expansion.GetUse(), use);
  EXPECT_EQ(inner_expansion.GetOrigin(0, 2),
            Origin("inner.oza", 0, 2, "INNER#2"));
  EXPECT_EQ(inner_expansion.GetOrigin(0, 7),
            Origin("outer.oza", 0, 8, "OUTER#1"));
  EXPECT_EQ(inner_expansion.GetOrigin(0, 8), Origin("main.oza", 3, 6, "START"));
  EXPECT_EQ(inner_expansion.GetOrigin(0, 12),
            Origin("main.oza", 3, 10, "START"));
  EXPECT_EQ(inner_expansion.GetOrigin(0, 13),
            Origin("outer.oza", 0, 10, "OUTER#1"));
}

}  // namespace
}  // namespace oz3
