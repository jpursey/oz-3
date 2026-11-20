// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_macro.h"

#include <memory>
#include <string>
#include <string_view>
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

  // Returns a macro whose body is every line of `body`, in the file
  // "macros.oza", which must read.
  std::unique_ptr<ProgramMacro> ReadMacro(std::vector<std::string> parameters,
                                          std::string_view body) {
    auto macro = std::make_unique<ProgramMacro>("M", std::move(parameters));
    macros_ = AddFile("macros.oza", body);
    for (int i = 0; i < lexer_->GetLineCount(macros_); ++i) {
      gb::Token error;
      EXPECT_TRUE(macro->ReadLine(*lexer_, macros_, &error))
          << error.GetString();
    }
    return macro;
  }

  // Returns the text of each argument after the first token of `line`.
  std::vector<std::string> ReadArgs(std::string_view line) {
    const gb::LexerContentId content = AddFile("args.oza", line);
    lexer_->NextToken(content);
    std::vector<std::string> args;
    const std::string_view text = lexer_->GetLineText(content, 0);
    for (const MacroArgSpan& arg : ReadMacroArgs(*lexer_, content)) {
      args.emplace_back(text.substr(arg.column, arg.size));
    }
    return args;
  }

  // Returns the location in `content` of `filename`.
  static gb::LexerLocation At(gb::LexerContentId content,
                              std::string_view filename, int line, int column) {
    return {.id = content,
            .filename = std::string(filename),
            .line = line,
            .column = column};
  }

  std::unique_ptr<gb::Lexer> lexer_;
  gb::LexerContentId macros_ = gb::kNoLexerContent;
};

//==============================================================================
// ProgramMacro
//==============================================================================

TEST_F(ProgramMacroTest, ParametersInALine) {
  // A parameter is a word anywhere on the line, in any case: a label, an
  // instruction's name and extension, and an argument, but not a name inside
  // a string or a local label, or one that is only part of a word. The
  // comment isn't part of the text.
  std::unique_ptr<ProgramMacro> macro =
      ReadMacro({"REG", "SIZE", "VALUE", "NAME"},
                "name:  ADD.size Reg, value+1 ; reg\n"
                "  .DW 'reg', \"value\", @reg, regs, value\n");
  ASSERT_EQ(macro->GetLines().size(), 2);
  const ProgramMacro::Line& first = macro->GetLines()[0];
  EXPECT_EQ(first.text, "name:  ADD.size Reg, value+1");
  EXPECT_EQ(first.location, At(macros_, "macros.oza", 0, 0));
  EXPECT_THAT(first.parameters,
              ElementsAre(Parameter{.column = 0, .size = 4, .index = 3},
                          Parameter{.column = 11, .size = 4, .index = 1},
                          Parameter{.column = 16, .size = 3, .index = 0},
                          Parameter{.column = 21, .size = 5, .index = 2}));
  const ProgramMacro::Line& second = macro->GetLines()[1];
  EXPECT_EQ(second.text, "  .DW 'reg', \"value\", @reg, regs, value");
  EXPECT_EQ(second.location, At(macros_, "macros.oza", 1, 0));
  EXPECT_THAT(second.parameters,
              ElementsAre(Parameter{.column = 34, .size = 5, .index = 2}));
}

TEST_F(ProgramMacroTest, LinesWithNoParameters) {
  std::unique_ptr<ProgramMacro> macro = ReadMacro({"REG"},
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
  const std::vector<MacroArgSpan> args = ReadMacroArgs(*lexer_, content);
  ASSERT_EQ(args.size(), 5);
  EXPECT_EQ(args[0].column, 2);
  EXPECT_EQ(args[0].size, 0);
  EXPECT_TRUE(args[0].token.IsSymbol(','));
  EXPECT_EQ(args[1].column, 3);
  EXPECT_EQ(args[1].size, 1);
  EXPECT_TRUE(args[1].token.IsInt(1));
  EXPECT_EQ(args[2].column, 5);
  EXPECT_EQ(args[2].size, 0);
  EXPECT_TRUE(args[2].token.IsSymbol(','));
  EXPECT_EQ(args[3].column, 6);
  EXPECT_EQ(args[3].size, 1);
  EXPECT_EQ(args[4].column, 8);
  EXPECT_EQ(args[4].size, 0);
  EXPECT_EQ(args[4].token.GetType(), gb::kTokenLineBreak);

  // The lexer is past the line.
  EXPECT_TRUE(lexer_->NextToken(content).IsIdent("NOP"));
}

//==============================================================================
// MacroExpansion
//==============================================================================

TEST_F(ProgramMacroTest, Expansion) {
  // Each parameter is replaced by its argument's text as the caller wrote it,
  // and each piece of the text is where it came from.
  std::unique_ptr<ProgramMacro> macro = ReadMacro({"PORT", "REG"},
                                                  "@wait:  IN reg, port\n"
                                                  "        JZ reg, @wait\n"
                                                  "        NOP\n");
  const gb::LexerContentId main = AddFile("main.oza", "");
  const SourceOrigin use = {.location = At(main, "main.oza", 5, 2),
                            .scope = "START"};
  const SourceText port = {
      .text = "KEYBOARD",
      .pieces = {{.column = 0,
                  .origin = {.location = At(main, "main.oza", 5, 9),
                             .scope = "START"}}}};
  const SourceText reg = {
      .text = "r1",
      .pieces = {{.column = 0,
                  .origin = {.location = At(main, "main.oza", 5, 19),
                             .scope = "START"}}}};
  const MacroExpansion expansion(macro.get(), {port, reg}, "M#1", use);
  EXPECT_EQ(&expansion.GetMacro(), macro.get());
  EXPECT_EQ(expansion.GetScope(), "M#1");
  EXPECT_EQ(expansion.GetUse(), use);
  EXPECT_EQ(expansion.GetText(),
            "@wait:  IN r1, KEYBOARD\n"
            "        JZ r1, @wait\n"
            "        NOP\n");

  // The start, middle, and end of the body's pieces, in the use's scope.
  const auto body = [&](int line, int column) {
    return SourceOrigin{.location = At(macros_, "macros.oza", line, column),
                        .scope = "M#1"};
  };
  EXPECT_EQ(expansion.GetOrigin(0, 0), body(0, 0));
  EXPECT_EQ(expansion.GetOrigin(0, 10), body(0, 10));
  EXPECT_EQ(expansion.GetOrigin(0, 13), body(0, 14));
  EXPECT_EQ(expansion.GetOrigin(0, 14), body(0, 15));
  EXPECT_EQ(expansion.GetOrigin(1, 13), body(1, 14));
  EXPECT_EQ(expansion.GetOrigin(1, 19), body(1, 20));
  EXPECT_EQ(expansion.GetOrigin(2, 8), body(2, 8));

  // The start, middle, and end of the arguments, in the caller's scope.
  const auto caller = [&](int column) {
    return SourceOrigin{.location = At(main, "main.oza", 5, column),
                        .scope = "START"};
  };
  EXPECT_EQ(expansion.GetOrigin(0, 11), caller(19));
  EXPECT_EQ(expansion.GetOrigin(0, 12), caller(20));
  EXPECT_EQ(expansion.GetOrigin(0, 15), caller(9));
  EXPECT_EQ(expansion.GetOrigin(0, 18), caller(12));
  EXPECT_EQ(expansion.GetOrigin(0, 22), caller(16));
  EXPECT_EQ(expansion.GetOrigin(1, 11), caller(19));
  EXPECT_EQ(expansion.GetOrigin(1, 12), caller(20));

  // Past the end of a line, and of the text.
  EXPECT_EQ(expansion.GetOrigin(0, 23), caller(17));
  EXPECT_EQ(expansion.GetOrigin(2, 11), body(2, 11));
  EXPECT_EQ(expansion.GetOrigin(3, 0), use);
}

TEST_F(ProgramMacroTest, ExpansionWithNoParameters) {
  std::unique_ptr<ProgramMacro> macro = ReadMacro({}, "  NOP\n\n  HALT\n");
  const SourceOrigin use = {.location = At(macros_, "macros.oza", 9, 0)};
  const MacroExpansion expansion(macro.get(), {}, "M#1", use);
  EXPECT_EQ(expansion.GetText(), "  NOP\n\n  HALT\n");
  EXPECT_EQ(expansion.GetOrigin(1, 0),
            (SourceOrigin{.location = At(macros_, "macros.oza", 1, 0),
                          .scope = "M#1"}));
  EXPECT_EQ(expansion.GetOrigin(2, 3),
            (SourceOrigin{.location = At(macros_, "macros.oza", 2, 3),
                          .scope = "M#1"}));
}

TEST_F(ProgramMacroTest, ExpansionInAnExpansion) {
  // An argument from an expansion's line keeps that line's origins, so every
  // origin in an expansion of it is in a file.
  std::unique_ptr<ProgramMacro> outer = ReadMacro({"X"}, "  INNER (x), 1\n");
  const gb::LexerContentId outer_body = macros_;
  std::unique_ptr<ProgramMacro> inner = ReadMacro({"V", "W"}, "  PUSH v\n");
  const gb::LexerContentId inner_body = macros_;
  const gb::LexerContentId main = AddFile("main.oza", "");
  const SourceOrigin use = {.location = At(main, "main.oza", 3, 0),
                            .scope = "START"};
  const SourceText x = {
      .text = "A + B",
      .pieces = {{.column = 0,
                  .origin = {.location = At(main, "main.oza", 3, 6),
                             .scope = "START"}}}};
  const MacroExpansion outer_expansion(outer.get(), {x}, "OUTER#1", use);
  ASSERT_EQ(outer_expansion.GetText(), "  INNER (A + B), 1\n");

  // The use of INNER in the expansion, as the assembler reads it.
  const gb::LexerContentId outer_content =
      lexer_->AddContent(outer_expansion.GetText());
  lexer_->NextToken(outer_content);
  const std::vector<MacroArgSpan> spans = ReadMacroArgs(*lexer_, outer_content);
  ASSERT_EQ(spans.size(), 2);
  std::vector<SourceText> args;
  for (const MacroArgSpan& span : spans) {
    args.push_back(outer_expansion.GetSourceText(0, span.column, span.size));
  }
  const auto outer_at = [&](int column) {
    return SourceOrigin{.location = At(outer_body, "macros.oza", 0, column),
                        .scope = "OUTER#1"};
  };
  const auto caller = [&](int column) {
    return SourceOrigin{.location = At(main, "main.oza", 3, column),
                        .scope = "START"};
  };
  EXPECT_EQ(args[0].text, "(A + B)");
  EXPECT_THAT(args[0].pieces,
              ElementsAre(Piece{.column = 0, .origin = outer_at(8)},
                          Piece{.column = 1, .origin = caller(6)},
                          Piece{.column = 6, .origin = outer_at(10)}));
  EXPECT_EQ(args[1].text, "1");
  EXPECT_THAT(args[1].pieces,
              ElementsAre(Piece{.column = 0, .origin = outer_at(13)}));

  const MacroExpansion inner_expansion(inner.get(), args, "INNER#2",
                                       outer_expansion.GetUse());
  EXPECT_EQ(inner_expansion.GetText(), "  PUSH (A + B)\n");
  EXPECT_EQ(inner_expansion.GetUse(), use);
  EXPECT_EQ(inner_expansion.GetOrigin(0, 2),
            (SourceOrigin{.location = At(inner_body, "macros.oza", 0, 2),
                          .scope = "INNER#2"}));
  EXPECT_EQ(inner_expansion.GetOrigin(0, 7), outer_at(8));
  EXPECT_EQ(inner_expansion.GetOrigin(0, 8), caller(6));
  EXPECT_EQ(inner_expansion.GetOrigin(0, 12), caller(10));
  EXPECT_EQ(inner_expansion.GetOrigin(0, 13), outer_at(10));
}

}  // namespace
}  // namespace oz3
