// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_grammar.h"

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/log/check.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "gb/parse/lexer.h"
#include "gb/parse/parse_error.h"
#include "gb/parse/parse_result.h"
#include "gb/parse/parser.h"
#include "gb/parse/token.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/instruction_def.h"
#include "oz3/tools/expression.h"
#include "oz3/tools/instruction_assembler.h"
#include "oz3/tools/instruction_set_syntax.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Not;
using ::testing::UnorderedElementsAre;

constexpr std::string_view kInstructionSet = R"---(
  values(bits:3) Condition { NZ = 0, Z = 4 }

  macro GetWord:r {
    code:m "$r" { UL; }
    code:C2 "($r + $v)" { LD(C2); UL; ADD(C2,m); LKR(m); ADR(C2); LD(C2); UL; }
    code:C2 "$v" { LD(C2); UL; }
    code:C2 "D($v)" { LD(C2); UL; LK(DATA); ADR(C2); LD(C2); UL; }
  }

  macro Bit:r {
    code:C0 "0" { UL; MVBI(C0:L,0x01); }
    code:C0 "1" { UL; MVBI(C0:L,0x02); }
    code:C0 "$r" { UL; MOV(C0,m); }
  }

  instruction(opcode:0) NOP { UL; }
  instruction(opcode:1) ADD.W "$r, $m" { $GetWord; ADD(a,r); }
  instruction(opcode:2) ADD.D "$R, $V" { LD(C0); LD(C1); UL; }
  instruction(opcode:3) PUSH.W "$v" { LD(C0); UL; }
  instruction(opcode:4) PUSH.D "$V" { LD(C0); LD(C1); UL; }
  instruction(opcode:5) SETC "$Condition" { UL; }
  instruction(opcode:6) ADQ.S "SP, $#5" { UL; ADD(SP,C0); }
  instruction(opcode:7) MOVX "$r4, $r2" { UL; }
  instruction(opcode:8) MOVD "$R1, $R" { UL; }
  instruction(opcode:9) RST "SELF, $#2" { UL; }
  instruction(opcode:10) RST.C "$#3, $#2" { UL; }
  instruction(opcode:11) BIT "$r, $m" { $Bit; MOV(a,r); }
  instruction(opcode:12) OUTQ "$#3, 1" { UL; }
)---";

// Symbols by name.
class FakeContext : public ExpressionContext {
 public:
  uint16_t GetOffset() const override { return 0; }
  std::optional<ExpressionResult> GetSymbol(
      std::string_view name, ExpressionWidth width) const override {
    auto it = symbols.find(name);
    if (it == symbols.end()) {
      return std::nullopt;
    }
    return ExpressionResult{.value = it->second};
  }

  absl::flat_hash_map<std::string, uint32_t> symbols;
};

class ProgramGrammarTest : public ::testing::Test {
 protected:
  ProgramGrammarTest() {
    gb::ParseError error;
    asm_set_ = AssembleInstructionSet(std::string(kInstructionSet), &error);
    CHECK(asm_set_ != nullptr) << error.FormatMessage();
    InstructionError syntax_error;
    syntax_ = InstructionSetSyntax::Create(asm_set_->GetInstructionSetDef(),
                                           asm_set_->GetInstructionSet().get(),
                                           &syntax_error);
    CHECK(syntax_ != nullptr) << syntax_error.message;
    absl::StatusOr<std::unique_ptr<ProgramGrammar>> grammar =
        ProgramGrammar::Create(*syntax_);
    CHECK(grammar.ok()) << grammar.status();
    grammar_ = *std::move(grammar);
    parser_ = gb::Parser::Create(grammar_->GetParserProgram());
    CHECK(parser_ != nullptr);
  }

  gb::Lexer& GetLexer() { return parser_->GetLexer(); }

  // Starts parsing `text`.
  void SetText(std::string_view text) {
    content_ = GetLexer().AddContent(std::string(text));
    CHECK(content_ != gb::kNoLexerContent);
  }

  // Parses the next line.
  gb::ParseResult ParseNextLine() {
    return parser_->Parse(content_, ProgramGrammar::kLineRule);
  }

  // Parses `text` as one line, which must parse, and returns it.
  gb::ParseResult ParseLine(std::string_view text) {
    SetText(text);
    gb::ParseResult line = ParseNextLine();
    EXPECT_TRUE(line.IsOk()) << text << ": " << line.GetError().FormatMessage();
    return line;
  }

  // Parses `text` as one line, which must not parse, and returns the error.
  gb::ParseError ParseLineError(std::string_view text) {
    SetText(text);
    gb::ParseResult line = ParseNextLine();
    EXPECT_FALSE(line.IsOk()) << text;
    return line.IsOk() ? gb::ParseError() : line.GetError();
  }

  // Parses `text` as a line with a directive, and returns the line.
  gb::ParseResult ParseDirective(std::string_view text) {
    gb::ParseResult line = ParseLine(text);
    if (line.IsOk()) {
      EXPECT_NE(line->GetItem("directive"), nullptr) << text;
    }
    return line;
  }

  // Parses `text` as a line with an instruction, and matches it, setting
  // `matches_`. Returns MatchInstruction's result.
  bool MatchInstruction(std::string_view text, gb::ParseError& error) {
    line_ = std::make_unique<gb::ParseResult>(ParseLine(text));
    CHECK(line_->IsOk()) << text;
    CHECK((*line_)->GetItem("instruction") != nullptr) << text;
    return grammar_->MatchInstruction(*parser_, content_, **line_, context_,
                                      matches_, error);
  }

  // Matches the instruction of the line `text`, and returns the forms it
  // matched as "NAME pattern". Its matches are in `matches_`.
  std::vector<std::string> Match(std::string_view text) {
    gb::ParseError error;
    if (!MatchInstruction(text, error)) {
      ADD_FAILURE() << text << ": " << error.FormatMessage();
      return {};
    }
    std::vector<std::string> forms;
    for (const InstructionMatch& match : matches_) {
      forms.push_back(absl::StrCat(match.form->GetInstruction().op_name, " ",
                                   match.form->GetPattern()));
    }
    return forms;
  }

  // Matches the instruction of the line `text`, which must have an error, and
  // returns it.
  gb::ParseError MatchError(std::string_view text) {
    gb::ParseError error;
    EXPECT_FALSE(MatchInstruction(text, error)) << text;
    EXPECT_THAT(matches_, IsEmpty());
    return error;
  }

  // Returns the value of argument `index` of the only match.
  uint32_t ArgValue(int index, const ExpressionContext& context) {
    CHECK_EQ(matches_.size(), 1);
    const InstructionArg& arg = matches_[0].args[index];
    CHECK(arg.value.has_value());
    ExpressionResult result =
        arg.value->Evaluate(ExpressionWidth::kWord, context);
    EXPECT_EQ(result.kind, ExpressionResult::Kind::kValue)
        << result.error.FormatMessage();
    return result.value;
  }
  uint32_t ArgValue(int index) { return ArgValue(index, context_); }

  // Returns the register number of argument `index` of the only match.
  int ArgReg(int index) {
    CHECK_EQ(matches_.size(), 1);
    EXPECT_FALSE(matches_[0].args[index].value.has_value());
    return matches_[0].args[index].reg;
  }

  // Returns the text of the lexer's next token, which is empty at the end.
  std::string_view NextTokenText() {
    return GetLexer().GetTokenText(GetLexer().NextToken(content_, false));
  }

  std::unique_ptr<AsmInstructionSet> asm_set_;
  std::unique_ptr<InstructionSetSyntax> syntax_;
  std::unique_ptr<ProgramGrammar> grammar_;
  std::unique_ptr<gb::Parser> parser_;
  gb::LexerContentId content_ = gb::kNoLexerContent;
  FakeContext context_;
  std::unique_ptr<gb::ParseResult> line_;
  std::vector<InstructionMatch> matches_;
};

//==============================================================================
// Lines
//==============================================================================

TEST_F(ProgramGrammarTest, Lines) {
  SetText("loop:\n@next: NOP\n\nADD: ADD R1, 5\n.DS 1");
  gb::ParseResult line = ParseNextLine();
  ASSERT_TRUE(line.IsOk());
  EXPECT_EQ(line->GetItem("label")->GetString("name"), "LOOP");
  EXPECT_EQ(line->GetItem("directive"), nullptr);
  EXPECT_EQ(line->GetItem("instruction"), nullptr);

  line = ParseNextLine();
  ASSERT_TRUE(line.IsOk());
  EXPECT_EQ(line->GetItem("label")->GetString("local_label"), "@next");
  EXPECT_EQ(line->GetItem("instruction")->GetString("name"), "NOP");

  line = ParseNextLine();
  ASSERT_TRUE(line.IsOk());
  EXPECT_EQ(line->GetItem("label"), nullptr);
  EXPECT_EQ(line->GetItem("instruction"), nullptr);

  // A label can share a name with an instruction.
  line = ParseNextLine();
  ASSERT_TRUE(line.IsOk());
  EXPECT_EQ(line->GetItem("label")->GetString("name"), "ADD");
  EXPECT_EQ(line->GetItem("instruction")->GetString("name"), "ADD");

  // The last line ends at the end of the content.
  line = ParseNextLine();
  ASSERT_TRUE(line.IsOk());
  EXPECT_EQ(line->GetItem("directive")->GetString("directive"), "DS");
  EXPECT_TRUE(GetLexer().NextToken(content_, false).IsEnd());
}

TEST_F(ProgramGrammarTest, LineAfterAnError) {
  // A line that doesn't parse leaves the lexer at its start, so the next line
  // can be read after it.
  SetText("5 + 1\nNOP\n");
  EXPECT_FALSE(ParseNextLine().IsOk());
  GetLexer().NextLine(content_);
  gb::ParseResult line = ParseNextLine();
  ASSERT_TRUE(line.IsOk());
  EXPECT_EQ(line->GetItem("instruction")->GetString("name"), "NOP");
}

TEST_F(ProgramGrammarTest, LineErrors) {
  EXPECT_EQ(ParseLineError("loop: 5").GetLocation().column, 6);
  EXPECT_EQ(ParseLineError("NOP: .DS 1 2").GetLocation().column, 11);
  EXPECT_EQ(ParseLineError("5").GetLocation().column, 0);
}

TEST_F(ProgramGrammarTest, Directives) {
  gb::ParseResult line = ParseDirective(".include \"file.oza\"");
  const gb::ParsedItem* directive = line->GetItem("directive");
  EXPECT_EQ(directive->GetString("directive"), "INCLUDE");
  EXPECT_EQ(directive->GetString("path"), "file.oza");

  line = ParseDirective(".CONST size = 4 * 2");
  directive = line->GetItem("directive");
  EXPECT_EQ(directive->GetString("directive"), "CONST");
  EXPECT_EQ(directive->GetString("name"), "SIZE");
  EXPECT_NE(directive->GetItem("value"), nullptr);

  line = ParseDirective(".dw 1, 'ab', x + 1");
  directive = line->GetItem("directive");
  EXPECT_EQ(directive->GetString("directive"), "DW");
  EXPECT_EQ(directive->GetItems("items").size(), 3);

  line = ParseDirective(".DD 1, 2");
  directive = line->GetItem("directive");
  EXPECT_EQ(directive->GetString("directive"), "DD");
  EXPECT_EQ(directive->GetItems("items").size(), 2);

  for (auto [text, name] :
       {std::pair{".DS 10", "DS"}, {".INSTANCES 2", "INSTANCES"}}) {
    line = ParseDirective(text);
    directive = line->GetItem("directive");
    EXPECT_EQ(directive->GetString("directive"), name);
    EXPECT_NE(directive->GetItem("count"), nullptr) << text;
  }

  line = ParseDirective("stack: .STACK 64");
  directive = line->GetItem("directive");
  EXPECT_EQ(directive->GetString("directive"), "STACK");
  EXPECT_NE(directive->GetItem("words"), nullptr);

  line = ParseDirective(".BANK video");
  directive = line->GetItem("directive");
  EXPECT_EQ(directive->GetString("directive"), "BANK");
  EXPECT_EQ(directive->GetString("name"), "VIDEO");
  EXPECT_EQ(directive->GetItem("mask"), nullptr);
  line = ParseDirective(".BANK video, VRAM0 | VRAM1");
  EXPECT_NE(line->GetItem("directive")->GetItem("mask"), nullptr);

  line = ParseDirective(".SEGMENT main");
  directive = line->GetItem("directive");
  EXPECT_EQ(directive->GetString("directive"), "SEGMENT");
  EXPECT_EQ(directive->GetString("name"), "MAIN");
  EXPECT_THAT(directive->GetItems("attributes"), IsEmpty());
}

TEST_F(ProgramGrammarTest, DirectiveErrors) {
  // The error is the one that gets furthest into the line.
  gb::ParseError error = ParseLineError(".CONST size 5");
  EXPECT_THAT(error.GetMessage(), HasSubstr("'='"));
  EXPECT_EQ(error.GetLocation().column, 12);
  error = ParseLineError(".DW");
  EXPECT_EQ(error.GetLocation().column, 3);
  error = ParseLineError(".DS 1, 2");
  EXPECT_EQ(error.GetLocation().column, 5);

  error = ParseLineError(".CONSTANT X = 1");
  EXPECT_THAT(error.GetMessage(), HasSubstr("Unknown directive"));
  EXPECT_EQ(error.GetLocation().column, 1);
  error = ParseLineError(".5");
  EXPECT_THAT(error.GetMessage(), HasSubstr("Unknown directive"));
}

TEST_F(ProgramGrammarTest, DirectiveNames) {
  gb::ParseError error;
  gb::ParseResult line = ParseDirective(".Const X = 1");
  EXPECT_EQ(ProgramGrammar::GetDirectiveName(
                GetLexer(), *line->GetItem("directive"), error),
            "CONST");

  line = ParseDirective(". CONST X = 1");
  EXPECT_EQ(ProgramGrammar::GetDirectiveName(
                GetLexer(), *line->GetItem("directive"), error),
            "");
  EXPECT_THAT(error.GetMessage(), HasSubstr("space"));
  EXPECT_EQ(error.GetLocation().column, 2);
}

TEST_F(ProgramGrammarTest, SegmentAttributes) {
  gb::ParseResult line = ParseDirective(
      ".SEGMENT screen code stack DATA EXTRA ROM INSTANCE PAGE(N + 1) "
      "PAGES(2) BANK(video)");
  auto attributes = line->GetItem("directive")->GetItems("attributes");
  ASSERT_EQ(attributes.size(), 9);
  EXPECT_EQ(attributes[0].GetString("role"), "CODE");
  EXPECT_EQ(attributes[1].GetString("role"), "STACK");
  EXPECT_EQ(attributes[2].GetString("role"), "DATA");
  EXPECT_EQ(attributes[3].GetString("role"), "EXTRA");
  EXPECT_NE(attributes[4].GetItem("rom"), nullptr);
  EXPECT_NE(attributes[5].GetItem("instance"), nullptr);
  EXPECT_NE(attributes[6].GetItem("page"), nullptr);
  EXPECT_NE(attributes[7].GetItem("pages"), nullptr);
  EXPECT_EQ(attributes[8].GetString("bank"), "VIDEO");

  // An attribute's parameter is in brackets.
  ParseLineError(".SEGMENT screen PAGE 2");
  ParseLineError(".SEGMENT screen CODES");
}

TEST_F(ProgramGrammarTest, InstructionArgs) {
  gb::ParseResult line = ParseLine("NOP");
  EXPECT_THAT(line->GetItem("instruction")->GetItems("args"), IsEmpty());

  // Any tokens, with brackets balanced.
  line = ParseLine("ADD.W R1, (SP + [x * {2}]) $ @local 'a' << ~ ? :");
  EXPECT_EQ(line->GetItem("instruction")->GetItems("args").size(), 10);

  EXPECT_EQ(ParseLineError("ADD R1, (5").GetLocation().column, 10);
  EXPECT_EQ(ParseLineError("ADD R1, 5)").GetLocation().column, 9);
  EXPECT_EQ(ParseLineError("ADD R1, (5]").GetLocation().column, 10);
}

TEST_F(ProgramGrammarTest, InstructionNames) {
  // A name is as written, which the syntax looks up ignoring case.
  for (auto [text, expected] : {std::pair{"NOP", "NOP"},
                                {"nop", "nop"},
                                {"ADD.W R1", "ADD.W"},
                                {"Add.w", "Add.w"},
                                {"ADD R1", "ADD"},
                                {"loop: ADD.W", "ADD.W"}}) {
    gb::ParseResult line = ParseLine(text);
    gb::ParseError error;
    EXPECT_EQ(ProgramGrammar::GetInstructionName(
                  GetLexer(), *line->GetItem("instruction"), error),
              expected)
        << error.FormatMessage();
  }
  for (auto [text, column] :
       {std::pair{"ADD .W", 4}, {"ADD. W", 5}, {"ADD . W", 4}}) {
    gb::ParseResult line = ParseLine(text);
    gb::ParseError error;
    EXPECT_EQ(ProgramGrammar::GetInstructionName(
                  GetLexer(), *line->GetItem("instruction"), error),
              "");
    EXPECT_THAT(error.GetMessage(), HasSubstr("space around its '.'")) << text;
    EXPECT_EQ(error.GetLocation().column, column) << text;
    EXPECT_EQ(MatchError(text).GetLocation().column, column) << text;
  }
}

//==============================================================================
// Instructions
//==============================================================================

TEST_F(ProgramGrammarTest, FullNamesAndRoots) {
  EXPECT_THAT(Match("NOP"), ElementsAre("NOP "));
  EXPECT_THAT(Match("PUSH.W 5"), ElementsAre("PUSH.W $v"));
  EXPECT_THAT(Match("push.d 5"), ElementsAre("PUSH.D $V"));
  EXPECT_THAT(Match("ADD.D D1, 5"), ElementsAre("ADD.D $R, $V"));
  EXPECT_THAT(Match("ADD D1, 5"), ElementsAre("ADD.D $R, $V"));
  EXPECT_THAT(Match("ADD R1, 5"), ElementsAre("ADD.W $r, $v"));
}

TEST_F(ProgramGrammarTest, UnknownName) {
  gb::ParseError error = MatchError("XYZ 5");
  EXPECT_EQ(error.GetMessage(), "Unknown instruction XYZ");
  EXPECT_EQ(error.GetLocation().column, 0);
  EXPECT_EQ(MatchError("loop: add.x R1, 5").GetMessage(),
            "Unknown instruction add.x");
}

TEST_F(ProgramGrammarTest, AmbiguousAcrossRoot) {
  // The two forms share a rule, as both are an expression.
  EXPECT_THAT(Match("PUSH 5"), UnorderedElementsAre("PUSH.W $v", "PUSH.D $V"));
  ASSERT_EQ(matches_.size(), 2);
  for (const InstructionMatch& match : matches_) {
    ASSERT_EQ(match.args.size(), 1);
    EXPECT_EQ(
        match.args[0].value->Evaluate(ExpressionWidth::kWord, context_).value,
        5);
  }
}

TEST_F(ProgramGrammarTest, LiteralWordOrSymbol) {
  // SELF is the literal word, unless it is also a symbol.
  EXPECT_THAT(Match("RST SELF, 1"), ElementsAre("RST SELF, $#2"));
  EXPECT_THAT(Match("RST self, 1"), ElementsAre("RST SELF, $#2"));
  EXPECT_THAT(Match("RST 5, 1"), ElementsAre("RST.C $#3, $#2"));
  EXPECT_THAT(MatchError("RST.C SELF, 1").GetMessage(),
              HasSubstr("SELF is not defined"));
  EXPECT_THAT(MatchError("RST OTHER, 1").GetMessage(),
              HasSubstr("OTHER is not defined"));
}

TEST_F(ProgramGrammarTest, AmbiguousThroughLiteralWord) {
  context_.symbols["SELF"] = 5;
  EXPECT_THAT(Match("RST SELF, 1"),
              UnorderedElementsAre("RST SELF, $#2", "RST.C $#3, $#2"));
  EXPECT_THAT(Match("RST self, 1"),
              UnorderedElementsAre("RST SELF, $#2", "RST.C $#3, $#2"));
  EXPECT_THAT(Match("RST.C SELF, 1"), ElementsAre("RST.C $#3, $#2"));
}

TEST_F(ProgramGrammarTest, CodeWordValueWithError) {
  gb::ParseError error = MatchError("ADQ.S SP, N");
  EXPECT_THAT(error.GetMessage(), HasSubstr("N is not defined"));
  EXPECT_EQ(error.GetLocation().column, 10);
  EXPECT_THAT(MatchError("ADQ.S SP, 1 % 0").GetMessage(), HasSubstr("zero"));
  EXPECT_THAT(MatchError("SETC X").GetMessage(), HasSubstr("X is not defined"));

  // Only first pass values are evaluated.
  EXPECT_THAT(Match("PUSH.W N"), ElementsAre("PUSH.W $v"));

  // A form a literal number rules out has no error.
  EXPECT_THAT(MatchError("OUTQ N, 1").GetMessage(),
              HasSubstr("N is not defined"));
  EXPECT_THAT(Match("OUTQ N, 2"), IsEmpty());
}

TEST_F(ProgramGrammarTest, RegistersBySize) {
  EXPECT_THAT(Match("MOVX BC, R3"), ElementsAre("MOVX $r4, $r2"));
  EXPECT_EQ(ArgReg(0), 8);
  EXPECT_EQ(ArgReg(1), 3);
  EXPECT_THAT(Match("MOVX sp, r0"), ElementsAre("MOVX $r4, $r2"));
  EXPECT_EQ(ArgReg(0), 15);
  EXPECT_EQ(ArgReg(1), 0);
  EXPECT_THAT(Match("MOVX R0, R4"), IsEmpty());
  EXPECT_THAT(Match("MOVX D0, R0"), IsEmpty());

  EXPECT_THAT(Match("MOVD D1, D3"), ElementsAre("MOVD $R1, $R"));
  EXPECT_EQ(ArgReg(0), 1);
  EXPECT_EQ(ArgReg(1), 3);
  EXPECT_THAT(Match("MOVD D2, D0"), IsEmpty());
  EXPECT_THAT(Match("MOVD R0, D0"), IsEmpty());

  EXPECT_THAT(Match("ADD.W R7, R2"), ElementsAre("ADD.W $r, $r"));
  EXPECT_EQ(ArgReg(0), 7);
  EXPECT_EQ(ArgReg(1), 2);
  EXPECT_THAT(Match("ADD.W BC, R2"), IsEmpty());
}

TEST_F(ProgramGrammarTest, LiteralWordsAndRegistersInAnyCase) {
  EXPECT_THAT(Match("ADQ.S SP, 3"), ElementsAre("ADQ.S SP, $#5"));
  EXPECT_EQ(ArgValue(0), 3);
  EXPECT_THAT(Match("adq.s sp, 3"), ElementsAre("ADQ.S SP, $#5"));
  EXPECT_THAT(Match("ADQ.S FP, 3"), IsEmpty());
  EXPECT_THAT(Match("ADD.W R1, d(5)"), ElementsAre("ADD.W $r, D($v)"));
  EXPECT_EQ(ArgReg(0), 1);
  EXPECT_EQ(ArgValue(1), 5);
}

TEST_F(ProgramGrammarTest, RegisterIsNeverAValue) {
  EXPECT_THAT(Match("PUSH.W R1"), IsEmpty());
  EXPECT_THAT(Match("PUSH.W R1 + 1"), IsEmpty());
  EXPECT_THAT(Match("PUSH.W 1 + R1"), IsEmpty());
}

TEST_F(ProgramGrammarTest, BracketsInsideAndAroundValues) {
  EXPECT_THAT(Match("PUSH.W (1 + 2) * 3"), ElementsAre("PUSH.W $v"));
  EXPECT_EQ(ArgValue(0), 9);
  EXPECT_THAT(Match("PUSH.W (5)"), IsEmpty());
  EXPECT_THAT(Match("PUSH.W ((5))"), IsEmpty());
  EXPECT_THAT(Match("ADD.W R1, (R2 + (1 + 2) * 2)"),
              ElementsAre("ADD.W $r, ($r + $v)"));
  EXPECT_EQ(ArgReg(1), 2);
  EXPECT_EQ(ArgValue(2), 6);
  EXPECT_THAT(Match("ADD.W R1, (R2 + (1 + 2))"), IsEmpty());
  EXPECT_THAT(Match("ADD.W R1, (5)"), IsEmpty());
  EXPECT_THAT(Match("ADD.W R1, D((5))"), IsEmpty());
}

TEST_F(ProgramGrammarTest, LiteralNumbersByValue) {
  EXPECT_THAT(Match("BIT R1, 1"), ElementsAre("BIT $r, 1"));
  EXPECT_THAT(Match("BIT R1, 0"), ElementsAre("BIT $r, 0"));
  EXPECT_THAT(Match("BIT R1, 3 - 2"), ElementsAre("BIT $r, 1"));
  EXPECT_THAT(Match("BIT R1, R2"), ElementsAre("BIT $r, $r"));
  EXPECT_THAT(Match("BIT R1, 2"), IsEmpty());
  EXPECT_THAT(Match("BIT R1, (1)"), IsEmpty());
  context_.symbols["N"] = 1;
  EXPECT_THAT(Match("BIT R1, N"), ElementsAre("BIT $r, 1"));
}

TEST_F(ProgramGrammarTest, LiteralNumberWithError) {
  gb::ParseError error = MatchError("BIT R1, N");
  EXPECT_THAT(error.GetMessage(), HasSubstr("N is not defined"));
  EXPECT_EQ(error.GetLocation().column, 8);

  error = MatchError("BIT R1, 1 / 0");
  EXPECT_THAT(error.GetMessage(), HasSubstr("zero"));
}

TEST_F(ProgramGrammarTest, ValueSetNames) {
  EXPECT_THAT(Match("SETC Z"), ElementsAre("SETC $Condition"));
  ASSERT_NE(matches_[0].form->GetArgs()[0].value_set, nullptr);
  const ValueSetDef* condition = matches_[0].form->GetArgs()[0].value_set;
  EXPECT_EQ(ArgValue(0, ValueSetContext(condition, &context_)), 4);

  // The set's names are ahead of symbols, in any case.
  context_.symbols["Z"] = 9;
  context_.symbols["N"] = 2;
  EXPECT_THAT(Match("SETC z"), ElementsAre("SETC $Condition"));
  EXPECT_EQ(ArgValue(0, ValueSetContext(condition, &context_)), 4);
  EXPECT_EQ(ArgValue(0), 9);
  EXPECT_THAT(Match("SETC nz | N"), ElementsAre("SETC $Condition"));
  EXPECT_EQ(ArgValue(0, ValueSetContext(condition, &context_)), 2);
}

TEST_F(ProgramGrammarTest, CodeValues) {
  // A code word value is kept as matching evaluated it, with a value set's
  // names, and whether it fits its bits or not.
  context_.symbols["Z"] = 9;
  EXPECT_THAT(Match("SETC Z"), ElementsAre("SETC $Condition"));
  EXPECT_EQ(matches_[0].args[0].code_value, 4);
  EXPECT_THAT(Match("RST.C 2 + 3, 9"), ElementsAre("RST.C $#3, $#2"));
  EXPECT_EQ(matches_[0].args[0].code_value, 5);
  EXPECT_EQ(matches_[0].args[1].code_value, 9);
}

TEST_F(ProgramGrammarTest, TextAfterAForm) {
  EXPECT_THAT(Match("PUSH.W 5 6"), IsEmpty());
  EXPECT_THAT(Match("NOP X"), IsEmpty());
  EXPECT_THAT(Match("ADQ.S SP, 3, 4"), IsEmpty());
}

TEST_F(ProgramGrammarTest, LexerAfterMatching) {
  // Matching leaves the lexer after the line, where parsing it did, whether
  // or not a form matches, and after forms that parsed partway.
  EXPECT_THAT(Match("PUSH 5\nNOP\n"), Not(IsEmpty()));
  EXPECT_EQ(NextTokenText(), "NOP");
  EXPECT_THAT(Match("ADD.W R1, (R2 + 5 5)\nNOP\n"), IsEmpty());
  EXPECT_EQ(NextTokenText(), "NOP");
  MatchError("ADQ.S SP, N\nNOP\n");
  EXPECT_EQ(NextTokenText(), "NOP");
  EXPECT_THAT(Match("PUSH.W 5"), Not(IsEmpty()));
  EXPECT_TRUE(GetLexer().NextToken(content_, false).IsEnd());
}

TEST_F(ProgramGrammarTest, LabelBeforeInstruction) {
  EXPECT_THAT(Match("loop: PUSH.W 5"), ElementsAre("PUSH.W $v"));
  EXPECT_EQ(ArgValue(0), 5);
}

TEST(ProgramGrammarCreateTest, LiteralTheLexerCantRead) {
  gb::ParseError error;
  auto asm_set = AssembleInstructionSet(
      "instruction(opcode:0) NOP \"2.5\" { UL; }", &error);
  ASSERT_NE(asm_set, nullptr) << error.FormatMessage();
  auto syntax = InstructionSetSyntax::Create(asm_set->GetInstructionSetDef());
  ASSERT_NE(syntax, nullptr);
  absl::StatusOr<std::unique_ptr<ProgramGrammar>> grammar =
      ProgramGrammar::Create(*syntax);
  EXPECT_FALSE(grammar.ok());
}

}  // namespace
}  // namespace oz3
