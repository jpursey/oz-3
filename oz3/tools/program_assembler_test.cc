// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_assembler.h"

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/log/check.h"
#include "absl/strings/match.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gb/file/memory_file_protocol.h"
#include "gb/parse/parse_error.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/processor.h"
#include "oz3/core/processor_config.h"
#include "oz3/tools/instruction_assembler.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program.h"
#include "oz3/tools/program_loader.h"
#include "oz3/tools/program_map.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Not;
using ::testing::SizeIs;

using MapConstant = ProgramMap::Constant;
using MapLabel = ProgramMap::Label;
using MapStatement = ProgramMap::Statement;

constexpr std::string_view kInstructionSet = R"---(
  values(bits:3) Condition { NZ = 0, Z = 4 }

  macro GetWord:r {
    code:m "$r" { UL; }
    code:C2 "$v" { LD(C2); UL; }
  }

  instruction(opcode:0) NOP { UL; }
  instruction(opcode:1) ADD.W "$r, $m" { $GetWord; ADD(a,r); }
  instruction(opcode:2) PUSH.W "$v" { LD(C0); UL; }
  instruction(opcode:3) PUSH.D "$V" { LD(C0); LD(C1); UL; }
  instruction(opcode:4) MVQ "$r, $#3" { UL; }
  instruction(opcode:5) JC "$Condition, $v" { LD(C0); UL; }
  instruction(opcode:6, relative) JPR "$v" { LD(C0); UL; }
  instruction(opcode:7) RST "SELF, $#2" { UL; }
  instruction(opcode:8) RST.C "$#3, $#2" { UL; }
  instruction(opcode:9, distinct, exclude_a:R7, exclude_b:R7)
      MVIR "($r), ($r)" { UL; }
  instruction(opcode:10, exclude_b:R7) CPIR "$r, ($r)" { UL; }
)---";

// Matches a gb::ParseError at a line and column (from 0), whose message has
// `message` in it.
MATCHER_P3(IsErrorAt, line, column, message, "") {
  const gb::LexerLocation& location = arg.GetLocation();
  if (location.line != line || location.column != column ||
      !absl::StrContains(arg.GetMessage(), message)) {
    *result_listener << arg.FormatMessage();
    return false;
  }
  return true;
}

class ProgramAssemblerTest : public ::testing::Test {
 protected:
  ProgramAssemblerTest() {
    gb::ParseError error;
    asm_set_ = AssembleInstructionSet(std::string(kInstructionSet), &error);
    CHECK(asm_set_ != nullptr) << error.FormatMessage();
    InstructionError syntax_error;
    syntax_ = InstructionSetSyntax::Create(asm_set_->GetInstructionSetDef(),
                                           asm_set_->GetInstructionSet().get(),
                                           &syntax_error);
    CHECK(syntax_ != nullptr) << syntax_error.message;
    file_system_.Register(std::make_unique<gb::MemoryFileProtocol>());
  }

  // Assembles `text`, which must assemble, and returns it, or null if it
  // doesn't.
  std::unique_ptr<AsmProgram> AssembleAsmProgram(std::string_view text) {
    std::vector<gb::ParseError> errors;
    std::unique_ptr<AsmProgram> program =
        AssembleProgram(*syntax_, std::string(text), &errors);
    for (const gb::ParseError& error : errors) {
      ADD_FAILURE() << error.FormatMessage();
    }
    if (program == nullptr) {
      ADD_FAILURE() << "No program for: " << text;
      return nullptr;
    }
    EXPECT_TRUE(ValidateProgram(program->GetProgram()).ok());
    return program;
  }

  // Assembles `text`, which must assemble, and returns its program, or its
  // map.
  Program Assemble(std::string_view text) {
    std::unique_ptr<AsmProgram> program = AssembleAsmProgram(text);
    return program != nullptr ? program->GetProgram() : Program{};
  }
  ProgramMap AssembleMap(std::string_view text) {
    std::unique_ptr<AsmProgram> program = AssembleAsmProgram(text);
    return program != nullptr ? program->GetMap() : ProgramMap{};
  }

  // Assembles `text`, which must not assemble, and returns its errors.
  std::vector<gb::ParseError> AssembleErrors(std::string_view text) {
    std::vector<gb::ParseError> errors;
    EXPECT_EQ(AssembleProgram(*syntax_, std::string(text), &errors), nullptr)
        << text;
    EXPECT_THAT(errors, Not(IsEmpty())) << text;
    return errors;
  }

  // Returns the words of the program assembled from `text`, which has one
  // segment (see GetWords).
  std::vector<uint16_t> AssembleWords(std::string_view text) {
    Program program = Assemble(text);
    if (program.segments.size() != 1) {
      ADD_FAILURE() << "Expected one segment";
      return {};
    }
    return GetWords(program.segments[0]);
  }

  // Returns a segment's words, from 0 to the end of its last data block, with
  // words that aren't written as 0.
  static std::vector<uint16_t> GetWords(const Program::Segment& segment) {
    std::vector<uint16_t> words;
    for (const Program::Segment::Data& data : segment.data) {
      words.resize(data.offset);
      words.insert(words.end(), data.data.begin(), data.data.end());
    }
    return words;
  }

  // Returns the instruction at `offset` in `words` as the syntax disassembles
  // it.
  std::string Disassemble(absl::Span<const uint16_t> words, int offset) {
    const InstructionForm* form = syntax_->FindForm(words[offset]);
    if (form == nullptr) {
      return "?";
    }
    return form->Disassemble(words.subspan(offset, form->GetSize()),
                             static_cast<uint16_t>(offset));
  }

  std::unique_ptr<AsmInstructionSet> asm_set_;
  std::unique_ptr<InstructionSetSyntax> syntax_;
  gb::FileSystem file_system_;
};

TEST_F(ProgramAssemblerTest, DefaultProgram) {
  Program program = Assemble(".STACK 16\nNOP\n");
  ASSERT_THAT(program.banks, SizeIs(1));
  EXPECT_EQ(program.banks[0].assign_mask, 0xFFFF);
  ASSERT_THAT(program.segments, SizeIs(1));
  const Program::Segment& segment = program.segments[0];
  EXPECT_EQ(segment.bank, 0);
  EXPECT_EQ(segment.access, Program::Segment::Access::kReadWrite);
  EXPECT_EQ(segment.page_start, 0xFFFF);
  EXPECT_EQ(segment.page_count, 1);
  EXPECT_THAT(segment.data, ElementsAre(Program::Segment::Data{
                                .offset = 0, .data = {0x0000, 0}}));
  EXPECT_EQ(program.code_segment, 0);
  EXPECT_EQ(program.stack_segment, 0);
  EXPECT_EQ(program.data_segment, 0);
  EXPECT_EQ(program.extra_segment, 0);
  EXPECT_THAT(program.instances, ElementsAre(Program::Instance{}));
}

TEST_F(ProgramAssemblerTest, ErrorsAreReplaced) {
  std::vector<gb::ParseError> errors = {gb::ParseError("Old")};
  EXPECT_EQ(AssembleProgram(*syntax_, ".STACK 1\nFOO\n", &errors), nullptr);
  EXPECT_THAT(errors, ElementsAre(IsErrorAt(1, 0, "Unknown instruction FOO")));
  EXPECT_NE(AssembleProgram(*syntax_, ".STACK 1\n", &errors), nullptr);
  EXPECT_THAT(errors, IsEmpty());
}

TEST_F(ProgramAssemblerTest, DefaultProgramSize) {
  // The contents and the stack, rounded up to whole pages.
  EXPECT_EQ(Assemble(".STACK 1").segments[0].page_count, 1);
  EXPECT_EQ(Assemble(".STACK 4096").segments[0].page_count, 1);
  EXPECT_EQ(Assemble(".STACK 4097").segments[0].page_count, 2);
  EXPECT_EQ(Assemble(".STACK 96\n.DS 4000").segments[0].page_count, 1);
  EXPECT_EQ(Assemble(".STACK 97\n.DS 4000").segments[0].page_count, 2);
  EXPECT_EQ(Assemble(".DS 4000\n.STACK 97").segments[0].page_count, 2);
  EXPECT_EQ(Assemble(".STACK 536\n.DS 65000").segments[0].page_count, 16);
}

TEST_F(ProgramAssemblerTest, ProgramTooBig) {
  EXPECT_THAT(AssembleErrors(".STACK 537\n.DS 65000"),
              ElementsAre(IsErrorAt(0, 0,
                                    "The stack passes the end of the "
                                    "segment, at 65536 words")));
  EXPECT_THAT(AssembleErrors(".STACK 1\n.DS 65535\n.DW 1, 2\n.DW 3\n"),
              ElementsAre(IsErrorAt(
                  2, 0, "This passes the end of the segment, at 65536 words")));
}

TEST_F(ProgramAssemblerTest, StackIsNeeded) {
  EXPECT_THAT(AssembleErrors(""),
              ElementsAre(IsErrorAt(0, 0, "needs its size, with .STACK")));
  EXPECT_THAT(AssembleErrors("NOP\n.DW 1\n"),
              ElementsAre(IsErrorAt(0, 0, "needs its size, with .STACK")));
  EXPECT_THAT(
      AssembleErrors(".STACK 1\n.STACK 2"),
      ElementsAre(IsErrorAt(1, 0, "The program's .STACK is already given")));
}

TEST_F(ProgramAssemblerTest, StackIsFirstPass) {
  EXPECT_THAT(AssembleErrors(".STACK SIZE\n.CONST SIZE = 5"),
              ElementsAre(IsErrorAt(0, 7, "SIZE is not defined")));
}

TEST_F(ProgramAssemblerTest, Instructions) {
  std::vector<uint16_t> words = AssembleWords(R"---(.STACK 1
    nop
    add r1, r2
    ADD R3, 1234h
    push.w 5
    PUSH.D 12345678h
    MVQ R7, 6
    JC Z, 0ABCDh
    RST SELF, 2
    RST.C 5, 3
  )---");
  int offset = 0;
  auto next = [&]() {
    const std::string text = Disassemble(words, offset);
    offset += syntax_->FindForm(words[offset])->GetSize();
    return text;
  };
  EXPECT_EQ(next(), "NOP");
  EXPECT_EQ(next(), "ADD.W R1, R2");
  EXPECT_EQ(next(), "ADD.W R3, 1234h");
  EXPECT_EQ(next(), "PUSH.W 0005h");
  EXPECT_EQ(next(), "PUSH.D 12345678h");
  EXPECT_EQ(next(), "MVQ R7, 6");
  EXPECT_EQ(next(), "JC Z, 0ABCDh");
  EXPECT_EQ(next(), "RST SELF, 2");
  EXPECT_EQ(next(), "RST.C 5, 3");
  EXPECT_EQ(offset, 14);
}

TEST_F(ProgramAssemblerTest, RelativeInstructions) {
  std::vector<uint16_t> words = AssembleWords(R"---(.STACK 1
    start:
      JPR start
      JPR end
      NOP
    end:
  )---");
  ASSERT_THAT(words, SizeIs(6));
  EXPECT_EQ(words[1], 0xFFFE);  // 0 - 2
  EXPECT_EQ(words[3], 0x0001);  // 5 - 4
  EXPECT_EQ(Disassemble(words, 0), "JPR 0000h");
  EXPECT_EQ(Disassemble(words, 2), "JPR 0005h");
}

TEST_F(ProgramAssemblerTest, Labels) {
  // A label's value is its offset, used above or below it.
  EXPECT_THAT(AssembleWords(R"---(.STACK 1
    .DW after, before
    before: .DW before
    LABEL:
    other:
      .DW label, OTHER, after
    after:
  )---"),
              ElementsAre(6, 2, 2, 3, 3, 6));
}

TEST_F(ProgramAssemblerTest, LocalLabels) {
  // Local labels are scoped from one label to the next, from the start of the
  // program.
  EXPECT_THAT(AssembleWords(R"---(.STACK 1
    @a: .DW @a
    first: .DW @A
    @a: .DW @a
    second:
    @a: .DW @a, first
  )---"),
              ElementsAre(0, 2, 2, 3, 1, 0));
}

TEST_F(ProgramAssemblerTest, LocalLabelErrors) {
  EXPECT_THAT(AssembleErrors(".STACK 1\n"
                             "first: .DW 0\n"
                             "@x: .DW 0\n"
                             "second: .DW @x\n"),
              ElementsAre(IsErrorAt(3, 12, "@X is not defined")));
  EXPECT_THAT(AssembleErrors(".STACK 1\n"
                             "@x: .DW 0\n"
                             "@X: .DW 0\n"),
              ElementsAre(IsErrorAt(2, 0, "@X is already defined")));
}

TEST_F(ProgramAssemblerTest, SymbolErrors) {
  EXPECT_THAT(AssembleErrors(".STACK 1\n"
                             "loop: NOP\n"
                             "LOOP: NOP\n"
                             ".CONST loop = 1\n"),
              ElementsAre(IsErrorAt(2, 0, "LOOP is already defined"),
                          IsErrorAt(3, 7, "LOOP is already defined")));
  EXPECT_THAT(AssembleErrors(".STACK 1\n.DW nowhere\n"),
              ElementsAre(IsErrorAt(1, 4, "NOWHERE is not defined")));
}

TEST_F(ProgramAssemblerTest, Constants) {
  // A constant is evaluated where it is used, in that width.
  EXPECT_THAT(
      AssembleWords(R"---(.STACK 1
    .CONST MASK = ~1
    .CONST TWICE = MASK * 2
    .DW MASK, TWICE
    .DD MASK, TWICE
    .DW LATER
    .CONST LATER = 5
  )---"),
      ElementsAre(0xFFFE, 0xFFFC, 0xFFFE, 0xFFFF, 0xFFFC, 0xFFFF, 5, 0));
}

TEST_F(ProgramAssemblerTest, OffsetInConstant) {
  // $ is the offset of the statement, and in a constant, where it is defined.
  EXPECT_THAT(AssembleWords(R"---(.STACK 1
    .DW $, $
    table:
      .DW 1, 2, 3
    .CONST TABLE_SIZE = $ - table
    .DW TABLE_SIZE, $
  )---"),
              ElementsAre(0, 0, 1, 2, 3, 3, 5, 0));
}

TEST_F(ProgramAssemblerTest, LocalLabelInConstant) {
  // A constant's local labels are those of the scope it is defined in.
  EXPECT_THAT(AssembleWords(R"---(.STACK 1
    first:
      .CONST HERE = @here
    @here:
      .DW HERE
    second:
    @here:
      .DW HERE, @here
  )---"),
              ElementsAre(0, 0, 1, 0));
}

TEST_F(ProgramAssemblerTest, RecursiveConstants) {
  EXPECT_THAT(AssembleErrors(".STACK 1\n.CONST A = A + 1\n"),
              ElementsAre(IsErrorAt(1, 7, "The value of A depends on itself")));
  EXPECT_THAT(AssembleErrors(".STACK 1\n"
                             ".CONST A = B\n"
                             ".CONST B = A\n"
                             ".DW A, B\n"),
              ElementsAre(IsErrorAt(1, 7, "The value of A depends on itself")));
}

TEST_F(ProgramAssemblerTest, ConstantErrors) {
  // An error in a constant is reported once, where it is.
  EXPECT_THAT(AssembleErrors(".STACK 1\n"
                             ".CONST A = 1 / 0\n"
                             ".DW A\n"
                             ".DD A\n"),
              ElementsAre(IsErrorAt(1, 13, "Division by zero")));

  // A constant's value as a symbol is its dword value, so it must have one,
  // even if it isn't used.
  EXPECT_THAT(AssembleErrors(".STACK 1\n.CONST BIG = 100000000h\n"),
              ElementsAre(IsErrorAt(
                  1, 13, "Value 4294967296 does not fit in a dword")));
  EXPECT_THAT(AssembleErrors(".STACK 1\n.CONST A = NOWHERE\n"),
              ElementsAre(IsErrorAt(1, 11, "NOWHERE is not defined")));
}

TEST_F(ProgramAssemblerTest, ConstantUsedAsAWord) {
  // A constant with a value that can't be evaluated as a word is an error
  // where it is used as one.
  EXPECT_THAT(
      AssembleErrors(".STACK 1\n"
                     ".CONST BIG = 70000\n"
                     ".CONST LESS = BIG - 10000\n"
                     ".DD BIG, LESS\n"
                     ".DW BIG\n"
                     ".DW 1, LESS + 1\n"
                     ".DW BIG\n"),
      ElementsAre(
          IsErrorAt(4, 4,
                    "BIG can't be evaluated as a word: Value 70000 does not "
                    "fit in a word"),
          IsErrorAt(5, 7,
                    "LESS can't be evaluated as a word: BIG can't be "
                    "evaluated as a word: Value 70000 does not fit in a word"),
          IsErrorAt(6, 4, "BIG can't be evaluated as a word")));
  EXPECT_THAT(AssembleErrors(".STACK 1\n"
                             ".CONST BIG = 70000\n"
                             ".DS BIG\n"),
              ElementsAre(IsErrorAt(2, 4, "BIG can't be evaluated as a word")));
}

TEST_F(ProgramAssemblerTest, FirstPassValues) {
  // Code word values, literal numbers, and .DS counts must be defined above.
  EXPECT_THAT(AssembleWords(".STACK 1\n"
                            ".CONST N = 2\n"
                            "start: .DS N\n"
                            "MVQ R1, N + start\n"),
              ElementsAre(0, 0, 0x0411, 0));
  EXPECT_THAT(AssembleErrors(".STACK 1\n"
                             ".DS N\n"
                             "MVQ R1, N\n"
                             "JC LATER, 0\n"
                             ".CONST N = 2\n"
                             ".CONST LATER = 4\n"),
              ElementsAre(IsErrorAt(1, 4, "N is not defined"),
                          IsErrorAt(2, 8, "N is not defined"),
                          IsErrorAt(3, 3, "LATER is not defined")));

  // A constant that uses a label below it is defined, but it can't be
  // evaluated until the label is.
  EXPECT_THAT(AssembleErrors(".STACK 1\n"
                             ".CONST N = later\n"
                             ".DS N\n"
                             "later:\n"),
              ElementsAre(IsErrorAt(1, 11, "LATER is not defined")));
}

TEST_F(ProgramAssemblerTest, ValueSetNames) {
  // A value set's names are ahead of a symbol of the same name.
  std::vector<uint16_t> words = AssembleWords(R"---(.STACK 1
    .CONST Z = 1
    JC Z, Z
    JC nz, Z + 1
  )---");
  EXPECT_EQ(Disassemble(words, 0), "JC Z, 0001h");
  EXPECT_EQ(Disassemble(words, 2), "JC NZ, 0002h");
}

TEST_F(ProgramAssemblerTest, CodeValueTooBig) {
  EXPECT_THAT(AssembleErrors(".STACK 1\nMVQ R1, 4 + 4\n"),
              ElementsAre(IsErrorAt(1, 8, "Value 8 does not fit in 3 bits")));
}

TEST_F(ProgramAssemblerTest, RegisterRules) {
  EXPECT_THAT(
      AssembleErrors(".STACK 1\n"
                     "MVIR (R1), (R1)\n"
                     "MVIR (R7), (R1)\n"
                     "MVIR (R1), (R7)\n"
                     "CPIR R7, (R7)\n"),
      ElementsAre(IsErrorAt(1, 12, "MVIR can't use R1 for both registers"),
                  IsErrorAt(2, 6, "MVIR can't use R7 for this argument"),
                  IsErrorAt(3, 12, "MVIR can't use R7 for this argument"),
                  IsErrorAt(4, 10, "CPIR can't use R7 for this argument")));
  EXPECT_THAT(AssembleWords(".STACK 1\n"
                            "MVIR (R1), (R2)\n"
                            "CPIR R7, (R6)\n"),
              ElementsAre(0x0911, 0x0A37));
}

TEST_F(ProgramAssemblerTest, Data) {
  EXPECT_THAT(
      AssembleWords(".STACK 1\n"
                    ".DW 1, -1, 'A' + 1, \"AB\"\n"
                    ".DD 12345678h, -1\n"),
      ElementsAre(1, 0xFFFF, 0x42, 0x4142, 0x5678, 0x1234, 0xFFFF, 0xFFFF));
  EXPECT_THAT(
      AssembleErrors(".STACK 1\n.DW 10000h, 20000h\n.DD 100000000h\n"),
      ElementsAre(IsErrorAt(1, 4, "Value 65536 does not fit in a word"),
                  IsErrorAt(1, 12, "Value 131072 does not fit in a word"),
                  IsErrorAt(2, 4, "Value 4294967296 does not fit in a dword")));
}

TEST_F(ProgramAssemblerTest, Strings) {
  // A string on its own in .DW is its words, and in an expression, a word.
  EXPECT_THAT(
      AssembleWords(".STACK 1\n"
                    ".DW 'Hi', \"Hi!\", '', 0, ('A'), 'It''s'\n"),
      ElementsAre('H', 'i', 0x4869, 0x2100, 0, 'A', 'I', 't', '\'', 's'));
  EXPECT_THAT(AssembleErrors(".STACK 1\n.DW \"\xE2\x82\xAC\"\n"),
              ElementsAre(IsErrorAt(1, 4, "")));
  EXPECT_THAT(
      AssembleErrors(".STACK 1\n.DW ('AB')\n.DD 'AB'\n"),
      ElementsAre(
          IsErrorAt(1, 5, "A string in an expression must be one word"),
          IsErrorAt(2, 4, "A string in an expression must be one word")));
}

TEST_F(ProgramAssemblerTest, DataBlocks) {
  // Each run of written words is a block, widened to aligned pairs, so
  // reserved words are left out.
  Program program = Assemble(R"---(.STACK 1
    .DW 1
    .DS 3
    .DW 2, 3
    .DS 4
    .DW 4
    .DS 1
    .DW 5, 6, 7
  )---");
  using Data = Program::Segment::Data;
  EXPECT_THAT(program.segments[0].data,
              ElementsAre(Data{.offset = 0, .data = {1, 0}},
                          Data{.offset = 4, .data = {2, 3}},
                          Data{.offset = 10, .data = {4, 0, 5, 6, 7, 0}}));
  EXPECT_THAT(Assemble(".STACK 1\n.DS 5").segments[0].data, IsEmpty());
}

TEST_F(ProgramAssemblerTest, NoFormMatches) {
  EXPECT_THAT(AssembleErrors(".STACK 1\n  add 5, r1\n"),
              ElementsAre(IsErrorAt(1, 2,
                                    "No form of add matches. Its forms are:\n"
                                    "  ADD.W $r, $r\n"
                                    "  ADD.W $r, $v")));
}

TEST_F(ProgramAssemblerTest, MoreThanOneFormMatches) {
  EXPECT_THAT(AssembleErrors(".STACK 1\n  PUSH 5\n"),
              ElementsAre(IsErrorAt(1, 2,
                                    "More than one form of PUSH matches:\n"
                                    "  PUSH.W $v\n"
                                    "  PUSH.D $V")));

  // SELF is only ambiguous where it is a symbol.
  EXPECT_THAT(AssembleWords(".STACK 1\nRST SELF, 1\n"), SizeIs(2));
  EXPECT_THAT(AssembleErrors(".STACK 1\n.CONST SELF = 1\nRST SELF, 1\n"),
              ElementsAre(IsErrorAt(2, 0,
                                    "More than one form of RST matches:\n"
                                    "  RST SELF, $#2\n"
                                    "  RST.C $#3, $#2")));
}

TEST_F(ProgramAssemblerTest, LineErrors) {
  EXPECT_THAT(AssembleErrors(".STACK 1\nFOO R1\n"),
              ElementsAre(IsErrorAt(1, 0, "Unknown instruction FOO")));
  EXPECT_THAT(AssembleErrors(".STACK 1\nADD . W R1, R2\n"),
              ElementsAre(IsErrorAt(1, 0, "No form of ADD matches")));
  EXPECT_THAT(AssembleErrors(".STACK 1\n. DW 1\n"),
              ElementsAre(IsErrorAt(1, 2, "with no space after the '.'")));
  EXPECT_THAT(AssembleErrors(".STACK 1\n.FOO 1\n"),
              ElementsAre(IsErrorAt(1, 1, "Unknown directive")));
  EXPECT_THAT(AssembleErrors(".STACK 1\n.CONST X 5\n"),
              ElementsAre(IsErrorAt(1, 9, "")));
  EXPECT_THAT(AssembleErrors(".STACK 1\nR1: NOP\n"),
              ElementsAre(IsErrorAt(1, 0, "")));
}

TEST_F(ProgramAssemblerTest, Segments) {
  // Statements assemble into the current segment, which .SEGMENT switches,
  // and a label is an offset in its segment.
  Program program = Assemble(R"---(.STACK 4
    .SEGMENT main CODE
    start:
      NOP
      .DW value
    .SEGMENT vars DATA
      .DW 1
    value:
      .DW 2
    .SEGMENT Main
      .DW start, $
    .SEGMENT stack STACK
  )---");
  ASSERT_THAT(program.segments, SizeIs(3));
  EXPECT_THAT(GetWords(program.segments[0]), ElementsAre(0, 1, 0, 2));
  EXPECT_THAT(GetWords(program.segments[1]), ElementsAre(1, 2));
  EXPECT_THAT(program.segments[2].data, IsEmpty());
  for (const Program::Segment& segment : program.segments) {
    EXPECT_EQ(segment.bank, 0);
    EXPECT_EQ(segment.access, Program::Segment::Access::kReadWrite);
    EXPECT_EQ(segment.page_start, 0xFFFF);
    EXPECT_EQ(segment.page_count, 1);
  }
  EXPECT_THAT(program.banks, ElementsAre(Program::Bank{}));
  EXPECT_EQ(program.code_segment, 0);
  EXPECT_EQ(program.stack_segment, 2);
  EXPECT_EQ(program.data_segment, 1);
  EXPECT_EQ(program.extra_segment, 0xFFFF);
  EXPECT_THAT(program.instances, ElementsAre(Program::Instance{}));
}

TEST_F(ProgramAssemblerTest, SegmentRoles) {
  // A segment can have several roles, or none, and a role can have no
  // segment, as a data only program has no code.
  Program program = Assemble(
      ".STACK 1\n"
      ".SEGMENT empty\n"
      ".SEGMENT all EXTRA CODE STACK DATA\n");
  EXPECT_THAT(program.segments, SizeIs(2));
  EXPECT_EQ(program.code_segment, 1);
  EXPECT_EQ(program.stack_segment, 1);
  EXPECT_EQ(program.data_segment, 1);
  EXPECT_EQ(program.extra_segment, 1);
  program = Assemble(".SEGMENT vars DATA\n.DW 5\n");
  EXPECT_EQ(program.code_segment, 0xFFFF);
  EXPECT_EQ(program.stack_segment, 0xFFFF);
  EXPECT_EQ(program.data_segment, 0);
  EXPECT_EQ(program.extra_segment, 0xFFFF);

  EXPECT_THAT(AssembleErrors(".SEGMENT a CODE\n.SEGMENT b DATA CODE\n"),
              ElementsAre(IsErrorAt(
                  1, 16, "The CODE role is already the segment A's")));
  EXPECT_THAT(AssembleErrors(".SEGMENT a DATA data\n"),
              ElementsAre(IsErrorAt(0, 16, "DATA is already given")));
}

TEST_F(ProgramAssemblerTest, SegmentAttributesOnlyWhereDeclared) {
  EXPECT_THAT(AssembleErrors(".SEGMENT a DATA\n.SEGMENT a DATA\n"),
              ElementsAre(IsErrorAt(1, 11,
                                    "A segment's attributes are only given "
                                    "where it is first declared, and A is "
                                    "already declared")));
}

TEST_F(ProgramAssemblerTest, BeforeTheFirstSegment) {
  // Constants and .STACK can be before the first .SEGMENT, but nothing in a
  // segment can.
  EXPECT_THAT(AssembleWords(".STACK 1\n"
                            ".CONST N = 5\n"
                            ".SEGMENT main CODE STACK\n"
                            ".DW N\n"),
              ElementsAre(5, 0));
  EXPECT_THAT(AssembleErrors(".STACK 1\n"
                             "NOP\n"
                             ".DW 1\n"
                             ".SEGMENT main CODE STACK\n"),
              ElementsAre(IsErrorAt(1, 0,
                                    "The program declares segments, so this "
                                    "must be in one, after a .SEGMENT")));
  EXPECT_THAT(AssembleErrors("start:\n.SEGMENT main CODE\n"),
              ElementsAre(IsErrorAt(0, 0, "The program declares segments")));
  EXPECT_THAT(AssembleErrors(".DS 0\n.SEGMENT main CODE\n"),
              ElementsAre(IsErrorAt(0, 0, "The program declares segments")));
}

TEST_F(ProgramAssemblerTest, SegmentStack) {
  EXPECT_THAT(AssembleErrors(".SEGMENT main CODE\n.SEGMENT stack STACK\n"),
              ElementsAre(IsErrorAt(1, 9, "needs its size, with .STACK")));
  EXPECT_THAT(AssembleErrors(".STACK 4\n.SEGMENT main CODE DATA\n"),
              ElementsAre(IsErrorAt(0, 0,
                                    "No segment has the STACK role, so the "
                                    "program can't have .STACK")));
}

TEST_F(ProgramAssemblerTest, SegmentSizes) {
  // Each segment is its contents, and the stack segment its stack too, in
  // whole pages.
  Program program = Assemble(
      ".STACK 100\n"
      ".SEGMENT main CODE\n"
      ".DS 4097\n"
      ".SEGMENT stack STACK\n"
      ".DS 3996\n"
      ".SEGMENT vars DATA\n"
      ".DS 3996\n");
  ASSERT_THAT(program.segments, SizeIs(3));
  EXPECT_EQ(program.segments[0].page_count, 2);
  EXPECT_EQ(program.segments[1].page_count, 1);
  EXPECT_EQ(program.segments[2].page_count, 1);
  program = Assemble(".STACK 100\n.SEGMENT stack STACK\n.DS 3997\n");
  EXPECT_EQ(program.segments[0].page_count, 2);

  EXPECT_THAT(AssembleErrors(".SEGMENT main CODE\n"
                             ".DS 65535\n"
                             ".SEGMENT vars DATA\n"
                             ".DS 65535\n"
                             ".SEGMENT main\n"
                             ".DW 1, 2\n"),
              ElementsAre(IsErrorAt(
                  5, 0, "This passes the end of the segment, at 65536 words")));
  EXPECT_THAT(AssembleErrors(".STACK 2\n"
                             ".SEGMENT stack STACK\n"
                             ".DS 65535\n"),
              ElementsAre(IsErrorAt(0, 0,
                                    "The stack passes the end of the "
                                    "segment, at 65536 words")));
}

TEST_F(ProgramAssemblerTest, SegmentsLoad) {
  Program program = Assemble(R"---(.STACK 4
    .SEGMENT stack STACK
    .SEGMENT main CODE
      .DW 1, 2, 3
    .SEGMENT vars DATA PAGE(10)
      .DS 4096
      .DW 4, 5
  )---");
  Processor processor(ProcessorConfig::MultiBank(1));
  ProgramLoader loader(&processor);
  const ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const CpuCore::ResetParams params = loader.GetResetParams(handle);
  MemoryBank& memory = *processor.GetMemory(0);
  EXPECT_THAT(memory.GetMem(params.bc, 3), ElementsAre(1, 2, 3));
  EXPECT_EQ(params.bd, 10 * kMemoryBankPageSize);
  EXPECT_THAT(memory.GetMem(params.bd + 4096, 2), ElementsAre(4, 5));

  // The stack is a page of its own, and its top is the end of the page.
  EXPECT_EQ(params.bs % kMemoryBankPageSize, 0);
  const int stack_page = params.bs / kMemoryBankPageSize - 1;
  EXPECT_NE(stack_page, params.bc / kMemoryBankPageSize);
  EXPECT_NE(stack_page, params.bd / kMemoryBankPageSize);
  EXPECT_NE(stack_page, params.bd / kMemoryBankPageSize + 1);
}

TEST_F(ProgramAssemblerTest, SegmentAttributes) {
  Program program = Assemble(
      ".CONST BASE = 4\n"
      ".SEGMENT fixed ROM PAGE(2) PAGES(3)\n"
      ".SEGMENT expressions PAGES(BASE / 2) "
      "PAGE(BASE + 1)\n");
  ASSERT_THAT(program.segments, SizeIs(2));
  EXPECT_EQ(program.segments[0].access, Program::Segment::Access::kReadOnly);
  EXPECT_EQ(program.segments[0].page_start, 2);
  EXPECT_EQ(program.segments[0].page_count, 3);
  EXPECT_EQ(program.segments[1].access, Program::Segment::Access::kReadWrite);
  EXPECT_EQ(program.segments[1].page_start, 5);
  EXPECT_EQ(program.segments[1].page_count, 2);
}

TEST_F(ProgramAssemblerTest, SegmentAttributeErrors) {
  EXPECT_THAT(AssembleErrors(".SEGMENT a PAGE(N)\n.CONST N = 1\n"),
              ElementsAre(IsErrorAt(0, 16, "N is not defined")));
  EXPECT_THAT(
      AssembleErrors(".SEGMENT a PAGE(16)\n"
                     ".SEGMENT b PAGES(0)\n"
                     ".SEGMENT c PAGES(17)\n"),
      ElementsAre(
          IsErrorAt(0, 16, "A segment's page must be from 0 to 15"),
          IsErrorAt(1, 17, "A segment's size in pages must be from 1 to 16"),
          IsErrorAt(2, 17, "A segment's size in pages must be from 1 to 16")));
  EXPECT_THAT(AssembleErrors(".SEGMENT a ROM rom PAGE(1) PAGE(2) PAGES(1) "
                             "PAGES(1)\n"),
              ElementsAre(IsErrorAt(0, 15, "ROM is already given"),
                          IsErrorAt(0, 27, "PAGE is already given"),
                          IsErrorAt(0, 44, "PAGES is already given")));
}

TEST_F(ProgramAssemblerTest, SegmentPages) {
  // PAGES gives a segment more pages than its contents need, at the end, so
  // the stack gets them.
  Program program = Assemble(
      ".STACK 10\n"
      ".SEGMENT stack STACK PAGES(3)\n"
      ".DW 1\n");
  ASSERT_THAT(program.segments, SizeIs(1));
  EXPECT_EQ(program.segments[0].page_count, 3);
  EXPECT_THAT(GetWords(program.segments[0]), ElementsAre(1, 0));

  EXPECT_THAT(AssembleErrors(".SEGMENT main PAGES(1)\n.DS 4097\n"),
              ElementsAre(IsErrorAt(0, 14,
                                    "MAIN needs 2 pages (4097 words), but "
                                    "PAGES gives it 1")));
  EXPECT_THAT(AssembleErrors(".STACK 2\n"
                             ".SEGMENT stack STACK PAGES(1)\n"
                             ".DS 4095\n"),
              ElementsAre(IsErrorAt(1, 21,
                                    "STACK needs 2 pages (4097 words), but "
                                    "PAGES gives it 1")));
}

TEST_F(ProgramAssemblerTest, SegmentPastTheEndOfTheBank) {
  EXPECT_THAT(AssembleErrors(".SEGMENT a PAGE(15) PAGES(2)\n"),
              ElementsAre(IsErrorAt(0, 11,
                                    "A's 2 pages from page 15 pass the end of "
                                    "the bank, at page 16")));
  EXPECT_THAT(AssembleErrors(".SEGMENT a PAGE(15)\n.DS 4097\n"),
              ElementsAre(IsErrorAt(0, 11,
                                    "A's 2 pages from page 15 pass the end of "
                                    "the bank, at page 16")));

  // A segment too big for any bank is reported once.
  EXPECT_THAT(
      AssembleErrors(".SEGMENT a PAGE(0) PAGES(1)\n"
                     ".DS 65535\n"
                     ".DW 1, 2\n"),
      ElementsAre(IsErrorAt(2, 0, "This passes the end of the segment")));

  // Segments with more pages than a bank between them are left to
  // ValidateProgram.
  EXPECT_THAT(AssembleErrors(".SEGMENT a PAGES(9)\n.SEGMENT b PAGES(8)\n"),
              ElementsAre(IsErrorAt(0, 0, "The program isn't valid: ")));
}

TEST_F(ProgramAssemblerTest, ExampleProgram) {
  // The design doc's example, for this instruction set.
  Program program = Assemble(R"---(
    ; Pushes a message, a word at a time.
    .CONST COUNT = 2
    .STACK 64

    .SEGMENT main CODE
    start:
        PUSH.W message
        PUSH.W message + 1
        JPR start

    .SEGMENT strings DATA ROM
    message:
        .DW 'Hi', COUNT

    .SEGMENT stack STACK
  )---");
  ASSERT_THAT(program.segments, SizeIs(3));
  std::vector<uint16_t> code = GetWords(program.segments[0]);
  ASSERT_THAT(code, SizeIs(6));
  EXPECT_EQ(Disassemble(code, 0), "PUSH.W 0000h");
  EXPECT_EQ(Disassemble(code, 2), "PUSH.W 0001h");
  EXPECT_EQ(Disassemble(code, 4), "JPR 0000h");
  EXPECT_EQ(program.segments[1].access, Program::Segment::Access::kReadOnly);
  EXPECT_THAT(GetWords(program.segments[1]), ElementsAre('H', 'i', 2, 0));
  EXPECT_THAT(program.segments[2].data, IsEmpty());
  EXPECT_EQ(program.segments[2].page_count, 1);
  EXPECT_EQ(program.code_segment, 0);
  EXPECT_EQ(program.stack_segment, 2);
  EXPECT_EQ(program.data_segment, 1);
  EXPECT_EQ(program.extra_segment, 0xFFFF);
}

TEST_F(ProgramAssemblerTest, Banks) {
  // Banks are in the order they are declared, and a segment's bank can be
  // declared after it.
  Program program = Assemble(
      ".CONST VRAM0 = 4\n"
      ".CONST VRAM1 = 8\n"
      ".BANK video, VRAM0 | VRAM1\n"
      ".BANK main\n"
      ".SEGMENT code CODE BANK(main)\n"
      ".SEGMENT screen DATA BANK(Video)\n"
      ".SEGMENT late EXTRA BANK(later)\n"
      ".BANK later, 1\n");
  EXPECT_THAT(program.banks, ElementsAre(Program::Bank{.assign_mask = 0x000C},
                                         Program::Bank{.assign_mask = 0xFFFF},
                                         Program::Bank{.assign_mask = 0x0001}));
  ASSERT_THAT(program.segments, SizeIs(3));
  EXPECT_EQ(program.segments[0].bank, 1);
  EXPECT_EQ(program.segments[1].bank, 0);
  EXPECT_EQ(program.segments[2].bank, 2);
}

TEST_F(ProgramAssemblerTest, BankErrors) {
  EXPECT_THAT(AssembleErrors(".BANK a\n.BANK A\n.SEGMENT s BANK(a)\n"),
              ElementsAre(IsErrorAt(1, 6, "The bank A is already declared")));
  EXPECT_THAT(AssembleErrors(".SEGMENT s BANK(a)\n"),
              ElementsAre(IsErrorAt(0, 16, "No bank A is declared")));
  EXPECT_THAT(AssembleErrors(".BANK a\n.SEGMENT s BANK(b)\n"),
              ElementsAre(IsErrorAt(1, 16, "No bank B is declared")));
  EXPECT_THAT(AssembleErrors(".BANK a\n.SEGMENT s BANK(a)\n.SEGMENT t\n"),
              ElementsAre(IsErrorAt(
                  2, 9, "The program declares banks, so T needs a BANK")));
  EXPECT_THAT(AssembleErrors(".STACK 1\n.BANK a\n"),
              ElementsAre(IsErrorAt(1, 6,
                                    "The program declares banks, so it needs "
                                    "segments, each with a BANK")));

  // A mask is used in the first pass, and a bank with an error in its mask is
  // still declared.
  EXPECT_THAT(AssembleErrors(".BANK a, M\n"
                             ".CONST M = 1\n"
                             ".SEGMENT s BANK(a)\n"),
              ElementsAre(IsErrorAt(0, 9, "M is not defined")));

  // A mask must allow at least one physical bank.
  EXPECT_THAT(AssembleErrors(".BANK a, 1 - 1\n.SEGMENT s BANK(a)\n"),
              ElementsAre(IsErrorAt(0, 9,
                                    "A bank's mask needs a bit for at least "
                                    "one physical bank")));
}

TEST_F(ProgramAssemblerTest, BanksLoad) {
  // Segments with more pages than a bank between them load into two banks.
  Program program = Assemble(
      ".BANK first, 1\n"
      ".BANK second, 2\n"
      ".SEGMENT a CODE BANK(first) PAGES(9)\n"
      ".SEGMENT b DATA BANK(second) PAGES(9)\n");
  Processor processor(ProcessorConfig::MultiBank(2));
  ProgramLoader loader(&processor);
  const ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  const ProgramLoader::LoadedProgram* loaded = loader.GetProgram(handle);
  ASSERT_NE(loaded, nullptr);
  ASSERT_THAT(loaded->pages, SizeIs(2));
  EXPECT_EQ(loaded->pages[0].bank, 0);
  EXPECT_EQ(loaded->pages[1].bank, 1);
}

// A program with three instances, each with its own data and stack, sharing
// code and extra.
constexpr std::string_view kInstancesProgram = R"---(
  .INSTANCES 3
  .STACK 100
  .SEGMENT main CODE
    NOP
  .SEGMENT vars DATA INSTANCE
    .DS 10
  .SEGMENT shared EXTRA
    .DW 1
  .SEGMENT stack STACK INSTANCE
    .DS 20
)---";

TEST_F(ProgramAssemblerTest, Instances) {
  // A per instance segment has each instance's words in turn, and its stack
  // is at the end of each instance's words.
  Program program = Assemble(kInstancesProgram);
  ASSERT_THAT(program.segments, SizeIs(4));
  EXPECT_EQ(program.segments[1].page_count, 1);
  EXPECT_THAT(program.segments[1].data, IsEmpty());
  EXPECT_EQ(program.segments[3].page_count, 1);
  EXPECT_THAT(program.instances,
              ElementsAre(Program::Instance{.stack = 4096 - 120, .data = 0},
                          Program::Instance{.stack = 4096 - 240, .data = 10},
                          Program::Instance{.stack = 4096 - 360, .data = 20}));

  // With one instance, a per instance segment is the same as a shared one,
  // and a shared stack is fine.
  program = Assemble(".STACK 10\n.SEGMENT stack STACK INSTANCE\n");
  EXPECT_THAT(program.instances, ElementsAre(Program::Instance{.stack = 4086}));
  program = Assemble(".INSTANCES 1\n.STACK 10\n.SEGMENT stack STACK\n");
  EXPECT_THAT(program.instances, ElementsAre(Program::Instance{}));
}

TEST_F(ProgramAssemblerTest, InstanceSizes) {
  // A per instance segment's size is its words for every instance, and PAGES
  // gives the extra space at the end.
  Program program = Assemble(
      ".INSTANCES 2\n"
      ".SEGMENT vars DATA INSTANCE\n"
      ".DS 3000\n");
  EXPECT_EQ(program.segments[0].page_count, 2);
  EXPECT_THAT(program.instances, ElementsAre(Program::Instance{.data = 0},
                                             Program::Instance{.data = 3000}));
  program = Assemble(
      ".INSTANCES 2\n"
      ".STACK 10\n"
      ".SEGMENT stack STACK INSTANCE PAGES(2)\n");
  EXPECT_THAT(program.instances,
              ElementsAre(Program::Instance{.stack = 8192 - 10},
                          Program::Instance{.stack = 8192 - 20}));

  EXPECT_THAT(AssembleErrors(".INSTANCES 2\n"
                             ".SEGMENT vars DATA INSTANCE PAGES(1)\n"
                             ".DS 3000\n"),
              ElementsAre(IsErrorAt(1, 28,
                                    "VARS needs 2 pages (6000 words), but "
                                    "PAGES gives it 1")));
  EXPECT_THAT(AssembleErrors(".INSTANCES 2\n"
                             ".SEGMENT vars DATA INSTANCE\n"
                             ".DS 40000\n"),
              ElementsAre(IsErrorAt(1, 19,
                                    "VARS's 2 instances of 40000 words pass "
                                    "the end of the segment, at 65536 words")));
}

TEST_F(ProgramAssemblerTest, PerInstanceSegmentErrors) {
  // A per instance segment can only reserve words, and is never code.
  EXPECT_THAT(AssembleErrors(".SEGMENT vars DATA INSTANCE\n"
                             ".DW 1\n"
                             "NOP\n"
                             ".DS 1\n"),
              ElementsAre(IsErrorAt(1, 0,
                                    "This writes words, but VARS is per "
                                    "instance, so it can only reserve them, "
                                    "with .DS"),
                          IsErrorAt(2, 0, "This writes words")));
  EXPECT_THAT(AssembleErrors(".SEGMENT main CODE INSTANCE\n"
                             ".SEGMENT other INSTANCE CODE\n"),
              ElementsAre(IsErrorAt(0, 19,
                                    "Code is shared by every instance, so MAIN "
                                    "can't have both CODE and INSTANCE"),
                          IsErrorAt(1, 24, "The CODE role is already")));
  EXPECT_THAT(AssembleErrors(".SEGMENT main INSTANCE CODE\n"),
              ElementsAre(IsErrorAt(0, 14,
                                    "Code is shared by every instance, so MAIN "
                                    "can't have both CODE and INSTANCE")));
}

TEST_F(ProgramAssemblerTest, SharedStackWithInstances) {
  EXPECT_THAT(AssembleErrors(".INSTANCES 2\n"
                             ".STACK 1\n"
                             ".SEGMENT stack STACK\n"),
              ElementsAre(IsErrorAt(2, 9,
                                    "The program has 2 instances, so its stack "
                                    "segment, STACK, needs INSTANCE")));
  EXPECT_THAT(AssembleErrors(".INSTANCES 2\n.STACK 1\nNOP\n"),
              ElementsAre(IsErrorAt(0, 0,
                                    "The program has 2 instances, so it needs "
                                    "segments, with a per instance stack "
                                    "segment")));
}

TEST_F(ProgramAssemblerTest, InstanceCount) {
  EXPECT_THAT(AssembleErrors(".INSTANCES 0\n.STACK 1\n"),
              ElementsAre(IsErrorAt(
                  0, 11, "The number of instances must be from 1 to 8")));
  EXPECT_THAT(AssembleErrors(".INSTANCES 9\n.STACK 1\n"),
              ElementsAre(IsErrorAt(
                  0, 11, "The number of instances must be from 1 to 8")));
  EXPECT_THAT(AssembleErrors(".INSTANCES N\n.STACK 1\n.CONST N = 2\n"),
              ElementsAre(IsErrorAt(0, 11, "N is not defined")));
  EXPECT_THAT(AssembleErrors(".INSTANCES 1\n.INSTANCES 1\n.STACK 1\n"),
              ElementsAre(IsErrorAt(
                  1, 0, "The program's .INSTANCES is already given")));
}

TEST_F(ProgramAssemblerTest, InstancesLoad) {
  Program program = Assemble(kInstancesProgram);
  Processor processor(ProcessorConfig::MultiBank(1));
  ProgramLoader loader(&processor);
  const ProgramLoader::Handle handle = loader.Load(program);
  ASSERT_NE(handle, ProgramLoader::kInvalidHandle);
  // The instances share code and extra, and each one's data and stack are its
  // own words in turn.
  const CpuCore::ResetParams first = loader.GetResetParams(handle, 0);
  for (int i = 1; i < 3; ++i) {
    const CpuCore::ResetParams params = loader.GetResetParams(handle, i);
    EXPECT_EQ(params.bc, first.bc) << "Instance " << i;
    EXPECT_EQ(params.be, first.be) << "Instance " << i;
    EXPECT_EQ(params.bd, first.bd + i * 10) << "Instance " << i;
    EXPECT_EQ(params.bs, first.bs + i * 120) << "Instance " << i;
  }
}

TEST_F(ProgramAssemblerTest, EveryErrorInAPass) {
  // A line with an error doesn't stop the lines after it, even one that
  // doesn't parse.
  EXPECT_THAT(AssembleErrors(".STACK 1\n"
                             "5\n"
                             "NOP\n"
                             "FOO\n"
                             "6\n"
                             "7\n"
                             ".DW X\n"),
              ElementsAre(IsErrorAt(1, 0, ""),
                          IsErrorAt(3, 0, "Unknown instruction FOO"),
                          IsErrorAt(4, 0, ""), IsErrorAt(5, 0, "")));

  // A pass with errors is the last, so the second pass's undefined X above
  // isn't reported. The second pass reports each of its own.
  EXPECT_THAT(AssembleErrors(".STACK 1\n.DW X\n.DW Y\n"),
              ElementsAre(IsErrorAt(1, 4, "X is not defined"),
                          IsErrorAt(2, 4, "Y is not defined")));
}

TEST_F(ProgramAssemblerTest, ErrorLimit) {
  std::string text = ".STACK 1\n";
  for (int i = 0; i < kMaxProgramErrors + 10; ++i) {
    absl::StrAppend(&text, "FOO\n");
  }
  std::vector<gb::ParseError> errors = AssembleErrors(text);
  ASSERT_THAT(errors, SizeIs(kMaxProgramErrors + 1));
  EXPECT_THAT(errors[kMaxProgramErrors - 1],
              IsErrorAt(kMaxProgramErrors, 0, "Unknown instruction FOO"));
  EXPECT_THAT(errors.back(),
              IsErrorAt(kMaxProgramErrors, 0, "Stopped after 100 errors"));
}

TEST_F(ProgramAssemblerTest, Includes) {
  // An included path is relative to the file that includes it, and its local
  // labels are in the scope of the label before the include.
  ASSERT_TRUE(file_system_.WriteFile("mem:/main.oza",
                                     ".STACK 1\n"
                                     "start: .INCLUDE \"lib/a.oza\"\n"
                                     ".DW @a, B\n"));
  ASSERT_TRUE(file_system_.CreateFolder("mem:/lib"));
  ASSERT_TRUE(file_system_.WriteFile("mem:/lib/a.oza",
                                     "NOP\n"
                                     "@a: .DW @a\n"
                                     ".INCLUDE \"../lib/b.oza\"\n"));
  ASSERT_TRUE(file_system_.WriteFile("mem:/lib/b.oza", ".CONST B = 7\n"));
  std::vector<gb::ParseError> errors;
  std::unique_ptr<AsmProgram> program =
      AssembleProgramFile(*syntax_, file_system_, "mem:/main.oza", &errors);
  EXPECT_THAT(errors, IsEmpty());
  ASSERT_NE(program, nullptr);
  EXPECT_THAT(
      program->GetProgram().segments[0].data,
      ElementsAre(Program::Segment::Data{.offset = 0, .data = {0, 1, 1, 7}}));
}

TEST_F(ProgramAssemblerTest, IncludedTwice) {
  // A file can be included more than once, as long as it doesn't define
  // symbols.
  ASSERT_TRUE(file_system_.WriteFile("mem:/main.oza",
                                     ".STACK 1\n"
                                     ".INCLUDE \"nop.oza\"\n"
                                     ".INCLUDE \"nop.oza\"\n"
                                     ".INCLUDE \"label.oza\"\n"
                                     ".INCLUDE \"label.oza\"\n"));
  ASSERT_TRUE(file_system_.WriteFile("mem:/nop.oza", "NOP\n"));
  ASSERT_TRUE(file_system_.WriteFile("mem:/label.oza", "NOP\nlabel: NOP\n"));
  std::vector<gb::ParseError> errors;
  EXPECT_EQ(
      AssembleProgramFile(*syntax_, file_system_, "mem:/main.oza", &errors),
      nullptr);
  ASSERT_THAT(errors, ElementsAre(IsErrorAt(1, 0, "LABEL is already defined")));
  EXPECT_EQ(errors[0].GetLocation().filename, "/label.oza");

  ASSERT_TRUE(file_system_.WriteFile("mem:/main.oza",
                                     ".STACK 1\n"
                                     ".INCLUDE \"nop.oza\"\n"
                                     ".INCLUDE \"nop.oza\"\n"));
  std::unique_ptr<AsmProgram> program =
      AssembleProgramFile(*syntax_, file_system_, "mem:/main.oza", &errors);
  EXPECT_THAT(errors, IsEmpty());
  ASSERT_NE(program, nullptr);
  EXPECT_THAT(program->GetProgram().segments[0].data,
              ElementsAre(Program::Segment::Data{.offset = 0, .data = {0, 0}}));
}

TEST_F(ProgramAssemblerTest, IncludeErrors) {
  ASSERT_TRUE(file_system_.WriteFile("mem:/main.oza",
                                     ".STACK 1\n"
                                     ".INCLUDE \"a.oza\"\n"
                                     ".INCLUDE \"missing.oza\"\n"
                                     "FOO\n"));
  ASSERT_TRUE(file_system_.WriteFile("mem:/a.oza",
                                     "NOP\n"
                                     ".INCLUDE \"mem:/main.oza\"\n"
                                     ".INCLUDE \"a.oza\"\n"));
  std::vector<gb::ParseError> errors;
  EXPECT_EQ(
      AssembleProgramFile(*syntax_, file_system_, "mem:/main.oza", &errors),
      nullptr);
  EXPECT_THAT(errors, ElementsAre(IsErrorAt(1, 9,
                                            "mem:/main.oza is already being "
                                            "included"),
                                  IsErrorAt(2, 9,
                                            "mem:/a.oza is already being "
                                            "included"),
                                  IsErrorAt(2, 9,
                                            "Could not read file: "
                                            "mem:/missing.oza"),
                                  IsErrorAt(3, 0, "Unknown instruction FOO")));
  EXPECT_EQ(errors[0].GetLocation().filename, "/a.oza");
  EXPECT_EQ(errors[2].GetLocation().filename, "/main.oza");
}

TEST_F(ProgramAssemblerTest, MainFileMissing) {
  std::vector<gb::ParseError> errors;
  EXPECT_EQ(
      AssembleProgramFile(*syntax_, file_system_, "mem:/main.oza", &errors),
      nullptr);
  EXPECT_THAT(errors, ElementsAre(IsErrorAt(-1, -1,
                                            "Could not read file: "
                                            "mem:/main.oza")));
}

TEST_F(ProgramAssemblerTest, IncludeFromText) {
  EXPECT_THAT(AssembleErrors(".STACK 1\n.INCLUDE \"a.oza\"\n"),
              ElementsAre(IsErrorAt(
                  1, 9, "A program assembled from text can't include files")));
}

TEST_F(ProgramAssemblerTest, MapOfTextProgram) {
  // A program from text has one file, and the default program's one segment,
  // which has no name.
  const ProgramMap map = AssembleMap(".STACK 1\nNOP\n");
  EXPECT_THAT(map.GetFiles(), ElementsAre("program"));
  EXPECT_THAT(map.GetSegments(), ElementsAre(""));
  EXPECT_THAT(map.GetStatements(),
              ElementsAre(MapStatement{.segment = 0,
                                       .offset = 0,
                                       .size = 1,
                                       .location = {.file = 0, .line = 1}}));
  EXPECT_THAT(map.GetLabels(), IsEmpty());
  EXPECT_THAT(map.GetConstants(), IsEmpty());
}

TEST_F(ProgramAssemblerTest, MapStatements) {
  // Each statement that writes words is in the map with its size, but words
  // that are only reserved aren't, by .DS or the stack.
  const ProgramMap map = AssembleMap(R"---(.STACK 4
    NOP
    PUSH.W 1
    PUSH.D 1
    .DW 1, "AB", 'Hi'
    .DS 2
    .DD 1, 2
    .CONST C = 1
    .DW ''
  )---");
  EXPECT_THAT(map.GetStatements(),
              ElementsAre(MapStatement{.segment = 0,
                                       .offset = 0,
                                       .size = 1,
                                       .location = {.file = 0, .line = 1}},
                          MapStatement{.segment = 0,
                                       .offset = 1,
                                       .size = 2,
                                       .location = {.file = 0, .line = 2}},
                          MapStatement{.segment = 0,
                                       .offset = 3,
                                       .size = 3,
                                       .location = {.file = 0, .line = 3}},
                          MapStatement{.segment = 0,
                                       .offset = 6,
                                       .size = 4,
                                       .location = {.file = 0, .line = 4}},
                          MapStatement{.segment = 0,
                                       .offset = 12,
                                       .size = 4,
                                       .location = {.file = 0, .line = 6}}));
  EXPECT_EQ(map.FindStatement(0, 10), nullptr);
  EXPECT_EQ(map.FindStatement(0, 11), nullptr);
  EXPECT_EQ(map.FindStatement(0, 16), nullptr);
  EXPECT_EQ(map.FindStatement(0, 19), nullptr);
}

TEST_F(ProgramAssemblerTest, MapSegments) {
  // Statements and labels are in their segments, which are in the program's
  // order.
  const ProgramMap map = AssembleMap(R"---(.STACK 4
    .SEGMENT main CODE
    start:
      NOP
    .SEGMENT vars DATA
    value:
      .DW 1
    .SEGMENT main
    next:
      .DW start
    .SEGMENT stack STACK
  )---");
  EXPECT_THAT(map.GetSegments(), ElementsAre("MAIN", "VARS", "STACK"));
  EXPECT_THAT(map.GetStatements(),
              ElementsAre(MapStatement{.segment = 0,
                                       .offset = 0,
                                       .size = 1,
                                       .location = {.file = 0, .line = 3}},
                          MapStatement{.segment = 0,
                                       .offset = 1,
                                       .size = 1,
                                       .location = {.file = 0, .line = 9}},
                          MapStatement{.segment = 1,
                                       .offset = 0,
                                       .size = 1,
                                       .location = {.file = 0, .line = 6}}));
  EXPECT_THAT(map.GetLabels(),
              ElementsAre(MapLabel{.name = "START",
                                   .segment = 0,
                                   .offset = 0,
                                   .location = {.file = 0, .line = 2}},
                          MapLabel{.name = "NEXT",
                                   .segment = 0,
                                   .offset = 1,
                                   .location = {.file = 0, .line = 8}},
                          MapLabel{.name = "VALUE",
                                   .segment = 1,
                                   .offset = 0,
                                   .location = {.file = 0, .line = 5}}));
}

TEST_F(ProgramAssemblerTest, MapLabels) {
  // A local label's name is its scope's label and then its own, or just its
  // own before the first label.
  const ProgramMap map = AssembleMap(R"---(.STACK 1
    @start: NOP
    first:
    @loop: NOP
      NOP
    second: .DW 1
    @loop:
  )---");
  EXPECT_THAT(map.GetLabels(),
              ElementsAre(MapLabel{.name = "@START",
                                   .segment = 0,
                                   .offset = 0,
                                   .location = {.file = 0, .line = 1}},
                          MapLabel{.name = "FIRST",
                                   .segment = 0,
                                   .offset = 1,
                                   .location = {.file = 0, .line = 2}},
                          MapLabel{.name = "FIRST@LOOP",
                                   .segment = 0,
                                   .offset = 1,
                                   .location = {.file = 0, .line = 3}},
                          MapLabel{.name = "SECOND",
                                   .segment = 0,
                                   .offset = 3,
                                   .location = {.file = 0, .line = 5}},
                          MapLabel{.name = "SECOND@LOOP",
                                   .segment = 0,
                                   .offset = 4,
                                   .location = {.file = 0, .line = 6}}));
}

TEST_F(ProgramAssemblerTest, MapConstants) {
  // A constant has a value in each width, and the word value isn't always the
  // dword value's low word. A constant that can't be a word has only a dword
  // value. $ and local labels are where the constant is defined.
  const ProgramMap map = AssembleMap(R"---(.STACK 1
    NOP
    .CONST MASK = ~1
    .CONST HALF = (0FFFFh + 1) / 2
    .CONST BIG = 10000h
    .CONST HERE = $
    first:
    @a: NOP
    .CONST LOCAL = @a
    second:
    @a: NOP
  )---");
  EXPECT_THAT(map.GetConstants(),
              ElementsAre(MapConstant{.name = "BIG",
                                      .word_value = std::nullopt,
                                      .dword_value = 0x10000,
                                      .location = {.file = 0, .line = 4}},
                          MapConstant{.name = "HALF",
                                      .word_value = 0,
                                      .dword_value = 0x8000,
                                      .location = {.file = 0, .line = 3}},
                          MapConstant{.name = "HERE",
                                      .word_value = 1,
                                      .dword_value = 1,
                                      .location = {.file = 0, .line = 5}},
                          MapConstant{.name = "LOCAL",
                                      .word_value = 1,
                                      .dword_value = 1,
                                      .location = {.file = 0, .line = 8}},
                          MapConstant{.name = "MASK",
                                      .word_value = 0xFFFE,
                                      .dword_value = 0xFFFFFFFE,
                                      .location = {.file = 0, .line = 2}}));
}

TEST_F(ProgramAssemblerTest, MapIncludes) {
  // Each file is in the map once, by the path it was read from, in the order
  // first read. A statement in a file included twice is in the map twice, at
  // the same line.
  ASSERT_TRUE(file_system_.WriteFile("mem:/main.oza",
                                     ".STACK 1\n"
                                     ".INCLUDE \"lib/a.oza\"\n"
                                     "start: .INCLUDE \"nop.oza\"\n"
                                     ".INCLUDE \"nop.oza\"\n"
                                     ".DW A\n"));
  ASSERT_TRUE(file_system_.CreateFolder("mem:/lib"));
  ASSERT_TRUE(file_system_.WriteFile("mem:/lib/a.oza", "NOP\n.CONST A = 5\n"));
  ASSERT_TRUE(file_system_.WriteFile("mem:/nop.oza", "\nNOP\n"));
  std::vector<gb::ParseError> errors;
  std::unique_ptr<AsmProgram> program =
      AssembleProgramFile(*syntax_, file_system_, "mem:/main.oza", &errors);
  EXPECT_THAT(errors, IsEmpty());
  ASSERT_NE(program, nullptr);
  const ProgramMap& map = program->GetMap();
  EXPECT_THAT(map.GetFiles(),
              ElementsAre("mem:/main.oza", "mem:/lib/a.oza", "mem:/nop.oza"));
  EXPECT_THAT(map.GetStatements(),
              ElementsAre(MapStatement{.segment = 0,
                                       .offset = 0,
                                       .size = 1,
                                       .location = {.file = 1, .line = 0}},
                          MapStatement{.segment = 0,
                                       .offset = 1,
                                       .size = 1,
                                       .location = {.file = 2, .line = 1}},
                          MapStatement{.segment = 0,
                                       .offset = 2,
                                       .size = 1,
                                       .location = {.file = 2, .line = 1}},
                          MapStatement{.segment = 0,
                                       .offset = 3,
                                       .size = 1,
                                       .location = {.file = 0, .line = 4}}));
  EXPECT_THAT(map.GetLabels(),
              ElementsAre(MapLabel{.name = "START",
                                   .segment = 0,
                                   .offset = 1,
                                   .location = {.file = 0, .line = 2}}));
  EXPECT_THAT(map.GetConstants(),
              ElementsAre(MapConstant{.name = "A",
                                      .word_value = 5,
                                      .dword_value = 5,
                                      .location = {.file = 1, .line = 1}}));
}

}  // namespace
}  // namespace oz3
