// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_set.h"
#include "absl/log/check.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "absl/types/span.h"
#include "gb/parse/parse_error.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/instruction_sets/default_instruction_set.h"
#include "oz3/instruction_sets/instruction_test.h"
#include "oz3/tools/instruction_set_syntax.h"
#include "oz3/tools/program.h"
#include "oz3/tools/program_assembler.h"
#include "oz3/tools/program_loader.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::SizeIs;

// Returns the syntax of the default instruction set.
std::unique_ptr<InstructionSetSyntax> CreateDefaultSyntax() {
  InstructionError error;
  std::unique_ptr<InstructionSetSyntax> syntax = InstructionSetSyntax::Create(
      GetDefaultInstructionSetDef(), GetDefaultInstructionSet().get(), &error);
  CHECK(syntax != nullptr) << error.message;
  return syntax;
}

// Assembles `text`, adding a test failure for each error. Returns null if it
// doesn't assemble.
std::unique_ptr<AsmProgram> AssembleOrFail(const InstructionSetSyntax& syntax,
                                           std::string text) {
  std::vector<gb::ParseError> errors;
  std::unique_ptr<AsmProgram> program =
      AssembleProgram(syntax, std::move(text), &errors);
  for (const gb::ParseError& error : errors) {
    ADD_FAILURE() << error.FormatMessage();
  }
  return program;
}

//==============================================================================
// Forms
//
// Every form of the default instruction set, written as the wiki's Default
// Instruction Set page writes it, assembles to the code word Encode() gives it,
// followed by the words of its values.
//==============================================================================

// The literal numbers of a macro's codes, such as a shift's count.
constexpr std::string_view kNumbers[] = {
    "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9",  "10",
    "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21",
    "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32"};

class AssembleFormTest : public InstructionTest {
 protected:
  // An operand of an instruction, as a program writes it.
  struct Operand {
    std::string text;

    // Its argument in the code word, if it has one, as Encode() takes it.
    std::optional<Arg> arg;

    // The words it adds after the code word.
    std::vector<uint16_t> words;
  };

  // A register or value in the code word, such as for "$r" or "$#5", or a
  // value by name, such as "Z | C" for "$Flags".
  static Operand Reg(int reg) { return {absl::StrCat("R", reg), Arg(reg)}; }
  static Operand DReg(int dreg) { return {absl::StrCat("D", dreg), Arg(dreg)}; }
  static Operand Number(int number) {
    return {absl::StrCat(number), Arg(number)};
  }
  static Operand Named(std::string_view text, int value) {
    return {std::string(text), Arg(value)};
  }

  // A register in brackets, as the block and repeating instructions take it.
  static Operand RegAt(int reg) {
    return {absl::StrCat("(R", reg, ")"), Arg(reg)};
  }
  static Operand DwordRegAt(int reg) {
    return {absl::StrCat("[R", reg, "]"), Arg(reg)};
  }

  // A value in the word after the code word, such as for "$v" in the pattern
  // itself (not a macro's code).
  static Operand Word(uint16_t value) {
    return {absl::StrCat(value), std::nullopt, {value}};
  }

  // Text that is only in the pattern, such as "SP".
  static Operand Literal(std::string_view text) { return {std::string(text)}; }

  // One of a macro's codes, with the register or value it encodes.
  static Operand Code(std::string_view text, std::string_view code,
                      int value = 0, std::vector<uint16_t> words = {}) {
    return {std::string(text), Arg(code, value), std::move(words)};
  }

  // The address forms, for a word and a dword.
  static std::vector<Operand> WordAddresses() {
    return {
        Code("(R2)", "($r)", CpuCore::R2),
        Code("(R5 + 3)", "($r + $v)", CpuCore::R5, {3}),
        Code("(SP)", "(SP)"),
        Code("(SP + 4)", "(SP + $v)", 0, {4}),
        Code("(FP)", "(FP)"),
        Code("(FP + -2)", "(FP + $v)", 0, {0xFFFE}),
        Code("S(10)", "S($v)", 0, {10}),
        Code("D(20h)", "D($v)", 0, {0x20}),
        Code("E(0FFFFh)", "E($v)", 0, {0xFFFF}),
    };
  }
  static std::vector<Operand> DwordAddresses() {
    return {
        Code("[R1]", "[$r]", CpuCore::R1),
        Code("[R6 + 5]", "[$r + $v]", CpuCore::R6, {5}),
        Code("[SP]", "[SP]"),
        Code("[SP + 6]", "[SP + $v]", 0, {6}),
        Code("[FP]", "[FP]"),
        Code("[FP + -4]", "[FP + $v]", 0, {0xFFFC}),
        Code("S[11]", "S[$v]", 0, {11}),
        Code("D[30h]", "D[$v]", 0, {0x30}),
        Code("E[0FFFEh]", "E[$v]", 0, {0xFFFE}),
    };
  }

  // A register, and then each address form: <reg> or <word-address>.
  static std::vector<Operand> WordRegOrAddresses() {
    std::vector<Operand> operands = WordAddresses();
    operands.insert(operands.begin(), Code("R3", "$r", CpuCore::R3));
    return operands;
  }
  static std::vector<Operand> DwordRegOrAddresses() {
    std::vector<Operand> operands = DwordAddresses();
    operands.insert(operands.begin(), Code("D2", "$R", 2));
    return operands;
  }

  // Every form of a value: <reg>, <integer>, or <word-address>.
  static std::vector<Operand> WordValues() {
    std::vector<Operand> operands = WordRegOrAddresses();
    operands.push_back(Code("1234", "$v", 0, {1234}));
    return operands;
  }
  static std::vector<Operand> DwordValues() {
    std::vector<Operand> operands = DwordRegOrAddresses();
    operands.push_back(Code("12345678h", "$V", 0, {0x5678, 0x1234}));
    return operands;
  }

  // As WordValues(), for a relative instruction. Its <integer> is a target
  // address, encoded as its offset from the next instruction. These are all
  // two words, so 100 words past the statement is 98.
  static std::vector<Operand> RelativeWordValues() {
    std::vector<Operand> operands = WordRegOrAddresses();
    operands.push_back(Code("$ + 100", "$v", 0, {98}));
    return operands;
  }

  // A register, and then each number from `first` to `last`, as a count or
  // bit position is: <reg>, or <1..16> and similar.
  static std::vector<Operand> RegOrNumbers(int first, int last) {
    std::vector<Operand> operands = {Code("R4", "$r", CpuCore::R4)};
    for (int number = first; number <= last; ++number) {
      operands.push_back(Code(kNumbers[number], kNumbers[number]));
    }
    return operands;
  }

  // Every condition, by name.
  static std::vector<Operand> Conditions() {
    return {Named("NZ", 0), Named("NS", 1), Named("NC", 2), Named("NO", 3),
            Named("Z", 4),  Named("S", 5),  Named("C", 6),  Named("O", 7)};
  }

  void SetUp() override { ASSERT_TRUE(Init()); }

  // Adds the statement "op a, b, ...", which must assemble to its code word, as
  // Encode() encodes it with the operands' arguments, followed by each
  // operand's words.
  void AddForm(std::string_view op, absl::Span<const Operand> operands) {
    std::vector<std::string_view> texts;
    std::vector<Arg> args;
    Statement statement;
    for (const Operand& operand : operands) {
      texts.push_back(operand.text);
      if (operand.arg.has_value()) {
        args.push_back(*operand.arg);
      }
      statement.words.insert(statement.words.end(), operand.words.begin(),
                             operand.words.end());
    }
    statement.text = texts.empty()
                         ? std::string(op)
                         : absl::StrCat(op, " ", absl::StrJoin(texts, ", "));
    ASSERT_LE(static_cast<int>(args.size()), 2) << statement.text;
    args.resize(2);
    statement.words.insert(statement.words.begin(),
                           Encode(op, args[0], args[1]));
    statements_.push_back(std::move(statement));
  }

  // Adds "op a" for each of `a`, or "op a, b" for each of `a` with each of `b`.
  void AddForms(std::string_view op, absl::Span<const Operand> a) {
    for (const Operand& operand : a) {
      AddForm(op, {operand});
    }
  }
  void AddForms(std::string_view op, absl::Span<const Operand> a,
                absl::Span<const Operand> b) {
    for (const Operand& first : a) {
      for (const Operand& second : b) {
        AddForm(op, {first, second});
      }
    }
  }

  // Assembles the statements added, in turn, as one program. Expects each to
  // assemble to its words, and every form of the instruction set to be one of
  // them.
  void ExpectStatementsAssemble() {
    std::string text = ".STACK 1\n";
    for (const Statement& statement : statements_) {
      absl::StrAppend(&text, statement.text, "\n");
    }
    std::unique_ptr<AsmProgram> program =
        AssembleOrFail(*syntax_, std::move(text));
    ASSERT_NE(program, nullptr);
    const Program::Segment& segment = program->GetProgram().segments[0];
    ASSERT_THAT(segment.data, SizeIs(1));
    absl::Span<const uint16_t> words = segment.data[0].data;

    // Each statement starts where the one before ends, which its form says,
    // so one with the wrong size doesn't throw off the rest.
    absl::flat_hash_set<const InstructionForm*> forms;
    int offset = 0;
    for (const Statement& statement : statements_) {
      SCOPED_TRACE(statement.text);
      ASSERT_LT(offset, static_cast<int>(words.size()));
      const InstructionForm* form = syntax_->FindForm(words[offset]);
      ASSERT_NE(form, nullptr);
      forms.insert(form);
      EXPECT_THAT(words.subspan(offset, form->GetSize()),
                  ElementsAreArray(statement.words));
      offset += form->GetSize();
    }
    for (const InstructionForm& form : syntax_->GetForms()) {
      EXPECT_TRUE(forms.contains(&form))
          << "No statement for " << form.GetInstruction().op_name << " \""
          << form.GetPattern() << "\"";
    }
  }

 private:
  // A statement, and the words it must assemble to.
  struct Statement {
    std::string text;
    std::vector<uint16_t> words;
  };

  const std::unique_ptr<InstructionSetSyntax> syntax_ = CreateDefaultSyntax();
  std::vector<Statement> statements_;
};

TEST_F(AssembleFormTest, EveryForm) {
  //----------------------------------------------------------------------------
  // Control
  //----------------------------------------------------------------------------

  AddForm("NOP", {});
  AddForm("HALT", {});
  AddForm("WAIT", {Reg(CpuCore::R6)});

  //----------------------------------------------------------------------------
  // Moving data
  //----------------------------------------------------------------------------

  AddForms("MOV.LW", {Reg(CpuCore::R1)}, WordValues());
  AddForms("MOV.LD", {DReg(3)}, DwordValues());
  AddForms("MOV.SWR", WordAddresses(), {Reg(CpuCore::R7)});
  AddForms("MOV.SDR", DwordAddresses(), {DReg(1)});
  AddForms("MOV.SWV",
           {
               Code("(R2), 1", "($r), $v", CpuCore::R2, {1}),
               Code("(R5 + 3), 2", "($r + $v), $v", CpuCore::R5, {3, 2}),
               Code("(SP), 3", "(SP), $v", 0, {3}),
               Code("(SP + 4), 4", "(SP + $v), $v", 0, {4, 4}),
               Code("(FP), 5", "(FP), $v", 0, {5}),
               Code("(FP + -2), 6", "(FP + $v), $v", 0, {0xFFFE, 6}),
               Code("S(10), 7", "S($v), $v", 0, {10, 7}),
               Code("D(20h), 8", "D($v), $v", 0, {0x20, 8}),
               Code("E(0FFFFh), 9", "E($v), $v", 0, {0xFFFF, 9}),
           });
  AddForms(
      "MOV.SDV",
      {
          Code("[R1], 10001h", "[$r], $V", CpuCore::R1, {1, 1}),
          Code("[R6 + 5], 20002h", "[$r + $v], $V", CpuCore::R6, {5, 2, 2}),
          Code("[SP], 30003h", "[SP], $V", 0, {3, 3}),
          Code("[SP + 6], 40004h", "[SP + $v], $V", 0, {6, 4, 4}),
          Code("[FP], 50005h", "[FP], $V", 0, {5, 5}),
          Code("[FP + -4], 60006h", "[FP + $v], $V", 0, {0xFFFC, 6, 6}),
          Code("S[11], 70007h", "S[$v], $V", 0, {11, 7, 7}),
          Code("D[30h], 80008h", "D[$v], $V", 0, {0x30, 8, 8}),
          Code("E[0FFFEh], 90009h", "E[$v], $V", 0, {0xFFFE, 9, 9}),
      });
  AddForms("MOV.S", {
                        Code("R0, BC", "$r, BC", CpuCore::R0),
                        Code("R1, BS", "$r, BS", CpuCore::R1),
                        Code("R2, BD", "$r, BD", CpuCore::R2),
                        Code("R3, BE", "$r, BE", CpuCore::R3),
                        Code("R4, SP", "$r, SP", CpuCore::R4),
                        Code("R5, FP", "$r, FP", CpuCore::R5),
                        Code("BC, R6", "BC, $r", CpuCore::R6),
                        Code("BS, R7", "BS, $r", CpuCore::R7),
                        Code("BD, R0", "BD, $r", CpuCore::R0),
                        Code("BE, R1", "BE, $r", CpuCore::R1),
                        Code("SP, R2", "SP, $r", CpuCore::R2),
                        Code("FP, R3", "FP, $r", CpuCore::R3),
                        Code("BC, 1", "BC, $v", 0, {1}),
                        Code("BS, 2", "BS, $v", 0, {2}),
                        Code("BD, 3", "BD, $v", 0, {3}),
                        Code("BE, 4", "BE, $v", 0, {4}),
                        Code("SP, 5", "SP, $v", 0, {5}),
                        Code("FP, 6", "FP, $v", 0, {6}),
                    });
  AddForm("MVQ.LW", {Reg(CpuCore::R2), Number(31)});
  AddForm("MVQ.LD", {DReg(2), Number(17)});
  AddForms("PUSH.W", WordValues());
  AddForms("PUSH.D", DwordValues());
  AddForms("POP.W", WordRegOrAddresses());
  AddForms("POP.D", DwordRegOrAddresses());
  AddForms("SWP.W", {Reg(CpuCore::R5)}, WordRegOrAddresses());
  AddForms("SWP.D", {DReg(0)}, DwordRegOrAddresses());
  AddForm("MVI", {RegAt(CpuCore::R1), RegAt(CpuCore::R4)});
  AddForm("MVD", {RegAt(CpuCore::R2), RegAt(CpuCore::R5)});
  AddForm("MVIR", {RegAt(CpuCore::R3), RegAt(CpuCore::R6)});
  AddForm("MVDR", {RegAt(CpuCore::R0), RegAt(CpuCore::R7)});

  //----------------------------------------------------------------------------
  // Arithmetic
  //----------------------------------------------------------------------------

  AddForm("NEG.W", {Reg(CpuCore::R3)});
  AddForm("NEG.D", {DReg(1)});
  for (std::string_view op : {"ADD", "ADC", "SUB", "SBC"}) {
    AddForms(absl::StrCat(op, ".W"), {Reg(CpuCore::R7)}, WordValues());
    AddForms(absl::StrCat(op, ".D"), {DReg(2)}, DwordValues());
  }
  AddForms("ADD.S", {Literal("SP")}, WordValues());
  AddForms("SUB.S", {Literal("SP")}, WordValues());
  for (std::string_view op : {"ADQ", "SBQ"}) {
    AddForm(absl::StrCat(op, ".W"), {Reg(CpuCore::R4), Number(31)});
    AddForm(absl::StrCat(op, ".D"), {DReg(3), Number(1)});
    AddForm(absl::StrCat(op, ".S"), {Literal("SP"), Number(16)});
  }

  //----------------------------------------------------------------------------
  // Multiply and divide
  //----------------------------------------------------------------------------

  for (std::string_view op : {"MUL.W", "MULS.W", "DIV.W", "DIVS.W", "MOD.W"}) {
    AddForms(op, {Reg(CpuCore::R0)}, WordValues());
  }
  for (std::string_view op : {"MUL.DW", "DIV.DW", "MOD.DW", "DVMD.W"}) {
    AddForms(op, {DReg(1)}, WordValues());
  }

  //----------------------------------------------------------------------------
  // Logic
  //----------------------------------------------------------------------------

  AddForm("NOT.W", {Reg(CpuCore::R5)});
  AddForm("NOT.D", {DReg(0)});
  for (std::string_view op : {"AND", "OR", "XOR"}) {
    AddForms(absl::StrCat(op, ".W"), {Reg(CpuCore::R6)}, WordValues());
    AddForms(absl::StrCat(op, ".D"), {DReg(3)}, DwordValues());
  }

  //----------------------------------------------------------------------------
  // Shifts and rotates
  //----------------------------------------------------------------------------

  for (std::string_view op :
       {"SHL", "SHR", "SRA", "ROL", "ROR", "RLC", "RRC"}) {
    AddForms(absl::StrCat(op, ".W"), {Reg(CpuCore::R1)}, RegOrNumbers(1, 16));
    AddForms(absl::StrCat(op, ".D"), {DReg(2)}, RegOrNumbers(1, 32));
  }

  //----------------------------------------------------------------------------
  // Bits and flags
  //----------------------------------------------------------------------------

  for (std::string_view op : {"CLRB", "SETB", "NOTB", "TSTB"}) {
    AddForms(absl::StrCat(op, ".W"), {Reg(CpuCore::R2)}, RegOrNumbers(0, 15));
    AddForms(absl::StrCat(op, ".D"), {DReg(1)}, RegOrNumbers(0, 31));
  }
  AddForm("CLRF", {Named("Z | C", 5)});
  AddForm("SETF", {Named("S | O", 10)});
  AddForm("NOTF", {Named("Z | S | C | O", 15)});

  //----------------------------------------------------------------------------
  // Compare
  //----------------------------------------------------------------------------

  AddForms("TST.W", WordValues());
  AddForms("TST.D", DwordValues());
  AddForms("CMP.W", {Reg(CpuCore::R3)}, WordValues());
  AddForms("CMP.D", {DReg(0)}, DwordValues());
  AddForm("CPI", {Reg(CpuCore::R1), RegAt(CpuCore::R2)});
  AddForm("CPD", {Reg(CpuCore::R3), RegAt(CpuCore::R4)});
  AddForm("CPIR", {Reg(CpuCore::R5), RegAt(CpuCore::R6)});
  AddForm("CPDR", {Reg(CpuCore::R7), RegAt(CpuCore::R0)});

  //----------------------------------------------------------------------------
  // Branches
  //----------------------------------------------------------------------------

  AddForms("JP", WordValues());
  AddForms("JPR", RelativeWordValues());
  AddForms("JC", Conditions(), {Code("R0", "$r", CpuCore::R0)});
  AddForms("JC", {Named("NC", 2)}, WordValues());
  AddForms("JCR", {Named("O", 7)}, RelativeWordValues());
  AddForms("JD", {Reg(CpuCore::R7)}, WordValues());
  AddForms("JDR", {Reg(CpuCore::R6)}, RelativeWordValues());
  AddForms("CALL", WordValues());
  AddForms("CALLR", RelativeWordValues());
  AddForm("FBGN", {});
  AddForm("FEND", {});
  AddForm("RET", {});
  AddForm("RET.A", {Number(31)});
  AddForms("RETC", Conditions());
  AddForm("RETC.A", {Named("NZ", 0), Number(3)});

  //----------------------------------------------------------------------------
  // Interrupts
  //----------------------------------------------------------------------------

  AddForm("IRT", {});
  AddForm("IRT.A", {Number(2)});
  AddForms("IRTC", Conditions());
  AddForm("IRTC.A", {Named("S", 5), Number(30)});
  AddForm("EI", {});
  AddForm("DI", {});
  AddForm("GETI", {Word(31), Reg(CpuCore::R3)});
  AddForms("SETI", {Word(1)}, WordValues());
  AddForms("INT", WordValues());

  //----------------------------------------------------------------------------
  // Ports
  //----------------------------------------------------------------------------

  for (std::string_view op : {"IN", "INS"}) {
    AddForms(absl::StrCat(op, ".RW"), {Reg(CpuCore::R1)}, WordRegOrAddresses());
    AddForms(absl::StrCat(op, ".IW"), {Word(255)}, WordRegOrAddresses());
    AddForms(absl::StrCat(op, ".RD"), {Reg(CpuCore::R2)},
             DwordRegOrAddresses());
    AddForms(absl::StrCat(op, ".ID"), {Word(0)}, DwordRegOrAddresses());
  }
  for (std::string_view op : {"OUT", "OUTS"}) {
    AddForms(absl::StrCat(op, ".RW"), {Reg(CpuCore::R3)}, WordValues());
    AddForms(absl::StrCat(op, ".IW"), {Word(7)}, WordValues());
    AddForms(absl::StrCat(op, ".RD"), {Reg(CpuCore::R4)}, DwordValues());
    AddForms(absl::StrCat(op, ".ID"), {Word(0x80)}, DwordValues());
  }
  for (std::string_view op : {"INR", "OUTR"}) {
    AddForm(absl::StrCat(op, ".RW"), {Reg(CpuCore::R5), RegAt(CpuCore::R6)});
    AddForm(absl::StrCat(op, ".IW"), {Word(12), RegAt(CpuCore::R7)});
    AddForm(absl::StrCat(op, ".RD"),
            {Reg(CpuCore::R0), DwordRegAt(CpuCore::R1)});
    AddForm(absl::StrCat(op, ".ID"), {Word(34), DwordRegAt(CpuCore::R2)});
  }

  //----------------------------------------------------------------------------
  // Cores
  //----------------------------------------------------------------------------

  AddForms("RST", {Code("5", "$#3", 5), Code("R2", "R2"), Code("SELF", "SELF")},
           {Named("CODE | EXTRA", 9)});
  AddForm("RST", {Code("0", "$#3", 0), Named("STACK | DATA", 6)});

  ExpectStatementsAssemble();
}

//==============================================================================
// Programs
//
// Small programs, written as programs are, assemble and run on a core.
//==============================================================================

class AssembledProgramTest : public InstructionTest {
 protected:
  // The most cycles a program may run before it must halt.
  static constexpr int kMaxCycles = 10000;

  void SetUp() override { ASSERT_TRUE(Init({.num_ports = 2})); }

  // Assembles `text`, which must assemble, loads it, and resets core 0 to run
  // it. Returns whether it did.
  bool Load(std::string_view text) {
    std::unique_ptr<AsmProgram> program =
        AssembleOrFail(*syntax_, std::string(text));
    if (program == nullptr) {
      return false;
    }
    ProgramLoader loader(&GetProcessor());
    const ProgramLoader::Handle handle = loader.Load(program->GetProgram());
    if (handle == ProgramLoader::kInvalidHandle) {
      ADD_FAILURE() << "The program doesn't load";
      return false;
    }
    GetState().ResetCore(loader.GetResetParams(handle));
    return true;
  }

  // Runs core 0 until it halts, updating `console` after every cycle if it
  // isn't null. Returns false if it doesn't halt within kMaxCycles, or its IP
  // passes 1000 (see ExecuteUntil).
  bool RunUntilHalt(PortDrainer* console = nullptr) {
    int cycles = 0;
    ExecuteUntil([&] {
      if (console != nullptr) {
        console->Update();
      }
      return ++cycles == kMaxCycles;
    });
    return GetState().core.GetState() == CpuCore::State::kIdle;
  }

 private:
  const std::unique_ptr<InstructionSetSyntax> syntax_ = CreateDefaultSyntax();
};

TEST_F(AssembledProgramTest, WritesMessageToPort) {
  // The example on the wiki's Program Source page.
  ASSERT_TRUE(Load(R"---(
    .CONST CONSOLE = 1
    .STACK 64

    .SEGMENT main CODE
    start:
        MOV R1, message     ; R1 is the message's address in DATA
    @next:
        MOV R2, (R1)
        TST R2
        JC Z, @done
        OUT CONSOLE, R2
        ADQ R1, 1
        JP @next
    @done:
        HALT

    .SEGMENT strings DATA ROM
    message:
        .DW 'Hello', 0

    .SEGMENT stack STACK
  )---"));
  // It may read more than the message, so any extra writes show.
  PortDrainer console(this, 1, PortSize::kWord, 16);
  ASSERT_TRUE(RunUntilHalt(&console));
  EXPECT_THAT(console.GetValues(), ElementsAre('H', 'e', 'l', 'l', 'o'));
}

TEST_F(AssembledProgramTest, CallsRelativeSubroutine) {
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        PUSH.W 5
        CALLR sum_squares
        ADQ SP, 1           ; Drops the argument
        HALT

    ; Returns in R0 the sum of the squares from 1 to the argument it is called
    ; with, which is pushed before the call.
    sum_squares:
        FBGN
        MOV R1, (FP + 2)    ; Past the old FP and the return address
        MVQ R0, 0
    @loop:
        MOV R2, R1
        MUL R2, R1
        ADD R0, R2
        JDR R1, @loop
        FEND
        RET
  )---"));
  ASSERT_TRUE(RunUntilHalt());
  const CoreState& state = GetState();
  EXPECT_EQ(state.r0, 55);
  EXPECT_EQ(state.r1, 0);
  EXPECT_EQ(state.sp, 0);
  EXPECT_EQ(state.fp, 0);
}

TEST_F(AssembledProgramTest, StoresDwordInDataSegment) {
  ASSERT_TRUE(Load(R"---(
    .CONST N = 10
    .STACK 16

    .SEGMENT main CODE
        MOV D0, 1
        MVQ R2, N
    @loop:
        MUL D0, R2          ; MUL.DW
        JD R2, @loop
        MOV D[factorial], D0
        MOV R3, factorial   ; R3 is its address in DATA
        MOV D2, [R3]
        HALT

    .SEGMENT vars DATA
        .DW 0FFFFh
    factorial:
        .DS 2

    .SEGMENT stack STACK
  )---"));
  ASSERT_TRUE(RunUntilHalt());
  MemAccessor data = GetMemory();
  data.SetAddress(GetState().bd);
  EXPECT_EQ(data.GetValue(), 0xFFFF);
  EXPECT_EQ(data.GetValue32(), 3628800u);
  EXPECT_EQ(GetState().d2(), 3628800u);
}

TEST_F(AssembledProgramTest, HandlesInterrupt) {
  ASSERT_TRUE(Load(R"---(
    .STACK 16
        SETI 3, handler
        EI
        MVQ R0, 0
        INT 3
        ADQ R0, 1           ; After the handler returns
        HALT

    handler:
        MVQ R1, 7
        IRT
  )---"));
  ASSERT_TRUE(RunUntilHalt());
  const CoreState& state = GetState();
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.r1, 7);
  EXPECT_EQ(state.sp, 0);
}

}  // namespace
}  // namespace oz3
