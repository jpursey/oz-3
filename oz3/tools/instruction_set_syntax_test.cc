// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_set_syntax.h"

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/log/check.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/parse/parse_error.h"
#include "gb/parse/symbol.h"
#include "gb/parse/token.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/instruction_def.h"
#include "oz3/tools/instruction_assembler.h"
#include "oz3/tools/source_pattern.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using Kind = InstructionForm::Arg::Kind;

// GetWord's codes are laid out as:
//   0 0 r r r  "$r"
//   0 1 r r r  "($r + $v)"
//   1 0 0 0 0  "$v"
//   1 0 0 0 1  "D($v)"
constexpr std::string_view kInstructionSet = R"---(
  values(bits:3) Condition { NZ = 0, Z = 4 }

  macro GetWord:r {
    code:m "$r" { UL; }
    code:C2 "($r + $v)" { LD(C2); UL; ADD(C2,m); LKR(m); ADR(C2); LD(C2); UL; }
    code:C2 "$v" { LD(C2); UL; }
    code:C2 "D($v)" { LD(C2); UL; LK(DATA); ADR(C2); LD(C2); UL; }
  }

  instruction(opcode:0) NOP { UL; }
  instruction(opcode:1) ADD.W "$r, $m" { $GetWord; ADD(a,r); MSR(ZSCO,ZSCO); }
  instruction(opcode:2) PUSH "$m" {
    $GetWord; LK(STACK); ADR(SP); STP(r); LAD(SP); UL;
  }
  instruction(opcode:3) SETC "$Condition" { UL; }
  instruction(opcode:4, relative) JPR "$m" { $GetWord; ADD(IP,r); }
  instruction(opcode:5) MOV.D "$R, $V" { LD(a0); LD(a1); UL; }
  instruction(opcode:6) MVQ "$r, $#5" { UL; MOV(a,C1); }
  instruction(opcode:7) PEEK "[$m]" { $GetWord; MOV(R0,r); }
)---";

// An InstructionSetDef that a test can change, copied from an assembled one.
class TestDef {
 public:
  explicit TestDef(std::string_view source) {
    gb::ParseError error;
    asm_set_ = AssembleInstructionSet(std::string(source), &error);
    CHECK(asm_set_ != nullptr) << error.FormatMessage();
    const InstructionSetDef& def = asm_set_->GetInstructionSetDef();
    instructions.assign(def.instructions.begin(), def.instructions.end());
    macros.assign(def.macros.begin(), def.macros.end());
    for (const MacroDef& macro : macros) {
      codes.emplace_back(macro.code.begin(), macro.code.end());
    }
    value_sets.assign(def.value_sets.begin(), def.value_sets.end());
    for (const ValueSetDef& value_set : value_sets) {
      values.emplace_back(value_set.values.begin(), value_set.values.end());
    }
  }

  // Returns the def, as it is now.
  const InstructionSetDef& GetDef() {
    for (int i = 0; i < static_cast<int>(macros.size()); ++i) {
      macros[i].code = codes[i];
    }
    for (int i = 0; i < static_cast<int>(value_sets.size()); ++i) {
      value_sets[i].values = values[i];
    }
    def_ = {.instructions = instructions,
            .macros = macros,
            .value_sets = value_sets};
    return def_;
  }

  // Returns the instruction set compiled from the def as it was assembled.
  const InstructionSet* GetCompiled() const {
    return asm_set_->GetInstructionSet().get();
  }

  std::vector<InstructionDef> instructions;
  std::vector<MacroDef> macros;
  std::vector<std::vector<MacroCodeDef>> codes;
  std::vector<ValueSetDef> value_sets;
  std::vector<std::vector<NamedValueDef>> values;

 private:
  std::unique_ptr<AsmInstructionSet> asm_set_;
  InstructionSetDef def_;
};

std::vector<std::string_view> GetPatterns(
    absl::Span<const InstructionForm* const> forms) {
  std::vector<std::string_view> patterns;
  for (const InstructionForm* form : forms) {
    patterns.push_back(form->GetPattern());
  }
  return patterns;
}

void ExpectArg(const InstructionForm::Arg& arg, Kind kind, int bits, int shift,
               const ValueSetDef* value_set = nullptr, bool relative = false) {
  EXPECT_EQ(arg.kind, kind);
  EXPECT_EQ(arg.bits, bits);
  EXPECT_EQ(arg.shift, shift);
  EXPECT_EQ(arg.value_set, value_set);
  EXPECT_EQ(arg.relative, relative);
}

void ExpectForm(const InstructionForm& form, std::string_view pattern,
                uint16_t code_word, uint16_t mask, int size) {
  EXPECT_EQ(form.GetPattern(), pattern);
  EXPECT_EQ(form.GetCodeWord(), code_word) << pattern;
  EXPECT_EQ(form.GetCodeWordMask(), mask) << pattern;
  EXPECT_EQ(form.GetSize(), size) << pattern;
}

//==============================================================================
// Forms
//==============================================================================

TEST(InstructionSetSyntaxTest, InstructionWithoutArguments) {
  TestDef def(kInstructionSet);
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  auto forms = syntax->GetForms("NOP");
  ASSERT_EQ(forms.size(), 1);
  EXPECT_EQ(&forms[0]->GetInstruction(), &def.instructions[0]);
  EXPECT_EQ(forms[0]->GetMacroCode(), nullptr);
  ExpectForm(*forms[0], "", 0x0000, 0xFF00, 1);
  EXPECT_THAT(forms[0]->GetParts(), IsEmpty());
  EXPECT_THAT(forms[0]->GetArgs(), IsEmpty());
}

TEST(InstructionSetSyntaxTest, MacroAsSecondArgument) {
  TestDef def(kInstructionSet);
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  auto forms = syntax->GetForms("ADD.W");
  ASSERT_EQ(forms.size(), 4);
  for (int i = 0; i < 4; ++i) {
    EXPECT_EQ(&forms[i]->GetInstruction(), &def.instructions[1]);
    EXPECT_EQ(forms[i]->GetMacroCode(), &def.codes[0][i]);
  }

  // The macro is in bits 3 to 7, above the register.
  ExpectForm(*forms[0], "$r, $r", 0x0100, 0xFFC0, 1);
  ExpectForm(*forms[1], "$r, ($r + $v)", 0x0140, 0xFFC0, 2);
  ExpectForm(*forms[2], "$r, $v", 0x0180, 0xFFF8, 2);
  ExpectForm(*forms[3], "$r, D($v)", 0x0188, 0xFFF8, 2);

  auto args = forms[1]->GetArgs();
  ASSERT_EQ(args.size(), 3);
  ExpectArg(args[0], Kind::kWordReg, 3, 0);
  ExpectArg(args[1], Kind::kWordReg, 3, 3);
  ExpectArg(args[2], Kind::kWord, 0, 0);

  auto parts = forms[1]->GetParts();
  ASSERT_EQ(parts.size(), 7);
  EXPECT_EQ(parts[0].kind, SourcePattern::Part::Kind::kArgument);
  EXPECT_TRUE(parts[0].whole_arg);
  EXPECT_EQ(parts[1].literal, gb::TokenValue(gb::Symbol(',')));
  EXPECT_EQ(parts[2].literal, gb::TokenValue(gb::Symbol('(')));
  EXPECT_EQ(parts[3].kind, SourcePattern::Part::Kind::kArgument);
  EXPECT_FALSE(parts[3].whole_arg);
  EXPECT_EQ(parts[5].kind, SourcePattern::Part::Kind::kWord);
  EXPECT_FALSE(parts[5].whole_arg);

  args = forms[2]->GetArgs();
  ASSERT_EQ(args.size(), 2);
  ExpectArg(args[0], Kind::kWordReg, 3, 0);
  ExpectArg(args[1], Kind::kWord, 0, 0);
  EXPECT_TRUE(forms[2]->GetParts()[2].whole_arg);
}

TEST(InstructionSetSyntaxTest, MacroAsFirstArgument) {
  TestDef def(kInstructionSet);
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  auto forms = syntax->GetForms("PUSH");
  ASSERT_EQ(forms.size(), 4);
  ExpectForm(*forms[0], "$r", 0x0200, 0xFF18, 1);
  ExpectForm(*forms[1], "($r + $v)", 0x0208, 0xFF18, 2);
  ExpectForm(*forms[2], "$v", 0x0210, 0xFF1F, 2);
  ExpectForm(*forms[3], "D($v)", 0x0211, 0xFF1F, 2);
  auto args = forms[1]->GetArgs();
  ASSERT_EQ(args.size(), 2);
  ExpectArg(args[0], Kind::kWordReg, 3, 0);
  ExpectArg(args[1], Kind::kWord, 0, 0);
}

TEST(InstructionSetSyntaxTest, CodeWordArguments) {
  TestDef def(kInstructionSet);
  const InstructionSetDef& instruction_set = def.GetDef();
  auto syntax = InstructionSetSyntax::Create(instruction_set);
  ASSERT_NE(syntax, nullptr);

  auto forms = syntax->GetForms("SETC");
  ASSERT_EQ(forms.size(), 1);
  ExpectForm(*forms[0], "$Condition", 0x0300, 0xFF00, 1);
  ASSERT_EQ(forms[0]->GetArgs().size(), 1);
  ExpectArg(forms[0]->GetArgs()[0], Kind::kCodeValue, 3, 0,
            &instruction_set.value_sets[0]);

  forms = syntax->GetForms("MVQ");
  ASSERT_EQ(forms.size(), 1);
  ExpectForm(*forms[0], "$r, $#5", 0x0600, 0xFF00, 1);
  ASSERT_EQ(forms[0]->GetArgs().size(), 2);
  ExpectArg(forms[0]->GetArgs()[0], Kind::kWordReg, 3, 0);
  ExpectArg(forms[0]->GetArgs()[1], Kind::kCodeValue, 5, 3);

  forms = syntax->GetForms("MOV.D");
  ASSERT_EQ(forms.size(), 1);
  ExpectForm(*forms[0], "$R, $V", 0x0500, 0xFF00, 3);
  ASSERT_EQ(forms[0]->GetArgs().size(), 2);
  ExpectArg(forms[0]->GetArgs()[0], Kind::kDwordReg, 2, 0);
  ExpectArg(forms[0]->GetArgs()[1], Kind::kDword, 0, 0);
}

TEST(InstructionSetSyntaxTest, RelativeValuesAreWholeArguments) {
  TestDef def(kInstructionSet);
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  auto forms = syntax->GetForms("JPR");
  ASSERT_EQ(forms.size(), 4);
  ASSERT_EQ(forms[1]->GetArgs().size(), 2);
  EXPECT_FALSE(forms[1]->GetArgs()[1].relative);  // ($r + $v)
  ASSERT_EQ(forms[2]->GetArgs().size(), 1);
  EXPECT_TRUE(forms[2]->GetArgs()[0].relative);  // $v
  ASSERT_EQ(forms[3]->GetArgs().size(), 1);
  EXPECT_FALSE(forms[3]->GetArgs()[0].relative);  // D($v)

  // ADD.W isn't relative, so neither is its $v.
  forms = syntax->GetForms("ADD.W");
  ASSERT_EQ(forms.size(), 4);
  EXPECT_FALSE(forms[2]->GetArgs()[1].relative);
}

TEST(InstructionSetSyntaxTest, MacroCodeIsOnlyWholeIfMacroIs) {
  TestDef def(kInstructionSet);
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  auto forms = syntax->GetForms("PEEK");
  ASSERT_EQ(forms.size(), 4);
  EXPECT_EQ(forms[2]->GetPattern(), "[$v]");
  auto parts = forms[2]->GetParts();
  ASSERT_EQ(parts.size(), 3);
  EXPECT_EQ(parts[1].kind, SourcePattern::Part::Kind::kWord);
  EXPECT_FALSE(parts[1].whole_arg);
}

TEST(InstructionSetSyntaxTest, PartPositionsAreInThePattern) {
  TestDef def(kInstructionSet);
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  for (const InstructionForm& form : syntax->GetForms()) {
    for (const SourcePattern::Part& part : form.GetParts()) {
      std::string_view text = form.GetPattern().substr(part.offset, part.size);
      if (part.kind == SourcePattern::Part::Kind::kLiteral) {
        EXPECT_FALSE(text.starts_with("$")) << form.GetPattern();
      } else {
        EXPECT_TRUE(text.starts_with("$")) << form.GetPattern();
      }
    }
  }

  // The parts after the macro code's move with it.
  auto forms = syntax->GetForms("PEEK");
  ASSERT_EQ(forms.size(), 4);
  EXPECT_EQ(forms[1]->GetPattern(), "[($r + $v)]");
  auto parts = forms[1]->GetParts();
  ASSERT_EQ(parts.size(), 7);
  const int kOffsets[] = {0, 1, 2, 5, 7, 9, 10};
  for (int i = 0; i < 7; ++i) {
    EXPECT_EQ(parts[i].offset, kOffsets[i]) << i;
  }
  EXPECT_EQ(parts[6].literal, gb::TokenValue(gb::Symbol(']')));
}

TEST(InstructionSetSyntaxTest, AllFormsInOrder) {
  TestDef def(kInstructionSet);
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  std::vector<std::string_view> names;
  for (const InstructionForm& form : syntax->GetForms()) {
    names.push_back(form.GetInstruction().op_name);
  }
  EXPECT_THAT(names, ElementsAre("NOP", "ADD.W", "ADD.W", "ADD.W", "ADD.W",
                                 "PUSH", "PUSH", "PUSH", "PUSH", "SETC", "JPR",
                                 "JPR", "JPR", "JPR", "MOV.D", "MVQ", "PEEK",
                                 "PEEK", "PEEK", "PEEK"));
}

TEST(InstructionSetSyntaxTest, FormsByName) {
  TestDef def(R"---(
    instruction(opcode:0) ADD { UL; }
    instruction(opcode:1) ADD.W "$r" { UL; }
    instruction(opcode:2) ADD.D "$R" { UL; }
    instruction(opcode:3) ADDX.W "$r, $r" { UL; }
  )---");
  def.instructions[3].op_name = "addx.w";
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  EXPECT_THAT(GetPatterns(syntax->GetForms("ADD")),
              ElementsAre("", "$r", "$R"));
  EXPECT_THAT(GetPatterns(syntax->GetForms("add")),
              ElementsAre("", "$r", "$R"));
  EXPECT_THAT(GetPatterns(syntax->GetForms("Add.W")), ElementsAre("$r"));
  EXPECT_THAT(GetPatterns(syntax->GetForms("ADD.D")), ElementsAre("$R"));
  EXPECT_THAT(GetPatterns(syntax->GetForms("ADDX")), ElementsAre("$r, $r"));
  EXPECT_THAT(GetPatterns(syntax->GetForms("ADDX.W")), ElementsAre("$r, $r"));
  EXPECT_THAT(syntax->GetForms("ADD.X"), IsEmpty());
  EXPECT_THAT(syntax->GetForms("SUB"), IsEmpty());
  EXPECT_THAT(syntax->GetForms(""), IsEmpty());
}

TEST(InstructionSetSyntaxTest, FindForm) {
  TestDef def(kInstructionSet);
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  for (const InstructionForm& form : syntax->GetForms()) {
    EXPECT_EQ(syntax->FindForm(form.GetCodeWord()), &form)
        << form.GetInstruction().op_name << " " << form.GetPattern();
  }

  // ADD.W R7, (R5 + $v)
  auto forms = syntax->GetForms("ADD.W");
  ASSERT_EQ(forms.size(), 4);
  EXPECT_EQ(syntax->FindForm(0x0100 | (0b01101 << 3) | 7), forms[1]);

  // Bits above every argument are ignored.
  forms = syntax->GetForms("SETC");
  ASSERT_EQ(forms.size(), 1);
  EXPECT_EQ(syntax->FindForm(0x03FF), forms[0]);
}

TEST(InstructionSetSyntaxTest, FindFormOfUnusedCodeWords) {
  TestDef def(kInstructionSet);
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  EXPECT_EQ(syntax->FindForm(0x0800), nullptr);
  EXPECT_EQ(syntax->FindForm(0xFFFF), nullptr);

  // GetWord's codes use values 0 to 17 of its 32.
  EXPECT_EQ(syntax->FindForm(0x0100 | (18 << 3)), nullptr);
  EXPECT_EQ(syntax->FindForm(0x0200 | 31), nullptr);
}

TEST(InstructionSetSyntaxTest, CreateWithCompiledInstructionSet) {
  TestDef def(kInstructionSet);
  auto syntax =
      InstructionSetSyntax::Create(def.GetDef(), def.GetCompiled(), nullptr);
  ASSERT_NE(syntax, nullptr);
  EXPECT_EQ(syntax->GetForms().size(), 20);
}

TEST(InstructionSetSyntaxTest, DifferentLiteralNumbersAreDifferentForms) {
  TestDef def(R"---(
    macro Shift {
      code "$r" { UL; }
      code "1" { UL; }
      code "2" { UL; }
    }
    instruction(opcode:0) SHL "$r, $m" { $Shift; }
  )---");
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  EXPECT_THAT(GetPatterns(syntax->GetForms("SHL")),
              ElementsAre("$r, $r", "$r, 1", "$r, 2"));
}

//==============================================================================
// Errors
//==============================================================================

// Creates the syntax for `def`, which must fail with an error in `def_name`,
// and returns the error.
InstructionError ExpectError(TestDef& def, InstructionError::Def def_kind,
                             std::string_view def_name,
                             const InstructionSet* instruction_set = nullptr) {
  InstructionError error;
  EXPECT_EQ(InstructionSetSyntax::Create(def.GetDef(), instruction_set, &error),
            nullptr);
  EXPECT_EQ(error.def, def_kind) << error.message;
  EXPECT_EQ(error.def_name, def_name) << error.message;
  return error;
}

TEST(InstructionSetSyntaxTest, ErrorIsOptional) {
  TestDef def(kInstructionSet);
  def.instructions[0].code = "XYZ;";
  EXPECT_EQ(InstructionSetSyntax::Create(def.GetDef()), nullptr);
}

TEST(InstructionSetSyntaxTest, CompileError) {
  TestDef def(kInstructionSet);
  def.instructions[0].code = "XYZ;";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "NOP");
  EXPECT_FALSE(error.message.empty());
}

TEST(InstructionSetSyntaxTest, DuplicateValueSet) {
  TestDef def(kInstructionSet);
  def.value_sets.push_back(def.value_sets[0]);
  def.values.push_back(def.values[0]);
  InstructionError error =
      ExpectError(def, InstructionError::Def::kValueSet, "Condition");
  EXPECT_THAT(error.message, HasSubstr("'Condition' is already defined"));
}

TEST(InstructionSetSyntaxTest, ValueSetNamedAsPlaceholder) {
  TestDef def(kInstructionSet);
  def.value_sets[0].name = "r4";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kValueSet, "r4");
  EXPECT_THAT(error.message, HasSubstr("placeholder $r4"));
}

TEST(InstructionSetSyntaxTest, ValueSetBitsInvalid) {
  for (int bits : {0, 9}) {
    TestDef def(kInstructionSet);
    def.value_sets[0].size = static_cast<uint8_t>(bits);
    InstructionError error =
        ExpectError(def, InstructionError::Def::kValueSet, "Condition");
    EXPECT_THAT(error.message,
                HasSubstr(absl::StrCat("Invalid bits value: ", bits)));
  }
}

TEST(InstructionSetSyntaxTest, ValueSetWithoutValues) {
  TestDef def(kInstructionSet);
  def.values[0].clear();
  InstructionError error =
      ExpectError(def, InstructionError::Def::kValueSet, "Condition");
  EXPECT_THAT(error.message, HasSubstr("no values"));
}

TEST(InstructionSetSyntaxTest, DuplicateValueNameIgnoringCase) {
  TestDef def(kInstructionSet);
  def.values[0][1].name = "nz";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kValueSet, "Condition");
  EXPECT_THAT(error.message, HasSubstr("Duplicate value name nz"));
}

TEST(InstructionSetSyntaxTest, ValueDoesNotFit) {
  TestDef def(kInstructionSet);
  def.values[0][1].value = 8;
  InstructionError error =
      ExpectError(def, InstructionError::Def::kValueSet, "Condition");
  EXPECT_THAT(error.message, HasSubstr("Z = 8 does not fit in 3 bits"));
}

TEST(InstructionSetSyntaxTest, DuplicateInstructionNameIgnoringCase) {
  TestDef def(kInstructionSet);
  def.instructions[3].op_name = "add.w";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "add.w");
  EXPECT_THAT(error.message, HasSubstr("'add.w' is already defined"));
}

TEST(InstructionSetSyntaxTest, InvalidMacroCodeSource) {
  TestDef def(kInstructionSet);
  def.codes[0][1].source = "($r + $v";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kMacro, "GetWord");
  EXPECT_EQ(error.macro_source_name, "($r + $v");
  EXPECT_THAT(error.message, HasSubstr("Invalid macro code source"));
}

TEST(InstructionSetSyntaxTest, MacroCodeWithTwoArguments) {
  TestDef def(kInstructionSet);
  def.codes[0][1].source = "($r + $r)";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kMacro, "GetWord");
  EXPECT_THAT(error.message, HasSubstr("Only one"));
}

TEST(InstructionSetSyntaxTest, MacroCodeWithMacroArgument) {
  TestDef def(kInstructionSet);
  def.codes[0][1].source = "($m)";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kMacro, "GetWord");
  EXPECT_THAT(error.message, HasSubstr("$m is not allowed"));
}

TEST(InstructionSetSyntaxTest, MacroCodeSourceDisagreesWithArgument) {
  for (std::string_view source : {"($R + $v)", "($r2 + $v)", "(R1 + $v)"}) {
    TestDef def(kInstructionSet);
    def.codes[0][1].source = source;
    InstructionError error =
        ExpectError(def, InstructionError::Def::kMacro, "GetWord");
    EXPECT_EQ(error.macro_source_name, source);
    EXPECT_THAT(error.message, HasSubstr("doesn't match its argument"));
  }
  TestDef def(kInstructionSet);
  def.codes[0][2].source = "$r";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kMacro, "GetWord");
  EXPECT_THAT(error.message, HasSubstr("doesn't match its argument"));
}

TEST(InstructionSetSyntaxTest, MacroCodesWrittenTheSameWay) {
  // Each case changes one of GetWord's codes, and expects the error at
  // `error_code`, which is written the same way as `same_as` before it.
  struct Case {
    int code;
    std::string_view source;
    Argument arg;
    std::string_view error_code;
    std::string_view same_as;
  };
  const Case kCases[] = {
      {2, "d( $v )", {}, "D($v)", "d( $v )"},
      {3, "$#4", {ArgType::kImmediate, 4}, "$#4", "$v"},
      {3, "$Condition", {ArgType::kImmediate, 3}, "$Condition", "$v"},
      {3, "3", {}, "3", "$v"},
      {3, "$V", {}, "$V", "$v"},
      {1, "$r2", {ArgType::kWordReg, 2}, "$r2", "$r"},
      {1, "$r4", {ArgType::kWordReg, 4}, "$r4", "$r"},
  };
  for (const Case& test_case : kCases) {
    TestDef def(kInstructionSet);
    MacroCodeDef& code = def.codes[0][test_case.code];
    code.source = test_case.source;
    code.arg = test_case.arg;
    InstructionError error = ExpectError(def, InstructionError::Def::kMacro,
                                         "GetWord", def.GetCompiled());
    EXPECT_EQ(error.macro_source_name, test_case.error_code);
    EXPECT_THAT(error.message,
                HasSubstr(absl::StrCat("is written the same way as \"",
                                       test_case.same_as, "\"")));
  }
}

TEST(InstructionSetSyntaxTest, InvalidInstructionSource) {
  TestDef def(kInstructionSet);
  def.instructions[1].source = "$r; $m";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "ADD.W");
  EXPECT_THAT(error.message, HasSubstr("Invalid instruction code source"));
}

TEST(InstructionSetSyntaxTest, InstructionWithThreeArguments) {
  TestDef def(kInstructionSet);
  def.instructions[6].source = "$r, $#5, $r";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "MVQ");
  EXPECT_THAT(error.message, HasSubstr("At most two"));
}

TEST(InstructionSetSyntaxTest, InstructionWithTwoMacroArguments) {
  TestDef def(kInstructionSet);
  def.instructions[1].source = "$m, $m";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "ADD.W");
  EXPECT_THAT(error.message, HasSubstr("Only one macro"));
}

TEST(InstructionSetSyntaxTest, InstructionSourceDisagreesWithArguments) {
  for (std::string_view source :
       {"$r", "$m, $r", "$r2, $m", "$r, $m4", "$R, $m", "$r, $#5"}) {
    TestDef def(kInstructionSet);
    def.instructions[1].source = source;
    InstructionError error =
        ExpectError(def, InstructionError::Def::kInstruction, "ADD.W");
    EXPECT_THAT(error.message, HasSubstr("doesn't match the instruction's"))
        << source;
  }
  TestDef def(kInstructionSet);
  def.instructions[0].source = "$r";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "NOP");
  EXPECT_THAT(error.message, HasSubstr("doesn't match the instruction's"));
}

TEST(InstructionSetSyntaxTest, MacroArgumentWithoutMacro) {
  TestDef def(kInstructionSet);
  def.instructions[1].arg_macro_name = {};
  InstructionError error = ExpectError(def, InstructionError::Def::kInstruction,
                                       "ADD.W", def.GetCompiled());
  EXPECT_THAT(error.message, HasSubstr("must have a $m exactly when"));
}

TEST(InstructionSetSyntaxTest, UnknownMacro) {
  TestDef def(kInstructionSet);
  def.instructions[1].arg_macro_name = "GetWords";
  InstructionError error = ExpectError(def, InstructionError::Def::kInstruction,
                                       "ADD.W", def.GetCompiled());
  EXPECT_THAT(error.message, HasSubstr("Unknown macro: GetWords"));
}

TEST(InstructionSetSyntaxTest, RelativeWithoutWholeValue) {
  TestDef def(kInstructionSet);
  def.instructions[7].relative = true;
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "PEEK");
  EXPECT_THAT(error.message,
              HasSubstr("has no form where $v is a whole argument"));
}

TEST(InstructionSetSyntaxTest, ExcludedRegistersOfArgumentThatIsNoRegister) {
  TestDef def(kInstructionSet);
  def.instructions[3].arg1_excluded = 0x0001;
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "SETC");
  EXPECT_THAT(error.message, HasSubstr("The first argument excludes registers, "
                                       "but isn't a register"));
}

TEST(InstructionSetSyntaxTest, ExcludedRegistersOfMacroArgument) {
  TestDef def(kInstructionSet);
  def.instructions[1].arg2_excluded = 0x0001;
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "ADD.W");
  EXPECT_THAT(error.message, HasSubstr("The second argument excludes "
                                       "registers, but isn't a register"));
}

TEST(InstructionSetSyntaxTest, ExcludedRegistersPastArgumentBits) {
  TestDef def(kInstructionSet);
  def.instructions[6].arg1_excluded = 0x0100;
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "MVQ");
  EXPECT_THAT(error.message,
              HasSubstr("The first argument excludes a register its 3 bits "
                        "can't hold"));
}

TEST(InstructionSetSyntaxTest, ExcludedRegistersAreEveryRegister) {
  TestDef def(kInstructionSet);
  def.instructions[6].arg1_excluded = 0x00FF;
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "MVQ");
  EXPECT_THAT(error.message,
              HasSubstr("The first argument excludes every register"));
}

TEST(InstructionSetSyntaxTest, DistinctArgumentsThatAreNotTwoRegisters) {
  TestDef def(kInstructionSet);
  def.instructions[1].distinct_args = true;
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "ADD.W");
  EXPECT_THAT(error.message,
              HasSubstr("Distinct arguments must be two registers of the same "
                        "type"));
}

TEST(InstructionSetSyntaxTest, DistinctArgumentsOfDifferentTypes) {
  TestDef def(kInstructionSet);
  def.instructions[6].source = "$r, $R";
  def.instructions[6].arg2 = {ArgType::kDwordReg, 2};
  def.instructions[6].distinct_args = true;
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "MVQ");
  EXPECT_THAT(error.message,
              HasSubstr("Distinct arguments must be two registers of the same "
                        "type"));
}

TEST(InstructionSetSyntaxTest, SizeDisagreesWithMicrocode) {
  TestDef def(kInstructionSet);
  def.codes[0][3].source = "D($v), $v";
  InstructionError error =
      ExpectError(def, InstructionError::Def::kInstruction, "ADD.W");
  EXPECT_THAT(error.message,
              AllOf(HasSubstr("with macro code \"D($v), $v\""),
                    HasSubstr("has size 3, but the microcode has size 2")));
}

TEST(InstructionSetSyntaxTest, SizeDisagreesWithCompiledInstructionSet) {
  TestDef def(kInstructionSet);
  def.instructions[5].source = "$R, $V, $v";
  InstructionError error = ExpectError(def, InstructionError::Def::kInstruction,
                                       "MOV.D", def.GetCompiled());
  EXPECT_THAT(error.message,
              HasSubstr("has size 4, but the microcode has size 3"));
}

TEST(InstructionSetSyntaxTest, InstructionValueFollowedByOperator) {
  TestDef def(kInstructionSet);
  def.instructions[6].source = "$r, $#5 + 1";
  InstructionError error = ExpectError(def, InstructionError::Def::kInstruction,
                                       "MVQ", def.GetCompiled());
  EXPECT_THAT(error.message, HasSubstr("$#5 is followed by '+'"));
}

TEST(InstructionSetSyntaxTest, MacroCodeValueFollowedByOperator) {
  TestDef def(kInstructionSet);
  def.codes[0][2].source = "$v + 1";
  InstructionError error = ExpectError(def, InstructionError::Def::kMacro,
                                       "GetWord", def.GetCompiled());
  EXPECT_EQ(error.macro_source_name, "$v + 1");
  EXPECT_THAT(error.message, HasSubstr("$v is followed by '+'"));
}

TEST(InstructionSetSyntaxTest, MacroCodeValueFollowedByInstructionOperator) {
  // Each code is fine on its own, but the form "[$v + 1]" isn't.
  TestDef def(kInstructionSet);
  def.instructions[7].source = "[$m + 1]";
  InstructionError error = ExpectError(def, InstructionError::Def::kInstruction,
                                       "PEEK", def.GetCompiled());
  EXPECT_THAT(error.message, AllOf(HasSubstr("with macro code \"$v\""),
                                   HasSubstr("$v is followed by '+'")));
}

TEST(InstructionSetSyntaxTest, InstructionValueFollowedByMacroCodeOperator) {
  // The pattern and code are fine on their own, but the form "[1 +$v]" isn't.
  TestDef def(kInstructionSet);
  def.instructions[7].source = "[1 $m]";
  def.codes[0][3].source = "+$v";
  InstructionError error = ExpectError(def, InstructionError::Def::kInstruction,
                                       "PEEK", def.GetCompiled());
  EXPECT_THAT(error.message, AllOf(HasSubstr("with macro code \"+$v\""),
                                   HasSubstr("1 is followed by '+'")));
}

//==============================================================================
// Encoding and disassembly
//==============================================================================

// Every kind of argument. GetWord's codes are laid out as:
//   0 r r r  "$r"
//   1 r r r  "($r + $v)"
constexpr std::string_view kEncodingSet = R"---(
  values(bits:2) Flags { Z = 1, ZERO = 1, C = 2 }

  macro GetWord:r {
    code:m "$r" { UL; }
    code:C2 "($r + $v)" { LD(C2); UL; ADD(C2,m); LKR(m); ADR(C2); LD(C2); UL; }
  }

  instruction(opcode:1) ADD.W "$r, $m" { $GetWord; ADD(a,r); MSR(ZSCO,ZSCO); }
  instruction(opcode:2) MOV "$r4, $r4" { UL; MOV(a,b); }
  instruction(opcode:3) MOV.D "$R, $V" { LD(a0); LD(a1); UL; }
  instruction(opcode:4) CLRF "$Flags" { UL; }
  instruction(opcode:5) MVQ "$r, $#5" { UL; MOV(a,C1); }
  instruction(opcode:6, relative) JPR "$v" { LD(C0); UL; ADD(IP,C0); }
  instruction(opcode:7) PUSH "$m" {
    $GetWord; LK(STACK); ADR(SP); STP(r); LAD(SP); UL;
  }
  instruction(opcode:8) SWP.D "$R, $R" { UL; }
  instruction(opcode:9) POKE "$m, $r" { $GetWord; MOV(b,r); }
)---";

class EncodingTest : public ::testing::Test {
 protected:
  EncodingTest() : def_(kEncodingSet) {
    syntax_ = InstructionSetSyntax::Create(def_.GetDef());
    CHECK(syntax_ != nullptr);
  }

  // Returns a form by its full name, and its index among the name's forms.
  const InstructionForm& GetForm(std::string_view name, int index = 0) {
    auto forms = syntax_->GetForms(name);
    CHECK(index < static_cast<int>(forms.size())) << name;
    return *forms[index];
  }

  // Encodes `values` with a form at `address`, which must succeed.
  std::vector<uint16_t> Encode(const InstructionForm& form,
                               std::vector<int64_t> values,
                               uint16_t address = 0) {
    std::vector<uint16_t> words;
    std::string error;
    EXPECT_TRUE(form.Encode(values, address, words, &error)) << error;
    return words;
  }

  // Encodes `values` with a form, which must fail, and returns the error.
  std::string EncodeError(const InstructionForm& form,
                          std::vector<int64_t> values) {
    std::vector<uint16_t> words = {0x1234};
    std::string error;
    EXPECT_FALSE(form.Encode(values, 0, words, &error));
    EXPECT_THAT(words, ElementsAre(0x1234)) << "Encode appended to words";
    return error;
  }

  TestDef def_;
  std::unique_ptr<InstructionSetSyntax> syntax_;
};

TEST_F(EncodingTest, EncodeWordRegisters) {
  EXPECT_THAT(Encode(GetForm("ADD.W"), {3, 5}), ElementsAre(0x012B));
  EXPECT_THAT(Encode(GetForm("MOV"), {15, 9}), ElementsAre(0x029F));
}

TEST_F(EncodingTest, EncodeDwordRegisterAndDword) {
  EXPECT_THAT(Encode(GetForm("MOV.D"), {2, 0x12345678}),
              ElementsAre(0x0302, 0x5678, 0x1234));
}

TEST_F(EncodingTest, EncodeCodeValues) {
  EXPECT_THAT(Encode(GetForm("CLRF"), {3}), ElementsAre(0x0403));
  EXPECT_THAT(Encode(GetForm("MVQ"), {7, 31}), ElementsAre(0x05FF));
}

TEST_F(EncodingTest, EncodeMacroCodeAndWord) {
  EXPECT_THAT(Encode(GetForm("ADD.W", 1), {1, 2, 0x1234}),
              ElementsAre(0x0151, 0x1234));
  EXPECT_THAT(Encode(GetForm("PUSH", 1), {5, 0x0042}),
              ElementsAre(0x070D, 0x0042));
}

TEST_F(EncodingTest, EncodeValueLimits) {
  const InstructionForm& add = GetForm("ADD.W", 1);
  EXPECT_THAT(Encode(add, {0, 0, -0x8000}), ElementsAre(0x0140, 0x8000));
  EXPECT_THAT(Encode(add, {0, 0, 0xFFFF}), ElementsAre(0x0140, 0xFFFF));
  const InstructionForm& mov = GetForm("MOV.D");
  EXPECT_THAT(Encode(mov, {0, -0x80000000LL}),
              ElementsAre(0x0300, 0x0000, 0x8000));
  EXPECT_THAT(Encode(mov, {0, 0xFFFFFFFFLL}),
              ElementsAre(0x0300, 0xFFFF, 0xFFFF));
}

TEST_F(EncodingTest, EncodeAppendsToWords) {
  std::vector<uint16_t> words = {0x1234};
  EXPECT_TRUE(GetForm("ADD.W", 1).Encode({1, 2, 5}, 0, words));
  EXPECT_THAT(words, ElementsAre(0x1234, 0x0151, 0x0005));
}

TEST_F(EncodingTest, EncodeRelative) {
  const InstructionForm& jpr = GetForm("JPR");
  EXPECT_THAT(Encode(jpr, {0x0120}, 0x0100), ElementsAre(0x0600, 0x001E));
  EXPECT_THAT(Encode(jpr, {0x0100}, 0x0100), ElementsAre(0x0600, 0xFFFE));
  EXPECT_THAT(Encode(jpr, {0x0010}, 0xFFFF), ElementsAre(0x0600, 0x000F));
  EXPECT_THAT(Encode(jpr, {0xFFF0}, 0x0000), ElementsAre(0x0600, 0xFFEE));
}

TEST_F(EncodingTest, EncodeValuesThatDontFit) {
  EXPECT_THAT(EncodeError(GetForm("ADD.W"), {8, 0}),
              HasSubstr("Register 8 is not R0 to R7"));
  EXPECT_THAT(EncodeError(GetForm("ADD.W"), {0, -1}),
              HasSubstr("Register -1 is not R0 to R7"));
  EXPECT_THAT(EncodeError(GetForm("MOV"), {16, 0}),
              HasSubstr("Register 16 is not R0 to SP"));
  EXPECT_THAT(EncodeError(GetForm("MOV.D"), {4, 0}),
              HasSubstr("Register 4 is not D0 to D3"));
  EXPECT_THAT(EncodeError(GetForm("CLRF"), {4}),
              HasSubstr("Value 4 does not fit in 2 bits"));
  EXPECT_THAT(EncodeError(GetForm("MVQ"), {0, 32}),
              HasSubstr("Value 32 does not fit in 5 bits"));
  EXPECT_THAT(EncodeError(GetForm("MVQ"), {0, -1}),
              HasSubstr("Value -1 does not fit in 5 bits"));
  EXPECT_THAT(EncodeError(GetForm("ADD.W", 1), {0, 0, 0x10000}),
              HasSubstr("Value 65536 does not fit in a word"));
  EXPECT_THAT(EncodeError(GetForm("ADD.W", 1), {0, 0, -0x8001}),
              HasSubstr("Value -32769 does not fit in a word"));
  EXPECT_THAT(EncodeError(GetForm("JPR"), {0x10000}),
              HasSubstr("Value 65536 does not fit in a word"));
  EXPECT_THAT(EncodeError(GetForm("MOV.D"), {0, 0x100000000LL}),
              HasSubstr("Value 4294967296 does not fit in a dword"));
  EXPECT_THAT(EncodeError(GetForm("MOV.D"), {0, -0x80000001LL}),
              HasSubstr("Value -2147483649 does not fit in a dword"));
  EXPECT_THAT(EncodeError(GetForm("MOV.D"), {0}),
              HasSubstr("Expected 2 values, but got 1"));
}

TEST_F(EncodingTest, RegisterRulesAreOnInstructionArguments) {
  def_.instructions[0].arg1_excluded = 0x0080;  // ADD.W: not R7
  def_.instructions[1].arg1_excluded = 0x8000;  // MOV: not SP, or R7
  def_.instructions[1].arg2_excluded = 0x0080;
  def_.instructions[1].distinct_args = true;
  syntax_ = InstructionSetSyntax::Create(def_.GetDef());
  ASSERT_NE(syntax_, nullptr);

  auto mov_args = GetForm("MOV").GetArgs();
  ASSERT_EQ(static_cast<int>(mov_args.size()), 2);
  EXPECT_EQ(mov_args[0].excluded, 0x8000);
  EXPECT_TRUE(mov_args[0].distinct);
  EXPECT_EQ(mov_args[1].excluded, 0x0080);
  EXPECT_TRUE(mov_args[1].distinct);

  // A macro code's register has no rules.
  auto add_args = GetForm("ADD.W", 1).GetArgs();
  ASSERT_EQ(static_cast<int>(add_args.size()), 3);
  EXPECT_EQ(add_args[0].excluded, 0x0080);
  EXPECT_FALSE(add_args[0].distinct);
  EXPECT_EQ(add_args[1].excluded, 0);
  EXPECT_EQ(add_args[2].excluded, 0);
}

TEST_F(EncodingTest, RegisterRulesAfterMacroArgument) {
  def_.instructions[8].arg2_excluded = 0x0080;  // POKE: not R7
  syntax_ = InstructionSetSyntax::Create(def_.GetDef());
  ASSERT_NE(syntax_, nullptr);

  // The macro code's register (and value) come before the instruction's own.
  auto args = GetForm("POKE", 1).GetArgs();
  ASSERT_EQ(static_cast<int>(args.size()), 3);
  EXPECT_EQ(args[0].excluded, 0);
  EXPECT_EQ(args[1].excluded, 0);
  EXPECT_EQ(args[2].excluded, 0x0080);
  EXPECT_THAT(EncodeError(GetForm("POKE", 1), {1, 0, 7}),
              HasSubstr("POKE can't use R7 for this argument"));
  EXPECT_THAT(Encode(GetForm("POKE", 1), {7, 0, 1}),
              ElementsAre(0x091F, 0x0000));
}

TEST_F(EncodingTest, EncodeExcludedRegisters) {
  def_.instructions[0].arg1_excluded = 0x0080;  // ADD.W: not R7
  def_.instructions[1].arg1_excluded = 0x8000;  // MOV: not SP, or R7
  def_.instructions[1].arg2_excluded = 0x0080;
  def_.instructions[2].arg1_excluded = 0x0008;  // MOV.D: not D3
  syntax_ = InstructionSetSyntax::Create(def_.GetDef());
  ASSERT_NE(syntax_, nullptr);

  EXPECT_THAT(EncodeError(GetForm("ADD.W"), {7, 1}),
              HasSubstr("ADD.W can't use R7 for this argument"));
  EXPECT_THAT(EncodeError(GetForm("MOV"), {15, 0}),
              HasSubstr("MOV can't use SP for this argument"));
  EXPECT_THAT(EncodeError(GetForm("MOV"), {0, 7}),
              HasSubstr("MOV can't use R7 for this argument"));
  EXPECT_THAT(EncodeError(GetForm("MOV.D"), {3, 0}),
              HasSubstr("MOV.D can't use D3 for this argument"));

  // Every other register, including a macro code's R7.
  EXPECT_THAT(Encode(GetForm("ADD.W"), {6, 7}), ElementsAre(0x013E));
  EXPECT_THAT(Encode(GetForm("ADD.W", 1), {1, 7, 5}),
              ElementsAre(0x0179, 0x0005));
  EXPECT_THAT(Encode(GetForm("MOV"), {7, 15}), ElementsAre(0x02F7));
  EXPECT_THAT(Encode(GetForm("MOV.D"), {2, 0}),
              ElementsAre(0x0302, 0x0000, 0x0000));
}

TEST_F(EncodingTest, EncodeDistinctRegisters) {
  def_.instructions[1].distinct_args = true;  // MOV
  def_.instructions[7].distinct_args = true;  // SWP.D
  syntax_ = InstructionSetSyntax::Create(def_.GetDef());
  ASSERT_NE(syntax_, nullptr);

  EXPECT_THAT(EncodeError(GetForm("MOV"), {3, 3}),
              HasSubstr("MOV can't use R3 for both registers"));
  EXPECT_THAT(EncodeError(GetForm("SWP.D"), {2, 2}),
              HasSubstr("SWP.D can't use D2 for both registers"));
  EXPECT_THAT(Encode(GetForm("MOV"), {3, 4}), ElementsAre(0x0243));
  EXPECT_THAT(Encode(GetForm("SWP.D"), {2, 1}), ElementsAre(0x0806));
}

TEST_F(EncodingTest, ValidateCodeWordValues) {
  def_.instructions[1].arg2_excluded = 0x0080;  // MOV: not R7
  def_.instructions[1].distinct_args = true;
  syntax_ = InstructionSetSyntax::Create(def_.GetDef());
  ASSERT_NE(syntax_, nullptr);
  const InstructionForm& mov = GetForm("MOV");

  int arg_index = -1;
  std::string error;
  EXPECT_FALSE(mov.ValidateCodeWordValues({{0, 7}}, &arg_index, &error));
  EXPECT_EQ(arg_index, 1);
  EXPECT_EQ(error, "MOV can't use R7 for this argument");
  EXPECT_FALSE(mov.ValidateCodeWordValues({{5, 5}}, &arg_index, &error));
  EXPECT_EQ(arg_index, 1);
  EXPECT_EQ(error, "MOV can't use R5 for both registers");
  EXPECT_FALSE(mov.ValidateCodeWordValues({{16, 0}}, &arg_index, &error));
  EXPECT_EQ(arg_index, 0);
  EXPECT_EQ(error, "Register 16 is not R0 to SP");
  EXPECT_FALSE(mov.ValidateCodeWordValues({{5}}, &arg_index, &error));
  EXPECT_EQ(arg_index, -1);
  EXPECT_EQ(error, "Expected 2 values, but got 1");
  EXPECT_FALSE(mov.ValidateCodeWordValues({{5, 5}}));
  EXPECT_TRUE(mov.ValidateCodeWordValues({{5, 6}}));

  const InstructionForm& mvq = GetForm("MVQ");
  EXPECT_FALSE(mvq.ValidateCodeWordValues({{0, 32}}, &arg_index, &error));
  EXPECT_EQ(arg_index, 1);
  EXPECT_EQ(error, "Value 32 does not fit in 5 bits");
}

TEST_F(EncodingTest, ValidateCodeWordValuesIgnoresValues) {
  EXPECT_TRUE(GetForm("ADD.W", 1).ValidateCodeWordValues({{1, 2, 0x10000}}));
  EXPECT_TRUE(GetForm("MOV.D").ValidateCodeWordValues({{3, -0x80000001LL}}));
}

TEST_F(EncodingTest, DisassembleRegistersThatBreakRules) {
  def_.instructions[1].arg2_excluded = 0x0080;  // MOV: not R7
  def_.instructions[1].distinct_args = true;
  syntax_ = InstructionSetSyntax::Create(def_.GetDef());
  ASSERT_NE(syntax_, nullptr);
  EXPECT_EQ(GetForm("MOV").Disassemble({{0x0233}}, 0),
            "MOV R3, R3 ; MOV can't use R3 for both registers");
  EXPECT_EQ(GetForm("MOV").Disassemble({{0x0270}}, 0),
            "MOV R0, R7 ; MOV can't use R7 for this argument");
  EXPECT_EQ(GetForm("MOV").Disassemble({{0x0234}}, 0), "MOV R4, R3");
}

TEST_F(EncodingTest, DisassembleRegistersThatBreakRulesWithValues) {
  def_.instructions[8].arg2_excluded = 0x0080;  // POKE: not R7
  syntax_ = InstructionSetSyntax::Create(def_.GetDef());
  ASSERT_NE(syntax_, nullptr);
  EXPECT_EQ(GetForm("POKE", 1).Disassemble({{0x097F, 0xFFFF}}, 0),
            "POKE (R7 + 0FFFFh), R7 ; POKE can't use R7 for this argument");
}

TEST_F(EncodingTest, DisassembleRegisters) {
  EXPECT_EQ(GetForm("ADD.W").Disassemble({{0x012B}}, 0), "ADD.W R3, R5");
  EXPECT_EQ(GetForm("MOV").Disassemble({{0x029F}}, 0), "MOV SP, BS");
  EXPECT_EQ(GetForm("MOV.D").Disassemble({{0x0302, 0, 0}}, 0),
            "MOV.D D2, 00000000h");
}

TEST_F(EncodingTest, DisassembleCodeValues) {
  const InstructionForm& clrf = GetForm("CLRF");
  EXPECT_EQ(clrf.Disassemble({{0x0400}}, 0), "CLRF 0");
  EXPECT_EQ(clrf.Disassemble({{0x0401}}, 0), "CLRF Z");
  EXPECT_EQ(clrf.Disassemble({{0x0402}}, 0), "CLRF C");
  EXPECT_EQ(clrf.Disassemble({{0x0403}}, 0), "CLRF 3");
  EXPECT_EQ(GetForm("MVQ").Disassemble({{0x05FF}}, 0), "MVQ R7, 31");
}

TEST_F(EncodingTest, DisassembleValues) {
  const InstructionForm& add = GetForm("ADD.W", 1);
  EXPECT_EQ(add.Disassemble({{0x0151, 0x0005}}, 0), "ADD.W R1, (R2 + 0005h)");
  EXPECT_EQ(add.Disassemble({{0x0151, 0x9ABC}}, 0), "ADD.W R1, (R2 + 9ABCh)");
  EXPECT_EQ(add.Disassemble({{0x0151, 0xFFFF}}, 0), "ADD.W R1, (R2 + 0FFFFh)");
  const InstructionForm& mov = GetForm("MOV.D");
  EXPECT_EQ(mov.Disassemble({{0x0300, 0x5678, 0x1234}}, 0),
            "MOV.D D0, 12345678h");
  EXPECT_EQ(mov.Disassemble({{0x0300, 0x0000, 0xA000}}, 0),
            "MOV.D D0, 0A0000000h");
}

TEST_F(EncodingTest, DisassembleRelative) {
  const InstructionForm& jpr = GetForm("JPR");
  EXPECT_EQ(jpr.Disassemble({{0x0600, 0x001E}}, 0x0100), "JPR 0120h");
  EXPECT_EQ(jpr.Disassemble({{0x0600, 0xFFFE}}, 0x0100), "JPR 0100h");
  EXPECT_EQ(jpr.Disassemble({{0x0600, 0x000F}}, 0xFFFF), "JPR 0010h");
}

TEST(InstructionSetSyntaxTest, DisassembleKeepsPatternSpacing) {
  TestDef def(R"---(
    instruction(opcode:0) MVQ "$r,$#5" { UL; MOV(a,C1); }
  )---");
  auto syntax = InstructionSetSyntax::Create(def.GetDef());
  ASSERT_NE(syntax, nullptr);
  EXPECT_EQ(syntax->GetForms("MVQ")[0]->Disassemble({{0x000A}}, 0), "MVQ R2,1");
}

TEST_F(EncodingTest, DisassembleEncodedForms) {
  struct Case {
    std::string_view name;
    int index;
    std::vector<int64_t> values;
    std::string_view text;
  };
  const Case cases[] = {
      {"ADD.W", 0, {7, 6}, "ADD.W R7, R6"},
      {"ADD.W", 1, {4, 3, 0xBEEF}, "ADD.W R4, (R3 + 0BEEFh)"},
      {"MOV", 0, {13, 0}, "MOV IP, R0"},
      {"MOV.D", 0, {3, 0x00C0FFEE}, "MOV.D D3, 00C0FFEEh"},
      {"CLRF", 0, {2}, "CLRF C"},
      {"MVQ", 0, {1, 17}, "MVQ R1, 17"},
      {"JPR", 0, {0x2000}, "JPR 2000h"},
      {"PUSH", 0, {2}, "PUSH R2"},
      {"PUSH", 1, {5, 0x0042}, "PUSH (R5 + 0042h)"},
  };
  for (const Case& test_case : cases) {
    const InstructionForm& form = GetForm(test_case.name, test_case.index);
    std::vector<uint16_t> words = Encode(form, test_case.values, 0x1234);
    ASSERT_EQ(static_cast<int>(words.size()), form.GetSize()) << test_case.text;
    EXPECT_EQ(syntax_->FindForm(words[0]), &form) << test_case.text;
    EXPECT_EQ(form.Disassemble(words, 0x1234), test_case.text);
  }
}

}  // namespace
}  // namespace oz3
