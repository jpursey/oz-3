// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/source_pattern.h"

#include <cstdint>
#include <string>
#include <string_view>

#include "absl/status/statusor.h"
#include "gb/parse/symbol.h"
#include "gb/parse/token.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/instruction_def.h"

namespace oz3 {
namespace {

using ::testing::HasSubstr;
using ::testing::IsEmpty;
using Kind = SourcePattern::Part::Kind;

constexpr NamedValueDef kConditionValues[] = {{.name = "Z", .value = 4}};
constexpr NamedValueDef kFlagsValues[] = {{.name = "C", .value = 4}};
constexpr ValueSetDef kValueSets[] = {
    {.name = "Condition", .size = 3, .values = kConditionValues},
    {.name = "Flags", .size = 4, .values = kFlagsValues},
};

void ExpectArgument(const SourcePattern::Part& part, ArgType type, int size,
                    bool whole_arg) {
  EXPECT_EQ(part.kind, Kind::kArgument);
  EXPECT_EQ(part.arg.type, type);
  EXPECT_EQ(part.arg.size, size);
  EXPECT_EQ(part.value_set, nullptr);
  EXPECT_EQ(part.whole_arg, whole_arg);
}

void ExpectSymbol(const SourcePattern::Part& part, char symbol) {
  EXPECT_EQ(part.kind, Kind::kLiteral);
  EXPECT_EQ(part.literal_type, gb::kTokenSymbol);
  EXPECT_EQ(part.literal, gb::TokenValue(gb::Symbol(symbol)));
}

void ExpectIdent(const SourcePattern::Part& part, std::string_view ident) {
  EXPECT_EQ(part.kind, Kind::kLiteral);
  EXPECT_EQ(part.literal_type, gb::kTokenIdentifier);
  EXPECT_EQ(part.literal, gb::TokenValue(std::string(ident)));
}

void ExpectError(std::string_view source, std::string_view message) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction(source, kValueSets);
  ASSERT_FALSE(pattern.ok()) << source;
  EXPECT_THAT(pattern.status().message(), HasSubstr(message)) << source;
}

TEST(SourcePatternTest, Empty) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction("", {});
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  EXPECT_THAT(pattern->GetParts(), IsEmpty());
  EXPECT_EQ(pattern->GetValueWords(), 0);
}

TEST(SourcePatternTest, RegistersWithDefaultSizes) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction("$r, $R", {});
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  auto parts = pattern->GetParts();
  ASSERT_EQ(parts.size(), 3);
  ExpectArgument(parts[0], ArgType::kWordReg,
                 GetDefaultArgTypeSize(ArgType::kWordReg), true);
  ExpectSymbol(parts[1], ',');
  ExpectArgument(parts[2], ArgType::kDwordReg,
                 GetDefaultArgTypeSize(ArgType::kDwordReg), true);
  EXPECT_EQ(pattern->GetValueWords(), 0);
}

TEST(SourcePatternTest, ArgumentsWithSizes) {
  struct Case {
    std::string_view source;
    ArgType type;
    int size;
  };
  const Case kCases[] = {
      {"$r1", ArgType::kWordReg, 1},   {"$r4", ArgType::kWordReg, 4},
      {"$R1", ArgType::kDwordReg, 1},  {"$#1", ArgType::kImmediate, 1},
      {"$#8", ArgType::kImmediate, 8}, {"$m", ArgType::kMacro, 0},
      {"$m5", ArgType::kMacro, 5},
  };
  for (const Case& test_case : kCases) {
    absl::StatusOr<SourcePattern> pattern =
        SourcePattern::ParseInstruction(test_case.source, {});
    ASSERT_TRUE(pattern.ok()) << pattern.status();
    auto parts = pattern->GetParts();
    ASSERT_EQ(parts.size(), 1) << test_case.source;
    ExpectArgument(parts[0], test_case.type, test_case.size, true);
  }
}

TEST(SourcePatternTest, ArgsInOrder) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction("$v, $r2, ($m + $v)", {});
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  auto args = pattern->GetArgs();
  ASSERT_EQ(args.size(), 2);
  EXPECT_EQ(args[0].type, ArgType::kWordReg);
  EXPECT_EQ(args[0].size, 2);
  EXPECT_EQ(args[1].type, ArgType::kMacro);
  EXPECT_EQ(args[1].size, 0);

  pattern = SourcePattern::ParseMacroCode("($r + $v), $V", {});
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  args = pattern->GetArgs();
  ASSERT_EQ(args.size(), 1);
  EXPECT_EQ(args[0].type, ArgType::kWordReg);

  pattern = SourcePattern::ParseMacroCode("(SP)", {});
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  EXPECT_THAT(pattern->GetArgs(), IsEmpty());
}

TEST(SourcePatternTest, PartPositions) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction("$r,  ($r4 + $v)", {});
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  auto parts = pattern->GetParts();
  ASSERT_EQ(parts.size(), 7);
  const int kOffsets[] = {0, 2, 5, 6, 10, 12, 14};
  const int kSizes[] = {2, 1, 1, 3, 1, 2, 1};
  for (int i = 0; i < 7; ++i) {
    EXPECT_EQ(parts[i].offset, kOffsets[i]) << i;
    EXPECT_EQ(parts[i].size, kSizes[i]) << i;
  }
}

TEST(SourcePatternTest, InstructionArgumentRules) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction("$r, $r, $r", {});
  ASSERT_FALSE(pattern.ok());
  EXPECT_THAT(pattern.status().message(), HasSubstr("At most two"));
  pattern = SourcePattern::ParseInstruction("$m, $m", {});
  ASSERT_FALSE(pattern.ok());
  EXPECT_THAT(pattern.status().message(), HasSubstr("Only one macro"));
}

TEST(SourcePatternTest, MacroCodeArgumentRules) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseMacroCode("$r, $r", {});
  ASSERT_FALSE(pattern.ok());
  EXPECT_THAT(pattern.status().message(), HasSubstr("Only one non-value"));
  pattern = SourcePattern::ParseMacroCode("($m)", {});
  ASSERT_FALSE(pattern.ok());
  EXPECT_THAT(pattern.status().message(), HasSubstr("$m is not allowed"));
}

TEST(SourcePatternTest, Values) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction("$v, ($r + $v), $V", {});
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  auto parts = pattern->GetParts();
  ASSERT_EQ(parts.size(), 9);
  EXPECT_EQ(parts[0].kind, Kind::kWord);
  EXPECT_TRUE(parts[0].whole_arg);
  ExpectSymbol(parts[1], ',');
  ExpectSymbol(parts[2], '(');
  ExpectArgument(parts[3], ArgType::kWordReg,
                 GetDefaultArgTypeSize(ArgType::kWordReg), false);
  ExpectSymbol(parts[4], '+');
  EXPECT_EQ(parts[5].kind, Kind::kWord);
  EXPECT_FALSE(parts[5].whole_arg);
  ExpectSymbol(parts[6], ')');
  ExpectSymbol(parts[7], ',');
  EXPECT_EQ(parts[8].kind, Kind::kDword);
  EXPECT_TRUE(parts[8].whole_arg);
  EXPECT_EQ(pattern->GetValueWords(), 4);
}

TEST(SourcePatternTest, WholeArgsAreOnlyAPlaceholder) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction("$v + 1, D($v), $v $v, $m", {});
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  int whole_args = 0;
  for (const SourcePattern::Part& part : pattern->GetParts()) {
    if (part.whole_arg) {
      ++whole_args;
      EXPECT_EQ(part.kind, Kind::kArgument);
    }
  }
  EXPECT_EQ(whole_args, 1);
}

TEST(SourcePatternTest, Literals) {
  absl::StatusOr<SourcePattern> pattern = SourcePattern::ParseInstruction(
      "SP, D[x_1] 3 2.5 'a' \"bc\" {-*/%~&|^!<>=:.?}", {});
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  auto parts = pattern->GetParts();
  ASSERT_EQ(parts.size(), 27);
  ExpectIdent(parts[0], "SP");
  ExpectSymbol(parts[1], ',');
  ExpectIdent(parts[2], "D");
  ExpectSymbol(parts[3], '[');
  ExpectIdent(parts[4], "x_1");
  ExpectSymbol(parts[5], ']');
  EXPECT_EQ(parts[6].literal_type, gb::kTokenInt);
  EXPECT_EQ(parts[6].literal, gb::TokenValue(int64_t{3}));
  EXPECT_EQ(parts[7].literal_type, gb::kTokenFloat);
  EXPECT_EQ(parts[7].literal, gb::TokenValue(2.5));
  EXPECT_EQ(parts[8].literal_type, gb::kTokenString);
  EXPECT_EQ(parts[8].literal, gb::TokenValue(std::string("a")));
  EXPECT_EQ(parts[9].literal_type, gb::kTokenString);
  EXPECT_EQ(parts[9].literal, gb::TokenValue(std::string("bc")));
  ExpectSymbol(parts[10], '{');
  int i = 11;
  for (char symbol : std::string_view("-*/%~&|^!<>=:.?")) {
    ExpectSymbol(parts[i++], symbol);
  }
  ExpectSymbol(parts[26], '}');
  for (const SourcePattern::Part& part : parts) {
    EXPECT_EQ(part.kind, Kind::kLiteral);
    EXPECT_FALSE(part.whole_arg);
  }
}

TEST(SourcePatternTest, ValueSets) {
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction("$Condition, ($Flags)", kValueSets);
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  auto parts = pattern->GetParts();
  ASSERT_EQ(parts.size(), 5);
  EXPECT_EQ(parts[0].kind, Kind::kArgument);
  EXPECT_EQ(parts[0].arg.type, ArgType::kImmediate);
  EXPECT_EQ(parts[0].arg.size, 3);
  EXPECT_EQ(parts[0].value_set, &kValueSets[0]);
  EXPECT_TRUE(parts[0].whole_arg);
  EXPECT_EQ(parts[3].kind, Kind::kArgument);
  EXPECT_EQ(parts[3].arg.type, ArgType::kImmediate);
  EXPECT_EQ(parts[3].arg.size, 4);
  EXPECT_EQ(parts[3].value_set, &kValueSets[1]);
  EXPECT_FALSE(parts[3].whole_arg);
}

TEST(SourcePatternTest, PlaceholdersBeforeValueSets) {
  static constexpr NamedValueDef kValues[] = {{.name = "X", .value = 1}};
  static constexpr ValueSetDef kPlaceholderSets[] = {
      {.name = "r", .size = 1, .values = kValues},
      {.name = "v", .size = 1, .values = kValues},
  };
  absl::StatusOr<SourcePattern> pattern =
      SourcePattern::ParseInstruction("$r, $v", kPlaceholderSets);
  ASSERT_TRUE(pattern.ok()) << pattern.status();
  auto parts = pattern->GetParts();
  ASSERT_EQ(parts.size(), 3);
  ExpectArgument(parts[0], ArgType::kWordReg,
                 GetDefaultArgTypeSize(ArgType::kWordReg), true);
  EXPECT_EQ(parts[2].kind, Kind::kWord);
}

TEST(SourcePatternTest, Errors) {
  ExpectError("$x", "$x is not a placeholder or value set");
  ExpectError("$r5", "$r5 is not a placeholder or value set");
  ExpectError("$R3", "$R3 is not a placeholder or value set");
  ExpectError("$m9", "$m9 is not a placeholder or value set");
  ExpectError("$condition", "$condition is not a placeholder or value set");
  ExpectError("$#0", "");
  ExpectError("$#9", "");
  ExpectError("(SP", "");
  ExpectError("SP)", "");
  ExpectError("(SP]", "");
  ExpectError("(SP, $r)", "");
  ExpectError("$r,", "");
  ExpectError(", $r", "");
  ExpectError("$r; $r", "");
  ExpectError("@x", "");
  ExpectError("#5", "");
}

}  // namespace
}  // namespace oz3
