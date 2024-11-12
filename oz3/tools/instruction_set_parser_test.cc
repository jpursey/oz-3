// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_set_parser.h"

#include <algorithm>
#include <cstdint>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

TEST(InstructionSetParserTest, EmptyTextParses) {
  EXPECT_TRUE(ParseInstructionSet(""));
}

TEST(InstructionSetParserTest, InvalidTopLevel) {
  EXPECT_FALSE(ParseInstructionSet("INVALID"));
}

TEST(InstructionSetParserTest, MinimalMacro) {
  EXPECT_TRUE(ParseInstructionSet("MACRO Name {}"));
  EXPECT_TRUE(
      ParseInstructionSet("MACRO Name {}\n"
                          "MACRO Other {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO Name }"));
  EXPECT_FALSE(ParseInstructionSet("MACRO Name {"));
}

TEST(InstructionSetParserTest, MacroConfig) {
  EXPECT_TRUE(ParseInstructionSet("MACRO(bits:5) Name {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO() Name {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO(bits = 5) Name {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO(bits:five) Name {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO(bits:5 Name {}"));
}

TEST(InstructionSetParserTest, MacroArgument) {
  EXPECT_TRUE(ParseInstructionSet("MACRO Name(p) {}"));
  EXPECT_TRUE(ParseInstructionSet("MACRO Name(P) {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO Name(a) {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO Name() {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO Name(p {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO Name(p,P) {}"));
}

TEST(InstructionSetParserTest, MacroReturn) {
  EXPECT_TRUE(ParseInstructionSet("MACRO Name:r {}"));
  EXPECT_TRUE(ParseInstructionSet("MACRO Name:R {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO Name:a {}"));
  EXPECT_FALSE(ParseInstructionSet("MACRO Name: {}"));
}

TEST(InstructionSetParserTest, MacroMinimalCode) {
  EXPECT_TRUE(
      ParseInstructionSet("MACRO Name {\n"
                          "CODE \"R0\" {}\n"
                          "}"));
  EXPECT_TRUE(
      ParseInstructionSet("MACRO Name {\n"
                          "CODE \"R0\" {}\n"
                          "CODE \"R1\" {}\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("MACRO Name {\n"
                          "INVALID \"R0\" {}\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("MACRO Name {\n"
                          "CODE {}\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("MACRO Name {\n"
                          "CODE \"R0\"\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("MACRO Name {\n"
                          "CODE \"R0\" {"));
}

TEST(InstructionSetParserTest, MacroCodeReturnRegister) {
  EXPECT_TRUE(
      ParseInstructionSet("MACRO Name {\n"
                          "CODE :R0 \"R0\" {}\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("MACRO Name {\n"
                          "CODE : \"R0\" {}\n"
                          "}"));
}

TEST(InstructionSetParserTest, MinimalInstruction) {
  EXPECT_TRUE(ParseInstructionSet("INSTRUCTION NAME {}"));
  EXPECT_TRUE(
      ParseInstructionSet("INSTRUCTION NAME {}\n"
                          "INSTRUCTION OTHER {}"));
  EXPECT_FALSE(ParseInstructionSet("INSTRUCTION {}"));
  EXPECT_FALSE(ParseInstructionSet("INSTRUCTION NAME }"));
  EXPECT_FALSE(ParseInstructionSet("INSTRUCTION NAME {"));
}

TEST(InstructionSetParserTest, InstructionConfig) {
  EXPECT_TRUE(ParseInstructionSet("INSTRUCTION(opcode:5) NAME {}"));
  EXPECT_FALSE(ParseInstructionSet("INSTRUCTION(config:5) NAME {}"));
  EXPECT_FALSE(ParseInstructionSet("INSTRUCTION() NAME {}"));
  EXPECT_FALSE(ParseInstructionSet("INSTRUCTION(opcode = 5) NAME {}"));
  EXPECT_FALSE(ParseInstructionSet("INSTRUCTION(opcode:five) NAME {}"));
  EXPECT_FALSE(ParseInstructionSet("INSTRUCTION(opcode:5 NAME {}"));
}

TEST(InstructionSetParserTest, InstructionVariant) {
  EXPECT_TRUE(ParseInstructionSet("INSTRUCTION NAME.X {}"));
  EXPECT_FALSE(ParseInstructionSet("INSTRUCTION NAME. {}"));
  EXPECT_FALSE(ParseInstructionSet("INSTRUCTION NAME.5 {}"));
}

TEST(InstructionSetParserTest, InstructionParameters) {
  EXPECT_TRUE(ParseInstructionSet("INSTRUCTION NAME \"r\" {}"));
}

TEST(InstructionSetParserTest, InstructionCode) {
  EXPECT_TRUE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "LD(C0);\n"
                          "UL;\n"
                          "@label:ADDI(a,1);\n"
                          "JC(NZ,@label);\n"
                          "$Macro(C1);\n"
                          "$Other;"
                          "}"));
  EXPECT_TRUE(
      ParseInstructionSet("MACRO NAME { CODE \"R0\" {\n"
                          "LD(C0);\n"
                          "UL;\n"
                          "@label:ADDI(a,1);\n"
                          "JC(NZ,@label);\n"
                          "$Macro(C1);\n"
                          "$Other;"
                          "}}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "LD(C0)\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("MACRO NAME { CODE \"R0\" {\n"
                          "LD(C0)\n"
                          "}}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "LD(C0;\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "LD C0);\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "LD C0;\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "(C0);\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "@5:UL;\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "label:UL;\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "@:UL;\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "@label UL;\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "JC(a,b,c);\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "JC(@)\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "JC(@label,);\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "$5;\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "$Macro(C0,C1);\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "$Macro(C0)\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "$Macro(C0;\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "$Macro();\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "$Macro C0);\n"
                          "}"));
  EXPECT_FALSE(
      ParseInstructionSet("INSTRUCTION NAME {\n"
                          "$Macro C0;\n"
                          "}"));
}

//==============================================================================
//==============================================================================
//==============================================================================

struct Option {
  int value_bits;  // Number of bits needed for option value: 0 to 8

  auto operator<=>(const Option&) const = default;

  template <typename Sink>
  friend void AbslStringify(Sink& sink, const Option& option) {
    absl::Format(&sink, "{bits=%d}", option.value_bits);
  }
};

struct Prefix {
  int prefix_bits;   // Number of bits needed for the prefix value: 0 to 7
  int prefix_value;  // Value of the prefix in range [0, 1 << prefix_bits)

  auto operator<=>(const Prefix&) const = default;

  template <typename Sink>
  friend void AbslStringify(Sink& sink, const Prefix& prefix) {
    absl::Format(&sink, "{bits=%d,val=%d}", prefix.prefix_bits,
                 prefix.prefix_value);
  }
};
std::vector<Prefix> CalculateOptionPrefixesV1(std::vector<Option>& options) {
  const int MAX_TOTAL_BITS = 8;
  int num_options = options.size();

  // Compute sum of codeword sizes and find max value_bits
  int64_t sum_codeword_sizes = 0;
  int max_value_bits = 0;
  for (const auto& opt : options) {
    sum_codeword_sizes += (1LL << opt.value_bits);
    if (opt.value_bits > max_value_bits) {
      max_value_bits = opt.value_bits;
    }
  }

  // Compute minimal N
  int N_min = max_value_bits;
  while ((1LL << N_min) < sum_codeword_sizes) {
    N_min++;
  }
  if (N_min > MAX_TOTAL_BITS) {
    return {};  // Cannot encode within 8 bits
  }

  // Prepare prefixes vector outside the loop
  std::vector<Prefix> prefixes(num_options);

  // Try N from N_min to MAX_TOTAL_BITS
  for (int N = N_min; N <= MAX_TOTAL_BITS; ++N) {
    int next_codeword = 0;
    bool assignment_failed = false;

    // Assign prefix_bits and prefix_values
    for (int i = 0; i < num_options; ++i) {
      const auto& opt = options[i];
      int prefix_bits = N - opt.value_bits;

      prefixes[i].prefix_bits = prefix_bits;

      int codeword_size = 1 << opt.value_bits;

      // Check if codeword space is sufficient
      if (next_codeword + codeword_size > (1 << N)) {
        assignment_failed = true;
        break;
      }

      int prefix_value = next_codeword >> opt.value_bits;
      prefixes[i].prefix_value = prefix_value;

      next_codeword += codeword_size;
    }

    if (!assignment_failed) {
      return prefixes;
    }
  }

  // If no N works, return empty vector
  return {};
}

std::vector<Prefix> CalculateOptionPrefixes(std::vector<Option>& options) {
  std::vector<Prefix> prefixes(options.size());

  // Calculate sum_values = sum over i of 2^{value_bits[i]}
  int sum_values = 0;
  int max_value_bits = 0;
  for (const auto& opt : options) {
    if (opt.value_bits > max_value_bits) {
      max_value_bits = opt.value_bits;
    }
    if (max_value_bits > 8) {
      return {};
    }
    sum_values += 1 << opt.value_bits;
    if (sum_values > 256) {
      return {};
    }
  }

  // Find minimal total_bits satisfying sum_values <= 2^{total_bits}
  int total_bits = max_value_bits;
  while ((1 << total_bits) < sum_values) {
    ++total_bits;
  }

  int next_code = 0;
  for (int i = 0; i < options.size(); ++i) {
    const int value_bits = options[i].value_bits;
    int prefix_bits = total_bits - value_bits;
    int prefix_value = next_code >> value_bits;
    prefixes[i].prefix_bits = prefix_bits;
    prefixes[i].prefix_value = static_cast<int>(prefix_value);
    next_code += 1 << value_bits;
  }

  return prefixes;
}

//==============================================================================
//==============================================================================
//==============================================================================

using ::testing::ElementsAre;

TEST(InstructionSetParserTest, CalculateOptionPrefixes) {
  std::vector<Option> options;
  options = {{0}};
  EXPECT_THAT(CalculateOptionPrefixes(options), ElementsAre(Prefix{0, 0}));
  options = {{5}};
  EXPECT_THAT(CalculateOptionPrefixes(options), ElementsAre(Prefix{0, 0}));
  options = {{0}, {0}, {0}, {0}, {0}};
  EXPECT_THAT(CalculateOptionPrefixes(options),
              ElementsAre(Prefix{3, 0}, Prefix{3, 1}, Prefix{3, 2},
                          Prefix{3, 3}, Prefix{3, 4}));
  options = {{2}, {2}, {2}};
  EXPECT_THAT(CalculateOptionPrefixes(options),
              ElementsAre(Prefix{2, 0}, Prefix{2, 1}, Prefix{2, 2}));
  options = {{3}, {2}, {1}};
  EXPECT_THAT(CalculateOptionPrefixes(options),
              ElementsAre(Prefix{1, 0}, Prefix{2, 2}, Prefix{3, 6}));
  options = {{3}, {3}, {1}, {0}, {0}, {0}};
  EXPECT_THAT(CalculateOptionPrefixes(options),
              ElementsAre(Prefix{2, 0}, Prefix{2, 1}, Prefix{4, 8},
                          Prefix{5, 18}, Prefix{5, 19}, Prefix{5, 20}));
  options = {{3}, {2}, {2}};
  EXPECT_THAT(CalculateOptionPrefixes(options),
              ElementsAre(Prefix{1, 0}, Prefix{2, 2}, Prefix{2, 3}));
  options = {{3}, {2}, {2}};
  EXPECT_THAT(CalculateOptionPrefixes(options),
              ElementsAre(Prefix{1, 0}, Prefix{2, 2}, Prefix{2, 3}));
  options = {{3}, {2}, {2}, {0}};
  EXPECT_THAT(
      CalculateOptionPrefixes(options),
      ElementsAre(Prefix{2, 0}, Prefix{3, 2}, Prefix{3, 3}, Prefix{5, 16}));
}

}  // namespace
}  // namespace oz3
