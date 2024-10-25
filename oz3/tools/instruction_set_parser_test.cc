// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/instruction_set_parser.h"

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

TEST(InstructionSetParserTest, MacroCodeBitPrefix) {
  EXPECT_TRUE(
      ParseInstructionSet("MACRO Name {\n"
                          "CODE 3 \"R0\" {}\n"
                          "}"));
  EXPECT_TRUE(
      ParseInstructionSet("MACRO Name {\n"
                          "CODE b10 \"R0\" {}\n"
                          "}"));
  EXPECT_TRUE(
      ParseInstructionSet("MACRO Name {\n"
                          "CODE xA \"R0\" {}\n"
                          "}"));
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

}  // namespace
}  // namespace oz3
