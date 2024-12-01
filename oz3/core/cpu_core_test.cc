// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/cpu_core.h"

#include "gb/base/callback.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/base_core_test.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/memory_bank_config.h"
#include "oz3/core/port.h"
#include "oz3/core/processor_config.h"

namespace oz3 {
namespace {

using ::testing::IsEmpty;

enum MicroTestOp : uint8_t {
  kTestOp_NOP,
  kTestOp_ZSCO,
  kTestOp_SETF,
  kTestOp_CLRF,
  kTestOp_XORF,
  kTestOp_MOVF,
  kTestOp_EI,
  kTestOp_DI,
  kTestOp_WAIT,
  kTestOp_HALT,
  kTestOp_LWORD,
  kTestOp_LDWORD,
  kTestOp_LCODE,
  kTestOp_LSTACK,
  kTestOp_LDATA,
  kTestOp_LEXTRA,
  kTestOp_SCODE,
  kTestOp_SSTACK,
  kTestOp_SDATA,
  kTestOp_SEXTRA,
  kTestOp_PCODE,
  kTestOp_PSTACK,
  kTestOp_PDATA,
  kTestOp_PEXTRA,
  kTestOp_SREG,
  kTestOp_MOV_ST,
  kTestOp_MOVI,
  kTestOp_MOV,
  kTestOp_MVBI,
  kTestOp_MVB,
  kTestOp_MVNI,
  kTestOp_MVN,
  kTestOp_LV,
  kTestOp_MSSC,
  kTestOp_MSX,
  kTestOp_MSI,
  kTestOp_MSR1,
  kTestOp_MSR2,
  kTestOp_ADDI,
  kTestOp_SUBI,
  kTestOp_ADD,
  kTestOp_SUB,
  kTestOp_ADC,
  kTestOp_SBC,
  kTestOp_NEG,
  kTestOp_TST,
  kTestOp_CMPI,
  kTestOp_CMP,
  kTestOp_MSKI,
  kTestOp_MSKI2,
  kTestOp_MSK,
  kTestOp_NOT,
  kTestOp_AND,
  kTestOp_OR,
  kTestOp_XOR,
  kTestOp_SL,
  kTestOp_SR,
  kTestOp_SRA,
  kTestOp_RL,
  kTestOp_RR,
  kTestOp_RLC,
  kTestOp_RRC,
  kTestOp_MATHI,
  kTestOp_JC,
  kTestOp_JCR,
  kTestOp_JD,
  kTestOp_JMP,
  kTestOp_INT,
  kTestOp_ILD,
  kTestOp_IST,
  kTestOp_IRET,
  kTestOp_PLD,
  kTestOp_PST,
  kTestOp_PLDS,
  kTestOp_PSTS,
  kTestOp_CBK,
  kTestOp_CBKS,
  kTestOp_CLD,
  kTestOp_CST,
  kTestOp_CINT,
  kTestOp_CSELF,
  kTestOp_ROREG,
};

const InstructionDef kMicroTestInstructions[] = {
    {.op = kTestOp_NOP, .op_name = "NOP", .code = "UL;"},
    {.op = kTestOp_ZSCO,
     .op_name = "ZSCO",
     .arg1 = {ArgType::kImmediate, 4},
     .code = "UL;"
             "MSM(ZSCO,C0);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_SETF,
     .op_name = "SETF",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MSS(a);"
             "MSR(_,a);"
             "MOV(b,ST);"},
    {.op = kTestOp_CLRF,
     .op_name = "CLRF",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MSC(a);"
             "MSR(a,_);"
             "MOV(b,ST);"},
    {.op = kTestOp_XORF,
     .op_name = "CLRF",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MSX(a);"
             "MSR(a,a);"
             "MOV(b,ST);"},
    {.op = kTestOp_MOVF,
     .op_name = "MOVF",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MSM(a,FP);"
             "MSR(a,a);"
             "MOV(b,ST);"},
    {.op = kTestOp_EI,
     .op_name = "EI",
     .code = "UL;"
             "MSS(I);"
             "MSR(I,I);"},
    {.op = kTestOp_DI,
     .op_name = "DI",
     .code = "UL;"
             "MSC(I);"
             "MSR(I,I);"},
    {.op = kTestOp_WAIT,
     .op_name = "WAIT",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "WAIT(a);"},
    {.op = kTestOp_HALT,
     .op_name = "HALT",
     .code = "UL;"
             "HALT;"},
    {.op = kTestOp_LWORD,
     .op_name = "LCODE",
     .arg1 = ArgType::kWordReg,
     .code = "LD(a);"
             "UL;"},
    {.op = kTestOp_LDWORD,
     .op_name = "LCODE",
     .arg1 = ArgType::kDwordReg,
     .code = "LD(a0);"
             "LD(a1);"
             "UL;"},
    {.op = kTestOp_LCODE,
     .op_name = "LCODE",
     .arg1 = ArgType::kWordReg,
     .arg2 = {ArgType::kImmediate, 5},
     .code = "ADR(C1);"
             "LD(a);"
             "UL;"},
    {.op = kTestOp_LSTACK,
     .op_name = "LSTACK",
     .arg1 = ArgType::kWordReg,
     .arg2 = {ArgType::kImmediate, 5},
     .code = "UL;"
             "LK(STACK);"
             "ADR(C1);"
             "LD(a);"
             "UL;"},
    {.op = kTestOp_LDATA,
     .op_name = "LDATA",
     .arg1 = ArgType::kWordReg,
     .arg2 = {ArgType::kImmediate, 5},
     .code = "UL;"
             "LK(DATA);"
             "ADR(C1);"
             "LD(a);"
             "UL;"},
    {.op = kTestOp_LEXTRA,
     .op_name = "LEXTRA",
     .arg1 = ArgType::kWordReg,
     .arg2 = {ArgType::kImmediate, 5},
     .code = "UL;"
             "LK(EXTRA);"
             "ADR(C1);"
             "LD(a);"
             "UL;"},
    {.op = kTestOp_SCODE,
     .op_name = "SCODE",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "ADR(C0);"
             "ST(b);"
             "UL;"},
    {.op = kTestOp_SSTACK,
     .op_name = "SSTACK",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "LK(STACK);"
             "ADR(C0);"
             "ST(b);"
             "UL;"},
    {.op = kTestOp_SDATA,
     .op_name = "SDATA",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "LK(DATA);"
             "ADR(C0);"
             "ST(b);"
             "UL;"},
    {.op = kTestOp_SEXTRA,
     .op_name = "SEXTRA",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "LK(EXTRA);"
             "ADR(C0);"
             "ST(b);"
             "UL;"},
    {.op = kTestOp_PCODE,
     .op_name = "PCODE",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "ADR(C0);"
             "STP(b);"
             "UL;"},
    {.op = kTestOp_PSTACK,
     .op_name = "PSTACK",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "LK(STACK);"
             "ADR(C0);"
             "STP(b);"
             "UL;"},
    {.op = kTestOp_PDATA,
     .op_name = "PDATA",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "LK(DATA);"
             "ADR(C0);"
             "STP(b);"
             "UL;"},
    {.op = kTestOp_PEXTRA,
     .op_name = "PEXTRA",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "LK(EXTRA);"
             "ADR(C0);"
             "STP(b);"
             "UL;"},
    {.op = kTestOp_SREG,
     .op_name = "SREG",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "LKR(a);"
             "ADR(b);"
             "ST(b);"
             "UL;"
             "ADDI(b,1);"},
    {.op = kTestOp_MOV_ST,
     .op_name = "MOV_ST",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MOV(C1,b);"
             "LK(DATA);"
             "ADR(C0);"
             "ST(C1);"
             "UL;"},
    {.op = kTestOp_MOVI,
     .op_name = "MOVI",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "MOVI(a,42);"},
    {.op = kTestOp_MOV,
     .op_name = "MOV",
     .arg1 = {ArgType::kWordReg, 4},
     .arg2 = {ArgType::kWordReg, 4},
     .code = "UL;"
             "MOV(a,b);"},
    {.op = kTestOp_MVBI,
     .op_name = "MVBI",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "LD(C2);UL;"
             "CMP(C2,C0);JC(Z,@low);"
             "@high:MVBI(a:h,142);MVBI(b:h,142);MVBI(SP:h,142);END;"
             "@low:MVBI(a:l,142);MVBI(b:l,142);MVBI(SP:l,142);"},
    {.op = kTestOp_MVB,
     .op_name = "MVB",
     .arg1 = {ArgType::kWordReg, 4},
     .arg2 = {ArgType::kWordReg, 4},
     .code = "LD(C2);UL;"
             "CMP(C2,C0);"
             "JC(Z,@low);"
             "@high:MVB(a:h,b:l);MVB(SP:h,FP:l);END;"
             "@low:MVB(a:l,b:h);MVB(SP:l,FP:h);"},
    {.op = kTestOp_MVNI,
     .op_name = "MVNI",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "LD(C2);UL;"
             "CMP(C2,C0);JC(Z,@nib0);"
             "MOVI(C0,1);CMP(C2,C0);JC(Z,@nib1);"
             "MOVI(C0,2);CMP(C2,C0);JC(Z,@nib2);"
             "@nib3:MVNI(a:3,9);MVNI(b:3,9);MVNI(SP:3,9);END;"
             "@nib2:MVNI(a:2,9);MVNI(b:2,9);MVNI(SP:2,9);END;"
             "@nib1:MVNI(a:1,9);MVNI(b:1,9);MVNI(SP:1,9);END;"
             "@nib0:MVNI(a:0,9);MVNI(b:0,9);MVNI(SP:0,9);"},
    {.op = kTestOp_MVN,
     .op_name = "MVN",
     .arg1 = {ArgType::kWordReg, 4},
     .arg2 = {ArgType::kWordReg, 4},
     .code = "LD(C2);UL;"
             "CMP(C2,C0);JC(Z,@nib0);"
             "MOVI(C0,1);CMP(C2,C0);JC(Z,@nib1);"
             "MOVI(C0,2);CMP(C2,C0);JC(Z,@nib2);"
             "@nib3:MVN(a:3,b:0);MVN(SP:3,FP:0);END;"
             "@nib2:MVN(a:2,b:1);MVN(SP:2,FP:1);END;"
             "@nib1:MVN(a:1,b:2);MVN(SP:1,FP:2);END;"
             "@nib0:MVN(a:0,b:3);MVN(SP:0,FP:3);"},
    {.op = kTestOp_LV,
     .op_name = "LV",
     .arg1 = ArgType::kWordReg,
     .code = "LD(C0);"
             "UL;"
             "MOV(a,C0);"},
    {.op = kTestOp_MSSC,
     .op_name = "MSSC",
     .code = "UL;"
             "MSS(Z);MSR(ZSCO,ZSCO);MOV(R0,ST);"
             "MSS(S);MSR(ZSCO,ZSCO);MOV(R1,ST);"
             "MSS(C);MSR(ZSCO,ZSCO);MOV(R2,ST);"
             "MSS(O);MSR(ZSCO,ZSCO);MOV(R3,ST);"
             "MSC(Z);MSR(ZSCO,ZSCO);MOV(R4,ST);"
             "MSC(S);MSR(ZSCO,ZSCO);MOV(R5,ST);"
             "MSC(C);MSR(ZSCO,ZSCO);MOV(R6,ST);"
             "MSC(O);MSR(ZSCO,ZSCO);MOV(R7,ST);"},
    {.op = kTestOp_MSX,
     .op_name = "MSX",
     .code = "UL;"
             "MSX(Z);MSR(ZSCO,ZSCO);MOV(R0,ST);"
             "MSX(S);MSR(ZSCO,ZSCO);MOV(R1,ST);"
             "MSX(C);MSR(ZSCO,ZSCO);MOV(R2,ST);"
             "MSX(O);MSR(ZSCO,ZSCO);MOV(R3,ST);"
             "MSX(Z);MSR(ZSCO,ZSCO);MOV(R4,ST);"
             "MSX(S);MSR(ZSCO,ZSCO);MOV(R5,ST);"
             "MSX(C);MSR(ZSCO,ZSCO);MOV(R6,ST);"
             "MSX(O);MSR(ZSCO,ZSCO);MOV(R7,ST);"},
    {.op = kTestOp_MSI,
     .op_name = "MSSC",
     .code = "UL;"
             "MSS(I);MSR(I,I);MOV(R0,ST);"
             "MSC(I);MSR(I,I);MOV(R1,ST);"
             "MSX(I);MSR(I,I);MOV(R2,ST);"
             "MSX(I);MSR(I,I);MOV(R3,ST);"},
    {.op = kTestOp_MSR1,
     .op_name = "MSR1",
     .code = "UL;"
             "MSS(I);MSR(ZSCOI,ZSCOI);MSS(ZSCOI);"
             "MSR(_,Z);MOV(R0,ST);"
             "MSR(_,S);MOV(R1,ST);"
             "MSR(_,C);MOV(R2,ST);"
             "MSR(_,O);MOV(R3,ST);"
             "MSC(ZSCO);"
             "MSR(Z,_);MOV(R4,ST);"
             "MSR(S,_);MOV(R5,ST);"
             "MSR(C,_);MOV(R6,ST);"
             "MSR(O,_);MOV(R7,ST);"},
    {.op = kTestOp_MSR2,
     .op_name = "MSR2",
     .code = "UL;"
             "MSC(ZSCOI);MSR(ZSCOI,_);"
             "MSR(_,Z);MOV(R0,ST);"
             "MSR(_,S);MOV(R1,ST);"
             "MSR(_,C);MOV(R2,ST);"
             "MSR(_,O);MOV(R3,ST);"
             "MSS(ZSCO);MSR(_,ZSCO);"
             "MSR(Z,_);MOV(R4,ST);"
             "MSR(S,_);MOV(R5,ST);"
             "MSR(C,_);MOV(R6,ST);"
             "MSR(O,_);MOV(R7,ST);"},
    {.op = kTestOp_ADDI,
     .op_name = "ADDI",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "ADDI(a,1);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_SUBI,
     .op_name = "SUBI",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "ADDI(a,-1);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_ADD,
     .op_name = "ADD",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "ADD(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_SUB,
     .op_name = "SUB",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "SUB(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_ADC,
     .op_name = "ADC",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MSR(ZSCO,_);"
             "ADC(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_SBC,
     .op_name = "SBC",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MSR(ZSCO,_);"
             "SBC(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_NEG,
     .op_name = "NEG",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "NEG(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_TST,
     .op_name = "TST",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "TST(a);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_CMPI,
     .op_name = "CMPI",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "CMPI(a,16);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_CMP,
     .op_name = "CMP",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "CMP(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_MSKI,
     .op_name = "MSKI",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "MSKI(a,12);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_MSKI2,
     .op_name = "MSKI2",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "MSKI(a,-128);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_MSK,
     .op_name = "MSK",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MSK(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_NOT,
     .op_name = "NOT",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "NOT(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_AND,
     .op_name = "AND",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "AND(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_OR,
     .op_name = "OR",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "OR(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_XOR,
     .op_name = "XOR",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "XOR(a,b);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_SL,
     .op_name = "SL",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "SL(a);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_SR,
     .op_name = "SR",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "SR(a);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_SRA,
     .op_name = "SRA",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "SRA(a);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_RL,
     .op_name = "RL",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "MSS(ZSCO);"
             "MSR(_,ZSCO);"
             "RL(a);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_RR,
     .op_name = "RR",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "MSS(ZSCO);"
             "MSR(_,ZSCO);"
             "RR(a);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_RLC,
     .op_name = "RLC",
     .arg1 = ArgType::kWordReg,
     .arg2 = {ArgType::kImmediate, 3},
     .code = "UL;"
             "RLC(a);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_RRC,
     .op_name = "RRC",
     .arg1 = ArgType::kWordReg,
     .arg2 = {ArgType::kImmediate, 3},
     .code = "UL;"
             "RRC(a);"
             "MSR(ZSCO,ZSCO);"},
    {.op = kTestOp_MATHI,
     .op_name = "MATHI",
     .code = "UL;"
             "ADDI(C0,1);"
             "ADD(C0,C1);"
             "ADC(C0,C1);"
             "SUB(C0,C1);"
             "SBC(C0,C1);"
             "NEG(C0,C1);"
             "CMP(C0,C1);"
             "NOT(C0,C1);"
             "AND(C0,C1);"
             "OR(C0,C1);"
             "XOR(C0,C1);"
             "SL(C0);"
             "SR(C0);"
             "SRA(C0);"
             "RL(C0);"
             "RR(C0);"
             "RLC(C0);"
             "RRC(C0);"},
    {.op = kTestOp_JC,
     .op_name = "JC",
     .arg1 = {ArgType::kImmediate, 7},
     .code = "UL;"
             "JC(Z,@Z);"
             "@1:JC(NZ,@NZ);"
             "@2:JC(S,@S);"
             "@3:JC(NS,@NS);"
             "@4:JC(C,@C);"
             "@5:JC(NC,@NC);"
             "@6:JC(O,@O);"
             "@7:JC(NO,@NO);"
             "END;"
             "@Z:MOV(R0,C0);JP(@1);"
             "@NZ:MOV(R1,C0);JP(@2);"
             "@S:MOV(R2,C0);JP(@3);"
             "@NS:MOV(R3,C0);JP(@4);"
             "@C:MOV(R4,C0);JP(@5);"
             "@NC:MOV(R5,C0);JP(@6);"
             "@O:MOV(R6,C0);JP(@7);"
             "@NO:MOV(R7,C0);"},
    {.op = kTestOp_JCR,
     .op_name = "JCR",
     .arg1 = {ArgType::kImmediate, 3},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "JC(a,@TRUE);"
             "MOVI(b,1);"
             "END;"
             "@TRUE:MOVI(b,2);"},
    {.op = kTestOp_JD,
     .op_name = "JD",
     .arg1 = {ArgType::kImmediate, 8},
     .code = "UL;"
             "MOVI(R7,0);"
             "@LOOP:ADDI(R7,1);"
             "JD(C0,@LOOP);"},
    {.op = kTestOp_JMP,
     .op_name = "JMP",
     .code = "LD(C0);"
             "UL;"
             "ADD(IP,C0);"},
    {.op = kTestOp_INT,
     .op_name = "INT",
     .arg1 = {ArgType::kImmediate, 5},
     .code = "UL;"
             "INT(C0);"},
    {.op = kTestOp_ILD,
     .op_name = "ILD",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "ILD(a,b);"},
    {.op = kTestOp_IST,
     .op_name = "IST",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "IST(a,b);"},
    {.op = kTestOp_IRET,
     .op_name = "IRET",
     .code = "UL;"
             "IRT;"},
    {.op = kTestOp_PLD,
     .op_name = "PLD",
     .arg1 = {ArgType::kImmediate, 3},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "PLK(R7);"
             "MOVI(C1,1);AND(C1,C0);JC(NZ,@TXX);"
             "MOVI(C1,2);AND(C1,C0);JC(NZ,@_SX);"
             "MOVI(C1,4);AND(C1,C0);JC(NZ,@__A);"
             "@___:PLD(___,b);JP(@END);"  // ___
             "@TXX:"                      // TXX
             "MOVI(C1,2);AND(C1,C0);JC(NZ,@TSX);"
             "MOVI(C1,4);AND(C1,C0);JC(NZ,@T_A);"
             "@T__:PLD(T__,b);JP(@END);"  // T__
             "@TSX:"                      // TSX
             "MOVI(C1,4);AND(C1,C0);JC(NZ,@TSA);"
             "@TS_:PLD(TS_,b);JP(@END);"  // TS_
             "@T_A:PLD(T_A,b);JP(@END);"  // T_A
             "@_SX:"                      // _SX
             "MOVI(C1,4);AND(C1,C0);JC(NZ,@_SA);"
             "@_S_:PLD(_S_,b);JP(@END);"  // _S_
             "@_SA:PLD(_SA,b);JP(@END);"  // _SA
             "@__A:PLD(__A,b);JP(@END);"  // __A
             "@TSA:PLD(TSA,b);"           // TSA
             "@END:MSR(S,S);PUL;"},
    {.op = kTestOp_PST,
     .op_name = "PST",
     .arg1 = {ArgType::kImmediate, 3},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "PLK(R7);"
             "MOVI(C1,1);AND(C1,C0);JC(NZ,@TXX);"
             "MOVI(C1,2);AND(C1,C0);JC(NZ,@_SX);"
             "MOVI(C1,4);AND(C1,C0);JC(NZ,@__A);"
             "@___:PST(___,b);JP(@END);"  // ___
             "@TXX:"                      // TXX
             "MOVI(C1,2);AND(C1,C0);JC(NZ,@TSX);"
             "MOVI(C1,4);AND(C1,C0);JC(NZ,@T_A);"
             "@T__:PST(T__,b);JP(@END);"  // T__
             "@TSX:"                      // TSX
             "MOVI(C1,4);AND(C1,C0);JC(NZ,@TSA);"
             "@TS_:PST(TS_,b);JP(@END);"  // TS_
             "@T_A:PST(T_A,b);JP(@END);"  // T_A
             "@_SX:"                      // _SX
             "MOVI(C1,4);AND(C1,C0);JC(NZ,@_SA);"
             "@_S_:PST(_S_,b);JP(@END);"  // _S_
             "@_SA:PST(_SA,b);JP(@END);"  // _SA
             "@__A:PST(__A,b);JP(@END);"  // __A
             "@TSA:PST(TSA,b);"           // TSA
             "@END:MSR(S,S);PUL;"},
    {.op = kTestOp_PLDS,
     .op_name = "PLDS",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MOV(C1,C1);"  // Forces an early return in CpuCore from PLK
             "PLK(C0);"
             "PLD(SA,b);"
             "MSR(IZSCO,IZSCO);"
             "PUL;"},
    {.op = kTestOp_PSTS,
     .op_name = "PSTS",
     .arg1 = {ArgType::kImmediate, 5},
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MOV(C1,C1);"  // Forces an early return in CpuCore from PLK
             "PLK(C0);"
             "PST(SA,b);"
             "MSR(IZSCO,IZSCO);"
             "PUL;"},
    {.op = kTestOp_CBK,
     .op_name = "CBK",
     .arg1 = {ArgType::kImmediate, 2},
     .arg2 = {ArgType::kImmediate, 6},
     .code = "UL;"
             "CLK(R6);"
             "MOVI(R7,0);CMP(C0,R7);JC(Z,@code);"
             "MOVI(R7,1);CMP(C0,R7);JC(Z,@stack);"
             "MOVI(R7,2);CMP(C0,R7);JC(Z,@data);"
             "@extra:CBK(EXTRA,C1);JP(@end);"
             "@code:CBK(CODE,C1);JP(@end);"
             "@stack:CBK(STACK,C1);JP(@end);"
             "@data:CBK(DATA,C1);"
             "@end:CUL;"},
    {.op = kTestOp_CBKS,
     .op_name = "CBKS",
     .arg1 = {ArgType::kImmediate, 4},
     .code = "UL;"
             "CLK(C1);"
             "@code:CBK(CODE,C0);"
             "CUL;"},
    {.op = kTestOp_CLD,
     .op_name = "CLD",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "CLK(R6);"
             "CLD(a,b);"
             "CUL;"},
    {.op = kTestOp_CST,
     .op_name = "CST",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "CLK(R6);"
             "CST(a,b);"
             "CUL;"},
    {.op = kTestOp_CINT,
     .op_name = "CINT",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "LD(C0);"
             "LD(C1);"
             "UL;"
             "CLK(C0);"
             "IST(C1,a);"
             "ILD(C1,b);"
             "INT(C1);"
             "CUL;"},
    {.op = kTestOp_CSELF,
     .op_name = "CSELF",
     .code = "UL;"
             "MOVI(C0,1);"
             "CBK(DATA,C0);"
             "CLD(R0,R4);"
             "CST(R1,R5);"
             "CLK(C0);"
             "CBK(STACK,C0);"
             "CLD(R1,R0);"
             "CST(R1,R2);"
             "CUL;"
             "MOVI(C0,2);"
             "CBK(EXTRA,C0);"
             "CLD(R2,R6);"
             "CST(R3,R7);"},
    {.op = kTestOp_ROREG,
     .op_name = "ROREG",
     .arg1 = ArgType::kWordReg,
     .code = "UL;"
             "MOV(ST,a);"
             "MOV(MB,a);"},
};

std::shared_ptr<const InstructionSet> GetMicroTestInstructionSet() {
  static std::shared_ptr<const InstructionSet> instruction_set =
      CompileInstructionSet({kMicroTestInstructions});
  return instruction_set;
}

class CpuCoreTest : public BaseCoreTest {
 protected:
  CpuCoreTest()
      : BaseCoreTest({kMicroTestInstructions}, GetMicroTestInstructionSet()) {}
};

TEST_F(CpuCoreTest, CpuCoreInitialState) {
  CpuCore core(CpuCoreConfig::Default());

  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);

  CoreState state(core);
  for (int i = 0; i < CpuCore::kRegisterCount; ++i) {
    EXPECT_EQ(state.r[i], 0) << "r[" << i << "]";
  }
  EXPECT_EQ(state.code.GetBank(), nullptr);
  EXPECT_EQ(state.stack.GetBank(), nullptr);
  EXPECT_EQ(state.data.GetBank(), nullptr);
  EXPECT_EQ(state.extra.GetBank(), nullptr);
}

TEST_F(CpuCoreTest, BanksSetAfterAttachProcessor) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  EXPECT_EQ(state.code.GetBank(), GetMemory(0).GetBank());
  EXPECT_EQ(state.stack.GetBank(), GetMemory(0).GetBank());
  EXPECT_EQ(state.data.GetBank(), GetMemory(0).GetBank());
  EXPECT_EQ(state.extra.GetBank(), GetMemory(0).GetBank());
}

TEST_F(CpuCoreTest, ResetStartsCoreRunning) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);
  state.ResetCore();
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kStartInstruction);

  state.Update();
  for (int i = 0; i < CpuCore::kRegisterCount; ++i) {
    EXPECT_EQ(state.r[i], 0) << "r[" << i << "]";
  }
}

TEST_F(CpuCoreTest, ResetWithSetRegisters) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.SetRegisters(
      {{CpuCore::IP, 100}, {CpuCore::FP, 200}, {CpuCore::SP, 300}});
  CpuCore::Banks banks = {.code = 1, .stack = 2, .data = 3, .extra = 4};
  state.ResetCore(CpuCore::ResetParams{.mask = CpuCore::ResetParams::ALL,
                                       .mb = banks.ToWord(),
                                       .bc = 5,
                                       .bs = 6,
                                       .bd = 7,
                                       .be = 8});
  state.Update();
  EXPECT_EQ(state.mb, banks.ToWord());
  EXPECT_EQ(state.bc, 5);
  EXPECT_EQ(state.ip, 0);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.fp, 0);
  EXPECT_EQ(state.sp, 0);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code.GetBank(), GetMemory(1).GetBank());
  EXPECT_EQ(state.stack.GetBank(), GetMemory(2).GetBank());
  EXPECT_EQ(state.data.GetBank(), GetMemory(3).GetBank());
  EXPECT_EQ(state.extra.GetBank(), GetMemory(4).GetBank());

  state.SetRegisters(
      {{CpuCore::IP, 100}, {CpuCore::FP, 200}, {CpuCore::SP, 300}});

  // Change only the code bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::MC,
      .mb = CpuCore::Banks::Default().SetCode(15).ToWord()});
  state.Update();
  EXPECT_EQ(state.mb, banks.SetCode(15).ToWord());
  EXPECT_EQ(state.bc, 5);
  EXPECT_EQ(state.ip, 100);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.fp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code.GetBank(), GetMemory(15).GetBank());
  EXPECT_EQ(state.stack.GetBank(), GetMemory(2).GetBank());
  EXPECT_EQ(state.data.GetBank(), GetMemory(3).GetBank());
  EXPECT_EQ(state.extra.GetBank(), GetMemory(4).GetBank());

  state.SetRegisters(
      {{CpuCore::IP, 100}, {CpuCore::FP, 200}, {CpuCore::SP, 300}});

  // Change only the stack bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::MS,
      .mb = CpuCore::Banks::Default().SetStack(14).ToWord()});
  state.Update();
  EXPECT_EQ(state.mb, banks.SetStack(14).ToWord());
  EXPECT_EQ(state.bc, 5);
  EXPECT_EQ(state.ip, 100);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.fp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code.GetBank(), GetMemory(15).GetBank());
  EXPECT_EQ(state.stack.GetBank(), GetMemory(14).GetBank());
  EXPECT_EQ(state.data.GetBank(), GetMemory(3).GetBank());
  EXPECT_EQ(state.extra.GetBank(), GetMemory(4).GetBank());

  state.SetRegisters(
      {{CpuCore::IP, 100}, {CpuCore::FP, 200}, {CpuCore::SP, 300}});

  // Change only the data bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::MD,
      .mb = CpuCore::Banks::Default().SetData(13).ToWord()});
  state.Update();
  EXPECT_EQ(state.mb, banks.SetData(13).ToWord());
  EXPECT_EQ(state.bc, 5);
  EXPECT_EQ(state.ip, 100);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.fp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code.GetBank(), GetMemory(15).GetBank());
  EXPECT_EQ(state.stack.GetBank(), GetMemory(14).GetBank());
  EXPECT_EQ(state.data.GetBank(), GetMemory(13).GetBank());
  EXPECT_EQ(state.extra.GetBank(), GetMemory(4).GetBank());

  state.SetRegisters(
      {{CpuCore::IP, 100}, {CpuCore::FP, 200}, {CpuCore::SP, 300}});

  // Change only the extra bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::ME,
      .mb = CpuCore::Banks::Default().SetExtra(12).ToWord()});
  state.Update();
  EXPECT_EQ(state.mb, banks.SetExtra(12).ToWord());
  EXPECT_EQ(state.bc, 5);
  EXPECT_EQ(state.ip, 100);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.fp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code.GetBank(), GetMemory(15).GetBank());
  EXPECT_EQ(state.stack.GetBank(), GetMemory(14).GetBank());
  EXPECT_EQ(state.data.GetBank(), GetMemory(13).GetBank());
  EXPECT_EQ(state.extra.GetBank(), GetMemory(12).GetBank());

  state.SetRegisters(
      {{CpuCore::IP, 100}, {CpuCore::FP, 200}, {CpuCore::SP, 300}});

  // Change only the base code offset and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::RBC, .bc = 10});
  state.Update();
  EXPECT_EQ(state.mb, banks.ToWord());
  EXPECT_EQ(state.bc, 10);
  EXPECT_EQ(state.ip, 0);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.fp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code.GetBank(), GetMemory(15).GetBank());
  EXPECT_EQ(state.stack.GetBank(), GetMemory(14).GetBank());
  EXPECT_EQ(state.data.GetBank(), GetMemory(13).GetBank());
  EXPECT_EQ(state.extra.GetBank(), GetMemory(12).GetBank());

  state.SetRegisters(
      {{CpuCore::IP, 100}, {CpuCore::FP, 200}, {CpuCore::SP, 300}});

  // Change only the stack pointer and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::RBS, .bs = 9});
  state.Update();
  EXPECT_EQ(state.mb, banks.ToWord());
  EXPECT_EQ(state.bc, 10);
  EXPECT_EQ(state.ip, 100);
  EXPECT_EQ(state.bs, 9);
  EXPECT_EQ(state.fp, 0);
  EXPECT_EQ(state.sp, 0);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code.GetBank(), GetMemory(15).GetBank());
  EXPECT_EQ(state.stack.GetBank(), GetMemory(14).GetBank());
  EXPECT_EQ(state.data.GetBank(), GetMemory(13).GetBank());
  EXPECT_EQ(state.extra.GetBank(), GetMemory(12).GetBank());

  state.SetRegisters(
      {{CpuCore::IP, 100}, {CpuCore::FP, 200}, {CpuCore::SP, 300}});

  // Change only the data pointer and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::RBD, .bd = 8});
  state.Update();
  EXPECT_EQ(state.mb, banks.ToWord());
  EXPECT_EQ(state.bc, 10);
  EXPECT_EQ(state.ip, 100);
  EXPECT_EQ(state.bs, 9);
  EXPECT_EQ(state.fp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 8);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code.GetBank(), GetMemory(15).GetBank());
  EXPECT_EQ(state.stack.GetBank(), GetMemory(14).GetBank());
  EXPECT_EQ(state.data.GetBank(), GetMemory(13).GetBank());
  EXPECT_EQ(state.extra.GetBank(), GetMemory(12).GetBank());

  // Change only the extra pointer and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::RBE, .be = 7});
  state.Update();
  EXPECT_EQ(state.mb, banks.ToWord());
  EXPECT_EQ(state.bc, 10);
  EXPECT_EQ(state.ip, 100);
  EXPECT_EQ(state.bs, 9);
  EXPECT_EQ(state.fp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 8);
  EXPECT_EQ(state.be, 7);
  EXPECT_EQ(state.code.GetBank(), GetMemory(15).GetBank());
  EXPECT_EQ(state.stack.GetBank(), GetMemory(14).GetBank());
  EXPECT_EQ(state.data.GetBank(), GetMemory(13).GetBank());
  EXPECT_EQ(state.extra.GetBank(), GetMemory(12).GetBank());
}

TEST_F(CpuCoreTest, RunsAfterReset) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  MemoryBank& memory_bank = *GetMemory(0).GetBank();
  state.ResetCore();

  // This runs the CPU to start executing the first instruction (NOP) until it
  // hits its first synchronization point which is UL.
  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_TRUE(memory_bank.IsLocked());
  state.Update();
  EXPECT_EQ(state.ip, 1);

  // The processor has only advanced one cycle
  EXPECT_EQ(GetCycles(), 1);

  // Executing one cycle at a time, will not hit the processor (no cycles
  // increase) until the processor reaches the CpuCore's cycles.
  while (state.core.GetCycles() > GetCycles()) {
    Execute(1);
    EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction)
        << "Cycles: " << GetCycles();
    EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles)
        << "Cycles: " << GetCycles();
    EXPECT_TRUE(memory_bank.IsLocked()) << "Cycles: " << GetCycles();
  }

  // Executing one more cycle will trigger the next instruction (another NOP).
  // This will result in a back to back unlock and lock, which succeed trivially
  // as there is only one core.
  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2);
  EXPECT_TRUE(memory_bank.IsLocked());
  state.Update();
  EXPECT_EQ(state.ip, 2);
}

TEST_F(CpuCoreTest, CoreLockPreventsExecution) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  MemoryBank& memory_bank = *GetMemory().GetBank();
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);

  auto lock = state.core.RequestLock();
  EXPECT_TRUE(lock->IsLocked());
  state.core.Reset(*lock, CpuCore::ResetParams{});

  EXPECT_EQ(state.core.GetState(), CpuCore::State::kStartInstruction);

  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kStartInstruction);
  EXPECT_EQ(state.core.GetCycles(), 1);
  EXPECT_FALSE(memory_bank.IsLocked());

  lock.reset();

  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_TRUE(memory_bank.IsLocked());
}

TEST_F(CpuCoreTest, CoreLockDeferredUntilEndOfInstruction) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  MemoryBank& memory_bank = *GetMemory().GetBank();
  state.ResetCore();

  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);

  // We are now running an instruction, so the core cannot be locked until it
  // completes.
  auto lock = state.core.RequestLock();
  EXPECT_FALSE(lock->IsLocked());

  // Run enough cycles to start the next instruction.
  Execute(kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kStartInstruction);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.ip, 1);
  EXPECT_TRUE(lock->IsLocked());
}

TEST_F(CpuCoreTest, MemoryLockBlocksFetch) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  MemoryBank& memory_bank = *GetMemory().GetBank();
  state.ResetCore();

  // Lock the memory bank, which will prevent the core from fetching the next
  // instruction.
  auto memory_lock = memory_bank.RequestLock();
  EXPECT_TRUE(memory_lock->IsLocked());

  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kFetchInstruction);
  EXPECT_EQ(state.core.GetCycles(), 1);

  // Unlock the memory bank, which will allow the core to run
  memory_lock.reset();
  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_TRUE(memory_bank.IsLocked());
}

TEST_F(CpuCoreTest, MultiCoreRoundRobinsExecution) {
  ASSERT_TRUE(Init({.num_cores = 3}));
  MemoryBank& memory_bank = *GetMemory().GetBank();
  CoreState& state0 = GetState(0);
  state0.ResetCore();
  state0.SetRegisters({{CpuCore::IP, 10}});
  CoreState& state1 = GetState(1);
  state1.ResetCore();
  state1.SetRegisters({{CpuCore::IP, 20}});
  CoreState& state2 = GetState(2);
  state2.ResetCore();
  state2.SetRegisters({{CpuCore::IP, 30}});

  // Run the processor for one fetch and decode cycle. This will result in the
  // first core locking memory and decoding the first NOP. The other cores
  // should be blocked waiting on the first core to release its lock on the CODE
  // memory bank.
  Execute(kCpuCoreFetchAndDecodeCycles);
  state0.Update();
  EXPECT_EQ(state0.ip, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  state1.Update();
  EXPECT_EQ(state1.ip, 20);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.ip, 30);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Run the processor for one more cycle. This will allow the first core to
  // finish executing the NOP, and the second core to lock memory and decode the
  // first NOP. This blocks the first core from fetching the next instruction.
  Execute(1);
  state0.Update();
  EXPECT_EQ(state0.ip, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kFetchInstruction);
  state1.Update();
  EXPECT_EQ(state1.ip, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kRunInstruction);
  state2.Update();
  EXPECT_EQ(state2.ip, 30);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Run the processor for a fetch and decode cycle. This will allow the second
  // core to finish executing the NOP, and the third core to lock memory and
  // decode the first NOP. This blocks the second core from fetching the next
  // instruction.
  Execute(kCpuCoreFetchAndDecodeCycles);
  state0.Update();
  EXPECT_EQ(state0.ip, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kFetchInstruction);
  state1.Update();
  EXPECT_EQ(state1.ip, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 1);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.ip, 31);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kRunInstruction);

  // One more round will cycle back to the first core. However, as the first
  // core executed first, it needs to wait another cycle to gain the lock.
  Execute(kCpuCoreFetchAndDecodeCycles);
  state0.Update();
  EXPECT_EQ(state0.ip, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kFetchInstruction);
  state1.Update();
  EXPECT_EQ(state1.ip, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 1);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.ip, 31);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 1);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // One more cycle gives the first core the lock, and it can execute the next
  // instruction.
  Execute(1);
  state0.Update();
  EXPECT_EQ(state0.ip, 12);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  state1.Update();
  EXPECT_EQ(state1.ip, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 2);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.ip, 31);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 2);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Finish fetch and decode for the first core, to catch all cores up to the
  // first core.
  Execute(kCpuCoreFetchAndDecodeCycles - 1);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Every kCpuCoreFetchAndDecodeCycles * 3 + 1 cycles will result in a full
  // round. Run for 5 more rounds to verify.
  const int start_cycles = kCpuCoreFetchAndDecodeCycles * 4 + 1;
  const int round_cycles = kCpuCoreFetchAndDecodeCycles * 3 + 1;
  Execute(round_cycles * 5);
  state0.Update();
  EXPECT_EQ(state0.ip, 17);
  EXPECT_EQ(state0.core.GetCycles(), start_cycles + round_cycles * 5);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  state1.Update();
  EXPECT_EQ(state1.ip, 26);
  EXPECT_EQ(state1.core.GetCycles(), start_cycles + round_cycles * 5);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.ip, 36);
  EXPECT_EQ(state2.core.GetCycles(), start_cycles + round_cycles * 5);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);
}

TEST_F(CpuCoreTest, LdOpInFetchExtendsCodeSize) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_LWORD, CpuCore::R0));
  state.code.AddValue(42);
  state.code.AddValue(Encode(kTestOp_LDWORD, CpuCore::D0));
  state.code.AddValue(0);
  state.code.AddValue(1);
  state.code.AddValue(Encode(kTestOp_HALT));

  // Run the processor for one fetch and decode cycle. Since the LWORD OP does
  // an LD microcode before unlocking the CODE memory bank, it will execute as
  // well.
  Execute(1);
  state.Update();
  EXPECT_EQ(state.ip, 2);  // Advances 2 due to LD
  EXPECT_EQ(state.r0, 42);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);

  // Run the processor for one fetch and decode cycle. Since the LWORD OP does
  // an LD microcode before unlocking the CODE memory bank, it will execute as
  // well.
  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.ip, 5);  // Advances 3 due to two LDs
  EXPECT_EQ(state.d0(), 0x10000);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 3);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);

  // Complete this instruction and run the HALT instruction.
  Execute(kCpuCoreFetchAndDecodeCycles * 2 + 3);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);
}

TEST_F(CpuCoreTest, HaltOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  MemoryBank& memory_bank = *GetMemory().GetBank();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_HALT));

  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.ip, 1);

  // The core should not execute any more instructions.
  Execute(100);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 101);
  state.Update();
  EXPECT_EQ(state.ip, 1);
}

TEST_F(CpuCoreTest, WaitOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  MemoryBank& memory_bank = *GetMemory().GetBank();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0},
                      {CpuCore::R1, 1},
                      {CpuCore::R2, kCpuCoreFetchAndDecodeCycles},
                      {CpuCore::R3, kCpuCoreFetchAndDecodeCycles + 1},
                      {CpuCore::R4, kCpuCoreFetchAndDecodeCycles + 2}});
  state.code.AddValue(Encode(kTestOp_WAIT, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_WAIT, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_WAIT, CpuCore::R2));
  state.code.AddValue(Encode(kTestOp_WAIT, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_WAIT, CpuCore::R4));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute through the first three WAITs which shouldn't actually put the CPU
  // into a wait state, as the wait times are too small.
  Execute(kCpuCoreFetchAndDecodeCycles * 4);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);
  state.Update();
  EXPECT_EQ(state.ip, 4);
  EXPECT_EQ(state.r0, 3);  // Ran 3 cycles longer than requested
  EXPECT_EQ(state.r1, 2);  // Ran 2 cycles longer than requested
  EXPECT_EQ(state.r2, 0);  // Ran exactly the requested number of cycles
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4);

  // The next wait (which is already fetched and decoded) should take exactly
  // one cycle, and then return the CPU to the kStartInstruction state.
  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kStartInstruction);
  state.Update();
  EXPECT_EQ(state.ip, 4);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state.r3, 0);

  // The final wait takes two cycles, so we wait 1 extra cycle to get through
  // the fetch+decode and the first cycle of waiting.
  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kWaiting);
  state.Update();
  EXPECT_EQ(state.ip, 5);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 2);

  // The final cycle of waiting will return the CPU to the kStartInstruction
  // state.
  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kStartInstruction);
  state.Update();
  EXPECT_EQ(state.ip, 5);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 3);
  EXPECT_EQ(state.r4, 0);

  // Execute the HALT instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);
  state.Update();
  EXPECT_EQ(state.ip, 6);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 4);
}

TEST_F(CpuCoreTest, AdrLdStOps) {
  ASSERT_TRUE(Init({.num_memory_banks = 4}));
  CpuCore::Banks banks = {.code = 0, .stack = 1, .data = 2, .extra = 3};
  CoreState& state = GetState();
  state.ResetCore({.mb = banks.ToWord()});

  state.code.AddValue(Encode(kTestOp_LCODE, CpuCore::R0, 15));
  state.code.AddValue(Encode(kTestOp_LSTACK, CpuCore::R1, 15));
  state.code.AddValue(Encode(kTestOp_LDATA, CpuCore::R2, 15));
  state.code.AddValue(Encode(kTestOp_LEXTRA, CpuCore::R3, 15));
  state.code.AddValue(Encode(kTestOp_SCODE, 20, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_SSTACK, 20, CpuCore::R2));
  state.code.AddValue(Encode(kTestOp_SDATA, 20, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_SEXTRA, 20, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_PCODE, 25, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_PSTACK, 25, CpuCore::R2));
  state.code.AddValue(Encode(kTestOp_PDATA, 25, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_PEXTRA, 25, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_HALT));

  state.code.SetAddress(15).AddValue(0x1234);
  state.stack.SetAddress(15).AddValue(0x5678);
  state.data.SetAddress(15).AddValue(0x9abc);
  state.extra.SetAddress(15).AddValue(0xdef0);

  // Execute the LCODE instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 2);
  state.Update();
  EXPECT_EQ(state.ip, 1);
  EXPECT_EQ(state.r0, 0x1234);

  // Execute the LSTACK instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 4);
  state.Update();
  EXPECT_EQ(state.ip, 2);
  EXPECT_EQ(state.r1, 0x5678);

  // Execute the LDATA instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 6);
  state.Update();
  EXPECT_EQ(state.ip, 3);
  EXPECT_EQ(state.r2, 0x9abc);

  // Execute the LEXTRA instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 8);
  state.Update();
  EXPECT_EQ(state.ip, 4);
  EXPECT_EQ(state.r3, 0xdef0);

  // Execute the SCODE instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 10);
  state.Update();
  EXPECT_EQ(state.ip, 5);
  EXPECT_EQ(state.code.SetAddress(20).GetValue(), 0x5678);

  // Execute the SSTACK instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 12);
  state.Update();
  EXPECT_EQ(state.ip, 6);
  EXPECT_EQ(state.stack.SetAddress(20).GetValue(), 0x9abc);

  // Execute the SDATA instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 7 + 14);
  state.Update();
  EXPECT_EQ(state.ip, 7);
  EXPECT_EQ(state.data.SetAddress(20).GetValue(), 0xdef0);

  // Execute the SEXTRA instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 16);
  state.Update();
  EXPECT_EQ(state.ip, 8);
  EXPECT_EQ(state.extra.SetAddress(20).GetValue(), 0x1234);

  // Execute the PCODE instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 9 + 18);
  state.Update();
  EXPECT_EQ(state.ip, 9);
  EXPECT_EQ(state.code.SetAddress(24).GetValue(), 0x5678);

  // Execute the PSTACK instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 10 + 20);
  state.Update();
  EXPECT_EQ(state.ip, 10);
  EXPECT_EQ(state.stack.SetAddress(24).GetValue(), 0x9abc);

  // Execute the PDATA instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 11 + 22);
  state.Update();
  EXPECT_EQ(state.ip, 11);
  EXPECT_EQ(state.data.SetAddress(24).GetValue(), 0xdef0);

  // Execute the PEXTRA instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 12 + 24);
  state.Update();
  EXPECT_EQ(state.ip, 12);
  EXPECT_EQ(state.extra.SetAddress(24).GetValue(), 0x1234);

  // Execute the HALT instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);
}

TEST_F(CpuCoreTest, RegisterMemoryStore) {
  ASSERT_TRUE(Init({.num_memory_banks = 4}));
  CoreState& state = GetState();
  CpuCore::Banks banks = {.code = 0, .stack = 1, .data = 2, .extra = 3};
  state.ResetCore({.mb = banks.ToWord()});

  state.SetRegisters({{CpuCore::R0, 1000}});

  state.code.AddValue(Encode(kTestOp_SREG, CpuCore::R0, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_SREG, CpuCore::R1, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_SREG, CpuCore::R2, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_SREG, CpuCore::R3, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_SREG, CpuCore::R4, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_SREG, CpuCore::R5, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_SREG, CpuCore::R6, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_SREG, CpuCore::R7, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
  EXPECT_EQ(state.data.SetAddress(1000).GetValue(), 1000);
  EXPECT_EQ(state.data.SetAddress(1001).GetValue(), 1001);
  EXPECT_EQ(state.data.SetAddress(1002).GetValue(), 1002);
  EXPECT_EQ(state.data.SetAddress(1003).GetValue(), 1003);
  EXPECT_EQ(state.extra.SetAddress(1004).GetValue(), 1004);
  EXPECT_EQ(state.extra.SetAddress(1005).GetValue(), 1005);
  EXPECT_EQ(state.stack.SetAddress(1006).GetValue(), 1006);
  EXPECT_EQ(state.stack.SetAddress(1007).GetValue(), 1007);
}

TEST_F(CpuCoreTest, LkWhenLocked) {
  ASSERT_TRUE(Init({.num_memory_banks = 2}));
  CoreState& state = GetState();
  CpuCore::Banks banks = {.code = 0, .data = 1};
  state.ResetCore({.mb = banks.ToWord()});

  state.code.AddValue(Encode(kTestOp_LDATA, CpuCore::R0, 10));
  state.code.AddValue(Encode(kTestOp_HALT));

  state.data.SetAddress(10).AddValue(0x1234);

  // Lock the memory bank, which will prevent the core from fetching the next
  // instruction.
  auto data_lock = state.data.GetBank()->RequestLock();
  EXPECT_TRUE(data_lock->IsLocked());

  Execute(kCpuCoreFetchAndDecodeCycles + 10);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);
  state.Update();
  EXPECT_EQ(state.ip, 1);
  EXPECT_EQ(state.r0, 0);

  // Unlock the memory bank, which will allow the core to run
  data_lock.reset();
  Execute(2);  // ADR, LD, UL
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);
  state.Update();
  EXPECT_EQ(state.ip, 1);
  EXPECT_EQ(state.r0, 0x1234);

  // Execute the HALT instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);
}

TEST_F(CpuCoreTest, MovStOp) {
  ASSERT_TRUE(Init({.num_memory_banks = 2}));
  CoreState& state = GetState();
  CpuCore::Banks banks = {.code = 0, .data = 1};
  state.ResetCore({.mb = banks.ToWord()});

  state.SetRegisters({{CpuCore::R0, 42}});

  state.code.AddValue(Encode(kTestOp_MOV_ST, 10, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_HALT));

  state.data.SetAddress(10).AddValue(0x1234);

  // Execute through the MOV instruction. This should not execute the LK.
  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.ip, 1);
  EXPECT_FALSE(state.data.GetBank()->IsLocked());

  // Execute the LK instruction, and everything up to the UL.
  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 3);
  EXPECT_TRUE(state.data.GetBank()->IsLocked());
  EXPECT_EQ(state.data.SetAddress(10).GetValue(), 42);

  // Process the UL instruction, and fetch the HALT instruction
  Execute(2);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 3);
  EXPECT_FALSE(state.data.GetBank()->IsLocked());

  // Execute the HALT instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);
}

TEST_F(CpuCoreTest, MoviOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_MOVI, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the MOVI instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.ip, 1);
  EXPECT_EQ(state.r3, 42);

  // Execute the HALT instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);
}

TEST_F(CpuCoreTest, MvbiOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R1, 0x1111},
                      {CpuCore::R2, 0x2222},
                      {CpuCore::R3, 0x3333},
                      {CpuCore::R4, 0x4444},
                      {CpuCore::SP, 0xAAAA}});

  state.code.AddValue(Encode(kTestOp_MVBI, CpuCore::R1, CpuCore::R2))
      .AddValue(0);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_MVBI, CpuCore::R3, CpuCore::R4))
      .AddValue(1);
  state.code.AddValue(Encode(kTestOp_HALT));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r1, 0x118E);
  EXPECT_EQ(state.r2, 0x228E);
  EXPECT_EQ(state.sp, 0xAA8E);

  ExecuteUntilHalt();
  EXPECT_EQ(state.r3, 0x8E33);
  EXPECT_EQ(state.r4, 0x8E44);
  EXPECT_EQ(state.sp, 0x8E8E);
}

TEST_F(CpuCoreTest, MvbOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R1, 0x1122},
                      {CpuCore::R2, 0x3344},
                      {CpuCore::R3, 0x5566},
                      {CpuCore::R4, 0x7788},
                      {CpuCore::FP, 0xAABB},
                      {CpuCore::SP, 0xEEEE}});

  state.code.AddValue(Encode(kTestOp_MVB, CpuCore::R1, CpuCore::R2))
      .AddValue(0);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_MVB, CpuCore::R3, CpuCore::R4))
      .AddValue(1);
  state.code.AddValue(Encode(kTestOp_HALT));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r1, 0x1133);
  EXPECT_EQ(state.sp, 0xEEAA);

  ExecuteUntilHalt();
  EXPECT_EQ(state.r3, 0x8866);
  EXPECT_EQ(state.sp, 0xBBAA);
}

TEST_F(CpuCoreTest, MvniOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x1111},
                      {CpuCore::R1, 0x2222},
                      {CpuCore::R2, 0x3333},
                      {CpuCore::R3, 0x4444},
                      {CpuCore::R4, 0x5555},
                      {CpuCore::R5, 0x6666},
                      {CpuCore::R6, 0x7777},
                      {CpuCore::R7, 0x8888},
                      {CpuCore::SP, 0xAAAA}});

  state.code.AddValue(Encode(kTestOp_MVNI, CpuCore::R0, CpuCore::R1))
      .AddValue(0);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_MVNI, CpuCore::R2, CpuCore::R3))
      .AddValue(1);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_MVNI, CpuCore::R4, CpuCore::R5))
      .AddValue(2);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_MVNI, CpuCore::R6, CpuCore::R7))
      .AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x1119);
  EXPECT_EQ(state.r1, 0x2229);
  EXPECT_EQ(state.sp, 0xAAA9);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r2, 0x3393);
  EXPECT_EQ(state.r3, 0x4494);
  EXPECT_EQ(state.sp, 0xAA99);

  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r4, 0x5955);
  EXPECT_EQ(state.r5, 0x6966);
  EXPECT_EQ(state.sp, 0xA999);

  ExecuteUntilHalt();
  EXPECT_EQ(state.r6, 0x9777);
  EXPECT_EQ(state.r7, 0x9888);
  EXPECT_EQ(state.sp, 0x9999);
}

TEST_F(CpuCoreTest, MvnOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x1234},
                      {CpuCore::R1, 0x5678},
                      {CpuCore::R2, 0x9ABC},
                      {CpuCore::R3, 0xDEF0},
                      {CpuCore::R4, 0x1234},
                      {CpuCore::R5, 0x5678},
                      {CpuCore::R6, 0x9ABC},
                      {CpuCore::R7, 0xDEF0},
                      {CpuCore::FP, 0xABCD},
                      {CpuCore::SP, 0xEEEE}});

  state.code.AddValue(Encode(kTestOp_MVN, CpuCore::R0, CpuCore::R1))
      .AddValue(0);
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_MVN, CpuCore::R2, CpuCore::R3))
      .AddValue(1);
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_MVN, CpuCore::R4, CpuCore::R5))
      .AddValue(2);
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_MVN, CpuCore::R6, CpuCore::R7))
      .AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x1235);
  EXPECT_EQ(state.sp, 0xEEEA);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r2, 0x9AEC);
  EXPECT_EQ(state.sp, 0xEEBA);

  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r4, 0x1734);
  EXPECT_EQ(state.sp, 0xECBA);

  ExecuteUntilHalt();
  EXPECT_EQ(state.r6, 0x0ABC);
  EXPECT_EQ(state.sp, 0xDCBA);
}

TEST_F(CpuCoreTest, MstsMstcOps) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_MSSC));
  state.code.AddValue(Encode(kTestOp_HALT));

  ExecuteUntilHalt();
  EXPECT_EQ(state.r0, CpuCore::Z);
  EXPECT_EQ(state.r1, CpuCore::Z | CpuCore::S);
  EXPECT_EQ(state.r2, CpuCore::Z | CpuCore::S | CpuCore::C);
  EXPECT_EQ(state.r3, CpuCore::Z | CpuCore::S | CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r4, CpuCore::S | CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r5, CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r6, CpuCore::O);
  EXPECT_EQ(state.r7, 0);
}

TEST_F(CpuCoreTest, MstxOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_MSX));
  state.code.AddValue(Encode(kTestOp_HALT));

  ExecuteUntilHalt();
  EXPECT_EQ(state.r0, CpuCore::Z);
  EXPECT_EQ(state.r1, CpuCore::Z | CpuCore::S);
  EXPECT_EQ(state.r2, CpuCore::Z | CpuCore::S | CpuCore::C);
  EXPECT_EQ(state.r3, CpuCore::Z | CpuCore::S | CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r4, CpuCore::S | CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r5, CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r6, CpuCore::O);
  EXPECT_EQ(state.r7, 0);
}

TEST_F(CpuCoreTest, MstiOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_MSI));
  state.code.AddValue(Encode(kTestOp_HALT));

  ExecuteUntilHalt();
  EXPECT_EQ(state.r0, CpuCore::I);
  EXPECT_EQ(state.r1, 0);
  EXPECT_EQ(state.r2, CpuCore::I);
  EXPECT_EQ(state.r3, 0);
}

TEST_F(CpuCoreTest, SetClrFOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(
      Encode(kTestOp_SETF, CpuCore::Z | CpuCore::C, CpuCore::R0));
  state.code.AddValue(
      Encode(kTestOp_SETF, CpuCore::S | CpuCore::O, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_SETF, CpuCore::I, CpuCore::R2));
  state.code.AddValue(Encode(kTestOp_CLRF, CpuCore::I, CpuCore::R3));
  state.code.AddValue(
      Encode(kTestOp_CLRF, CpuCore::Z | CpuCore::O, CpuCore::R4));
  state.code.AddValue(
      Encode(kTestOp_CLRF, CpuCore::S | CpuCore::C, CpuCore::R5));
  state.code.AddValue(Encode(kTestOp_HALT));

  ExecuteUntilHalt();
  EXPECT_EQ(state.r0, CpuCore::Z | CpuCore::C);
  EXPECT_EQ(state.r1, CpuCore::ZSCO);
  EXPECT_EQ(state.r2, CpuCore::ZSCOI);
  EXPECT_EQ(state.r3, CpuCore::ZSCO);
  EXPECT_EQ(state.r4, CpuCore::S | CpuCore::C);
  EXPECT_EQ(state.r5, 0);
  EXPECT_EQ(state.r6, 0);
  EXPECT_EQ(state.r7, 0);
}

TEST_F(CpuCoreTest, XorFOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(
      Encode(kTestOp_XORF, CpuCore::Z | CpuCore::C, CpuCore::R0));
  state.code.AddValue(
      Encode(kTestOp_XORF, CpuCore::S | CpuCore::O, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_XORF, CpuCore::I, CpuCore::R2));
  state.code.AddValue(Encode(kTestOp_XORF, CpuCore::I, CpuCore::R3));
  state.code.AddValue(
      Encode(kTestOp_XORF, CpuCore::Z | CpuCore::O, CpuCore::R4));
  state.code.AddValue(
      Encode(kTestOp_XORF, CpuCore::S | CpuCore::C, CpuCore::R5));
  state.code.AddValue(Encode(kTestOp_HALT));

  ExecuteUntilHalt();
  EXPECT_EQ(state.r0, CpuCore::Z | CpuCore::C);
  EXPECT_EQ(state.r1, CpuCore::ZSCO);
  EXPECT_EQ(state.r2, CpuCore::ZSCOI);
  EXPECT_EQ(state.r3, CpuCore::ZSCO);
  EXPECT_EQ(state.r4, CpuCore::S | CpuCore::C);
  EXPECT_EQ(state.r5, 0);
  EXPECT_EQ(state.r6, 0);
  EXPECT_EQ(state.r7, 0);
}

TEST_F(CpuCoreTest, MovFOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R6, CpuCore::ZSCOI}});

  state.code.AddValue(Encode(kTestOp_MOV, CpuCore::FP, CpuCore::R6));
  state.code.AddValue(
      Encode(kTestOp_MOVF, CpuCore::Z | CpuCore::C, CpuCore::R0));
  state.code.AddValue(
      Encode(kTestOp_MOVF, CpuCore::S | CpuCore::O, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_MOVF, CpuCore::I, CpuCore::R2));
  state.code.AddValue(Encode(kTestOp_MOV, CpuCore::FP, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_MOVF, CpuCore::I, CpuCore::R3));
  state.code.AddValue(
      Encode(kTestOp_MOVF, CpuCore::Z | CpuCore::O, CpuCore::R4));
  state.code.AddValue(
      Encode(kTestOp_MOVF, CpuCore::S | CpuCore::C, CpuCore::R5));
  state.code.AddValue(Encode(kTestOp_HALT));

  ExecuteUntilHalt();
  EXPECT_EQ(state.r0, CpuCore::Z | CpuCore::C);
  EXPECT_EQ(state.r1, CpuCore::ZSCO);
  EXPECT_EQ(state.r2, CpuCore::ZSCOI);
  EXPECT_EQ(state.r3, CpuCore::ZSCO);
  EXPECT_EQ(state.r4, CpuCore::S | CpuCore::C);
  EXPECT_EQ(state.r5, 0);
  EXPECT_EQ(state.r6, CpuCore::ZSCOI);
  EXPECT_EQ(state.r7, 0);
}

TEST_F(CpuCoreTest, MstrOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_MSR1));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_MSR2));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the MSR1 instruction.
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 1 | CpuCore::I);
  EXPECT_EQ(state.r1, 3 | CpuCore::I);
  EXPECT_EQ(state.r2, 7 | CpuCore::I);
  EXPECT_EQ(state.r3, 15 | CpuCore::I);
  EXPECT_EQ(state.r4, 14 | CpuCore::I);
  EXPECT_EQ(state.r5, 12 | CpuCore::I);
  EXPECT_EQ(state.r6, 8 | CpuCore::I);
  EXPECT_EQ(state.r7, 0 | CpuCore::I);

  // Execute the MSR2 instruction.
  ExecuteUntilHalt();
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.r1, 0);
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 0);
  EXPECT_EQ(state.r4, 15);
  EXPECT_EQ(state.r5, 15);
  EXPECT_EQ(state.r6, 15);
  EXPECT_EQ(state.r7, 15);
}

TEST_F(CpuCoreTest, AddiOp1) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x0001},
                      {CpuCore::R2, 0xFFFE},
                      {CpuCore::R3, 0xFFFF},
                      {CpuCore::R4, 0x7FFF}});

  state.code.AddValue(Encode(kTestOp_ADDI, CpuCore::R0));  // R0: 0x0000, IMM: 1
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ADDI, CpuCore::R1));  // R1: 0x0001, IMM: 1
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ADDI, CpuCore::R2));  // R2: 0xFFFE, IMM: 1
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ADDI, CpuCore::R3));  // R3: 0xFFFF, IMM: 1
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ADDI, CpuCore::R4));  // R4: 0x7FFF, IMM: 1
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the ADDI(R0,1) instruction. R0 = 0x0000
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADDI(R1,1) instruction. R1 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADDI(R2,1) instruction. R2 = 0xFFFE
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the ADDI(R3,1) instruction. R3 = 0xFFFF
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADDI(R4,1) instruction. R4 = 0x7FFF
  ExecuteUntilHalt();
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::O);
}

TEST_F(CpuCoreTest, AddiOp2) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x0001},
                      {CpuCore::R2, 0x0002},
                      {CpuCore::R3, 0xFFFF},
                      {CpuCore::R4, 0x8000}});

  state.code.AddValue(Encode(kTestOp_SUBI, CpuCore::R0));  // 0x0000, -1
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SUBI, CpuCore::R1));  // 0x0001, -1
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SUBI, CpuCore::R2));  // 0x0002, -1
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SUBI, CpuCore::R3));  // 0xFFFF, -1
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SUBI, CpuCore::R4));  // 0x8000, -1
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the ADDI(R0,-1) instruction. R0 = 0x0000
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the ADDI(R1,-1) instruction. R1 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADDI(R2,-1) instruction. R2 = 0x0002
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the ADDI(R3,-1) instruction. R3 = 0xFFFF
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0xFFFE);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the ADDI(R4,-1) instruction. R4 = 0x8000
  ExecuteUntilHalt();
  EXPECT_EQ(state.r4, 0x7FFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C | CpuCore::O);
}

TEST_F(CpuCoreTest, AddOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x0001},
                      {CpuCore::R2, 0xFFFE},
                      {CpuCore::R3, 0xFFFF},
                      {CpuCore::R4, 0x7FFF},
                      {CpuCore::R7, 0x0001}});

  state.code.AddValue(Encode(kTestOp_ADD, CpuCore::R0, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ADD, CpuCore::R1, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ADD, CpuCore::R2, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ADD, CpuCore::R3, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ADD, CpuCore::R4, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the ADD(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADD(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADD(R2,R7) instruction. R2 = 0xFFFE, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the ADD(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADD(R4,R7) instruction. R4 = 0x8000, R7 = 0x0001
  ExecuteUntilHalt();
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::O);
}

TEST_F(CpuCoreTest, SubOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x0001},
                      {CpuCore::R2, 0x0002},
                      {CpuCore::R3, 0xFFFF},
                      {CpuCore::R4, 0x8000},
                      {CpuCore::R7, 0x0001}});

  state.code.AddValue(Encode(kTestOp_SUB, CpuCore::R0, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SUB, CpuCore::R1, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SUB, CpuCore::R2, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SUB, CpuCore::R3, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SUB, CpuCore::R4, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the SUB(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the SUB(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the SUB(R2,R7) instruction. R2 = 0x0002, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the SUB(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0xFFFE);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the SUB(R4,R7) instruction. R4 = 0x8000, R7 = 0x0001
  ExecuteUntilHalt();
  EXPECT_EQ(state.r4, 0x7FFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C | CpuCore::O);
}

TEST_F(CpuCoreTest, AdcOpNoCarry) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x0001},
                      {CpuCore::R2, 0xFFFE},
                      {CpuCore::R3, 0xFFFF},
                      {CpuCore::R4, 0x7FFF},
                      {CpuCore::R5, 0x0001},
                      {CpuCore::R6, 0xFFFF},
                      {CpuCore::R7, 0x0001}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R0, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R1, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R2, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R3, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R4, CpuCore::R7));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R5, CpuCore::R6));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R5, CpuCore::R5));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the ADC(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADC(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADC(R2,R7) instruction. R2 = 0xFFFE, R7 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the ADC(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADC(R4,R7) instruction. R4 = 0x7FFF, R7 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::O);

  // Execute the ADC(R5,R6) instruction. R5 = 0x0001, R6 = 0xFFFF, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip6));
  EXPECT_EQ(state.r5, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADC(R5,R5) instruction. R5 = 0x0000, C = 0
  ExecuteUntilHalt();
  EXPECT_EQ(state.r5, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);
}

TEST_F(CpuCoreTest, AdcOpWithCarry) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0xFFFF},
                      {CpuCore::R1, 0x0000},
                      {CpuCore::R2, 0xFFFD},
                      {CpuCore::R3, 0xFFFE},
                      {CpuCore::R4, 0x7FFE},
                      {CpuCore::R5, 0x0000},
                      {CpuCore::R6, 0xFFFF},
                      {CpuCore::R7, 0x0001}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R0, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R1, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R2, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R3, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R4, CpuCore::R7));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R5, CpuCore::R6));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_ADC, CpuCore::R5, CpuCore::R5));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the ADC(R0,R7) instruction. R0 = 0xFFFF, R7 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the ADC(R1,R7) instruction. R1 = 0x0000, R7 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADC(R2,R7) instruction. R2 = 0xFFFD, R7 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the ADC(R3,R7) instruction. R3 = 0xFFFE, R7 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADC(R4,R7) instruction. R4 = 0x7FFE, R7 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::O);

  // Execute the ADC(R5,R6) instruction. R5 = 0x0000, R6 = 0xFFFF, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip6));
  EXPECT_EQ(state.r5, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADC(R5,R5) instruction. R5 = 0x0000, C = 1
  ExecuteUntilHalt();
  EXPECT_EQ(state.r5, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);
}

TEST_F(CpuCoreTest, SbcOpNoCarry) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x0001},
                      {CpuCore::R2, 0x0002},
                      {CpuCore::R3, 0xFFFF},
                      {CpuCore::R4, 0x8000},
                      {CpuCore::R7, 0x0001}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R0, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R1, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R2, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R3, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R4, CpuCore::R7));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R1, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the SBC(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the SBC(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the SBC(R2,R7) instruction. R2 = 0x0002, R7 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the SBC(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0xFFFE);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the SBC(R4,R7) instruction. R4 = 0x8000, R7 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0x7FFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C | CpuCore::O);

  // Execute the SBC(R1,R1) instruction. R1 = 0x0000, C = 0
  ExecuteUntilHalt();
  EXPECT_EQ(state.r1, 0);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);
}

TEST_F(CpuCoreTest, SbcOpWithCarry) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x0001},
                      {CpuCore::R2, 0x0002},
                      {CpuCore::R3, 0xFFFF},
                      {CpuCore::R4, 0x8001},
                      {CpuCore::R5, 0x0000},
                      {CpuCore::R6, 0xFFFF},
                      {CpuCore::R7, 0x0001}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R0, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R1, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R2, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R3, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R4, CpuCore::R7));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_SBC, CpuCore::R5, CpuCore::R6));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the SBC(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0xFFFE);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the SBC(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the SBC(R2,R7) instruction. R2 = 0x0002, R7 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the SBC(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0xFFFD);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the SBC(R4,R7) instruction. R4 = 0x8001, R7 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0x7FFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C | CpuCore::O);

  // Execute the SBC(R5,R6) instruction. R5 = 0x0000, R6 = 0xFFFF, C = 1
  ExecuteUntilHalt();
  EXPECT_EQ(state.r5, 0);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);
}

TEST_F(CpuCoreTest, NegOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x0001},
                      {CpuCore::R2, 0x0002},
                      {CpuCore::R3, 0xFFFF},
                      {CpuCore::R4, 0x8000}});

  state.code.AddValue(Encode(kTestOp_NEG, CpuCore::R7, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_NEG, CpuCore::R7, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_NEG, CpuCore::R7, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_NEG, CpuCore::R7, CpuCore::R3));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_NEG, CpuCore::R7, CpuCore::R4));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the NEG(R7,R0) instruction. R0 = 0x0000
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r7, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the NEG(R7,R1) instruction. R1 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r7, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the NEG(R7,R2) instruction. R2 = 0x0002
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r7, 0xFFFE);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the NEG(R7,R3) instruction. R3 = 0xFFFF
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r7, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the NEG(R7,R4) instruction. R4 = 0x8000
  ExecuteUntilHalt();
  EXPECT_EQ(state.r7, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::O);
}

TEST_F(CpuCoreTest, TstOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x0001},
                      {CpuCore::R2, 0x8000},
                      {CpuCore::R3, 0xFFFF}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::Z));
  state.code.AddValue(Encode(kTestOp_TST, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO));
  state.code.AddValue(Encode(kTestOp_TST, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::S));
  state.code.AddValue(Encode(kTestOp_TST, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::S));
  state.code.AddValue(Encode(kTestOp_TST, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_HALT));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, CpuCore::S);
  ExecuteUntilHalt();
  EXPECT_EQ(state.st, CpuCore::S);
}

TEST_F(CpuCoreTest, CmpiOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 15},
                      {CpuCore::R1, 16},
                      {CpuCore::R2, 17},
                      {CpuCore::R3, 0x8000}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::S));
  state.code.AddValue(Encode(kTestOp_CMPI, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(
      Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::Z - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_CMPI, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_CMPI, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(
      Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C - CpuCore::O));
  state.code.AddValue(Encode(kTestOp_CMPI, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_HALT));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, CpuCore::S);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::Z | CpuCore::C);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, CpuCore::C);
  ExecuteUntilHalt();
  EXPECT_EQ(state.st, CpuCore::C | CpuCore::O);
}

TEST_F(CpuCoreTest, CmpOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x0001},
                      {CpuCore::R2, 0x0002},
                      {CpuCore::R3, 0xFFFF},
                      {CpuCore::R4, 0x8000},
                      {CpuCore::R7, 0x0001}});

  state.code.AddValue(Encode(kTestOp_CMP, CpuCore::R0, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_CMP, CpuCore::R1, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_CMP, CpuCore::R2, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_CMP, CpuCore::R3, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_CMP, CpuCore::R4, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the CMP(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the CMP(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the CMP(R2,R7) instruction. R2 = 0x0002, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the CMP(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the CMP(R4,R7) instruction. R4 = 0x8000, R7 = 0x0001
  ExecuteUntilHalt();
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C | CpuCore::O);
}

TEST_F(CpuCoreTest, MskiOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, static_cast<uint16_t>(~12)},
                      {CpuCore::R1, 12},
                      {CpuCore::R2, 8},
                      {CpuCore::R3, 4}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::Z));
  state.code.AddValue(Encode(kTestOp_MSKI, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO));
  state.code.AddValue(Encode(kTestOp_MSKI, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO));
  state.code.AddValue(Encode(kTestOp_MSKI, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO));
  state.code.AddValue(Encode(kTestOp_MSKI, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_HALT));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, 0);
  ExecuteUntilHalt();
  EXPECT_EQ(state.st, 0);
}

TEST_F(CpuCoreTest, Mski2Op) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x007F},
                      {CpuCore::R1, 0x0080},
                      {CpuCore::R2, 0x8000},
                      {CpuCore::R3, 0xFFFF}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::Z));
  state.code.AddValue(Encode(kTestOp_MSKI2, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO));
  state.code.AddValue(Encode(kTestOp_MSKI2, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::S));
  state.code.AddValue(Encode(kTestOp_MSKI2, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::S));
  state.code.AddValue(Encode(kTestOp_MSKI2, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_HALT));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, CpuCore::S);
  ExecuteUntilHalt();
  EXPECT_EQ(state.st, CpuCore::S);
}

TEST_F(CpuCoreTest, MskOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0xAAAA},
                      {CpuCore::R1, 0x5555},
                      {CpuCore::R2, 0xFFFF},
                      {CpuCore::R3, 0x7FFF},
                      {CpuCore::R4, 0x8000},
                      {CpuCore::R5, 0x8000},
                      {CpuCore::R6, 0xFFFF},
                      {CpuCore::R7, 0x0000}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::Z));
  state.code.AddValue(Encode(kTestOp_MSK, CpuCore::R0, CpuCore::R1));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO));
  state.code.AddValue(Encode(kTestOp_MSK, CpuCore::R2, CpuCore::R3));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::S));
  state.code.AddValue(Encode(kTestOp_MSK, CpuCore::R4, CpuCore::R5));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::Z));
  state.code.AddValue(Encode(kTestOp_MSK, CpuCore::R6, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, CpuCore::Z);
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, CpuCore::S);
  ExecuteUntilHalt();
  EXPECT_EQ(state.st, CpuCore::Z);
}

TEST_F(CpuCoreTest, NotOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x1234},
                      {CpuCore::R2, 0x5678},
                      {CpuCore::R3, 0x9ABC},
                      {CpuCore::R4, 0xDEF0},
                      {CpuCore::R5, 0xFFFF}});

  state.code.AddValue(Encode(kTestOp_NOT, CpuCore::R7, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_NOT, CpuCore::R7, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_NOT, CpuCore::R7, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_NOT, CpuCore::R7, CpuCore::R3));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_NOT, CpuCore::R7, CpuCore::R4));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_NOT, CpuCore::R7, CpuCore::R5));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the NOT(R7,R0) instruction. R0 = 0x0000
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r7, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the NOT(R7,R1) instruction. R1 = 0x1234
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r7, 0xEDCB);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the NOT(R7,R2) instruction. R2 = 0x5678
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r7, 0xA987);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the NOT(R7,R3) instruction. R3 = 0x9ABC
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r7, 0x6543);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the NOT(R7,R4) instruction. R4 = 0xDEF0
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r7, 0x210F);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the NOT(R7,R5) instruction. R5 = 0xFFFF
  ExecuteUntilHalt();
  EXPECT_EQ(state.r7, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);
}

TEST_F(CpuCoreTest, AndOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x1234},
                      {CpuCore::R2, 0x5678},
                      {CpuCore::R3, 0x9ABC},
                      {CpuCore::R4, 0xDEF0},
                      {CpuCore::R5, 0xA5C3},
                      {CpuCore::R6, 0xFFFF},
                      {CpuCore::R7, 0x5A3C}});

  state.code.AddValue(Encode(kTestOp_AND, CpuCore::R0, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_AND, CpuCore::R1, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_AND, CpuCore::R2, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_AND, CpuCore::R3, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_AND, CpuCore::R4, CpuCore::R7));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_AND, CpuCore::R5, CpuCore::R6));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_AND, CpuCore::R5, CpuCore::R7));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_AND, CpuCore::R6, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the AND(R0,R7) instruction. R0 =  0x0000, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the AND(R1,R7) instruction. R1 = 0x1234, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x1234);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the AND(R2,R7) instruction. R2 = 0x5678, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x5238);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the AND(R3,R7) instruction. R3 = 0x9ABC, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0x1A3C);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the AND(R4,R7) instruction. R4 = 0xDEF0, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0x5A30);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the AND(R5,R6) instruction. R5 = 0xA5C3, R6 = 0xFFFF
  ASSERT_TRUE(ExecuteUntilIp(ip6));
  EXPECT_EQ(state.r5, 0xA5C3);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the AND(R5,R7) instruction. R5 = 0xA5C3, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip7));
  EXPECT_EQ(state.r5, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the AND(R6,R7) instruction. R6 = 0xFFFF, R7 = 0x5A3C
  ExecuteUntilHalt();
  EXPECT_EQ(state.r6, 0x5A3C);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);
}

TEST_F(CpuCoreTest, OrOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x1234},
                      {CpuCore::R2, 0x5678},
                      {CpuCore::R3, 0x9ABC},
                      {CpuCore::R4, 0xDEF0},
                      {CpuCore::R5, 0xA5C3},
                      {CpuCore::R6, 0xFFFF},
                      {CpuCore::R7, 0x5A3C}});

  state.code.AddValue(Encode(kTestOp_OR, CpuCore::R0, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_OR, CpuCore::R1, CpuCore::R7));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_OR, CpuCore::R2, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_OR, CpuCore::R3, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_OR, CpuCore::R4, CpuCore::R7));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_OR, CpuCore::R5, CpuCore::R6));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_OR, CpuCore::R5, CpuCore::R7));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_OR, CpuCore::R6, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the OR(R0,R0) instruction. R0 = 0x0000
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the OR(R1,R7) instruction. R1 = 0x1234, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x5A3C);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the OR(R2,R7) instruction. R2 = 0x5678, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x5E7C);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the OR(R3,R7) instruction. R3 = 0x9ABC, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0xDABC);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the OR(R4,R7) instruction. R4 = 0xDEF0, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0xDEFC);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the OR(R5,R6) instruction. R5 = 0xA5C3, R6 = 0xFFFF
  ASSERT_TRUE(ExecuteUntilIp(ip6));
  EXPECT_EQ(state.r5, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the OR(R5,R7) instruction. R5 = 0xFFFF, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip7));
  EXPECT_EQ(state.r5, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the OR(R6,R7) instruction. R6 = 0xFFFF, R7 = 0x5A3C
  ExecuteUntilHalt();
  EXPECT_EQ(state.r6, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);
}

TEST_F(CpuCoreTest, XorOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0000},
                      {CpuCore::R1, 0x1234},
                      {CpuCore::R2, 0x5678},
                      {CpuCore::R3, 0x9ABC},
                      {CpuCore::R4, 0xDEF0},
                      {CpuCore::R5, 0xA5C3},
                      {CpuCore::R6, 0xFFFF},
                      {CpuCore::R7, 0x5A3C}});

  state.code.AddValue(Encode(kTestOp_XOR, CpuCore::R0, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_XOR, CpuCore::R1, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_XOR, CpuCore::R2, CpuCore::R7));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_XOR, CpuCore::R3, CpuCore::R7));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_XOR, CpuCore::R4, CpuCore::R7));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_XOR, CpuCore::R5, CpuCore::R6));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_XOR, CpuCore::R5, CpuCore::R7));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_XOR, CpuCore::R6, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the XOR(R0,R0) instruction. R0 = 0x0000
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the XOR(R1,R1) instruction. R1 = 0x1234
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the XOR(R2,R7) instruction. R2 = 0x5678, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x0C44);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the XOR(R3,R7) instruction. R3 = 0x9ABC, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0xC080);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the XOR(R4,R7) instruction. R4 = 0xDEF0, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0x84CC);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the XOR(R5,R6) instruction. R5 = 0xA5C3, R6 = 0xFFFF
  ASSERT_TRUE(ExecuteUntilIp(ip6));
  EXPECT_EQ(state.r5, 0x5A3C);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the XOR(R5,R7) instruction. R5 = 0x5A3C, R7 = 0x5A3C
  ASSERT_TRUE(ExecuteUntilIp(ip7));
  EXPECT_EQ(state.r5, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the XOR(R6,R7) instruction. R6 = 0xFFFF, R7 = 0x5A3C
  ExecuteUntilHalt();
  EXPECT_EQ(state.r6, 0xA5C3);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);
}

TEST_F(CpuCoreTest, SlSrSraOps) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0001},
                      {CpuCore::R1, 0x8000},
                      {CpuCore::R2, 0xC000},
                      {CpuCore::R3, 0x8000},
                      {CpuCore::R4, 0x0001},
                      {CpuCore::R5, 0x8000},
                      {CpuCore::R6, 0x4001},
                      {CpuCore::R7, 0x8001}});

  state.code.AddValue(Encode(kTestOp_SL, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SL, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SL, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SR, CpuCore::R3));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SR, CpuCore::R4));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SRA, CpuCore::R5));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SRA, CpuCore::R6));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_SRA, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the SL(R0) instruction. R0 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the SL(R1) instruction. R1 = 0x8000
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the SL(R2) instruction. R2 = 0xC000
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the SR(R3) instruction. R3 = 0x8000
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0x4000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the SR(R4) instruction. R4 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the SRA(R5) instruction. R5 = 0x8000
  ASSERT_TRUE(ExecuteUntilIp(ip6));
  EXPECT_EQ(state.r5, 0xC000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the SRA(R6) instruction. R6 = 0x4001
  ASSERT_TRUE(ExecuteUntilIp(ip7));
  EXPECT_EQ(state.r6, 0x2000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the SRA(R7) instruction. R7 = 0x8001
  ExecuteUntilHalt();
  EXPECT_EQ(state.r7, 0xC000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);
}

TEST_F(CpuCoreTest, RlRrOps) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0001},
                      {CpuCore::R1, 0x8000},
                      {CpuCore::R2, 0xC000},
                      {CpuCore::R3, 0x8000},
                      {CpuCore::R4, 0x0001},
                      {CpuCore::R5, 0x8001},
                      {CpuCore::R6, 0x0000}});

  state.code.AddValue(Encode(kTestOp_RL, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_RL, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_RL, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_RR, CpuCore::R3));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_RR, CpuCore::R4));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_RR, CpuCore::R5));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_RL, CpuCore::R6));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_RR, CpuCore::R6));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the RL(R0) instruction. R0 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RL(R1) instruction. R1 = 0x8000
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the RL(R2) instruction. R2 = 0xC000
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x8001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RR(R3) instruction. R3 = 0x8000
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0x4000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RR(R4) instruction. R4 = 0x0001
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RR(R5) instruction. R5 = 0x8001
  ASSERT_TRUE(ExecuteUntilIp(ip6));
  EXPECT_EQ(state.r5, 0xC000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RL(R6) instruction. R6 = 0x0000
  ASSERT_TRUE(ExecuteUntilIp(ip7));
  EXPECT_EQ(state.r6, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the RR(R6) instruction. R6 = 0x0000
  ExecuteUntilHalt();
  EXPECT_EQ(state.r6, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);
}

TEST_F(CpuCoreTest, RlcRrcOpsNoCarry) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0001},
                      {CpuCore::R1, 0x8000},
                      {CpuCore::R2, 0xC000},
                      {CpuCore::R3, 0x8000},
                      {CpuCore::R4, 0x0001},
                      {CpuCore::R5, 0x8001},
                      {CpuCore::R6, 0x0000},
                      {CpuCore::R7, 0x0000}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RLC, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RLC, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RLC, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RRC, CpuCore::R3));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RRC, CpuCore::R4));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RRC, CpuCore::R5));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RLC, CpuCore::R6));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RRC, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the RLC(R0) instruction. R0 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RLC(R1) instruction. R1 = 0x8000, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the RLC(R2) instruction. R2 = 0xC000, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RRC(R3) instruction. R3 = 0x8000, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0x4000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RRC(R4) instruction. R4 = 0x0001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the RRC(R5) instruction. R5 = 0x8001, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip6));
  EXPECT_EQ(state.r5, 0x4000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the RLC(R6) instruction. R6 = 0x0000, C = 0
  ASSERT_TRUE(ExecuteUntilIp(ip7));
  EXPECT_EQ(state.r6, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the RRC(R7) instruction. R7 = 0x0000, C = 0
  ExecuteUntilHalt();
  EXPECT_EQ(state.r7, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);
}

TEST_F(CpuCoreTest, RlcRrcOpsWithCarry) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 0x0001},
                      {CpuCore::R1, 0x8000},
                      {CpuCore::R2, 0xC000},
                      {CpuCore::R3, 0x8000},
                      {CpuCore::R4, 0x0001},
                      {CpuCore::R5, 0x8001},
                      {CpuCore::R6, 0x0000},
                      {CpuCore::R7, 0x0000}});

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RLC, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RLC, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RLC, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RRC, CpuCore::R3));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RRC, CpuCore::R4));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RRC, CpuCore::R5));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RLC, CpuCore::R6));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_RRC, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the RLC(R0) instruction. R0 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0x0003);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RLC(R1) instruction. R1 = 0x8000, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the RLC(R2) instruction. R2 = 0xC000, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0x8001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RRC(R3) instruction. R3 = 0x8000, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip4));
  EXPECT_EQ(state.r3, 0xC000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the RRC(R4) instruction. R4 = 0x0001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip5));
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RRC(R5) instruction. R5 = 0x8001, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip6));
  EXPECT_EQ(state.r5, 0xC000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RLC(R6) instruction. R6 = 0x0000, C = 1
  ASSERT_TRUE(ExecuteUntilIp(ip7));
  EXPECT_EQ(state.r6, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RRC(R7) instruction. R7 = 0x0000, C = 1
  ExecuteUntilHalt();
  EXPECT_EQ(state.r7, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);
}

TEST_F(CpuCoreTest, MathOpsLeaveInterruptFlagAlone) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::ST, CpuCore::I}});

  state.code.AddValue(Encode(kTestOp_MATHI));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute MATHI instruction. ST = I
  ExecuteUntilHalt();
  EXPECT_EQ(state.st & CpuCore::I, CpuCore::I);
}

TEST_F(CpuCoreTest, JcJpEndOps) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_ZSCO, 0));
  state.code.AddValue(Encode(kTestOp_JC, 1));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::Z));
  state.code.AddValue(Encode(kTestOp_JC, 2));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::S));
  state.code.AddValue(Encode(kTestOp_JC, 3));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_JC, 4));
  const uint16_t ip4 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::O));
  state.code.AddValue(Encode(kTestOp_JC, 5));
  const uint16_t ip5 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::Z | CpuCore::C));
  state.code.AddValue(Encode(kTestOp_JC, 6));
  const uint16_t ip6 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::S | CpuCore::O));
  state.code.AddValue(Encode(kTestOp_JC, 7));
  const uint16_t ip7 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO));
  state.code.AddValue(Encode(kTestOp_JC, 8));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the JC instruction. ST = 0
  ExecuteUntilIp(ip1);
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.r1, 1);
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 1);
  EXPECT_EQ(state.r4, 0);
  EXPECT_EQ(state.r5, 1);
  EXPECT_EQ(state.r6, 0);
  EXPECT_EQ(state.r7, 1);

  // Execute the JC instruction. ST = Z
  ExecuteUntilIp(ip2);
  EXPECT_EQ(state.r0, 2);
  EXPECT_EQ(state.r1, 1);
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 2);
  EXPECT_EQ(state.r4, 0);
  EXPECT_EQ(state.r5, 2);
  EXPECT_EQ(state.r6, 0);
  EXPECT_EQ(state.r7, 2);

  // Execute the JC instruction. ST = S
  ExecuteUntilIp(ip3);
  EXPECT_EQ(state.r0, 2);
  EXPECT_EQ(state.r1, 3);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 2);
  EXPECT_EQ(state.r4, 0);
  EXPECT_EQ(state.r5, 3);
  EXPECT_EQ(state.r6, 0);
  EXPECT_EQ(state.r7, 3);

  // Execute the JC instruction. ST = C
  ExecuteUntilIp(ip4);
  EXPECT_EQ(state.r0, 2);
  EXPECT_EQ(state.r1, 4);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 4);
  EXPECT_EQ(state.r4, 4);
  EXPECT_EQ(state.r5, 3);
  EXPECT_EQ(state.r6, 0);
  EXPECT_EQ(state.r7, 4);

  // Execute the JC instruction. ST = O
  ExecuteUntilIp(ip5);
  EXPECT_EQ(state.r0, 2);
  EXPECT_EQ(state.r1, 5);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 5);
  EXPECT_EQ(state.r4, 4);
  EXPECT_EQ(state.r5, 5);
  EXPECT_EQ(state.r6, 5);
  EXPECT_EQ(state.r7, 4);

  // Execute the JC instruction. ST = ZC
  ExecuteUntilIp(ip6);
  EXPECT_EQ(state.r0, 6);
  EXPECT_EQ(state.r1, 5);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 6);
  EXPECT_EQ(state.r4, 6);
  EXPECT_EQ(state.r5, 5);
  EXPECT_EQ(state.r6, 5);
  EXPECT_EQ(state.r7, 6);

  // Execute the JC instruction. ST = 0
  ExecuteUntilIp(ip7);
  EXPECT_EQ(state.r0, 6);
  EXPECT_EQ(state.r1, 7);
  EXPECT_EQ(state.r2, 7);
  EXPECT_EQ(state.r3, 6);
  EXPECT_EQ(state.r4, 6);
  EXPECT_EQ(state.r5, 7);
  EXPECT_EQ(state.r6, 7);
  EXPECT_EQ(state.r7, 6);

  // Execute the JC instruction. ST = ZCSO
  ExecuteUntilHalt();
  EXPECT_EQ(state.r0, 8);
  EXPECT_EQ(state.r1, 7);
  EXPECT_EQ(state.r2, 8);
  EXPECT_EQ(state.r3, 6);
  EXPECT_EQ(state.r4, 8);
  EXPECT_EQ(state.r5, 7);
  EXPECT_EQ(state.r6, 8);
  EXPECT_EQ(state.r7, 6);
}

TEST_F(CpuCoreTest, JcOpWithRegTrue) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::Z));
  state.code.AddValue(Encode(kTestOp_JCR, 4 | CpuCore::ZShift, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::S));
  state.code.AddValue(Encode(kTestOp_JCR, 4 | CpuCore::SShift, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_JCR, 4 | CpuCore::CShift, CpuCore::R2));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::O));
  state.code.AddValue(Encode(kTestOp_JCR, 4 | CpuCore::OShift, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::Z));
  state.code.AddValue(Encode(kTestOp_JCR, CpuCore::ZShift, CpuCore::R4));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::S));
  state.code.AddValue(Encode(kTestOp_JCR, CpuCore::SShift, CpuCore::R5));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_JCR, CpuCore::CShift, CpuCore::R6));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::O));
  state.code.AddValue(Encode(kTestOp_JCR, CpuCore::OShift, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the HALT instruction.
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, 17);
  EXPECT_EQ(state.r0, 2);
  EXPECT_EQ(state.r1, 2);
  EXPECT_EQ(state.r2, 2);
  EXPECT_EQ(state.r3, 2);
  EXPECT_EQ(state.r4, 2);
  EXPECT_EQ(state.r5, 2);
  EXPECT_EQ(state.r6, 2);
  EXPECT_EQ(state.r7, 2);
}

TEST_F(CpuCoreTest, JcOpWithRegFalse) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::Z));
  state.code.AddValue(Encode(kTestOp_JCR, 4 | CpuCore::ZShift, CpuCore::R0));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::S));
  state.code.AddValue(Encode(kTestOp_JCR, 4 | CpuCore::SShift, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C));
  state.code.AddValue(Encode(kTestOp_JCR, 4 | CpuCore::CShift, CpuCore::R2));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::O));
  state.code.AddValue(Encode(kTestOp_JCR, 4 | CpuCore::OShift, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::Z));
  state.code.AddValue(Encode(kTestOp_JCR, CpuCore::ZShift, CpuCore::R4));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::S));
  state.code.AddValue(Encode(kTestOp_JCR, CpuCore::SShift, CpuCore::R5));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::C));
  state.code.AddValue(Encode(kTestOp_JCR, CpuCore::CShift, CpuCore::R6));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::O));
  state.code.AddValue(Encode(kTestOp_JCR, CpuCore::OShift, CpuCore::R7));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the HALT instruction.
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, 17);
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.r1, 1);
  EXPECT_EQ(state.r2, 1);
  EXPECT_EQ(state.r3, 1);
  EXPECT_EQ(state.r4, 1);
  EXPECT_EQ(state.r5, 1);
  EXPECT_EQ(state.r6, 1);
  EXPECT_EQ(state.r7, 1);
}

TEST_F(CpuCoreTest, JdOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  state.code.AddValue(Encode(kTestOp_JD, 1));
  state.code.AddValue(Encode(kTestOp_JD, 10));
  state.code.AddValue(Encode(kTestOp_JD, 0));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute the JD instruction, loop once
  Execute(kCpuCoreFetchAndDecodeCycles + 1 + 2 * 1);
  EXPECT_EQ(state.core.GetCycles(), GetCycles());
  state.Update();
  EXPECT_EQ(state.ip, 1);
  EXPECT_EQ(state.r7, 1);

  // Execute the JD instruction, loop 10 times
  Execute(kCpuCoreFetchAndDecodeCycles + 1 + 2 * 10);
  EXPECT_EQ(state.core.GetCycles(), GetCycles());
  state.Update();
  EXPECT_EQ(state.ip, 2);
  EXPECT_EQ(state.r7, 10);

  // Execute the JD instruction, loop 65536 times
  Execute(kCpuCoreFetchAndDecodeCycles + 1 + 2 * 65536);
  EXPECT_EQ(state.core.GetCycles(), GetCycles());
  state.Update();
  EXPECT_EQ(state.ip, 3);
  EXPECT_EQ(state.r7, 0);

  // Execute the HALT instruction.
  Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kIdle);
}

TEST_F(CpuCoreTest, InterruptDuringExecution) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(100);
  // Interrupt 1 to 100
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::Z | CpuCore::O));
  state.code.AddValue(Encode(kTestOp_EI));
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(2);
  const uint16_t ip1 = state.code.GetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t ip2 = state.code.GetAddress();

  // Interrupt 1
  state.code.SetAddress(100);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(42);
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute until R2 is being set to 2 and before it is set to 3
  ASSERT_TRUE(ExecuteUntilIp(ip1));

  // Trigger interrupt 1
  state.core.RaiseInterrupt(1);
  ASSERT_TRUE(ExecuteUntilIp(100));
  EXPECT_EQ(state.r2, 2);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(state.sp, 0xFFFE);
  state.stack.SetAddress(state.sp);
  EXPECT_EQ(state.stack.GetValue(), CpuCore::I | CpuCore::Z | CpuCore::O);
  EXPECT_EQ(state.stack.GetValue(), ip1);

  // Execute the interrupt through the IRET instruction
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r3, 42);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::Z | CpuCore::O);

  // Execute through the HALT instruction
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, ip2);
  EXPECT_EQ(state.r2, 3);
}

TEST_F(CpuCoreTest, InterruptDuringWait) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(100);
  // Interrupt 1 to 100
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_ZSCO, CpuCore::Z | CpuCore::O));
  state.code.AddValue(Encode(kTestOp_EI));
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(1000);
  state.code.AddValue(Encode(kTestOp_WAIT, CpuCore::R3));
  const uint16_t ip1 = state.code.GetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t ip2 = state.code.GetAddress();

  // Interrupt 1
  state.code.SetAddress(100);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(5000);
  state.code.AddValue(Encode(kTestOp_WAIT, CpuCore::R3));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute until waiting
  ASSERT_TRUE(ExecuteUntil(
      [&] { return state.core.GetState() == CpuCore::State::kWaiting; }));
  EXPECT_EQ(state.ip, ip1);
  const Cycles wait_start =
      state.core.GetCycles() - (kCpuCoreFetchAndDecodeCycles + 1);

  // Wait a little longer
  Execute(100);

  // Execute interrupt 1
  state.core.RaiseInterrupt(1);
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, CpuCore::W | CpuCore::O);
  EXPECT_EQ(state.r3, 5000);
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, CpuCore::W | CpuCore::I | CpuCore::Z);
  EXPECT_EQ(state.ip, ip1);

  // Execute until waiting again
  ASSERT_TRUE(ExecuteUntil(
      [&] { return state.core.GetState() == CpuCore::State::kWaiting; }));
  ASSERT_TRUE(ExecuteUntil(
      [&] { return state.core.GetState() != CpuCore::State::kWaiting; }));
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kStartInstruction);
  const Cycles wait_end = state.core.GetCycles();
  EXPECT_EQ(wait_end - wait_start, 1000);
  EXPECT_EQ(state.r3, 0);

  // Execute through the HALT instruction
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, ip2);
  EXPECT_EQ(state.r2, 3);
}

TEST_F(CpuCoreTest, InterruptDuringHalt) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(100);
  // Interrupt 1 to 100
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_EI));
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t ip1 = state.code.GetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t ip2 = state.code.GetAddress();

  // Interrupt 1
  state.code.SetAddress(100);
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute until halting the first time
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, ip1);
  EXPECT_EQ(state.r2, 0);

  // Execute interrupt 1
  state.core.RaiseInterrupt(1);
  ASSERT_TRUE(ExecuteUntilIp(100));
  ASSERT_TRUE(ExecuteUntilIp(ip1));

  // Execute through the HALT instruction
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, ip2);
  EXPECT_EQ(state.r2, 3);
}

TEST_F(CpuCoreTest, RaiseInterruptWhileDisabled) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(100);
  // Interrupt 1 to 100
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_NOP));
  const uint16_t ip1 = state.code.GetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t ip2 = state.code.GetAddress();

  // Interrupt 1
  state.code.SetAddress(100);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(42);
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute until halting the first time
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r2, 1);

  // Execute interrupt 1
  state.core.RaiseInterrupt(1);

  // Execute through the HALT instruction
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, ip2);
  EXPECT_EQ(state.r3, 0);
  EXPECT_EQ(state.r2, 3);
}

TEST_F(CpuCoreTest, EnableInterruptAfterRaise) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(100);
  // Interrupt 1 to 100
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(2);
  state.code.AddValue(Encode(kTestOp_EI));
  const uint16_t ip1 = state.code.GetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t ip2 = state.code.GetAddress();

  // Interrupt 1
  state.code.SetAddress(100);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(42);
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  state.core.RaiseInterrupt(1);
  ASSERT_TRUE(ExecuteUntilIp(100));
  EXPECT_EQ(state.r2, 2);
  state.stack.SetAddress(state.sp);
  EXPECT_EQ(state.stack.GetValue(), CpuCore::I);
  EXPECT_EQ(state.stack.GetValue(), ip1);

  // Execute through the HALT instruction
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, ip2);
  EXPECT_EQ(state.r2, 3);
}

TEST_F(CpuCoreTest, InterruptInInterrupt) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(100);
  // Interrupt 1 to 100
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(2);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(200);
  // Interrupt 2 to 200
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(3);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(300);
  // Interrupt 3 to 300
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_EI));
  state.code.AddValue(Encode(kTestOp_INT, 1));
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t ip1 = state.code.GetAddress();

  // Interrupt 1
  state.code.SetAddress(100);
  state.code.AddValue(Encode(kTestOp_INT, 2));
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(42);
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Interrupt 2
  state.code.SetAddress(200);
  state.code.AddValue(Encode(kTestOp_EI));
  state.code.AddValue(Encode(kTestOp_INT, 3));
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(24);
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Interrupt 3
  state.code.SetAddress(300);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(100);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R4)).AddValue(321);
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  ASSERT_TRUE(ExecuteUntilIp(100));
  EXPECT_EQ(state.r3, 0);
  ASSERT_TRUE(ExecuteUntilIp(200));
  EXPECT_EQ(state.r3, 42);
  ASSERT_TRUE(ExecuteUntilIp(300));
  EXPECT_EQ(state.r3, 42);
  ASSERT_TRUE(ExecuteUntil([&] { return state.ip < 300; }));
  EXPECT_EQ(state.r3, 100);
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, ip1);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 24);
  EXPECT_EQ(state.r4, 321);
}

TEST_F(CpuCoreTest, LockCoreDuringInterrupt) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(100);
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_EI));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t ip2 = state.code.GetAddress();

  // Interrupt 1
  state.code.SetAddress(100);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(42);
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute until after interrupts are enabled.
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.st, CpuCore::I);

  // Lock the core, but it will not *yet* be locked, as the core is in the
  // middle of an instruction (the NOP).
  auto lock = state.core.RequestLock();
  EXPECT_FALSE(lock->IsLocked());
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);

  // Start the interrupt execution, but it will be stuck at kHandleInterrupt.
  state.core.RaiseInterrupt(1);
  Execute(kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kHandleInterrupt);
  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kHandleInterrupt);

  // Release the lock and finish execution
  lock = nullptr;
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, ip2);
  EXPECT_EQ(state.r3, 42);
  EXPECT_EQ(state.r2, 3);
}

TEST_F(CpuCoreTest, LockMemoryDuringInterrupt) {
  ASSERT_TRUE(Init({.num_memory_banks = 2}));
  CoreState& state = GetState();
  state.ResetCore({.mb = CpuCore::Banks().SetStack(1).ToWord()});

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(100);
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  state.code.AddValue(Encode(kTestOp_EI));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t ip2 = state.code.GetAddress();

  // Interrupt 1
  state.code.SetAddress(100);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(42);
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute until after interrupts are enabled.
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.st, CpuCore::I);

  // Lock the stack, which is not yet in use.
  auto lock = GetMemory(1).GetBank()->RequestLock();
  EXPECT_TRUE(lock->IsLocked());
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kRunInstruction);

  // Start the interrupt execution, but it will be stuck at kPushInterruptState.
  state.core.RaiseInterrupt(1);
  Execute(kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kPushInterruptState);
  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kPushInterruptState);

  // Release the lock on the stack and lock code
  lock = GetMemory().GetBank()->RequestLock();
  EXPECT_TRUE(lock->IsLocked());
  Execute(kCpuCoreStartInterruptCycles);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kStartInterrupt);
  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kFetchInstruction);
  Execute(kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kFetchInstruction);
  Execute(1);
  EXPECT_EQ(state.core.GetState(), CpuCore::State::kFetchInstruction);

  // Release the code memory lock and complete the program.
  lock = nullptr;
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, ip2);
  EXPECT_EQ(state.r3, 42);
  EXPECT_EQ(state.r2, 3);
}

TEST_F(CpuCoreTest, RaiseInterruptsThatAreNotMapped) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_EI));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R1)).AddValue(100);
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R0, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(3);
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t ip4 = state.code.GetAddress();

  // Interrupt 1
  state.code.SetAddress(100);
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R3)).AddValue(42);
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute until after interrupts are enabled.
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 0);
  EXPECT_EQ(state.st, CpuCore::I);

  // Raise all the interrupts, but none are mapped yet.
  for (int i = 0; i < CpuCore::kInterruptCount; ++i) {
    state.core.RaiseInterrupt(i);
  }

  // Run to the next instruction, and verify that no interrupt fired.
  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 0);

  // Raise all the interrupts again, now 1 is mapped
  for (int i = 0; i < CpuCore::kInterruptCount; ++i) {
    state.core.RaiseInterrupt(i);
  }

  // Run to the next instruction, and this time the interrupt fired.
  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 42);

  // Finish execution, and no interrupt is fired
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, ip4);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 42);
}

TEST_F(CpuCoreTest, LoadFromInvalidPort) {
  ASSERT_TRUE(Init({.num_ports = 10}));
  CoreState& state = GetState();
  state.ResetCore();

  state.SetRegisters({{CpuCore::R0, 42}, {CpuCore::R1, 24}});

  // Main program
  state.code.AddValue(Encode(kTestOp_PLDS, 10, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_PLDS, 11, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 42);
  EXPECT_EQ(state.st, 0);

  auto lock = state.core.RequestLock();
  while (!lock->IsLocked()) {
    Execute(1);
  }
  state.core.SetWordRegister(*lock, CpuCore::ST, CpuCore::ZSCO);
  lock.reset();

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 24);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, LoadFromPortOnlyChangesFlagS) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::ZSCOI}, {CpuCore::R0, 100}, {CpuCore::R1, 100}});
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 42);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 24);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_PLDS, 0, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_PLDS, 1, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 42);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::Z | CpuCore::C | CpuCore::O);

  lock = state.core.RequestLock();
  while (!lock->IsLocked()) {
    Execute(1);
  }
  state.core.SetWordRegister(*lock, CpuCore::ST, 0);
  lock.reset();

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 24);
  EXPECT_EQ(state.st, CpuCore::S);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, LoadWordFromPortMode0) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 1);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 0xFFFF);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0);
  state.code.AddValue(Encode(kTestOp_PLD, 0, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_PLD, 0, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(0).GetAddress(), 0);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 0);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, LoadWordFromPortModeS) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 1);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 0xFFFF);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0);
  state.code.AddValue(Encode(kTestOp_PLD, Port::S, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_PLD, Port::S, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(0).GetAddress(), 0);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 0);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, LoadWordFromPortModeA) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 1);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 0xFFFF);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0);
  state.code.AddValue(Encode(kTestOp_PLD, Port::A, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_PLD, Port::A, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(0).GetAddress(), 1);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 1);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, LoadWordFromPortModeT) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 1);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 0xFFFF);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0);
  state.code.AddValue(Encode(kTestOp_PLD, Port::T, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_PLD, Port::T, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(0).GetAddress(), 0);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 0);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, LoadWordFromPortModeTSA) {
  ASSERT_TRUE(Init({.num_ports = 3}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 1);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 0xFFFF);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock = LockPort(2);
  GetPort(2).StoreWord(*lock, Port::S, 42);
  EXPECT_EQ(GetPort(2).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0);
  state.code.AddValue(
      Encode(kTestOp_PLD, Port::T | Port::S | Port::A, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(1);
  state.code.AddValue(
      Encode(kTestOp_PLD, Port::T | Port::S | Port::A, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(2);
  state.code.AddValue(
      Encode(kTestOp_PLD, Port::T | Port::S | Port::A, CpuCore::R1));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(0).GetAddress(), 0);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 1);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.r1, 42);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 1);
  EXPECT_EQ(GetPort(1).GetStatus(), 0);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, StoreToInvalidPort) {
  ASSERT_TRUE(Init({.num_ports = 10}));
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_PSTS, 10, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_PSTS, 11, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, 0);

  auto lock = state.core.RequestLock();
  while (!lock->IsLocked()) {
    Execute(1);
  }
  state.core.SetWordRegister(*lock, CpuCore::ST, CpuCore::ZSCO);
  lock.reset();

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, StoreToPortOnlyChangesFlagS) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters(
      {{CpuCore::ST, CpuCore::ZSCOI}, {CpuCore::R0, 42}, {CpuCore::R1, 24}});
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_PSTS, 0, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_PSTS, 1, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::Z | CpuCore::C | CpuCore::O);
  EXPECT_EQ(GetPort(0).GetStatus(), 1);
  EXPECT_EQ(GetPort(0).GetValue(0), 42);

  lock = state.core.RequestLock();
  while (!lock->IsLocked()) {
    Execute(1);
  }
  state.core.SetWordRegister(*lock, CpuCore::ST, 0);
  lock.reset();

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(GetPort(1).GetValue(0), 24);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, StoreWordToPortMode0) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters({{CpuCore::R0, 1}, {CpuCore::R1, 0xFFFF}});
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0);
  state.code.AddValue(Encode(kTestOp_PST, 0, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_PST, 0, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(0).GetAddress(), 0);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  EXPECT_EQ(GetPort(0).GetValue(0), 1);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 0);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(GetPort(1).GetValue(0), 0xFFFF);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, StoreWordToPortModeA) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters({{CpuCore::R0, 1}, {CpuCore::R1, 0xFFFF}});
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0);
  state.code.AddValue(Encode(kTestOp_PST, Port::A, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_PST, Port::A, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(0).GetAddress(), 1);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  EXPECT_EQ(GetPort(0).GetValue(0), 1);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 1);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(GetPort(1).GetValue(0), 0xFFFF);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, StoreWordToPortModeS) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters({{CpuCore::R0, 1}, {CpuCore::R1, 0xFFFF}});
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0);
  state.code.AddValue(Encode(kTestOp_PST, Port::S, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_PST, Port::S, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(0).GetAddress(), 0);
  EXPECT_EQ(GetPort(0).GetStatus(), 1);
  EXPECT_EQ(GetPort(0).GetValue(0), 1);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 0);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(GetPort(1).GetValue(0), 0xFFFF);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, StoreWordToPortModeT) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters({{CpuCore::R0, 1}, {CpuCore::R1, 0xFFFF}});
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0);
  state.code.AddValue(Encode(kTestOp_PST, Port::T, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(1);
  state.code.AddValue(Encode(kTestOp_PST, Port::T, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(0).GetAddress(), 0);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  EXPECT_EQ(GetPort(0).GetValue(0), 1);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 0);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(GetPort(1).GetValue(0), 100);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, StoreWordToPortModeTSA) {
  ASSERT_TRUE(Init({.num_ports = 3}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters(
      {{CpuCore::R0, 1}, {CpuCore::R1, 0xFFFF}, {CpuCore::R2, 42}});
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(GetPort(0).GetStatus(), 0);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  lock = LockPort(2);
  GetPort(2).StoreWord(*lock, 0, 100);
  EXPECT_EQ(GetPort(2).GetStatus(), 0);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0);
  state.code.AddValue(
      Encode(kTestOp_PST, Port::T | Port::S | Port::A, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(1);
  state.code.AddValue(
      Encode(kTestOp_PST, Port::T | Port::S | Port::A, CpuCore::R1));
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(2);
  state.code.AddValue(
      Encode(kTestOp_PST, Port::T | Port::S | Port::A, CpuCore::R2));
  const uint16_t ip3 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(0).GetAddress(), 1);
  EXPECT_EQ(GetPort(0).GetStatus(), 1);
  EXPECT_EQ(GetPort(0).GetValue(0), 1);

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(GetPort(1).GetAddress(), 0);
  EXPECT_EQ(GetPort(1).GetStatus(), 1);
  EXPECT_EQ(GetPort(1).GetValue(0), 100);

  ASSERT_TRUE(ExecuteUntilIp(ip3));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(GetPort(2).GetAddress(), 1);
  EXPECT_EQ(GetPort(2).GetStatus(), 1);
  EXPECT_EQ(GetPort(2).GetValue(0), 42);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, LockPortBlocksCpuCore) {
  ASSERT_TRUE(Init({.num_ports = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters({{CpuCore::R0, 42}, {CpuCore::R1, 24}});
  auto lock = LockPort(0);
  GetPort(0).StoreWord(*lock, 0, 100);
  lock = LockPort(1);
  GetPort(1).StoreWord(*lock, 0, 100);
  lock.reset();

  // Main program
  state.code.AddValue(Encode(kTestOp_PSTS, 0, CpuCore::R0));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_PSTS, 1, CpuCore::R1));
  const uint16_t blocked_ip = state.code.GetAddress();
  const uint16_t ip2 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Lock port 1, which should not effect writing to port 0
  lock = LockPort(1);

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(GetPort(0).GetValue(0), 42);

  ASSERT_TRUE(ExecuteUntilIp(blocked_ip));
  EXPECT_EQ(GetPort(1).GetValue(0), 100);
  for (int i = 0; i < 10; ++i) {
    Execute(1);
  }
  state.Update();
  EXPECT_EQ(state.ip, blocked_ip);
  EXPECT_EQ(GetPort(1).GetValue(0), 100);

  // Reset port 1 lock, which allows the CpuCore to continue
  lock.reset();

  ASSERT_TRUE(ExecuteUntilIp(ip2));
  EXPECT_EQ(GetPort(1).GetValue(0), 24);

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, SelfCoreOp) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters({{CpuCore::R0, 1000},
                      {CpuCore::R1, 1001},
                      {CpuCore::R2, 1002},
                      {CpuCore::R3, 1003},
                      {CpuCore::R4, 1004},
                      {CpuCore::R5, 1005},
                      {CpuCore::R6, 1006},
                      {CpuCore::R7, 1007}});

  // Main program
  state.code.AddValue(Encode(kTestOp_CSELF));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.mb, CpuCore::Banks().SetData(1).SetExtra(2).ToWord());
  EXPECT_EQ(state.r0, 1000);
  EXPECT_EQ(state.r1, 1005);
  EXPECT_EQ(state.r2, 1002);
  EXPECT_EQ(state.r3, 1007);
  EXPECT_EQ(state.r4, 1000);
  EXPECT_EQ(state.r5, 1005);
  EXPECT_EQ(state.r6, 1002);
  EXPECT_EQ(state.r7, 1007);
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
}

TEST_F(CpuCoreTest, ModifyOtherCore) {
  ASSERT_TRUE(Init({.num_cores = 2, .num_memory_banks = 2}));
  CoreState& state0 = GetState(0);
  CoreState& state1 = GetState(1);
  state0.ResetCore();
  state1.ResetCore({.mb = CpuCore::Banks().SetCode(1).ToWord()});
  MemAccessor mem0 = GetMemory();
  MemAccessor mem1 = GetMemory(1);

  // Initialize processor
  state0.SetRegisters({{CpuCore::R0, 1000},
                       {CpuCore::R1, 1001},
                       {CpuCore::R2, 1002},
                       {CpuCore::R3, 1003},
                       {CpuCore::R6, 1}});
  state1.SetRegisters({{CpuCore::R0, 2000},
                       {CpuCore::R1, 2001},
                       {CpuCore::R2, 2002},
                       {CpuCore::R3, 2003}});

  // Main program
  mem0.AddValue(Encode(kTestOp_CLD, CpuCore::R0, CpuCore::R2));
  mem0.AddValue(Encode(kTestOp_CST, CpuCore::R1, CpuCore::R3));
  mem0.AddValue(Encode(kTestOp_CBK, CpuCore::CODE, 2));
  mem0.AddValue(Encode(kTestOp_CBK, CpuCore::STACK, 3));
  mem0.AddValue(Encode(kTestOp_CBK, CpuCore::DATA, 4));
  mem0.AddValue(Encode(kTestOp_CBK, CpuCore::EXTRA, 5));
  mem0.AddValue(Encode(kTestOp_CBK, CpuCore::EXTRA, 16));
  mem0.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip0 = mem0.GetAddress();

  mem1.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip1 = mem1.GetAddress();

  // Execute code
  ExecuteUntilHalt();
  EXPECT_EQ(state0.ip, end_ip0);
  EXPECT_EQ(state0.mb, 0);
  EXPECT_EQ(state0.r0, 1000);
  EXPECT_EQ(state0.r1, 1001);
  EXPECT_EQ(state0.r2, 2000);
  EXPECT_EQ(state0.r3, 1003);
  EXPECT_EQ(state1.ip, end_ip1);
  EXPECT_EQ(
      state1.mb,
      CpuCore::Banks().SetCode(2).SetStack(3).SetData(4).SetExtra(5).ToWord());
  EXPECT_EQ(state1.r0, 2000);
  EXPECT_EQ(state1.r1, 1003);
  EXPECT_EQ(state1.r2, 2002);
  EXPECT_EQ(state1.r3, 2003);
}

TEST_F(CpuCoreTest, ModifySelfWithCbk) {
  ASSERT_TRUE(Init({.num_memory_banks = 2}));
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters({{CpuCore::R0, 1000},
                      {CpuCore::R1, 1001},
                      {CpuCore::R2, 1002},
                      {CpuCore::R3, 1003},
                      {CpuCore::R6, 0xFFFF}});

  // Main program
  state.code.AddValue(Encode(kTestOp_CLD, CpuCore::R0, CpuCore::R2));
  state.code.AddValue(Encode(kTestOp_CST, CpuCore::R1, CpuCore::R3));
  state.code.AddValue(Encode(kTestOp_CBK, CpuCore::STACK, 3));
  state.code.AddValue(Encode(kTestOp_CBK, CpuCore::DATA, 4));
  state.code.AddValue(Encode(kTestOp_CBK, CpuCore::EXTRA, 5));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute code
  ExecuteUntilHalt();
  EXPECT_EQ(
      state.mb,
      CpuCore::Banks().SetCode(0).SetStack(3).SetData(4).SetExtra(5).ToWord());
  EXPECT_EQ(state.r0, 1000);
  EXPECT_EQ(state.r1, 1003);
  EXPECT_EQ(state.r2, 1000);
  EXPECT_EQ(state.r3, 1003);
}

TEST_F(CpuCoreTest, SelfCoreInterrupt) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(100);
  state.code.AddValue(Encode(kTestOp_CINT, CpuCore::R0, CpuCore::R1))
      .AddValue(0)
      .AddValue(7);
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
  EXPECT_EQ(state.core.GetInterrupts(), 1 << 7);
  EXPECT_EQ(state.core.GetInterruptAddress(7), 100);
  EXPECT_EQ(state.r1, 100);
}

TEST_F(CpuCoreTest, OtherCoreInterrupt) {
  ASSERT_TRUE(Init({.num_cores = 2}));
  CoreState& state0 = GetState(0);
  CoreState& state1 = GetState(1);
  state0.ResetCore();
  state1.ResetCore();

  state1.SetRegisters({{CpuCore::IP, 100}});

  // Core 0 program
  state0.code.AddValue(Encode(kTestOp_LV, CpuCore::R0)).AddValue(100);
  state0.code.AddValue(Encode(kTestOp_CINT, CpuCore::R0, CpuCore::R1))
      .AddValue(1)
      .AddValue(7);
  state0.code.AddValue(Encode(kTestOp_LV, CpuCore::R2)).AddValue(200);
  state0.code.AddValue(Encode(kTestOp_CINT, CpuCore::R2, CpuCore::R3))
      .AddValue(2)
      .AddValue(8);
  state0.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip0 = state0.code.GetAddress();

  // Core 1 program
  state1.code.SetAddress(100);
  state1.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip1 = state1.code.GetAddress();

  // Execute code
  ExecuteUntilHalt();
  state1.Update();
  EXPECT_EQ(state1.ip, end_ip1);
  EXPECT_EQ(state0.ip, end_ip0);
  EXPECT_EQ(state1.core.GetInterrupts(), 1 << 7);
  EXPECT_EQ(state1.core.GetInterruptAddress(7), 100);
  EXPECT_EQ(state0.r1, 100);
  EXPECT_EQ(state0.r3, 0);
}

TEST_F(CpuCoreTest, CbkResetsWaitOnOtherCore) {
  ASSERT_TRUE(Init({.num_cores = 2, .num_memory_banks = 3}));
  CoreState& state0 = GetState(0);
  CoreState& state1 = GetState(1);
  state0.ResetCore();
  state1.ResetCore({.mb = CpuCore::Banks().SetCode(1).ToWord()});
  MemAccessor mem0 = GetMemory(0);
  MemAccessor mem1 = GetMemory(1);
  MemAccessor mem2 = GetMemory(2);

  // Initialize processor
  state0.SetRegisters({{CpuCore::R6, 1}});
  state1.SetRegisters({{CpuCore::ST, CpuCore::ZSCO}, {CpuCore::R0, 2000}});

  // Main program
  mem0.AddValue(Encode(kTestOp_NOP));
  mem0.AddValue(Encode(kTestOp_NOP));
  mem0.AddValue(Encode(kTestOp_NOP));
  const uint16_t blocked_ip0 = mem0.GetAddress();
  mem0.AddValue(Encode(kTestOp_CBK, CpuCore::CODE, 2));
  mem0.AddValue(Encode(kTestOp_NOP));
  const uint16_t nop_ip0 = mem0.GetAddress();
  mem0.AddValue(Encode(kTestOp_NOP));
  mem0.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip0 = mem0.GetAddress();

  mem1.AddValue(Encode(kTestOp_WAIT, CpuCore::R0));
  mem1.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip1 = mem1.GetAddress();

  mem2.AddValue(Encode(kTestOp_WAIT, CpuCore::R0));
  mem2.AddValue(Encode(kTestOp_NOP));
  mem2.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip2 = mem2.GetAddress();
  ASSERT_NE(end_ip1, end_ip2);

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(blocked_ip0));
  EXPECT_NE(state0.core.GetState(), CpuCore::State::kWaiting);
  EXPECT_EQ(state0.ip, blocked_ip0);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kWaiting);
  EXPECT_EQ(state1.st, CpuCore::W | CpuCore::Z | CpuCore::S | CpuCore::C);

  ASSERT_TRUE(ExecuteUntilIp(nop_ip0));
  EXPECT_NE(state1.core.GetState(), CpuCore::State::kWaiting);
  EXPECT_EQ(state1.st, CpuCore::Z | CpuCore::S | CpuCore::C);

  ExecuteUntilHalt();
  EXPECT_EQ(state0.ip, end_ip0);
  EXPECT_EQ(state1.ip, end_ip2);
}

TEST_F(CpuCoreTest, CbkResetsWaitOnSelf) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters({{CpuCore::R0, 1000},
                      {CpuCore::R2, 100},
                      {CpuCore::ST, CpuCore::I | CpuCore::S}});

  // Main program
  state.code.AddValue(Encode(kTestOp_IST, CpuCore::R7, CpuCore::R2));
  state.code.AddValue(Encode(kTestOp_WAIT, CpuCore::R0));
  const uint16_t ip0 = state.code.GetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  state.code.SetAddress(100);
  state.code.AddValue(Encode(kTestOp_CBKS, 0));
  state.code.AddValue(Encode(kTestOp_NOP));
  const uint16_t ip1 = state.code.GetAddress();
  state.code.AddValue(Encode(kTestOp_IRET));
  state.code.AddValue(Encode(kTestOp_HALT));

  // Execute code
  ASSERT_TRUE(ExecuteUntil(
      [&] { return state.core.GetState() == CpuCore::State::kWaiting; }));
  EXPECT_EQ(state.ip, ip0);
  EXPECT_EQ(state.st, CpuCore::W | CpuCore::I | CpuCore::S);
  state.core.RaiseInterrupt(0);
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntilIp(ip0));
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::S);
  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::S);
  EXPECT_LT(state.core.GetCycles(), 1000);
}

TEST_F(CpuCoreTest, ResetDuringWait) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Initialize processor
  state.SetRegisters({{CpuCore::R0, 1000}, {CpuCore::ST, CpuCore::S}});

  // Main program
  state.code.AddValue(Encode(kTestOp_WAIT, CpuCore::R0));
  const uint16_t ip0 = state.code.GetAddress();
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(
      [&] { return state.core.GetState() == CpuCore::State::kWaiting; }));
  EXPECT_EQ(state.ip, ip0);
  EXPECT_EQ(state.st, CpuCore::W | CpuCore::S);

  auto lock = state.core.RequestLock();
  ASSERT_TRUE(lock->IsLocked());
  state.core.Reset(*lock, {.mask = CpuCore::ResetParams::MC, .mb = 0});
  lock.reset();

  ExecuteUntilHalt();
  EXPECT_EQ(state.ip, end_ip);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_LT(state.core.GetCycles(), 1000);
}

TEST_F(CpuCoreTest, CoreLockBlocks) {
  ASSERT_TRUE(Init({.num_cores = 2, .num_memory_banks = 2}));
  CoreState& state0 = GetState(0);
  CoreState& state1 = GetState(1);
  state0.ResetCore();
  state1.ResetCore({.mb = CpuCore::Banks().SetCode(1).ToWord()});
  MemAccessor mem0 = GetMemory(0);
  MemAccessor mem1 = GetMemory(1);

  // Initialize processor
  state0.SetRegisters({{CpuCore::R6, 1}});

  // Main program
  mem0.AddValue(Encode(kTestOp_CBK, CpuCore::DATA, 1));
  const uint16_t blocked_ip0 = mem0.GetAddress();
  mem0.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip0 = mem0.GetAddress();

  mem1.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip1 = mem1.GetAddress();

  // Start with core 1 locked, to block execution on core 0
  auto lock = state1.core.RequestLock();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(blocked_ip0));
  state1.Update();
  EXPECT_EQ(state1.mb, CpuCore::Banks().SetCode(1).ToWord());
  for (int i = 0; i < 10; ++i) {
    Execute(1);
  }
  state0.Update();
  EXPECT_EQ(state0.ip, blocked_ip0);
  EXPECT_EQ(state1.mb, CpuCore::Banks().SetCode(1).ToWord());

  // Unblock execution of core 0 by releasing the lock.
  lock.reset();

  ExecuteUntilHalt();
  state1.Update();
  EXPECT_EQ(state0.ip, end_ip0);
  EXPECT_EQ(state0.mb, 0);
  EXPECT_EQ(state1.ip, end_ip1);
  EXPECT_EQ(state1.mb, CpuCore::Banks().SetCode(1).SetData(1).ToWord());
}

TEST_F(CpuCoreTest, ReadOnlyRegisters) {
  ASSERT_TRUE(Init());
  CoreState& state = GetState();
  state.ResetCore();

  // Main program
  state.code.AddValue(Encode(kTestOp_LV, CpuCore::R7)).AddValue(0xFFFF);
  state.code.AddValue(Encode(kTestOp_ROREG, CpuCore::R7));
  const uint16_t ip1 = state.code.AddNopGetAddress();
  state.code.AddValue(Encode(kTestOp_HALT));
  const uint16_t end_ip = state.code.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntilIp(ip1));
  EXPECT_EQ(state.st, 0x0000);
  EXPECT_EQ(state.mb, 0x0000);
}

}  // namespace
}  // namespace oz3
