// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/cpu_core.h"

#include "gb/base/callback.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/instruction_compiler.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/memory_bank_config.h"
#include "oz3/core/port.h"
#include "oz3/core/processor.h"
#include "oz3/core/processor_config.h"

namespace oz3 {
namespace {

using ::testing::IsEmpty;

enum MicroTestOp : uint8_t {
  kTestOp_NOP,
  kTestOp_ZSCO,
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
  kTestOp_CMP,
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
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MOV(a,b);"},
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
    {.op = kTestOp_CMP,
     .op_name = "CMP",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "CMP(a,b);"
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
             "MOVI(R7,1);"
             "CLK(R7);"
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
             "MOVI(C0,1);"
             "CLK(C0);"
             "CLD(a,b);"
             "CUL;"},
    {.op = kTestOp_CST,
     .op_name = "CST",
     .arg1 = ArgType::kWordReg,
     .arg2 = ArgType::kWordReg,
     .code = "UL;"
             "MOVI(C0,1);"
             "CLK(C0);"
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

// Helper class to fetch and update the state of a CpuCore.
struct CoreState {
  explicit CoreState(CpuCore& core) : core(core) { Update(); }

  void ResetCore(CpuCore::ResetParams reset_params = {}) {
    auto lock = core.RequestLock();
    EXPECT_TRUE(lock->IsLocked());
    core.Reset(*lock, reset_params);
    lock.reset();
  }

  void Update() {
    core.GetRegisters(r);
    code_bank = core.GetMemoryBank(CpuCore::CODE);
    stack_bank = core.GetMemoryBank(CpuCore::STACK);
    data_bank = core.GetMemoryBank(CpuCore::DATA);
    extra_bank = core.GetMemoryBank(CpuCore::EXTRA);
  }

  CpuCore& core;

  CpuCore::Registers r;
  const uint16_t& r0 = r[CpuCore::R0];
  const uint16_t& r1 = r[CpuCore::R1];
  const uint16_t& r2 = r[CpuCore::R2];
  const uint16_t& r3 = r[CpuCore::R3];
  const uint16_t& r4 = r[CpuCore::R4];
  const uint16_t& r5 = r[CpuCore::R5];
  const uint16_t& r6 = r[CpuCore::R6];
  const uint16_t& r7 = r[CpuCore::R7];
  uint32_t d0() const {
    return static_cast<uint32_t>(r[CpuCore::D0]) |
           (static_cast<uint32_t>(r[CpuCore::D0 + 1]) << 16);
  }
  uint32_t d1() const {
    return static_cast<uint32_t>(r[CpuCore::D1]) |
           (static_cast<uint32_t>(r[CpuCore::D1 + 1]) << 16);
  }
  uint32_t d2() const {
    return static_cast<uint32_t>(r[CpuCore::D2]) |
           (static_cast<uint32_t>(r[CpuCore::D2 + 1]) << 16);
  }
  uint32_t d3() const {
    return static_cast<uint32_t>(r[CpuCore::D3]) |
           (static_cast<uint32_t>(r[CpuCore::D3 + 1]) << 16);
  }
  const uint16_t& bc = r[CpuCore::BC];
  const uint16_t& bs = r[CpuCore::BS];
  const uint16_t& bd = r[CpuCore::BD];
  const uint16_t& be = r[CpuCore::BE];
  const uint16_t& mb = r[CpuCore::MB];
  const uint16_t& pc = r[CpuCore::IP];
  const uint16_t& bp = r[CpuCore::FP];
  const uint16_t& sp = r[CpuCore::SP];

  const uint16_t& st = r[CpuCore::ST];

  const uint16_t& c0 = r[CpuCore::C0];
  const uint16_t& c1 = r[CpuCore::C1];
  const uint16_t& c2 = r[CpuCore::C2];

  MemoryBank* code_bank = nullptr;
  MemoryBank* stack_bank = nullptr;
  MemoryBank* data_bank = nullptr;
  MemoryBank* extra_bank = nullptr;
};

// Helper class to sequentially write code and read/write data ina MemoryBank.
class MemAccessor {
 public:
  MemAccessor(MemoryBank& memory_bank, uint16_t address = 0)
      : mem_(memory_bank.GetMem(0, kMemoryBankMaxSize)), address_(address) {}

  uint16_t GetAddress() const { return address_; }

  MemAccessor& SetAddress(uint16_t address) {
    address_ = address;
    return *this;
  }

  MemAccessor& AddValue(uint16_t value) {
    mem_[address_++] = value;
    return *this;
  }

  MemAccessor& AddCode(uint8_t op, uint16_t a = 0, uint16_t b = 0) {
    absl::Span<const InstructionDef> instructions(kMicroTestInstructions);
    auto it =
        std::find_if(instructions.begin(), instructions.end(),
                     [op](const InstructionDef& def) { return def.op == op; });
    CHECK(it != instructions.end()) << "Unknown op: " << static_cast<int>(op);
    mem_[address_++] = it->Encode(a, b);
    return *this;
  }

  uint16_t GetValue() { return mem_[address_++]; }

 private:
  absl::Span<uint16_t> mem_;
  uint16_t address_;
};

bool ExecuteUntil(Processor& processor, CoreState& state,
                  gb::Callback<bool()> condition) {
  do {
    processor.Execute(1);
    if (state.core.GetState() == CpuCore::State::kIdle) {
      return false;
    }
    state.Update();
    if (state.pc > 1000) {
      return false;
    }
  } while (!condition());
  return true;
}

void ExecuteUntilHalt(Processor& processor, CoreState& state) {
  ExecuteUntil(processor, state, [] { return false; });
}

TEST(CpuCoreTest, CpuCoreInitialState) {
  CpuCore core(CpuCoreConfig::Default());

  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);

  CoreState state(core);
  for (int i = 0; i < CpuCore::kRegisterCount; ++i) {
    EXPECT_EQ(state.r[i], 0) << "r[" << i << "]";
  }
  EXPECT_EQ(state.code_bank, nullptr);
  EXPECT_EQ(state.stack_bank, nullptr);
  EXPECT_EQ(state.data_bank, nullptr);
  EXPECT_EQ(state.extra_bank, nullptr);
}

TEST(CpuCoreTest, BanksSetAfterAttachProcessor) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  EXPECT_EQ(state.code_bank, processor.GetMemory(0));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(0));
  EXPECT_EQ(state.data_bank, processor.GetMemory(0));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(0));
}

TEST(CpuCoreTest, ResetStartsCoreRunning) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);

  CoreState state(core);
  state.ResetCore();
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);

  state.Update();
  for (int i = 0; i < CpuCore::kRegisterCount; ++i) {
    EXPECT_EQ(state.r[i], 0) << "r[" << i << "]";
  }
}

TEST(CpuCoreTest, ResetWithSetRegisters) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::IP, 100);
  core.SetWordRegister(*lock, CpuCore::FP, 200);
  core.SetWordRegister(*lock, CpuCore::SP, 300);
  lock.reset();

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
  EXPECT_EQ(state.pc, 0);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.bp, 0);
  EXPECT_EQ(state.sp, 0);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code_bank, processor.GetMemory(1));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(2));
  EXPECT_EQ(state.data_bank, processor.GetMemory(3));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(4));

  lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::IP, 100);
  core.SetWordRegister(*lock, CpuCore::FP, 200);
  core.SetWordRegister(*lock, CpuCore::SP, 300);
  lock.reset();

  // Change only the code bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::MC,
      .mb = CpuCore::Banks::Default().SetCode(15).ToWord()});
  state.Update();
  EXPECT_EQ(state.mb, banks.SetCode(15).ToWord());
  EXPECT_EQ(state.bc, 5);
  EXPECT_EQ(state.pc, 100);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.bp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(2));
  EXPECT_EQ(state.data_bank, processor.GetMemory(3));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(4));

  lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::IP, 100);
  core.SetWordRegister(*lock, CpuCore::FP, 200);
  core.SetWordRegister(*lock, CpuCore::SP, 300);
  lock.reset();

  // Change only the stack bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::MS,
      .mb = CpuCore::Banks::Default().SetStack(14).ToWord()});
  state.Update();
  EXPECT_EQ(state.mb, banks.SetStack(14).ToWord());
  EXPECT_EQ(state.bc, 5);
  EXPECT_EQ(state.pc, 100);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.bp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(3));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(4));

  lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::IP, 100);
  core.SetWordRegister(*lock, CpuCore::FP, 200);
  core.SetWordRegister(*lock, CpuCore::SP, 300);
  lock.reset();

  // Change only the data bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::MD,
      .mb = CpuCore::Banks::Default().SetData(13).ToWord()});
  state.Update();
  EXPECT_EQ(state.mb, banks.SetData(13).ToWord());
  EXPECT_EQ(state.bc, 5);
  EXPECT_EQ(state.pc, 100);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.bp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(4));

  lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::IP, 100);
  core.SetWordRegister(*lock, CpuCore::FP, 200);
  core.SetWordRegister(*lock, CpuCore::SP, 300);
  lock.reset();

  // Change only the extra bank and reset again.
  state.ResetCore(CpuCore::ResetParams{
      .mask = CpuCore::ResetParams::ME,
      .mb = CpuCore::Banks::Default().SetExtra(12).ToWord()});
  state.Update();
  EXPECT_EQ(state.mb, banks.SetExtra(12).ToWord());
  EXPECT_EQ(state.bc, 5);
  EXPECT_EQ(state.pc, 100);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.bp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(12));

  lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::IP, 100);
  core.SetWordRegister(*lock, CpuCore::FP, 200);
  core.SetWordRegister(*lock, CpuCore::SP, 300);
  lock.reset();

  // Change only the base code offset and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::RBC, .bc = 10});
  state.Update();
  EXPECT_EQ(state.mb, banks.ToWord());
  EXPECT_EQ(state.bc, 10);
  EXPECT_EQ(state.pc, 0);
  EXPECT_EQ(state.bs, 6);
  EXPECT_EQ(state.bp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(12));

  lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::IP, 100);
  core.SetWordRegister(*lock, CpuCore::FP, 200);
  core.SetWordRegister(*lock, CpuCore::SP, 300);
  lock.reset();

  // Change only the stack pointer and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::RBS, .bs = 9});
  state.Update();
  EXPECT_EQ(state.mb, banks.ToWord());
  EXPECT_EQ(state.bc, 10);
  EXPECT_EQ(state.pc, 100);
  EXPECT_EQ(state.bs, 9);
  EXPECT_EQ(state.bp, 0);
  EXPECT_EQ(state.sp, 0);
  EXPECT_EQ(state.bd, 7);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(12));

  lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::IP, 100);
  core.SetWordRegister(*lock, CpuCore::FP, 200);
  core.SetWordRegister(*lock, CpuCore::SP, 300);
  lock.reset();

  // Change only the data pointer and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::RBD, .bd = 8});
  state.Update();
  EXPECT_EQ(state.mb, banks.ToWord());
  EXPECT_EQ(state.bc, 10);
  EXPECT_EQ(state.pc, 100);
  EXPECT_EQ(state.bs, 9);
  EXPECT_EQ(state.bp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 8);
  EXPECT_EQ(state.be, 8);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(12));

  // Change only the extra pointer and reset again.
  state.ResetCore(
      CpuCore::ResetParams{.mask = CpuCore::ResetParams::RBE, .be = 7});
  state.Update();
  EXPECT_EQ(state.mb, banks.ToWord());
  EXPECT_EQ(state.bc, 10);
  EXPECT_EQ(state.pc, 100);
  EXPECT_EQ(state.bs, 9);
  EXPECT_EQ(state.bp, 200);
  EXPECT_EQ(state.sp, 300);
  EXPECT_EQ(state.bd, 8);
  EXPECT_EQ(state.be, 7);
  EXPECT_EQ(state.code_bank, processor.GetMemory(15));
  EXPECT_EQ(state.stack_bank, processor.GetMemory(14));
  EXPECT_EQ(state.data_bank, processor.GetMemory(13));
  EXPECT_EQ(state.extra_bank, processor.GetMemory(12));
}

TEST(CpuCoreTest, RunsAfterReset) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  // This runs the CPU to start executing the first instruction (NOP) until it
  // hits its first synchronization point which is UL.
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_TRUE(memory_bank.IsLocked());
  state.Update();
  EXPECT_EQ(state.pc, 1);

  // The processor has only advanced one cycle
  EXPECT_EQ(processor.GetCycles(), 1);

  // Executing one cycle at a time, will not hit the processor (no cycles
  // increase) until the processor reaches the CpuCore's cycles.
  while (core.GetCycles() > processor.GetCycles()) {
    processor.Execute(1);
    EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction)
        << "Cycles: " << processor.GetCycles();
    EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles)
        << "Cycles: " << processor.GetCycles();
    EXPECT_TRUE(memory_bank.IsLocked()) << "Cycles: " << processor.GetCycles();
  }

  // Executing one more cycle will trigger the next instruction (another NOP).
  // This will result in a back to back unlock and lock, which succeed trivially
  // as there is only one core.
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2);
  EXPECT_TRUE(memory_bank.IsLocked());
  state.Update();
  EXPECT_EQ(state.pc, 2);
}

TEST(CpuCoreTest, CoreLockPreventsExecution) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);

  auto lock = core.RequestLock();
  EXPECT_TRUE(lock->IsLocked());
  core.Reset(*lock, CpuCore::ResetParams{});

  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);

  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);
  EXPECT_EQ(core.GetCycles(), 1);
  EXPECT_FALSE(memory_bank.IsLocked());

  lock.reset();

  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_TRUE(memory_bank.IsLocked());
}

TEST(CpuCoreTest, CoreLockDeferredUntilEndOfInstruction) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);

  // We are now running an instruction, so the core cannot be locked until it
  // completes.
  auto lock = core.RequestLock();
  EXPECT_FALSE(lock->IsLocked());

  // Run enough cycles to start the next instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_TRUE(lock->IsLocked());
}

TEST(CpuCoreTest, MemoryLockBlocksFetch) {
  Processor processor(ProcessorConfig::OneCore());
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  // Lock the memory bank, which will prevent the core from fetching the next
  // instruction.
  auto memory_lock = memory_bank.RequestLock();
  EXPECT_TRUE(memory_lock->IsLocked());

  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kFetchInstruction);
  EXPECT_EQ(core.GetCycles(), 1);

  // Unlock the memory bank, which will allow the core to run
  memory_lock.reset();
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_TRUE(memory_bank.IsLocked());
}

TEST(CpuCoreTest, MultiCoreRoundRobinsExecution) {
  Processor processor(ProcessorConfig::MultiCore(3));
  MemoryBank& memory_bank = *processor.GetMemory(0);
  CoreState state0(*processor.GetCore(0));
  state0.ResetCore();
  auto lock = state0.core.RequestLock();
  state0.core.SetWordRegister(*lock, CpuCore::IP, 10);
  lock.reset();
  CoreState state1(*processor.GetCore(1));
  state1.ResetCore();
  lock = state1.core.RequestLock();
  state1.core.SetWordRegister(*lock, CpuCore::IP, 20);
  lock.reset();
  CoreState state2(*processor.GetCore(2));
  state2.ResetCore();
  lock = state2.core.RequestLock();
  state2.core.SetWordRegister(*lock, CpuCore::IP, 30);
  lock.reset();

  // Run the processor for one fetch and decode cycle. This will result in the
  // first core locking memory and decoding the first NOP. The other cores
  // should be blocked waiting on the first core to release its lock on the CODE
  // memory bank.
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  state0.Update();
  EXPECT_EQ(state0.pc, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 20);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 30);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Run the processor for one more cycle. This will allow the first core to
  // finish executing the NOP, and the second core to lock memory and decode the
  // first NOP. This blocks the first core from fetching the next instruction.
  processor.Execute(1);
  state0.Update();
  EXPECT_EQ(state0.pc, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kFetchInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kRunInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 30);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Run the processor for a fetch and decode cycle. This will allow the second
  // core to finish executing the NOP, and the third core to lock memory and
  // decode the first NOP. This blocks the second core from fetching the next
  // instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  state0.Update();
  EXPECT_EQ(state0.pc, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kFetchInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 1);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 31);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kRunInstruction);

  // One more round will cycle back to the first core. However, as the first
  // core executed first, it needs to wait another cycle to gain the lock.
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  state0.Update();
  EXPECT_EQ(state0.pc, 11);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kFetchInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 1);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 31);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 1);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // One more cycle gives the first core the lock, and it can execute the next
  // instruction.
  processor.Execute(1);
  state0.Update();
  EXPECT_EQ(state0.pc, 12);
  EXPECT_EQ(state0.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 21);
  EXPECT_EQ(state1.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 2);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 31);
  EXPECT_EQ(state2.core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 2);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);

  // Finish fetch and decode for the first core, to catch all cores up to the
  // first core.
  processor.Execute(kCpuCoreFetchAndDecodeCycles - 1);
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
  processor.Execute(round_cycles * 5);
  state0.Update();
  EXPECT_EQ(state0.pc, 17);
  EXPECT_EQ(state0.core.GetCycles(), start_cycles + round_cycles * 5);
  EXPECT_EQ(state0.core.GetState(), CpuCore::State::kRunInstruction);
  state1.Update();
  EXPECT_EQ(state1.pc, 26);
  EXPECT_EQ(state1.core.GetCycles(), start_cycles + round_cycles * 5);
  EXPECT_EQ(state1.core.GetState(), CpuCore::State::kFetchInstruction);
  state2.Update();
  EXPECT_EQ(state2.pc, 36);
  EXPECT_EQ(state2.core.GetCycles(), start_cycles + round_cycles * 5);
  EXPECT_EQ(state2.core.GetState(), CpuCore::State::kFetchInstruction);
}

TEST(CpuCoreTest, MicrocodeTestInstructionsCompile) {
  std::string error;
  EXPECT_NE(CompileInstructionSet({kMicroTestInstructions}, &error), nullptr)
      << error;
}

TEST(CpuCoreTest, LdOpInFetchExtendsCodeSize) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  MemAccessor mem(memory_bank);
  mem.AddCode(kTestOp_LWORD, CpuCore::R0);
  mem.AddValue(42);
  mem.AddCode(kTestOp_LDWORD, CpuCore::D0);
  mem.AddValue(0);
  mem.AddValue(1);
  mem.AddCode(kTestOp_HALT);

  // Run the processor for one fetch and decode cycle. Since the LWORD OP does
  // an LD microcode before unlocking the CODE memory bank, it will execute as
  // well.
  processor.Execute(1);
  state.Update();
  EXPECT_EQ(state.pc, 2);  // Advances 2 due to LD
  EXPECT_EQ(state.r0, 42);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);

  // Run the processor for one fetch and decode cycle. Since the LWORD OP does
  // an LD microcode before unlocking the CODE memory bank, it will execute as
  // well.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 5);  // Advances 3 due to two LDs
  EXPECT_EQ(state.d0(), 0x10000);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 3);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);

  // Complete this instruction and run the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 3);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, HaltOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  MemAccessor mem(memory_bank);
  mem.AddCode(kTestOp_HALT);

  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);

  // The core should not execute any more instructions.
  processor.Execute(100);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 101);
  state.Update();
  EXPECT_EQ(state.pc, 1);
}

TEST(CpuCoreTest, WaitOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  MemoryBank& memory_bank = *processor.GetMemory(0);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0);
  core.SetWordRegister(*lock, CpuCore::R1, 1);
  core.SetWordRegister(*lock, CpuCore::R2, kCpuCoreFetchAndDecodeCycles);
  core.SetWordRegister(*lock, CpuCore::R3, kCpuCoreFetchAndDecodeCycles + 1);
  core.SetWordRegister(*lock, CpuCore::R4, kCpuCoreFetchAndDecodeCycles + 2);
  lock.reset();

  MemAccessor mem(memory_bank);
  mem.AddCode(kTestOp_WAIT, CpuCore::R0);
  mem.AddCode(kTestOp_WAIT, CpuCore::R1);
  mem.AddCode(kTestOp_WAIT, CpuCore::R2);
  mem.AddCode(kTestOp_WAIT, CpuCore::R3);
  mem.AddCode(kTestOp_WAIT, CpuCore::R4);
  mem.AddCode(kTestOp_HALT);

  // Execute through the first three WAITs which shouldn't actually put the CPU
  // into a wait state, as the wait times are too small.
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 4);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r0, 3);  // Ran 3 cycles longer than requested
  EXPECT_EQ(state.r1, 2);  // Ran 2 cycles longer than requested
  EXPECT_EQ(state.r2, 0);  // Ran exactly the requested number of cycles
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4);

  // The next wait (which is already fetched and decoded) should take exactly
  // one cycle, and then return the CPU to the kStartInstruction state.
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 1);
  EXPECT_EQ(state.r3, 0);

  // The final wait takes two cycles, so we wait 1 extra cycle to get through
  // the fetch+decode and the first cycle of waiting.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kWaiting);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 2);

  // The final cycle of waiting will return the CPU to the kStartInstruction
  // state.
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 3);
  EXPECT_EQ(state.r4, 0);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 4);
}

TEST(CpuCoreTest, AdrLdStOps) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet())
                          .SetMemoryBank(1, MemoryBankConfig::MaxRam())
                          .SetMemoryBank(2, MemoryBankConfig::MaxRam())
                          .SetMemoryBank(3, MemoryBankConfig::MaxRam()));
  CpuCore& core = *processor.GetCore(0);
  CpuCore::Banks banks = {.code = 0, .stack = 1, .data = 2, .extra = 3};
  MemoryBank& code_bank = *processor.GetMemory(banks.code);
  MemoryBank& stack_bank = *processor.GetMemory(banks.stack);
  MemoryBank& data_bank = *processor.GetMemory(banks.data);
  MemoryBank& extra_bank = *processor.GetMemory(banks.extra);
  CoreState state(core);
  state.ResetCore({.mb = banks.ToWord()});

  MemAccessor code_mem(code_bank);
  code_mem.AddCode(kTestOp_LCODE, CpuCore::R0, 15);
  code_mem.AddCode(kTestOp_LSTACK, CpuCore::R1, 15);
  code_mem.AddCode(kTestOp_LDATA, CpuCore::R2, 15);
  code_mem.AddCode(kTestOp_LEXTRA, CpuCore::R3, 15);
  code_mem.AddCode(kTestOp_SCODE, 20, CpuCore::R1);
  code_mem.AddCode(kTestOp_SSTACK, 20, CpuCore::R2);
  code_mem.AddCode(kTestOp_SDATA, 20, CpuCore::R3);
  code_mem.AddCode(kTestOp_SEXTRA, 20, CpuCore::R0);
  code_mem.AddCode(kTestOp_PCODE, 25, CpuCore::R1);
  code_mem.AddCode(kTestOp_PSTACK, 25, CpuCore::R2);
  code_mem.AddCode(kTestOp_PDATA, 25, CpuCore::R3);
  code_mem.AddCode(kTestOp_PEXTRA, 25, CpuCore::R0);
  code_mem.AddCode(kTestOp_HALT);

  code_mem.SetAddress(15).AddValue(0x1234);

  MemAccessor stack_mem(stack_bank, 15);
  stack_mem.AddValue(0x5678);

  MemAccessor data_mem(data_bank, 15);
  data_mem.AddValue(0x9abc);

  MemAccessor extra_mem(extra_bank, 15);
  extra_mem.AddValue(0xdef0);

  // Execute the LCODE instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 2);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x1234);

  // Execute the LSTACK instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x5678);

  // Execute the LDATA instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0x9abc);

  // Execute the LEXTRA instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0xdef0);

  // Execute the SCODE instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 10);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(code_mem.SetAddress(20).GetValue(), 0x5678);

  // Execute the SSTACK instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 12);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(stack_mem.SetAddress(20).GetValue(), 0x9abc);

  // Execute the SDATA instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 7 + 14);
  state.Update();
  EXPECT_EQ(state.pc, 7);
  EXPECT_EQ(data_mem.SetAddress(20).GetValue(), 0xdef0);

  // Execute the SEXTRA instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 16);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(extra_mem.SetAddress(20).GetValue(), 0x1234);

  // Execute the PCODE instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 9 + 18);
  state.Update();
  EXPECT_EQ(state.pc, 9);
  EXPECT_EQ(code_mem.SetAddress(24).GetValue(), 0x5678);

  // Execute the PSTACK instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 10 + 20);
  state.Update();
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(stack_mem.SetAddress(24).GetValue(), 0x9abc);

  // Execute the PDATA instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 11 + 22);
  state.Update();
  EXPECT_EQ(state.pc, 11);
  EXPECT_EQ(data_mem.SetAddress(24).GetValue(), 0xdef0);

  // Execute the PEXTRA instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 12 + 24);
  state.Update();
  EXPECT_EQ(state.pc, 12);
  EXPECT_EQ(extra_mem.SetAddress(24).GetValue(), 0x1234);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, RegisterMemoryStore) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet())
                          .SetMemoryBank(1, MemoryBankConfig::MaxRam())
                          .SetMemoryBank(2, MemoryBankConfig::MaxRam())
                          .SetMemoryBank(3, MemoryBankConfig::MaxRam()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  CpuCore::Banks banks = {.code = 0, .stack = 1, .data = 2, .extra = 3};
  MemoryBank& code_bank = *processor.GetMemory(banks.code);
  MemoryBank& stack_bank = *processor.GetMemory(banks.stack);
  MemoryBank& data_bank = *processor.GetMemory(banks.data);
  MemoryBank& extra_bank = *processor.GetMemory(banks.extra);
  state.ResetCore({.mb = banks.ToWord()});

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 1000);
  lock.reset();

  MemAccessor code_mem(code_bank);
  MemAccessor stack_mem(stack_bank);
  MemAccessor data_mem(data_bank);
  MemAccessor extra_mem(extra_bank);

  code_mem.AddCode(kTestOp_SREG, CpuCore::R0, CpuCore::R0);
  code_mem.AddCode(kTestOp_SREG, CpuCore::R1, CpuCore::R0);
  code_mem.AddCode(kTestOp_SREG, CpuCore::R2, CpuCore::R0);
  code_mem.AddCode(kTestOp_SREG, CpuCore::R3, CpuCore::R0);
  code_mem.AddCode(kTestOp_SREG, CpuCore::R4, CpuCore::R0);
  code_mem.AddCode(kTestOp_SREG, CpuCore::R5, CpuCore::R0);
  code_mem.AddCode(kTestOp_SREG, CpuCore::R6, CpuCore::R0);
  code_mem.AddCode(kTestOp_SREG, CpuCore::R7, CpuCore::R0);
  code_mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = code_mem.GetAddress();

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
  EXPECT_EQ(data_mem.SetAddress(1000).GetValue(), 1000);
  EXPECT_EQ(data_mem.SetAddress(1001).GetValue(), 1001);
  EXPECT_EQ(data_mem.SetAddress(1002).GetValue(), 1002);
  EXPECT_EQ(data_mem.SetAddress(1003).GetValue(), 1003);
  EXPECT_EQ(extra_mem.SetAddress(1004).GetValue(), 1004);
  EXPECT_EQ(extra_mem.SetAddress(1005).GetValue(), 1005);
  EXPECT_EQ(stack_mem.SetAddress(1006).GetValue(), 1006);
  EXPECT_EQ(stack_mem.SetAddress(1007).GetValue(), 1007);
}

TEST(CpuCoreTest, LkWhenLocked) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet())
                          .SetMemoryBank(1, MemoryBankConfig::MaxRam()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  CpuCore::Banks banks = {.code = 0, .data = 1};
  MemoryBank& code_bank = *processor.GetMemory(banks.code);
  MemoryBank& data_bank = *processor.GetMemory(banks.data);
  state.ResetCore({.mb = banks.ToWord()});

  MemAccessor code_mem(code_bank);
  code_mem.AddCode(kTestOp_LDATA, CpuCore::R0, 10);
  code_mem.AddCode(kTestOp_HALT);

  MemAccessor data_mem(data_bank, 10);
  data_mem.SetAddress(10).AddValue(0x1234);

  // Lock the memory bank, which will prevent the core from fetching the next
  // instruction.
  auto data_lock = data_bank.RequestLock();
  EXPECT_TRUE(data_lock->IsLocked());

  processor.Execute(kCpuCoreFetchAndDecodeCycles + 10);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0);

  // Unlock the memory bank, which will allow the core to run
  data_lock.reset();
  processor.Execute(2);  // ADR, LD, UL
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x1234);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, MovStOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet())
                          .SetMemoryBank(1, MemoryBankConfig::MaxRam()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  CpuCore::Banks banks = {.code = 0, .data = 1};
  MemoryBank& code_bank = *processor.GetMemory(banks.code);
  MemoryBank& data_bank = *processor.GetMemory(banks.data);
  state.ResetCore({.mb = banks.ToWord()});

  auto lock = core.RequestLock();
  EXPECT_TRUE(lock->IsLocked());
  core.SetWordRegister(*lock, CpuCore::R0, 42);
  lock.reset();

  MemAccessor code_mem(code_bank);
  code_mem.AddCode(kTestOp_MOV_ST, 10, CpuCore::R0);
  code_mem.AddCode(kTestOp_HALT);

  MemAccessor data_mem(data_bank, 10);
  data_mem.SetAddress(10).AddValue(0x1234);

  // Execute through the MOV instruction. This should not execute the LK.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_FALSE(data_bank.IsLocked());

  // Execute the LK instruction, and everything up to the UL.
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 3);
  EXPECT_TRUE(data_bank.IsLocked());
  EXPECT_EQ(data_mem.SetAddress(10).GetValue(), 42);

  // Process the UL instruction, and fetch the HALT instruction
  processor.Execute(2);
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 3);
  EXPECT_FALSE(data_bank.IsLocked());

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, MoviOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_MOVI, CpuCore::R3);
  mem.AddCode(kTestOp_HALT);

  // Execute the MOVI instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r3, 42);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, MstsMstcOps) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_MSSC);
  mem.AddCode(kTestOp_HALT);

  // Execute the MSS / MSC instructions.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 8);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 8);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, CpuCore::Z);
  EXPECT_EQ(state.r1, CpuCore::Z | CpuCore::S);
  EXPECT_EQ(state.r2, CpuCore::Z | CpuCore::S | CpuCore::C);
  EXPECT_EQ(state.r3, CpuCore::Z | CpuCore::S | CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r4, CpuCore::S | CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r5, CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r6, CpuCore::O);
  EXPECT_EQ(state.r7, 0);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, MstxOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_MSX);
  mem.AddCode(kTestOp_HALT);

  // Execute the MSX instructions.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 8);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 8);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, CpuCore::Z);
  EXPECT_EQ(state.r1, CpuCore::Z | CpuCore::S);
  EXPECT_EQ(state.r2, CpuCore::Z | CpuCore::S | CpuCore::C);
  EXPECT_EQ(state.r3, CpuCore::Z | CpuCore::S | CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r4, CpuCore::S | CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r5, CpuCore::C | CpuCore::O);
  EXPECT_EQ(state.r6, CpuCore::O);
  EXPECT_EQ(state.r7, 0);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, MstiOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_MSI);
  mem.AddCode(kTestOp_HALT);

  // Execute the MSI instructions.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 4);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, CpuCore::I);
  EXPECT_EQ(state.r1, 0);
  EXPECT_EQ(state.r2, CpuCore::I);
  EXPECT_EQ(state.r3, 0);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, MstrOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_MSR1);
  mem.AddCode(kTestOp_MSR2);
  mem.AddCode(kTestOp_HALT);

  // Execute the MSR1 instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 8);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 8);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 1 | CpuCore::I);
  EXPECT_EQ(state.r1, 3 | CpuCore::I);
  EXPECT_EQ(state.r2, 7 | CpuCore::I);
  EXPECT_EQ(state.r3, 15 | CpuCore::I);
  EXPECT_EQ(state.r4, 14 | CpuCore::I);
  EXPECT_EQ(state.r5, 12 | CpuCore::I);
  EXPECT_EQ(state.r6, 8 | CpuCore::I);
  EXPECT_EQ(state.r7, 0 | CpuCore::I);

  // Execute the MSR2 instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 8);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 16);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.r1, 0);
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 0);
  EXPECT_EQ(state.r4, 15);
  EXPECT_EQ(state.r5, 15);
  EXPECT_EQ(state.r6, 15);
  EXPECT_EQ(state.r7, 15);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, AddiOp1) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R2, 0xFFFE);
  core.SetWordRegister(*lock, CpuCore::R3, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R4, 0x7FFF);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_ADDI, CpuCore::R0);  // R0: 0x0000, IMM: 1
  mem.AddCode(kTestOp_ADDI, CpuCore::R1);  // R1: 0x0001, IMM: 1
  mem.AddCode(kTestOp_ADDI, CpuCore::R2);  // R2: 0xFFFE, IMM: 1
  mem.AddCode(kTestOp_ADDI, CpuCore::R3);  // R3: 0xFFFF, IMM: 1
  mem.AddCode(kTestOp_ADDI, CpuCore::R4);  // R4: 0x7FFF, IMM: 1
  mem.AddCode(kTestOp_HALT);

  // Execute the ADDI(R0,1) instruction. R0 = 0x0000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADDI(R1,1) instruction. R1 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADDI(R2,1) instruction. R2 = 0xFFFE
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the ADDI(R3,1) instruction. R3 = 0xFFFF
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADDI(R4,1) instruction. R4 = 0x7FFF
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::O);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, AddiOp2) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R2, 0x0002);
  core.SetWordRegister(*lock, CpuCore::R3, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R4, 0x8000);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_SUBI, CpuCore::R0);  // R0: 0x0000, IMM: -1
  mem.AddCode(kTestOp_SUBI, CpuCore::R1);  // R1: 0x0001, IMM: -1
  mem.AddCode(kTestOp_SUBI, CpuCore::R2);  // R2: 0x0002, IMM: -1
  mem.AddCode(kTestOp_SUBI, CpuCore::R3);  // R3: 0xFFFF, IMM: -1
  mem.AddCode(kTestOp_SUBI, CpuCore::R4);  // R4: 0x8000, IMM: -1
  mem.AddCode(kTestOp_HALT);

  // Execute the ADDI(R0,-1) instruction. R0 = 0x0000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the ADDI(R1,-1) instruction. R1 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADDI(R2,-1) instruction. R2 = 0x0002
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the ADDI(R3,-1) instruction. R3 = 0xFFFF
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0xFFFE);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the ADDI(R4,-1) instruction. R4 = 0x8000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r4, 0x7FFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C | CpuCore::O);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, AddOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R2, 0xFFFE);
  core.SetWordRegister(*lock, CpuCore::R3, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R4, 0x7FFF);
  core.SetWordRegister(*lock, CpuCore::R7, 0x0001);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_ADD, CpuCore::R0, CpuCore::R7);  // R0: 0x0000, R7: 0x0001
  mem.AddCode(kTestOp_ADD, CpuCore::R1, CpuCore::R7);  // R1: 0x0001, R7: 0x0001
  mem.AddCode(kTestOp_ADD, CpuCore::R2, CpuCore::R7);  // R2: 0xFFFE, R7: 0x0001
  mem.AddCode(kTestOp_ADD, CpuCore::R3, CpuCore::R7);  // R3: 0xFFFF, R7: 0x0001
  mem.AddCode(kTestOp_ADD, CpuCore::R4, CpuCore::R7);  // R4: 0x7FFF, R7: 0x0001
  mem.AddCode(kTestOp_HALT);

  // Execute the ADD(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADD(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADD(R2,R7) instruction. R2 = 0xFFFE, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the ADD(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADD(R4,R7) instruction. R4 = 0x8000, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::O);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, SubOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R2, 0x0002);
  core.SetWordRegister(*lock, CpuCore::R3, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R4, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R7, 0x0001);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_SUB, CpuCore::R0, CpuCore::R7);  // R0: 0x0000, R7: 0x0001
  mem.AddCode(kTestOp_SUB, CpuCore::R1, CpuCore::R7);  // R1: 0x0001, R7: 0x0001
  mem.AddCode(kTestOp_SUB, CpuCore::R2, CpuCore::R7);  // R2: 0x0002, R7: 0x0001
  mem.AddCode(kTestOp_SUB, CpuCore::R3, CpuCore::R7);  // R3: 0xFFFF, R7: 0x0001
  mem.AddCode(kTestOp_SUB, CpuCore::R4, CpuCore::R7);  // R4: 0x8000, R7: 0x0001
  mem.AddCode(kTestOp_HALT);

  // Execute the SUB(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the SUB(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the SUB(R2,R7) instruction. R2 = 0x0002, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the SUB(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0xFFFE);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the SUB(R4,R7) instruction. R4 = 0x8000, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r4, 0x7FFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C | CpuCore::O);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, AdcOpNoCarry) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R2, 0xFFFE);
  core.SetWordRegister(*lock, CpuCore::R3, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R4, 0x7FFF);
  core.SetWordRegister(*lock, CpuCore::R5, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R6, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R7, 0x0001);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R0, CpuCore::R7);  // R0: 0x0000, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R1, CpuCore::R7);  // R1: 0x0001, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R2, CpuCore::R7);  // R2: 0xFFFE, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R3, CpuCore::R7);  // R3: 0xFFFF, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R4, CpuCore::R7);  // R4: 0x7FFF, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R5, CpuCore::R6);  // R5: 0x0001, R6: 0xFFFF
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R5, CpuCore::R5);  // R5: 0x0000
  mem.AddCode(kTestOp_HALT);

  // Execute the ADC(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r0, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADC(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r1, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADC(R2,R7) instruction. R2 = 0xFFFE, R7 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r2, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the ADC(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r3, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADC(R4,R7) instruction. R4 = 0x7FFF, R7 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 10 + 10);
  state.Update();
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::O);

  // Execute the ADC(R5,R6) instruction. R5 = 0x0001, R6 = 0xFFFF, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 12 + 12);
  state.Update();
  EXPECT_EQ(state.pc, 12);
  EXPECT_EQ(state.r5, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADC(R5,R5) instruction. R5 = 0x0000, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 14 + 14);
  state.Update();
  EXPECT_EQ(state.pc, 14);
  EXPECT_EQ(state.r5, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, AdcOpWithCarry) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R1, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R2, 0xFFFD);
  core.SetWordRegister(*lock, CpuCore::R3, 0xFFFE);
  core.SetWordRegister(*lock, CpuCore::R4, 0x7FFE);
  core.SetWordRegister(*lock, CpuCore::R5, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R6, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R7, 0x0001);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R0, CpuCore::R7);  // R0: 0xFFFF, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R1, CpuCore::R7);  // R1: 0x0000, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R2, CpuCore::R7);  // R2: 0xFFFD, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R3, CpuCore::R7);  // R3: 0xFFFE, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R4, CpuCore::R7);  // R4: 0x7FFE, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R5, CpuCore::R6);  // R5: 0x0000, R6: 0xFFFF
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_ADC, CpuCore::R5, CpuCore::R5);  // R5: 0x0000
  mem.AddCode(kTestOp_HALT);

  // Execute the ADC(R0,R7) instruction. R0 = 0xFFFF, R7 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r0, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the ADC(R1,R7) instruction. R1 = 0x0000, R7 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r1, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the ADC(R2,R7) instruction. R2 = 0xFFFD, R7 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r2, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the ADC(R3,R7) instruction. R3 = 0xFFFE, R7 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r3, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADC(R4,R7) instruction. R4 = 0x7FFE, R7 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 10 + 10);
  state.Update();
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::O);

  // Execute the ADC(R5,R6) instruction. R5 = 0x0000, R6 = 0xFFFF, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 12 + 12);
  state.Update();
  EXPECT_EQ(state.pc, 12);
  EXPECT_EQ(state.r5, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the ADC(R5,R5) instruction. R5 = 0x0000, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 14 + 14);
  state.Update();
  EXPECT_EQ(state.pc, 14);
  EXPECT_EQ(state.r5, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, SbcOpNoCarry) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R2, 0x0002);
  core.SetWordRegister(*lock, CpuCore::R3, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R4, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R7, 0x0001);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R0, CpuCore::R7);  // R0: 0x0000, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R1, CpuCore::R7);  // R1: 0x0001, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R2, CpuCore::R7);  // R2: 0x0002, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R3, CpuCore::R7);  // R3: 0xFFFF, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R4, CpuCore::R7);  // R4: 0x8000, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R1, CpuCore::R1);  // R1: 0x0000
  mem.AddCode(kTestOp_HALT);

  // Execute the SBC(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r0, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the SBC(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the SBC(R2,R7) instruction. R2 = 0x0002, R7 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r2, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the SBC(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r3, 0xFFFE);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the SBC(R4,R7) instruction. R4 = 0x8000, R7 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 10 + 10);
  state.Update();
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(state.r4, 0x7FFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C | CpuCore::O);

  // Execute the SBC(R1,R1) instruction. R1 = 0x0000, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 12 + 12);
  state.Update();
  EXPECT_EQ(state.pc, 12);
  EXPECT_EQ(state.r1, 0);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, SbcOpWithCarry) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R2, 0x0002);
  core.SetWordRegister(*lock, CpuCore::R3, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R4, 0x8001);
  core.SetWordRegister(*lock, CpuCore::R5, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R6, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R7, 0x0001);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R0, CpuCore::R7);  // R0: 0x0000, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R1, CpuCore::R7);  // R1: 0x0001, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R2, CpuCore::R7);  // R2: 0x0002, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R3, CpuCore::R7);  // R3: 0xFFFF, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R4, CpuCore::R7);  // R4: 0x8001, R7: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_SBC, CpuCore::R5, CpuCore::R6);  // R5: 0x0000, R6: 0xFFFF
  mem.AddCode(kTestOp_HALT);

  // Execute the SBC(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r0, 0xFFFE);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the SBC(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the SBC(R2,R7) instruction. R2 = 0x0002, R7 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r2, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the SBC(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r3, 0xFFFD);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the SBC(R4,R7) instruction. R4 = 0x8001, R7 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 10 + 10);
  state.Update();
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(state.r4, 0x7FFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C | CpuCore::O);

  // Execute the SBC(R5,R6) instruction. R5 = 0x0000, R6 = 0xFFFF, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 12 + 12);
  state.Update();
  EXPECT_EQ(state.pc, 12);
  EXPECT_EQ(state.r5, 0);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, NegOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R2, 0x0002);
  core.SetWordRegister(*lock, CpuCore::R3, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R4, 0x8000);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_NEG, CpuCore::R7, CpuCore::R0);  // R0: 0x0000
  mem.AddCode(kTestOp_NEG, CpuCore::R7, CpuCore::R1);  // R1: 0x0001
  mem.AddCode(kTestOp_NEG, CpuCore::R7, CpuCore::R2);  // R2: 0x0002
  mem.AddCode(kTestOp_NEG, CpuCore::R7, CpuCore::R3);  // R3: 0xFFFF
  mem.AddCode(kTestOp_NEG, CpuCore::R7, CpuCore::R4);  // R4: 0x8000
  mem.AddCode(kTestOp_HALT);

  // Execute the NEG(R7,R0) instruction. R0 = 0x0000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r7, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the NEG(R7,R1) instruction. R1 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r7, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the NEG(R7,R2) instruction. R2 = 0x0002
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r7, 0xFFFE);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the NEG(R7,R3) instruction. R3 = 0xFFFF
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r7, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the NEG(R7,R4) instruction. R4 = 0x8000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r7, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::O);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, CmpOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R2, 0x0002);
  core.SetWordRegister(*lock, CpuCore::R3, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R4, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R7, 0x0001);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_CMP, CpuCore::R0, CpuCore::R7);  // R0: 0x0000, R7: 0x0001
  mem.AddCode(kTestOp_CMP, CpuCore::R1, CpuCore::R7);  // R1: 0x0001, R7: 0x0001
  mem.AddCode(kTestOp_CMP, CpuCore::R2, CpuCore::R7);  // R2: 0x0002, R7: 0x0001
  mem.AddCode(kTestOp_CMP, CpuCore::R3, CpuCore::R7);  // R3: 0xFFFF, R7: 0x0001
  mem.AddCode(kTestOp_CMP, CpuCore::R4, CpuCore::R7);  // R4: 0x8000, R7: 0x0001
  mem.AddCode(kTestOp_HALT);

  // Execute the CMP(R0,R7) instruction. R0 = 0x0000, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the CMP(R1,R7) instruction. R1 = 0x0001, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the CMP(R2,R7) instruction. R2 = 0x0002, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the CMP(R3,R7) instruction. R3 = 0xFFFF, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the CMP(R4,R7) instruction. R4 = 0x8000, R7 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C | CpuCore::O);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, NotOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x1234);
  core.SetWordRegister(*lock, CpuCore::R2, 0x5678);
  core.SetWordRegister(*lock, CpuCore::R3, 0x9ABC);
  core.SetWordRegister(*lock, CpuCore::R4, 0xDEF0);
  core.SetWordRegister(*lock, CpuCore::R5, 0xFFFF);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_NOT, CpuCore::R7, CpuCore::R0);  // R0: 0x0000
  mem.AddCode(kTestOp_NOT, CpuCore::R7, CpuCore::R1);  // R1: 0x1234
  mem.AddCode(kTestOp_NOT, CpuCore::R7, CpuCore::R2);  // R2: 0x5678
  mem.AddCode(kTestOp_NOT, CpuCore::R7, CpuCore::R3);  // R3: 0x9ABC
  mem.AddCode(kTestOp_NOT, CpuCore::R7, CpuCore::R4);  // R4: 0xDEF0
  mem.AddCode(kTestOp_NOT, CpuCore::R7, CpuCore::R5);  // R5: 0xFFFF
  mem.AddCode(kTestOp_HALT);

  // Execute the NOT(R7,R0) instruction. R0 = 0x0000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r7, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the NOT(R7,R1) instruction. R1 = 0x1234
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r7, 0xEDCB);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the NOT(R7,R2) instruction. R2 = 0x5678
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r7, 0xA987);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the NOT(R7,R3) instruction. R3 = 0x9ABC
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r7, 0x6543);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the NOT(R7,R4) instruction. R4 = 0xDEF0
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r7, 0x210F);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the NOT(R7,R5) instruction. R5 = 0xFFFF
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r7, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, AndOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x1234);
  core.SetWordRegister(*lock, CpuCore::R2, 0x5678);
  core.SetWordRegister(*lock, CpuCore::R3, 0x9ABC);
  core.SetWordRegister(*lock, CpuCore::R4, 0xDEF0);
  core.SetWordRegister(*lock, CpuCore::R5, 0xA5C3);
  core.SetWordRegister(*lock, CpuCore::R6, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R7, 0x5A3C);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_AND, CpuCore::R0, CpuCore::R7);  // R0: 0x0000, R7: 0x5A3C
  mem.AddCode(kTestOp_AND, CpuCore::R1, CpuCore::R7);  // R1: 0x1234, R7: 0x5A3C
  mem.AddCode(kTestOp_AND, CpuCore::R2, CpuCore::R7);  // R2: 0x5678, R7: 0x5A3C
  mem.AddCode(kTestOp_AND, CpuCore::R3, CpuCore::R7);  // R3: 0x9ABC, R7: 0x5A3C
  mem.AddCode(kTestOp_AND, CpuCore::R4, CpuCore::R7);  // R4: 0xDEF0, R7: 0x5A3C
  mem.AddCode(kTestOp_AND, CpuCore::R5, CpuCore::R6);  // R5: 0xA5C3, R6: 0xFFFF
  mem.AddCode(kTestOp_AND, CpuCore::R5, CpuCore::R7);  // R5: 0xA5C3, R7: 0x5A3C
  mem.AddCode(kTestOp_AND, CpuCore::R6, CpuCore::R7);  // R6: 0xFFFF, R7: 0x5A3C
  mem.AddCode(kTestOp_HALT);

  // Execute the AND(R0,R7) instruction. R0 =  0x0000, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the AND(R1,R7) instruction. R1 = 0x1234, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x1234);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the AND(R2,R7) instruction. R2 = 0x5678, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0x5238);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the AND(R3,R7) instruction. R3 = 0x9ABC, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0x1A3C);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the AND(R4,R7) instruction. R4 = 0xDEF0, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r4, 0x5A30);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the AND(R5,R6) instruction. R5 = 0xA5C3, R6 = 0xFFFF
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r5, 0xA5C3);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the AND(R5,R7) instruction. R5 = 0xA5C3, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 7 + 7);
  state.Update();
  EXPECT_EQ(state.pc, 7);
  EXPECT_EQ(state.r5, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the AND(R6,R7) instruction. R6 = 0xFFFF, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r6, 0x5A3C);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, OrOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x1234);
  core.SetWordRegister(*lock, CpuCore::R2, 0x5678);
  core.SetWordRegister(*lock, CpuCore::R3, 0x9ABC);
  core.SetWordRegister(*lock, CpuCore::R4, 0xDEF0);
  core.SetWordRegister(*lock, CpuCore::R5, 0xA5C3);
  core.SetWordRegister(*lock, CpuCore::R6, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R7, 0x5A3C);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_OR, CpuCore::R0, CpuCore::R0);  // R0: 0x0000
  mem.AddCode(kTestOp_OR, CpuCore::R1, CpuCore::R7);  // R1: 0x1234, R7: 0x5A3C
  mem.AddCode(kTestOp_OR, CpuCore::R2, CpuCore::R7);  // R2: 0x5678, R7: 0x5A3C
  mem.AddCode(kTestOp_OR, CpuCore::R3, CpuCore::R7);  // R3: 0x9ABC, R7: 0x5A3C
  mem.AddCode(kTestOp_OR, CpuCore::R4, CpuCore::R7);  // R4: 0xDEF0, R7: 0x5A3C
  mem.AddCode(kTestOp_OR, CpuCore::R5, CpuCore::R6);  // R5: 0xA5C3, R6: 0xFFFF
  mem.AddCode(kTestOp_OR, CpuCore::R5, CpuCore::R7);  // R5: 0xFFFF, R7: 0x5A3C
  mem.AddCode(kTestOp_OR, CpuCore::R6, CpuCore::R7);  // R6: 0xFFFF, R7: 0x5A3C
  mem.AddCode(kTestOp_HALT);

  // Execute the OR(R0,R0) instruction. R0 = 0x0000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the OR(R1,R7) instruction. R1 = 0x1234, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x5A3C);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the OR(R2,R7) instruction. R2 = 0x5678, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0x5E7C);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the OR(R3,R7) instruction. R3 = 0x9ABC, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0xDABC);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the OR(R4,R7) instruction. R4 = 0xDEF0, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r4, 0xDEFC);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the OR(R5,R6) instruction. R5 = 0xA5C3, R6 = 0xFFFF
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r5, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the OR(R5,R7) instruction. R5 = 0xFFFF, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 7 + 7);
  state.Update();
  EXPECT_EQ(state.pc, 7);
  EXPECT_EQ(state.r5, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the OR(R6,R7) instruction. R6 = 0xFFFF, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r6, 0xFFFF);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, XorOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R1, 0x1234);
  core.SetWordRegister(*lock, CpuCore::R2, 0x5678);
  core.SetWordRegister(*lock, CpuCore::R3, 0x9ABC);
  core.SetWordRegister(*lock, CpuCore::R4, 0xDEF0);
  core.SetWordRegister(*lock, CpuCore::R5, 0xA5C3);
  core.SetWordRegister(*lock, CpuCore::R6, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R7, 0x5A3C);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_XOR, CpuCore::R0, CpuCore::R0);  // R0: 0x0000
  mem.AddCode(kTestOp_XOR, CpuCore::R1, CpuCore::R1);  // R1: 0x0000
  mem.AddCode(kTestOp_XOR, CpuCore::R2, CpuCore::R7);  // R2: 0x5678, R7: 0x5A3C
  mem.AddCode(kTestOp_XOR, CpuCore::R3, CpuCore::R7);  // R3: 0x9ABC, R7: 0x5A3C
  mem.AddCode(kTestOp_XOR, CpuCore::R4, CpuCore::R7);  // R4: 0xDEF0, R7: 0x5A3C
  mem.AddCode(kTestOp_XOR, CpuCore::R5, CpuCore::R6);  // R5: 0xA5C3, R6: 0xFFFF
  mem.AddCode(kTestOp_XOR, CpuCore::R5, CpuCore::R7);  // R5: 0x5A3C, R7: 0x5A3C
  mem.AddCode(kTestOp_XOR, CpuCore::R6, CpuCore::R7);  // R6: 0xFFFF, R7: 0x5A3C
  mem.AddCode(kTestOp_HALT);

  // Execute the XOR(R0,R0) instruction. R0 = 0x0000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the XOR(R1,R1) instruction. R1 = 0x1234
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the XOR(R2,R7) instruction. R2 = 0x5678, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0x0C44);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the XOR(R3,R7) instruction. R3 = 0x9ABC, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0xC080);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the XOR(R4,R7) instruction. R4 = 0xDEF0, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r4, 0x84CC);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the XOR(R5,R6) instruction. R5 = 0xA5C3, R6 = 0xFFFF
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r5, 0x5A3C);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the XOR(R5,R7) instruction. R5 = 0x5A3C, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 7 + 7);
  state.Update();
  EXPECT_EQ(state.pc, 7);
  EXPECT_EQ(state.r5, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the XOR(R6,R7) instruction. R6 = 0xFFFF, R7 = 0x5A3C
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r6, 0xA5C3);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, SlSrSraOps) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R1, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R2, 0xC000);
  core.SetWordRegister(*lock, CpuCore::R3, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R4, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R5, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R6, 0x4001);
  core.SetWordRegister(*lock, CpuCore::R7, 0x8001);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_SL, CpuCore::R0);   // R0: 0x0001
  mem.AddCode(kTestOp_SL, CpuCore::R1);   // R1: 0x8000
  mem.AddCode(kTestOp_SL, CpuCore::R2);   // R2: 0xC000
  mem.AddCode(kTestOp_SR, CpuCore::R3);   // R3: 0x8000
  mem.AddCode(kTestOp_SR, CpuCore::R4);   // R4: 0x0001
  mem.AddCode(kTestOp_SRA, CpuCore::R5);  // R5: 0x8000
  mem.AddCode(kTestOp_SRA, CpuCore::R6);  // R6: 0x4001
  mem.AddCode(kTestOp_SRA, CpuCore::R7);  // R7: 0x8001
  mem.AddCode(kTestOp_HALT);

  // Execute the SL(R0) instruction. R0 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the SL(R1) instruction. R1 = 0x8000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the SL(R2) instruction. R2 = 0xC000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the SR(R3) instruction. R3 = 0x8000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0x4000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the SR(R4) instruction. R4 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r4, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the SRA(R5) instruction. R5 = 0x8000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r5, 0xC000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the SRA(R6) instruction. R6 = 0x4001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 7 + 7);
  state.Update();
  EXPECT_EQ(state.pc, 7);
  EXPECT_EQ(state.r6, 0x2000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the SRA(R7) instruction. R7 = 0x8001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r7, 0xC000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, RlRrOps) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R1, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R2, 0xC000);
  core.SetWordRegister(*lock, CpuCore::R3, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R4, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R5, 0x8001);
  core.SetWordRegister(*lock, CpuCore::R6, 0x0000);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_RL, CpuCore::R0);  // R0: 0x0001
  mem.AddCode(kTestOp_RL, CpuCore::R1);  // R1: 0x8000
  mem.AddCode(kTestOp_RL, CpuCore::R2);  // R2: 0xC000
  mem.AddCode(kTestOp_RR, CpuCore::R3);  // R3: 0x8000
  mem.AddCode(kTestOp_RR, CpuCore::R4);  // R4: 0x0001
  mem.AddCode(kTestOp_RR, CpuCore::R5);  // R5: 0x8001
  mem.AddCode(kTestOp_RL, CpuCore::R6);  // R6: 0x0000
  mem.AddCode(kTestOp_RR, CpuCore::R6);  // R6: 0x0000
  mem.AddCode(kTestOp_HALT);

  // Execute the RL(R0) instruction. R0 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles + 1);
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r0, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RL(R1) instruction. R1 = 0x8000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r1, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the RL(R2) instruction. R2 = 0xC000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 3 + 3);
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r2, 0x8001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RR(R3) instruction. R3 = 0x8000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r3, 0x4000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RR(R4) instruction. R4 = 0x0001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 5 + 5);
  state.Update();
  EXPECT_EQ(state.pc, 5);
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RR(R5) instruction. R5 = 0x8001
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r5, 0xC000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RL(R6) instruction. R6 = 0x0000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 7 + 7);
  state.Update();
  EXPECT_EQ(state.pc, 7);
  EXPECT_EQ(state.r6, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the RR(R6) instruction. R6 = 0x0000
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r6, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, RlcRrcOpsNoCarry) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R1, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R2, 0xC000);
  core.SetWordRegister(*lock, CpuCore::R3, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R4, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R5, 0x8001);
  core.SetWordRegister(*lock, CpuCore::R6, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R7, 0x0000);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_RLC, CpuCore::R0);  // R0: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_RLC, CpuCore::R1);  // R1: 0x8000
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_RLC, CpuCore::R2);  // R2: 0xC000
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_RRC, CpuCore::R3);  // R3: 0x8000
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_RRC, CpuCore::R4);  // R4: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_RRC, CpuCore::R5);  // R5: 0x8001
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_RLC, CpuCore::R6);  // R6: 0x0000
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO - CpuCore::C);
  mem.AddCode(kTestOp_RRC, CpuCore::R7);  // R7: 0x0000
  mem.AddCode(kTestOp_HALT);

  // Execute the RLC(R0) instruction. R0 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r0, 0x0002);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RLC(R1) instruction. R1 = 0x8000, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r1, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the RLC(R2) instruction. R2 = 0xC000, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r2, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RRC(R3) instruction. R3 = 0x8000, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r3, 0x4000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RRC(R4) instruction. R4 = 0x0001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 10 + 10);
  state.Update();
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(state.r4, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z | CpuCore::C);

  // Execute the RRC(R5) instruction. R5 = 0x8001, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 12 + 12);
  state.Update();
  EXPECT_EQ(state.pc, 12);
  EXPECT_EQ(state.r5, 0x4000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the RLC(R6) instruction. R6 = 0x0000, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 14 + 14);
  state.Update();
  EXPECT_EQ(state.pc, 14);
  EXPECT_EQ(state.r6, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the RRC(R7) instruction. R7 = 0x0000, C = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 16 + 16);
  state.Update();
  EXPECT_EQ(state.pc, 16);
  EXPECT_EQ(state.r7, 0x0000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::Z);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, RlcRrcOpsWithCarry) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R1, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R2, 0xC000);
  core.SetWordRegister(*lock, CpuCore::R3, 0x8000);
  core.SetWordRegister(*lock, CpuCore::R4, 0x0001);
  core.SetWordRegister(*lock, CpuCore::R5, 0x8001);
  core.SetWordRegister(*lock, CpuCore::R6, 0x0000);
  core.SetWordRegister(*lock, CpuCore::R7, 0x0000);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_RLC, CpuCore::R0);  // R0: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_RLC, CpuCore::R1);  // R1: 0x8000
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_RLC, CpuCore::R2);  // R2: 0xC000
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_RRC, CpuCore::R3);  // R3: 0x8000
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_RRC, CpuCore::R4);  // R4: 0x0001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_RRC, CpuCore::R5);  // R5: 0x8001
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_RLC, CpuCore::R6);  // R6: 0x0000
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_RRC, CpuCore::R7);  // R7: 0x0000
  mem.AddCode(kTestOp_HALT);

  // Execute the RLC(R0) instruction. R0 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 2 + 2);
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r0, 0x0003);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RLC(R1) instruction. R1 = 0x8000, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 4 + 4);
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r1, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::C);

  // Execute the RLC(R2) instruction. R2 = 0xC000, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 6 + 6);
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r2, 0x8001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RRC(R3) instruction. R3 = 0x8000, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 8 + 8);
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r3, 0xC000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the RRC(R4) instruction. R4 = 0x0001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 10 + 10);
  state.Update();
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(state.r4, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RRC(R5) instruction. R5 = 0x8001, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 12 + 12);
  state.Update();
  EXPECT_EQ(state.pc, 12);
  EXPECT_EQ(state.r5, 0xC000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S | CpuCore::C);

  // Execute the RLC(R6) instruction. R6 = 0x0000, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 14 + 14);
  state.Update();
  EXPECT_EQ(state.pc, 14);
  EXPECT_EQ(state.r6, 0x0001);
  EXPECT_EQ(state.st & CpuCore::ZSCO, 0);

  // Execute the RRC(R7) instruction. R7 = 0x0000, C = 1
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 2);
  EXPECT_EQ(core.GetCycles(), kCpuCoreFetchAndDecodeCycles * 16 + 16);
  state.Update();
  EXPECT_EQ(state.pc, 16);
  EXPECT_EQ(state.r7, 0x8000);
  EXPECT_EQ(state.st & CpuCore::ZSCO, CpuCore::S);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, MathOpsLeaveInterruptFlagAlone) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::ST, CpuCore::I);
  lock.reset();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_MATHI);
  mem.AddCode(kTestOp_HALT);

  // Execute MATHI instruction. ST = I
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 18);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.st & CpuCore::I, CpuCore::I);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, JcJpEndOps) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_ZSCO, 0);
  mem.AddCode(kTestOp_JC, 1);
  mem.AddCode(kTestOp_ZSCO, CpuCore::Z);
  mem.AddCode(kTestOp_JC, 2);
  mem.AddCode(kTestOp_ZSCO, CpuCore::S);
  mem.AddCode(kTestOp_JC, 3);
  mem.AddCode(kTestOp_ZSCO, CpuCore::C);
  mem.AddCode(kTestOp_JC, 4);
  mem.AddCode(kTestOp_ZSCO, CpuCore::O);
  mem.AddCode(kTestOp_JC, 5);
  mem.AddCode(kTestOp_ZSCO, CpuCore::Z | CpuCore::C);
  mem.AddCode(kTestOp_JC, 6);
  mem.AddCode(kTestOp_ZSCO, CpuCore::S | CpuCore::O);
  mem.AddCode(kTestOp_JC, 7);
  mem.AddCode(kTestOp_ZSCO, CpuCore::ZSCO);
  mem.AddCode(kTestOp_JC, 8);
  mem.AddCode(kTestOp_HALT);

  // Execute the JC instruction. ST = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 9);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.r1, 1);
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 1);
  EXPECT_EQ(state.r4, 0);
  EXPECT_EQ(state.r5, 1);
  EXPECT_EQ(state.r6, 0);
  EXPECT_EQ(state.r7, 1);

  // Execute the JC instruction. ST = Z
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 9);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 4);
  EXPECT_EQ(state.r0, 2);
  EXPECT_EQ(state.r1, 1);
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 2);
  EXPECT_EQ(state.r4, 0);
  EXPECT_EQ(state.r5, 2);
  EXPECT_EQ(state.r6, 0);
  EXPECT_EQ(state.r7, 2);

  // Execute the JC instruction. ST = S
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 9);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 6);
  EXPECT_EQ(state.r0, 2);
  EXPECT_EQ(state.r1, 3);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 2);
  EXPECT_EQ(state.r4, 0);
  EXPECT_EQ(state.r5, 3);
  EXPECT_EQ(state.r6, 0);
  EXPECT_EQ(state.r7, 3);

  // Execute the JC instruction. ST = C
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 9);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 8);
  EXPECT_EQ(state.r0, 2);
  EXPECT_EQ(state.r1, 4);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 4);
  EXPECT_EQ(state.r4, 4);
  EXPECT_EQ(state.r5, 3);
  EXPECT_EQ(state.r6, 0);
  EXPECT_EQ(state.r7, 4);

  // Execute the JC instruction. ST = O
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 9);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 10);
  EXPECT_EQ(state.r0, 2);
  EXPECT_EQ(state.r1, 5);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 5);
  EXPECT_EQ(state.r4, 4);
  EXPECT_EQ(state.r5, 5);
  EXPECT_EQ(state.r6, 5);
  EXPECT_EQ(state.r7, 4);

  // Execute the JC instruction. ST = ZC
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 9);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 12);
  EXPECT_EQ(state.r0, 6);
  EXPECT_EQ(state.r1, 5);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 6);
  EXPECT_EQ(state.r4, 6);
  EXPECT_EQ(state.r5, 5);
  EXPECT_EQ(state.r6, 5);
  EXPECT_EQ(state.r7, 6);

  // Execute the JC instruction. ST = 0
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 9);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 14);
  EXPECT_EQ(state.r0, 6);
  EXPECT_EQ(state.r1, 7);
  EXPECT_EQ(state.r2, 7);
  EXPECT_EQ(state.r3, 6);
  EXPECT_EQ(state.r4, 6);
  EXPECT_EQ(state.r5, 7);
  EXPECT_EQ(state.r6, 7);
  EXPECT_EQ(state.r7, 6);

  // Execute the JC instruction. ST = ZCSO
  processor.Execute(kCpuCoreFetchAndDecodeCycles * 2 + 9);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 16);
  EXPECT_EQ(state.r0, 8);
  EXPECT_EQ(state.r1, 7);
  EXPECT_EQ(state.r2, 8);
  EXPECT_EQ(state.r3, 6);
  EXPECT_EQ(state.r4, 8);
  EXPECT_EQ(state.r5, 7);
  EXPECT_EQ(state.r6, 8);
  EXPECT_EQ(state.r7, 6);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, JdOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();

  MemAccessor mem(*processor.GetMemory(0));
  mem.AddCode(kTestOp_JD, 1);
  mem.AddCode(kTestOp_JD, 10);
  mem.AddCode(kTestOp_JD, 0);
  mem.AddCode(kTestOp_HALT);

  // Execute the JD instruction, loop once
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1 + 2 * 1);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 1);
  EXPECT_EQ(state.r7, 1);

  // Execute the JD instruction, loop 10 times
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1 + 2 * 10);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 2);
  EXPECT_EQ(state.r7, 10);

  // Execute the JD instruction, loop 65536 times
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1 + 2 * 65536);
  EXPECT_EQ(core.GetCycles(), processor.GetCycles());
  state.Update();
  EXPECT_EQ(state.pc, 3);
  EXPECT_EQ(state.r7, 0);

  // Execute the HALT instruction.
  processor.Execute(kCpuCoreFetchAndDecodeCycles + 1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kIdle);
}

TEST(CpuCoreTest, InterruptDuringExecution) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(1);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(100);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);  // Interrupt 1 to 100
  mem.AddCode(kTestOp_ZSCO, CpuCore::Z | CpuCore::O);
  mem.AddCode(kTestOp_EI);
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(2);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(3);
  mem.AddCode(kTestOp_HALT);
  const uint16_t pc2 = mem.GetAddress();

  // Interrupt 1
  mem.SetAddress(100);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(42);
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  // Execute until R2 is being set to 2 and before it is set to 3
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));

  // Trigger interrupt 1
  core.RaiseInterrupt(1);
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == 100; }));
  EXPECT_EQ(state.r2, 2);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(state.sp, 0xFFFE);
  mem.SetAddress(state.sp);
  EXPECT_EQ(mem.GetValue(), CpuCore::I | CpuCore::Z | CpuCore::O);
  EXPECT_EQ(mem.GetValue(), pc1);

  // Execute the interrupt through the IRET instruction
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r3, 42);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::Z | CpuCore::O);

  // Execute through the HALT instruction
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, pc2);
  EXPECT_EQ(state.r2, 3);
}

TEST(CpuCoreTest, InterruptDuringWait) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(1);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(100);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);  // Interrupt 1 to 100
  mem.AddCode(kTestOp_ZSCO, CpuCore::Z | CpuCore::O);
  mem.AddCode(kTestOp_EI);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(1000);
  mem.AddCode(kTestOp_WAIT, CpuCore::R3);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(3);
  mem.AddCode(kTestOp_HALT);
  const uint16_t pc2 = mem.GetAddress();

  // Interrupt 1
  mem.SetAddress(100);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(5000);
  mem.AddCode(kTestOp_WAIT, CpuCore::R3);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc3 = mem.GetAddress();
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  // Execute until waiting
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] {
    return core.GetState() == CpuCore::State::kWaiting;
  }));
  EXPECT_EQ(state.pc, pc1);
  const Cycles wait_start =
      core.GetCycles() - (kCpuCoreFetchAndDecodeCycles + 1);

  // Wait a little longer
  processor.Execute(100);

  // Execute interrupt 1
  core.RaiseInterrupt(1);
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc3; }));
  EXPECT_EQ(state.st, CpuCore::W | CpuCore::O);
  EXPECT_EQ(state.r3, 5000);
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.st, CpuCore::W | CpuCore::I | CpuCore::Z);
  EXPECT_EQ(state.pc, pc1);

  // Execute until waiting again
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] {
    return core.GetState() == CpuCore::State::kWaiting;
  }));
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] {
    return core.GetState() != CpuCore::State::kWaiting;
  }));
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInstruction);
  const Cycles wait_end = core.GetCycles();
  EXPECT_EQ(wait_end - wait_start, 1000);
  EXPECT_EQ(state.r3, 0);

  // Execute through the HALT instruction
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, pc2);
  EXPECT_EQ(state.r2, 3);
}

TEST(CpuCoreTest, InterruptDuringHalt) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(1);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(100);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);  // Interrupt 1 to 100
  mem.AddCode(kTestOp_EI);
  mem.AddCode(kTestOp_HALT);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(3);
  mem.AddCode(kTestOp_HALT);
  const uint16_t pc2 = mem.GetAddress();

  // Interrupt 1
  mem.SetAddress(100);
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  // Execute until halting the first time
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, pc1);
  EXPECT_EQ(state.r2, 0);

  // Execute interrupt 1
  core.RaiseInterrupt(1);
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == 100; }));
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));

  // Execute through the HALT instruction
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, pc2);
  EXPECT_EQ(state.r2, 3);
}

TEST(CpuCoreTest, RaiseInterruptWhileDisabled) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(1);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(100);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);  // Interrupt 1 to 100
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(3);
  mem.AddCode(kTestOp_HALT);
  const uint16_t pc2 = mem.GetAddress();

  // Interrupt 1
  mem.SetAddress(100);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(42);
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  // Execute until halting the first time
  ExecuteUntil(processor, state, [&] { return state.pc == pc1; });
  EXPECT_EQ(state.r2, 1);

  // Execute interrupt 1
  core.RaiseInterrupt(1);

  // Execute through the HALT instruction
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, pc2);
  EXPECT_EQ(state.r3, 0);
  EXPECT_EQ(state.r2, 3);
}

TEST(CpuCoreTest, EnableInterruptAfterRaise) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(1);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(100);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);  // Interrupt 1 to 100
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(2);
  mem.AddCode(kTestOp_EI);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(3);
  mem.AddCode(kTestOp_HALT);
  const uint16_t pc2 = mem.GetAddress();

  // Interrupt 1
  mem.SetAddress(100);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(42);
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  core.RaiseInterrupt(1);
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == 100; }));
  EXPECT_EQ(state.r2, 2);
  mem.SetAddress(state.sp);
  EXPECT_EQ(mem.GetValue(), CpuCore::I);
  EXPECT_EQ(mem.GetValue(), pc1);

  // Execute through the HALT instruction
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, pc2);
  EXPECT_EQ(state.r2, 3);
}

TEST(CpuCoreTest, InterruptInInterrupt) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(1);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(100);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);  // Interrupt 1 to 100
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(2);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(200);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);  // Interrupt 2 to 200
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(3);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(300);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);  // Interrupt 3 to 300
  mem.AddCode(kTestOp_EI);
  mem.AddCode(kTestOp_INT, 1);
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(3);
  mem.AddCode(kTestOp_HALT);
  const uint16_t pc1 = mem.GetAddress();

  // Interrupt 1
  mem.SetAddress(100);
  mem.AddCode(kTestOp_INT, 2);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(42);
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  // Interrupt 2
  mem.SetAddress(200);
  mem.AddCode(kTestOp_EI);
  mem.AddCode(kTestOp_INT, 3);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(24);
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  // Interrupt 3
  mem.SetAddress(300);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(100);
  mem.AddCode(kTestOp_LV, CpuCore::R4).AddValue(321);
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == 100; }));
  EXPECT_EQ(state.r3, 0);
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == 200; }));
  EXPECT_EQ(state.r3, 42);
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == 300; }));
  EXPECT_EQ(state.r3, 42);
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc < 300; }));
  EXPECT_EQ(state.r3, 100);
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, pc1);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 24);
  EXPECT_EQ(state.r4, 321);
}

TEST(CpuCoreTest, LockCoreDuringInterrupt) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(1);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(100);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);
  mem.AddCode(kTestOp_EI);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(3);
  mem.AddCode(kTestOp_HALT);
  const uint16_t pc2 = mem.GetAddress();

  // Interrupt 1
  mem.SetAddress(100);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(42);
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  // Execute until after interrupts are enabled.
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.st, CpuCore::I);

  // Lock the core, but it will not *yet* be locked, as the core is in the
  // middle of an instruction (the NOP).
  auto lock = core.RequestLock();
  EXPECT_FALSE(lock->IsLocked());
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);

  // Start the interrupt execution, but it will be stuck at kHandleInterrupt.
  core.RaiseInterrupt(1);
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(core.GetState(), CpuCore::State::kHandleInterrupt);
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kHandleInterrupt);

  // Release the lock and finish execution
  lock = nullptr;
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, pc2);
  EXPECT_EQ(state.r3, 42);
  EXPECT_EQ(state.r2, 3);
}

TEST(CpuCoreTest, LockMemoryDuringInterrupt) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet())
                          .SetMemoryBank(1, MemoryBankConfig::MaxRam()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore({.mb = CpuCore::Banks().SetStack(1).ToWord()});
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(1);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(100);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);
  mem.AddCode(kTestOp_EI);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(3);
  mem.AddCode(kTestOp_HALT);
  const uint16_t pc2 = mem.GetAddress();

  // Interrupt 1
  mem.SetAddress(100);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(42);
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  // Execute until after interrupts are enabled.
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.st, CpuCore::I);

  // Lock the stack, which is not yet in use.
  auto lock = processor.GetMemory(1)->RequestLock();
  EXPECT_TRUE(lock->IsLocked());
  EXPECT_EQ(core.GetState(), CpuCore::State::kRunInstruction);

  // Start the interrupt execution, but it will be stuck at kPushInterruptState.
  core.RaiseInterrupt(1);
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(core.GetState(), CpuCore::State::kPushInterruptState);
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kPushInterruptState);

  // Release the lock on the stack and lock code
  lock = processor.GetMemory(0)->RequestLock();
  EXPECT_TRUE(lock->IsLocked());
  processor.Execute(kCpuCoreStartInterruptCycles);
  EXPECT_EQ(core.GetState(), CpuCore::State::kStartInterrupt);
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kFetchInstruction);
  processor.Execute(kCpuCoreFetchAndDecodeCycles);
  EXPECT_EQ(core.GetState(), CpuCore::State::kFetchInstruction);
  processor.Execute(1);
  EXPECT_EQ(core.GetState(), CpuCore::State::kFetchInstruction);

  // Release the code memory lock and complete the program.
  lock = nullptr;
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, pc2);
  EXPECT_EQ(state.r3, 42);
  EXPECT_EQ(state.r2, 3);
}

TEST(CpuCoreTest, RaiseInterruptsThatAreNotMapped) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_EI);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(1);
  mem.AddCode(kTestOp_LV, CpuCore::R1).AddValue(100);
  mem.AddCode(kTestOp_IST, CpuCore::R0, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc3 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(3);
  mem.AddCode(kTestOp_HALT);
  const uint16_t pc4 = mem.GetAddress();

  // Interrupt 1
  mem.SetAddress(100);
  mem.AddCode(kTestOp_LV, CpuCore::R3).AddValue(42);
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  // Execute until after interrupts are enabled.
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 0);
  EXPECT_EQ(state.st, CpuCore::I);

  // Raise all the interrupts, but none are mapped yet.
  for (int i = 0; i < CpuCore::kInterruptCount; ++i) {
    core.RaiseInterrupt(i);
  }

  // Run to the next instruction, and verify that no interrupt fired.
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 0);

  // Raise all the interrupts again, now 1 is mapped
  for (int i = 0; i < CpuCore::kInterruptCount; ++i) {
    core.RaiseInterrupt(i);
  }

  // Run to the next instruction, and this time the interrupt fired.
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc3; }));
  EXPECT_EQ(state.r2, 0);
  EXPECT_EQ(state.r3, 42);

  // Finish execution, and no interrupt is fired
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, pc4);
  EXPECT_EQ(state.r2, 3);
  EXPECT_EQ(state.r3, 42);
}

TEST(CpuCoreTest, LoadFromInvalidPort) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(10));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 42);
  core.SetWordRegister(*lock, CpuCore::R1, 24);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_PLDS, 10, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_PLDS, 11, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r0, 42);
  EXPECT_EQ(state.st, 0);

  lock = core.RequestLock();
  while (!lock->IsLocked()) {
    processor.Execute(1);
  }
  core.SetWordRegister(*lock, CpuCore::ST, CpuCore::ZSCO);
  lock.reset();

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.r1, 24);
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, LoadFromPortOnlyChangesFlagS) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initlaize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::ST, CpuCore::ZSCO | CpuCore::I);
  core.SetWordRegister(*lock, CpuCore::R0, 100);
  core.SetWordRegister(*lock, CpuCore::R1, 100);
  lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 42);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 24);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_PLDS, 0, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_PLDS, 1, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r0, 42);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::Z | CpuCore::C | CpuCore::O);

  lock = core.RequestLock();
  while (!lock->IsLocked()) {
    processor.Execute(1);
  }
  core.SetWordRegister(*lock, CpuCore::ST, 0);
  lock.reset();

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.r1, 24);
  EXPECT_EQ(state.st, CpuCore::S);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, LoadWordFromPortMode0) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 1);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 0xFFFF);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0);
  mem.AddCode(kTestOp_PLD, 0, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(1);
  mem.AddCode(kTestOp_PLD, 0, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(0).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, LoadWordFromPortModeS) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 1);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 0xFFFF);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0);
  mem.AddCode(kTestOp_PLD, Port::S, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(1);
  mem.AddCode(kTestOp_PLD, Port::S, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(0).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 0);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, LoadWordFromPortModeA) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 1);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 0xFFFF);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0);
  mem.AddCode(kTestOp_PLD, Port::A, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(1);
  mem.AddCode(kTestOp_PLD, Port::A, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r0, 1);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(0).GetAddress(), 1);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 1);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, LoadWordFromPortModeT) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 1);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 0xFFFF);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0);
  mem.AddCode(kTestOp_PLD, Port::T, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(1);
  mem.AddCode(kTestOp_PLD, Port::T, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(0).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, LoadWordFromPortModeTSA) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(3));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 1);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 0xFFFF);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock = processor.LockPort(2);
  processor.GetPort(2).StoreWord(*lock, Port::S, 42);
  EXPECT_EQ(processor.GetPort(2).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0);
  mem.AddCode(kTestOp_PLD, Port::T | Port::S | Port::A, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(1);
  mem.AddCode(kTestOp_PLD, Port::T | Port::S | Port::A, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(2);
  mem.AddCode(kTestOp_PLD, Port::T | Port::S | Port::A, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc3 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.r0, 0);
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(0).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.r1, 0xFFFF);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 1);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 0);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc3; }));
  EXPECT_EQ(state.r1, 42);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 1);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 0);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, StoreToInvalidPort) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(10));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_PSTS, 10, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_PSTS, 11, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.st, 0);

  auto lock = core.RequestLock();
  while (!lock->IsLocked()) {
    processor.Execute(1);
  }
  core.SetWordRegister(*lock, CpuCore::ST, CpuCore::ZSCO);
  lock.reset();

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.st, CpuCore::ZSCO);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, StoreToPortOnlyChangesFlagS) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initlaize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::ST, CpuCore::ZSCO | CpuCore::I);
  core.SetWordRegister(*lock, CpuCore::R0, 42);
  core.SetWordRegister(*lock, CpuCore::R1, 24);
  lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_PSTS, 0, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_PSTS, 1, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::Z | CpuCore::C | CpuCore::O);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 1);
  EXPECT_EQ(processor.GetPort(0).GetValue(0), 42);

  lock = core.RequestLock();
  while (!lock->IsLocked()) {
    processor.Execute(1);
  }
  core.SetWordRegister(*lock, CpuCore::ST, 0);
  lock.reset();

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  EXPECT_EQ(processor.GetPort(1).GetValue(0), 24);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, StoreWordToPortMode0) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 1);
  core.SetWordRegister(*lock, CpuCore::R1, 0xFFFF);
  lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0);
  mem.AddCode(kTestOp_PST, 0, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(1);
  mem.AddCode(kTestOp_PST, 0, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(0).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  EXPECT_EQ(processor.GetPort(0).GetValue(0), 1);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  EXPECT_EQ(processor.GetPort(1).GetValue(0), 0xFFFF);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, StoreWordToPortModeA) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 1);
  core.SetWordRegister(*lock, CpuCore::R1, 0xFFFF);
  lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0);
  mem.AddCode(kTestOp_PST, Port::A, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(1);
  mem.AddCode(kTestOp_PST, Port::A, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(0).GetAddress(), 1);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  EXPECT_EQ(processor.GetPort(0).GetValue(0), 1);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 1);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  EXPECT_EQ(processor.GetPort(1).GetValue(0), 0xFFFF);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, StoreWordToPortModeS) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 1);
  core.SetWordRegister(*lock, CpuCore::R1, 0xFFFF);
  lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0);
  mem.AddCode(kTestOp_PST, Port::S, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(1);
  mem.AddCode(kTestOp_PST, Port::S, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(0).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 1);
  EXPECT_EQ(processor.GetPort(0).GetValue(0), 1);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  EXPECT_EQ(processor.GetPort(1).GetValue(0), 0xFFFF);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, StoreWordToPortModeT) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 1);
  core.SetWordRegister(*lock, CpuCore::R1, 0xFFFF);
  lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0);
  mem.AddCode(kTestOp_PST, Port::T, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(1);
  mem.AddCode(kTestOp_PST, Port::T, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(0).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  EXPECT_EQ(processor.GetPort(0).GetValue(0), 1);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  EXPECT_EQ(processor.GetPort(1).GetValue(0), 100);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, StoreWordToPortModeTSA) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(3));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initialize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 1);
  core.SetWordRegister(*lock, CpuCore::R1, 0xFFFF);
  core.SetWordRegister(*lock, CpuCore::R2, 42);
  lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 100);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 0);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, Port::S, 100);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  lock = processor.LockPort(2);
  processor.GetPort(2).StoreWord(*lock, 0, 100);
  EXPECT_EQ(processor.GetPort(2).GetStatus(), 0);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0);
  mem.AddCode(kTestOp_PST, Port::T | Port::S | Port::A, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(1);
  mem.AddCode(kTestOp_PST, Port::T | Port::S | Port::A, CpuCore::R1);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(2);
  mem.AddCode(kTestOp_PST, Port::T | Port::S | Port::A, CpuCore::R2);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc3 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(0).GetAddress(), 1);
  EXPECT_EQ(processor.GetPort(0).GetStatus(), 1);
  EXPECT_EQ(processor.GetPort(0).GetValue(0), 1);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_EQ(processor.GetPort(1).GetAddress(), 0);
  EXPECT_EQ(processor.GetPort(1).GetStatus(), 1);
  EXPECT_EQ(processor.GetPort(1).GetValue(0), 100);

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc3; }));
  EXPECT_EQ(state.st, 0);
  EXPECT_EQ(processor.GetPort(2).GetAddress(), 1);
  EXPECT_EQ(processor.GetPort(2).GetStatus(), 1);
  EXPECT_EQ(processor.GetPort(2).GetValue(0), 42);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, LockPortBlocksCpuCore) {
  Processor processor(
      ProcessorConfig::OneCore(GetMicroTestInstructionSet()).SetPortCount(2));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initlaize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 42);
  core.SetWordRegister(*lock, CpuCore::R1, 24);
  lock = processor.LockPort(0);
  processor.GetPort(0).StoreWord(*lock, 0, 100);
  lock = processor.LockPort(1);
  processor.GetPort(1).StoreWord(*lock, 0, 100);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_PSTS, 0, CpuCore::R0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_PSTS, 1, CpuCore::R1);
  const uint16_t blocked_pc = mem.GetAddress();
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc2 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Lock port 1, which should not effect writing to port 0
  lock = processor.LockPort(1);

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(processor.GetPort(0).GetValue(0), 42);

  ASSERT_TRUE(
      ExecuteUntil(processor, state, [&] { return state.pc == blocked_pc; }));
  EXPECT_EQ(processor.GetPort(1).GetValue(0), 100);
  for (int i = 0; i < 10; ++i) {
    processor.Execute(1);
  }
  state.Update();
  EXPECT_EQ(state.pc, blocked_pc);
  EXPECT_EQ(processor.GetPort(1).GetValue(0), 100);

  // Reset port 1 lock, which allows the CpuCore to continue
  lock.reset();

  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc2; }));
  EXPECT_EQ(processor.GetPort(1).GetValue(0), 24);

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, SelfCoreOp) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initlaize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 1000);
  core.SetWordRegister(*lock, CpuCore::R1, 1001);
  core.SetWordRegister(*lock, CpuCore::R2, 1002);
  core.SetWordRegister(*lock, CpuCore::R3, 1003);
  core.SetWordRegister(*lock, CpuCore::R4, 1004);
  core.SetWordRegister(*lock, CpuCore::R5, 1005);
  core.SetWordRegister(*lock, CpuCore::R6, 1006);
  core.SetWordRegister(*lock, CpuCore::R7, 1007);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_CSELF);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.mb, CpuCore::Banks().SetData(1).SetExtra(2).ToWord());
  EXPECT_EQ(state.r0, 1000);
  EXPECT_EQ(state.r1, 1005);
  EXPECT_EQ(state.r2, 1002);
  EXPECT_EQ(state.r3, 1007);
  EXPECT_EQ(state.r4, 1000);
  EXPECT_EQ(state.r5, 1005);
  EXPECT_EQ(state.r6, 1002);
  EXPECT_EQ(state.r7, 1007);
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
}

TEST(CpuCoreTest, ModifyOtherCore) {
  Processor processor(
      ProcessorConfig::MultiCore(2, GetMicroTestInstructionSet())
          .SetMemoryBank(1, MemoryBankConfig::MaxRam()));
  CpuCore& core0 = *processor.GetCore(0);
  CpuCore& core1 = *processor.GetCore(1);
  CoreState state0(core0);
  CoreState state1(core1);
  state0.ResetCore();
  state1.ResetCore({.mb = CpuCore::Banks().SetCode(1).ToWord()});
  MemAccessor mem0(*processor.GetMemory(0));
  MemAccessor mem1(*processor.GetMemory(1));

  // Initlaize processor
  auto lock = core0.RequestLock();
  core0.SetWordRegister(*lock, CpuCore::R0, 1000);
  core0.SetWordRegister(*lock, CpuCore::R1, 1001);
  core0.SetWordRegister(*lock, CpuCore::R2, 1002);
  core0.SetWordRegister(*lock, CpuCore::R3, 1003);
  lock = core1.RequestLock();
  core1.SetWordRegister(*lock, CpuCore::R0, 2000);
  core1.SetWordRegister(*lock, CpuCore::R1, 2001);
  core1.SetWordRegister(*lock, CpuCore::R2, 2002);
  core1.SetWordRegister(*lock, CpuCore::R3, 2003);
  lock.reset();

  // Main program
  mem0.AddCode(kTestOp_CLD, CpuCore::R0, CpuCore::R2);
  mem0.AddCode(kTestOp_CST, CpuCore::R1, CpuCore::R3);
  mem0.AddCode(kTestOp_CBK, CpuCore::CODE, 2);
  mem0.AddCode(kTestOp_CBK, CpuCore::STACK, 3);
  mem0.AddCode(kTestOp_CBK, CpuCore::DATA, 4);
  mem0.AddCode(kTestOp_CBK, CpuCore::EXTRA, 5);
  mem0.AddCode(kTestOp_CBK, CpuCore::EXTRA, 16);
  mem0.AddCode(kTestOp_HALT);
  const uint16_t end_pc0 = mem0.GetAddress();

  mem1.AddCode(kTestOp_HALT);
  const uint16_t end_pc1 = mem1.GetAddress();

  // Execute code
  ExecuteUntilHalt(processor, state0);
  state1.Update();
  EXPECT_EQ(state0.pc, end_pc0);
  EXPECT_EQ(state0.mb, 0);
  EXPECT_EQ(state0.r0, 1000);
  EXPECT_EQ(state0.r1, 1001);
  EXPECT_EQ(state0.r2, 2000);
  EXPECT_EQ(state0.r3, 1003);
  EXPECT_EQ(state1.pc, end_pc1);
  EXPECT_EQ(
      state1.mb,
      CpuCore::Banks().SetCode(2).SetStack(3).SetData(4).SetExtra(5).ToWord());
  EXPECT_EQ(state1.r0, 2000);
  EXPECT_EQ(state1.r1, 1003);
  EXPECT_EQ(state1.r2, 2002);
  EXPECT_EQ(state1.r3, 2003);
}

TEST(CpuCoreTest, SelfCoreInterrupt) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(100);
  mem.AddCode(kTestOp_CINT, CpuCore::R0, CpuCore::R1).AddValue(0).AddValue(7);
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
  EXPECT_EQ(core.GetInterrupts(), 1 << 7);
  EXPECT_EQ(core.GetInterruptAddress(7), 100);
  EXPECT_EQ(state.r1, 100);
}

TEST(CpuCoreTest, OtherCoreInterrupt) {
  Processor processor(
      ProcessorConfig::MultiCore(2, GetMicroTestInstructionSet()));
  CpuCore& core0 = *processor.GetCore(0);
  CpuCore& core1 = *processor.GetCore(1);
  CoreState state0(core0);
  CoreState state1(core1);
  state0.ResetCore();
  state1.ResetCore();
  auto lock = core1.RequestLock();
  core1.SetWordRegister(*lock, CpuCore::IP, 100);
  lock.reset();
  MemAccessor mem(*processor.GetMemory(0));

  // Core 0 program
  mem.AddCode(kTestOp_LV, CpuCore::R0).AddValue(100);
  mem.AddCode(kTestOp_CINT, CpuCore::R0, CpuCore::R1).AddValue(1).AddValue(7);
  mem.AddCode(kTestOp_LV, CpuCore::R2).AddValue(200);
  mem.AddCode(kTestOp_CINT, CpuCore::R2, CpuCore::R3).AddValue(2).AddValue(8);
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc0 = mem.GetAddress();

  // Core 1 program
  mem.SetAddress(100);
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc1 = mem.GetAddress();

  // Execute code
  ExecuteUntilHalt(processor, state0);
  state1.Update();
  EXPECT_EQ(state1.pc, end_pc1);
  EXPECT_EQ(state0.pc, end_pc0);
  EXPECT_EQ(core1.GetInterrupts(), 1 << 7);
  EXPECT_EQ(core1.GetInterruptAddress(7), 100);
  EXPECT_EQ(state0.r1, 100);
  EXPECT_EQ(state0.r3, 0);
}

TEST(CpuCoreTest, CbkResetsWaitOnOtherCore) {
  Processor processor(
      ProcessorConfig::MultiCore(2, GetMicroTestInstructionSet())
          .SetMemoryBank(1, MemoryBankConfig::MaxRam())
          .SetMemoryBank(2, MemoryBankConfig::MaxRam()));
  CpuCore& core0 = *processor.GetCore(0);
  CpuCore& core1 = *processor.GetCore(1);
  CoreState state0(core0);
  CoreState state1(core1);
  state0.ResetCore();
  state1.ResetCore({.mb = CpuCore::Banks().SetCode(1).ToWord()});
  MemAccessor mem0(*processor.GetMemory(0));
  MemAccessor mem1(*processor.GetMemory(1));
  MemAccessor mem2(*processor.GetMemory(2));

  // Initlaize processor
  auto lock = core1.RequestLock();
  core1.SetWordRegister(*lock, CpuCore::ST, CpuCore::ZSCO);
  core1.SetWordRegister(*lock, CpuCore::R0, 2000);
  lock.reset();

  // Main program
  mem0.AddCode(kTestOp_NOP);
  mem0.AddCode(kTestOp_NOP);
  mem0.AddCode(kTestOp_NOP);
  const uint16_t blocked_pc0 = mem0.GetAddress();
  mem0.AddCode(kTestOp_CBK, CpuCore::CODE, 2);
  mem0.AddCode(kTestOp_NOP);
  const uint16_t nop_pc0 = mem0.GetAddress();
  mem0.AddCode(kTestOp_NOP);
  mem0.AddCode(kTestOp_HALT);
  const uint16_t end_pc0 = mem0.GetAddress();

  mem1.AddCode(kTestOp_WAIT, CpuCore::R0);
  mem1.AddCode(kTestOp_HALT);
  const uint16_t end_pc1 = mem1.GetAddress();

  mem2.AddCode(kTestOp_WAIT, CpuCore::R0);
  mem2.AddCode(kTestOp_NOP);
  mem2.AddCode(kTestOp_HALT);
  const uint16_t end_pc2 = mem2.GetAddress();
  ASSERT_NE(end_pc1, end_pc2);

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state0,
                           [&] { return state0.pc == blocked_pc0; }));
  state1.Update();
  EXPECT_NE(core0.GetState(), CpuCore::State::kWaiting);
  EXPECT_EQ(state0.pc, blocked_pc0);
  EXPECT_EQ(core1.GetState(), CpuCore::State::kWaiting);
  EXPECT_EQ(state1.st, CpuCore::W | CpuCore::Z | CpuCore::S | CpuCore::C);

  ASSERT_TRUE(
      ExecuteUntil(processor, state0, [&] { return state0.pc == nop_pc0; }));
  state1.Update();
  EXPECT_EQ(state0.pc, nop_pc0);
  EXPECT_NE(core1.GetState(), CpuCore::State::kWaiting);
  EXPECT_EQ(state1.st, CpuCore::Z | CpuCore::S | CpuCore::C);

  ExecuteUntilHalt(processor, state0);
  state1.Update();
  EXPECT_EQ(state0.pc, end_pc0);
  EXPECT_EQ(state1.pc, end_pc2);
}

TEST(CpuCoreTest, CbkResetsWaitOnSelf) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initlaize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 1000);
  core.SetWordRegister(*lock, CpuCore::R2, 100);
  core.SetWordRegister(*lock, CpuCore::ST, CpuCore::I | CpuCore::S);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_IST, CpuCore::R7, CpuCore::R2);
  mem.AddCode(kTestOp_WAIT, CpuCore::R0);
  const uint16_t pc0 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  mem.SetAddress(100);
  mem.AddCode(kTestOp_CBKS, 0);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_IRET);
  mem.AddCode(kTestOp_HALT);

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] {
    return core.GetState() == CpuCore::State::kWaiting;
  }));
  EXPECT_EQ(state.pc, pc0);
  EXPECT_EQ(state.st, CpuCore::W | CpuCore::I | CpuCore::S);
  core.RaiseInterrupt(0);
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.st, 0);
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc0; }));
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::S);
  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
  EXPECT_EQ(state.st, CpuCore::I | CpuCore::S);
  EXPECT_LT(core.GetCycles(), 1000);
}

TEST(CpuCoreTest, ResetDuringWait) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Initlaize processor
  auto lock = core.RequestLock();
  core.SetWordRegister(*lock, CpuCore::R0, 1000);
  core.SetWordRegister(*lock, CpuCore::ST, CpuCore::S);
  lock.reset();

  // Main program
  mem.AddCode(kTestOp_WAIT, CpuCore::R0);
  const uint16_t pc0 = mem.GetAddress();
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] {
    return core.GetState() == CpuCore::State::kWaiting;
  }));
  EXPECT_EQ(state.pc, pc0);
  EXPECT_EQ(state.st, CpuCore::W | CpuCore::S);

  lock = core.RequestLock();
  ASSERT_TRUE(lock->IsLocked());
  core.Reset(*lock, {.mask = CpuCore::ResetParams::MC, .mb = 0});
  lock.reset();

  ExecuteUntilHalt(processor, state);
  EXPECT_EQ(state.pc, end_pc);
  EXPECT_EQ(state.st, CpuCore::S);
  EXPECT_LT(core.GetCycles(), 1000);
}

TEST(CpuCoreTest, CoreLockBlocks) {
  Processor processor(
      ProcessorConfig::MultiCore(2, GetMicroTestInstructionSet())
          .SetMemoryBank(1, MemoryBankConfig::MaxRam()));
  CpuCore& core0 = *processor.GetCore(0);
  CpuCore& core1 = *processor.GetCore(1);
  CoreState state0(core0);
  CoreState state1(core1);
  state0.ResetCore();
  state1.ResetCore({.mb = CpuCore::Banks().SetCode(1).ToWord()});
  MemAccessor mem0(*processor.GetMemory(0));
  MemAccessor mem1(*processor.GetMemory(1));

  // Main program
  mem0.AddCode(kTestOp_CBK, CpuCore::DATA, 1);
  const uint16_t blocked_pc0 = mem0.GetAddress();
  mem0.AddCode(kTestOp_HALT);
  const uint16_t end_pc0 = mem0.GetAddress();

  mem1.AddCode(kTestOp_HALT);
  const uint16_t end_pc1 = mem1.GetAddress();

  // Start with core 1 locked, to block execution on core 0
  auto lock = core1.RequestLock();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state0,
                           [&] { return state0.pc == blocked_pc0; }));
  state1.Update();
  EXPECT_EQ(state1.mb, CpuCore::Banks().SetCode(1).ToWord());
  for (int i = 0; i < 10; ++i) {
    processor.Execute(1);
  }
  state0.Update();
  EXPECT_EQ(state0.pc, blocked_pc0);
  EXPECT_EQ(state1.mb, CpuCore::Banks().SetCode(1).ToWord());

  // Unblock execution of core 0 by releasing the lock.
  lock.reset();

  ExecuteUntilHalt(processor, state0);
  state1.Update();
  EXPECT_EQ(state0.pc, end_pc0);
  EXPECT_EQ(state0.mb, 0);
  EXPECT_EQ(state1.pc, end_pc1);
  EXPECT_EQ(state1.mb, CpuCore::Banks().SetCode(1).SetData(1).ToWord());
}

TEST(CpuCoreTest, ReadOnlyRegisters) {
  Processor processor(ProcessorConfig::OneCore(GetMicroTestInstructionSet()));
  CpuCore& core = *processor.GetCore(0);
  CoreState state(core);
  state.ResetCore();
  MemAccessor mem(*processor.GetMemory(0));

  // Main program
  mem.AddCode(kTestOp_LV, CpuCore::R7).AddValue(0xFFFF);
  mem.AddCode(kTestOp_ROREG, CpuCore::R7);
  mem.AddCode(kTestOp_NOP);
  const uint16_t pc1 = mem.GetAddress();
  mem.AddCode(kTestOp_HALT);
  const uint16_t end_pc = mem.GetAddress();

  // Execute code
  ASSERT_TRUE(ExecuteUntil(processor, state, [&] { return state.pc == pc1; }));
  EXPECT_EQ(state.st, 0x0000);
  EXPECT_EQ(state.mb, 0x0000);
}

}  // namespace
}  // namespace oz3
