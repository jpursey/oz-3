// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/microcode.h"

#include <algorithm>
#include <optional>
#include <string>

#include "absl/container/flat_hash_map.h"
#include "absl/log/log.h"
#include "absl/strings/ascii.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"
#include "absl/types/span.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/port.h"

namespace oz3 {

namespace {

const MicrocodeDef kMicroCodeDefs[] = {
    {kMicro_MSC, "MSC", MicroArgType::kStatus},
    {kMicro_MSS, "MSS", MicroArgType::kStatus},
    {kMicro_MSX, "MSX", MicroArgType::kStatus},
    {kMicro_MSM, "MSM", MicroArgType::kStatus, MicroArgType::kWordReg},
    {kMicro_MSR, "MSR", MicroArgType::kStatus, MicroArgType::kStatus},
    {kMicro_WAIT, "WAIT", MicroArgType::kWordReg},
    {kMicro_HALT, "HALT"},
    {kMicro_LK, "LK", MicroArgType::kBank},
    {kMicro_LKR, "LKR", MicroArgType::kWordReg},
    {kMicro_UL, "UL"},
    {kMicro_ADR, "ADR", MicroArgType::kWordReg},
    {kMicro_LAD, "LAD", MicroArgType::kWordReg},
    {kMicro_LD, "LD", MicroArgType::kWordReg},
    {kMicro_ST, "ST", MicroArgType::kWordReg},
    {kMicro_STP, "STP", MicroArgType::kWordReg},
    {kMicro_MOVI, "MOVI", MicroArgType::kWordReg, MicroArgType::kValue},
    {kMicro_MOV, "MOV", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_MVBI, "MVBI", MicroArgType::kRegByte, MicroArgType::kValue},
    {kMicro_MVB, "MVB", MicroArgType::kRegByte, MicroArgType::kRegByte},
    {kMicro_MVNI, "MVNI", MicroArgType::kRegNibble, MicroArgType::kValue},
    {kMicro_MVN, "MVN", MicroArgType::kRegNibble, MicroArgType::kRegNibble},
    {kMicro_ADDI, "ADDI", MicroArgType::kWordReg, MicroArgType::kValue},
    {kMicro_ADD, "ADD", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_ADC, "ADC", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_SUB, "SUB", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_SBC, "SBC", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_NEG, "NEG", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_TST, "TST", MicroArgType::kWordReg},
    {kMicro_CMP, "CMP", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_NOT, "NOT", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_AND, "AND", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_OR, "OR", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_XOR, "XOR", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_SL, "SL", MicroArgType::kWordReg},
    {kMicro_SR, "SR", MicroArgType::kWordReg},
    {kMicro_SRA, "SRA", MicroArgType::kWordReg},
    {kMicro_RL, "RL", MicroArgType::kWordReg},
    {kMicro_RR, "RR", MicroArgType::kWordReg},
    {kMicro_RLC, "RLC", MicroArgType::kWordReg},
    {kMicro_RRC, "RRC", MicroArgType::kWordReg},
    {kMicro_JP, "JP", MicroArgType::kAddress},
    {kMicro_JC, "JC", MicroArgType::kCondition, MicroArgType::kAddress},
    {kMicro_JD, "JD", MicroArgType::kWordReg, MicroArgType::kAddress},
    {kMicro_INT, "INT", MicroArgType::kWordReg},
    {kMicro_ILD, "ILD", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_IST, "IST", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_IRT, "IRT"},
    {kMicro_PLK, "PLK", MicroArgType::kWordReg},
    {kMicro_PUL, "PUL"},
    {kMicro_PLD, "PLD", MicroArgType::kPortMode, MicroArgType::kWordReg},
    {kMicro_PST, "PST", MicroArgType::kPortMode, MicroArgType::kWordReg},
    {kMicro_CLK, "CLK", MicroArgType::kWordReg},
    {kMicro_CUL, "CUL"},
    {kMicro_CBK, "CBK", MicroArgType::kBank, MicroArgType::kWordReg},
    {kMicro_CLD, "CLD", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_CST, "CST", MicroArgType::kWordReg, MicroArgType::kWordReg},
    {kMicro_END, "END"},
};

}  // namespace

MicroArgType ToMicroArgType(ArgType type) {
  switch (type) {
    case ArgType::kNone:
      return MicroArgType::kNone;
    case ArgType::kImmediate:
      // Immediate args become the C0/C1 registers in microcode.
      return MicroArgType::kWordReg;
    case ArgType::kWordReg:
      return MicroArgType::kWordReg;
    case ArgType::kDwordReg:
      return MicroArgType::kDwordReg;
    case ArgType::kMacro:
      return MicroArgType::kNone;
  }
  LOG(FATAL) << "Unknown argument type: " << static_cast<int>(type);
  return MicroArgType::kNone;
}

absl::Span<const MicrocodeDef> GetMicrocodeDefs() { return kMicroCodeDefs; }

}  // namespace oz3
