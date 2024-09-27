// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction.h"

#include "absl/types/span.h"
#include "glog/logging.h"

namespace oz3 {

std::string_view ArgTypeToString(ArgType type) {
  switch (type) {
    case ArgType::kNone:
      return "kNone";
    case ArgType::kImmediate:
      return "kImmediate";
    case ArgType::kWordReg:
      return "kWordReg";
    case ArgType::kDwordReg:
      return "kDwordReg";
    default:
      LOG(FATAL) << "Unknown argument type: " << static_cast<int>(type);
      return "Unknown";
  }
}

uint8_t GetDefaultArgTypeSize(ArgType type) {
  switch (type) {
    case ArgType::kNone:
      return 0;
    case ArgType::kWordReg:
      return 3;
    case ArgType::kDwordReg:
      return 2;
    default:
      LOG(FATAL) << "Cannot derive default size for argument type "
                 << static_cast<int>(type);
      // The rest do not have a default size.
      return 0;
  }
}

uint16_t InstructionDef::Encode(uint16_t a, uint16_t b) const {
  uint16_t code = static_cast<uint16_t>(op) << 8;
  uint16_t args = 0;
  if (arg1.size > 0) {
    args = a & ((1 << arg1.size) - 1);
  }
  if (arg2.size > 0) {
    args |= (b & ((1 << arg2.size) - 1)) << arg1.size;
  }
  return code | args;
}

}  // namespace oz3
