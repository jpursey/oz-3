// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/instruction_def.h"

#include "absl/log/log.h"
#include "absl/types/span.h"

namespace oz3 {

std::string_view ArgTypeToString(ArgType type) {
  switch (type) {
    case ArgType::kNone:
      return "none";
    case ArgType::kImmediate:
      return "immediate";
    case ArgType::kWordReg:
      return "word register";
    case ArgType::kDwordReg:
      return "dword register";
    case ArgType::kMacro:
      return "macro";
    default:
      LOG(FATAL) << "Unknown argument type: " << static_cast<int>(type);
      return "unknown";
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
                 << ArgTypeToString(type);
      // The rest do not have a default size.
      return 0;
  }
}

bool Argument::IsValid() const {
  switch (type) {
    case ArgType::kNone:
      return size == 0;
    case ArgType::kImmediate:
      return size >= 1 && size <= 8;
    case ArgType::kWordReg:
      return size >= 1 && size <= 4;
    case ArgType::kDwordReg:
      return size >= 1 && size <= 2;
    case ArgType::kMacro:
      return size >= 1 && size <= 8;
    default:
      return false;
  }
}

uint16_t InstructionDef::Encode(uint16_t a, uint16_t b) const {
  uint16_t encoded = op << 8;
  uint16_t args = 0;
  if (arg1.size > 0) {
    args = a & ((1 << arg1.size) - 1);
  }
  if (arg2.size > 0) {
    args |= (b & ((1 << arg2.size) - 1)) << arg1.size;
  }
  return encoded | args;
}

}  // namespace oz3
