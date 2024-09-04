// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_TYPES_H_
#define OZ3_CORE_TYPES_H_

#include <cstdint>

namespace oz3 {

class Component;
class ComponentLock;
class MemoryMap;
class MemoryBank;
class MemoryBankConfig;

// Defines a number of cycles over a period of time or since "boot".
using Cycles = int64_t;

}  // namespace oz3


#endif  // OZ3_CORE_TYPES_H_