// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_CORE_TYPES_H_
#define OZ3_CORE_CORE_TYPES_H_

#include <cstdint>

#include "gb/base/access_token.h"

namespace oz3 {

class Component;
class CpuCore;
class CpuCoreConfig;
class ExecutionComponent;
class Lock;
class Lockable;
class MemoryMap;
class MemoryBank;
class MemoryBankConfig;
class Port;
class PortBank;
class Processor;
class ProcessorConfig;

// Defines a number of cycles over a period of time or since "boot".
using Cycles = int64_t;

GB_BEGIN_ACCESS_TOKEN(CoreInternal)
friend class Processor;
friend class CpuCore;
GB_END_ACCESS_TOKEN()

}  // namespace oz3

#endif  // OZ3_CORE_CORE_TYPES_H_