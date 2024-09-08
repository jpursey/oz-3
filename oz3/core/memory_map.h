// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_MEMORY_MAP_H_
#define OZ3_CORE_MEMORY_MAP_H_

#include <cstring>

#include "oz3/core/core_types.h"

namespace oz3 {

//==============================================================================
// MemoryMap
//==============================================================================

// The MemoryMap interface provides a way for an external class (usually a
// Coprocessor or Device derived class) to map functionality to specific
// memory addresses in a MemoryBank. Memory maps always overlay full pages of
// physical memory (it is not possible to memory map less than a page).
class MemoryMap {
 public:
  MemoryMap() = default;
  virtual ~MemoryMap() = default;

  // Read from the specified address `size` words into `data`.
  virtual void Read(int address, uint16_t* data, int size) {
    std::memset(data, 0, size * sizeof(uint16_t));
  }

  // Write into the specified address `size` words from `data`.
  virtual void Write(int address, const uint16_t* data, int size) {}
};

}  // namespace oz3

#endif  // OZ3_CORE_MEMORY_MAP_H_