// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/memory_map.h"

#include <cstring>

#include "absl/log/check.h"
#include "oz3/core/memory_bank.h"

namespace oz3 {

std::unique_ptr<Lock> MemoryMap::RequestLock() {
  DCHECK(bank_ != nullptr);
  return bank_->RequestLock();
}

bool MemoryMap::IsLocked(const Lock& lock) const {
  return bank_ != nullptr && lock.IsLocked(*bank_);
}

void MemoryMap::Read(int address, uint16_t* data, int size) {
  std::memset(data, 0, size * sizeof(uint16_t));
}

void MemoryMap::Write(int address, const uint16_t* data, int size) {}

}  // namespace oz3
