// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/fake_memory_map.h"

#include <cstdint>
#include <cstring>

#include "absl/log/check.h"

namespace oz3 {

FakeMemoryMap::FakeMemoryMap(int page_count)
    : mem_(page_count * kMemoryBankPageSize) {}

void FakeMemoryMap::Read(int address, uint16_t* data, int size) {
  CHECK(address >= 0 && address + size <= static_cast<int>(mem_.size()));
  std::memcpy(data, mem_.data() + address, size * sizeof(uint16_t));
}

void FakeMemoryMap::Write(int address, const uint16_t* data, int size) {
  CHECK(address >= 0 && address + size <= static_cast<int>(mem_.size()));
  std::memcpy(mem_.data() + address, data, size * sizeof(uint16_t));
  if (on_write_ != nullptr) {
    on_write_();
  }
}

}  // namespace oz3
