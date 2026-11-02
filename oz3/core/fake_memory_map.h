// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_FAKE_MEMORY_MAP_H_
#define OZ3_CORE_FAKE_MEMORY_MAP_H_

#include <cstdint>
#include <vector>

#include "absl/types/span.h"
#include "oz3/core/core_types.h"
#include "oz3/core/memory_map.h"

namespace oz3 {

//==============================================================================
// FakeMemoryMap
//==============================================================================

// A memory map for tests, with `page_count` pages of memory of its own, which
// hold whatever is written to them, as physical memory does.
class FakeMemoryMap final : public MemoryMap {
 public:
  explicit FakeMemoryMap(int page_count = kMemoryBankPageCount);
  ~FakeMemoryMap() override = default;

  // Returns the map's memory, at its own addresses. This takes no lock, so a
  // test can set it up and check it directly.
  absl::Span<uint16_t> GetMem() { return absl::MakeSpan(mem_); }

 protected:
  // MemoryMap implementation.
  void Read(int address, uint16_t* data, int size) override;
  void Write(int address, const uint16_t* data, int size) override;

 private:
  std::vector<uint16_t> mem_;
};

}  // namespace oz3

#endif  // OZ3_CORE_FAKE_MEMORY_MAP_H_
