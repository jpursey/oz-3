// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/memory_bank_config.h"

#include <algorithm>

namespace oz3 {

MemoryBankConfig& MemoryBankConfig::SetMemPages(MemoryPageRange range,
                                                MemoryPageMasks masks) {
  mem_pages_.start_page = std::clamp(range.start_page, 0, 15);
  mem_pages_.page_count =
      std::clamp(range.page_count, 0, std::max(0, 16 - range.start_page));
  uint16_t pages_mask = 0xFFFF;
  if (mem_pages_.start_page > 0) {
    pages_mask &= ~((1 << mem_pages_.start_page) - 1);
  }
  if (int mem_page_end = mem_pages_.start_page + mem_pages_.page_count;
      mem_page_end < 16) {
    pages_mask &= (1 << mem_page_end) - 1;
  }
  mem_masks_.read_mask = (masks.read_mask & pages_mask);
  mem_masks_.write_mask = (masks.write_mask & pages_mask);
  return *this;
}

MemoryBankConfig& MemoryBankConfig::SetMemoryMap(MemoryMap* map,
                                                 MemoryPageMasks masks) {
  map_ = map;
  map_masks_ = masks;
  return *this;
}

}  // namespace oz3