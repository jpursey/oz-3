// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/memory_bank_config.h"

#include <algorithm>

namespace oz3 {

MemoryBankConfig MemoryBankConfig::MaxRam() {
  return MemoryBankConfig().SetMemPages(MemoryPageRange::Max());
}

MemoryBankConfig MemoryBankConfig::RomRam(int rom_pages, int ram_pages) {
  const int total_pages =
      std::clamp(rom_pages + ram_pages, 0, kMemoryBankPageCount);
  rom_pages = std::clamp(rom_pages, 0, total_pages);
  ram_pages = std::clamp(ram_pages, 0, total_pages - rom_pages);
  return MemoryBankConfig().SetMemPages(
      MemoryPageRange{.page_count = total_pages},
      MemoryPageMasks{
          .read_mask = MemoryPageRange{.page_count = total_pages}.GetMask(),
          .write_mask =
              MemoryPageRange{.start_page = rom_pages, .page_count = ram_pages}
                  .GetMask()});
}

MemoryBankConfig& MemoryBankConfig::SetMemPages(MemoryPageRange range,
                                                MemoryPageMasks masks) {
  mem_pages_.start_page = std::clamp(range.start_page, 0, 15);
  mem_pages_.page_count =
      std::clamp(range.page_count, 0, std::max(0, 16 - range.start_page));
  const uint16_t pages_mask = mem_pages_.GetMask();
  mem_masks_.read_mask = (masks.read_mask & pages_mask);
  mem_masks_.write_mask = (masks.write_mask & pages_mask);
  return *this;
}

}  // namespace oz3