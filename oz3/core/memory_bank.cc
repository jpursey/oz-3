// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/memory_bank.h"

#include "absl/log/log.h"
#include "absl/memory/memory.h"

namespace oz3 {

//==============================================================================
// MemoryBank
//==============================================================================

MemoryBank::MemoryBank(const MemoryBankConfig& config,
                       absl::Span<const MemoryMapPages> maps)
    : mem_range_(config.GetMemPages()), mem_masks_(config.GetMemMasks()) {
  for (const MemoryMapPages& map_pages : maps) {
    MemoryMap* const map = map_pages.map;
    const MemoryPageRange& pages = map_pages.pages;
    DCHECK(map != nullptr && map->bank_ == nullptr);
    DCHECK(pages.IsValid() && pages.page_count > 0);
    map->bank_ = this;
    const PageMap page_map = {.map = map, .start = MemoryRange(pages).start};
    for (int i = 0; i < pages.page_count; ++i) {
      DCHECK(page_maps_[pages.start_page + i].map == nullptr);
      page_maps_[pages.start_page + i] = page_map;
    }
    const uint16_t pages_mask = pages.GetMask();
    mem_masks_.read_mask &= ~pages_mask;
    mem_masks_.write_mask &= ~pages_mask;
    map_masks_.read_mask |=
        (map_pages.masks.read_mask << pages.start_page) & pages_mask;
    map_masks_.write_mask |=
        (map_pages.masks.write_mask << pages.start_page) & pages_mask;
  }
  mem_.resize(mem_range_.count);
}

MemoryBank::~MemoryBank() {
  for (const PageMap& page_map : page_maps_) {
    if (page_map.map != nullptr) {
      page_map.map->bank_ = nullptr;
    }
  }
}

absl::Span<uint16_t> MemoryBank::GetMem(int address, int size) {
  if (address < mem_range_.start ||
      address + size > mem_range_.start + mem_range_.count) {
    return {};
  }
  return absl::MakeSpan(mem_.data() + (address - mem_range_.start), size);
}

absl::Span<const uint16_t> MemoryBank::GetMem(int address, int size) const {
  if (address < mem_range_.start ||
      address + size > mem_range_.start + mem_range_.count) {
    return {};
  }
  return absl::MakeSpan(mem_.data() + (address - mem_range_.start), size);
}

void MemoryBank::ReadWrap(uint16_t* data, int size) {
  uint16_t new_address = address_ + size;
  if (new_address > address_ || new_address == 0) {
    Read(address_, data, size);
  } else {
    const uint16_t initial_size = -address_;
    Read(address_, data, initial_size);
    Read(0, data + initial_size, size - initial_size);
  }
}

void MemoryBank::WriteWrap(const uint16_t* data, int size) {
  uint16_t new_address = address_ + size;
  if (new_address > address_ || new_address == 0) {
    Write(address_, data, size);
  } else {
    const uint16_t initial_size = -address_;
    Write(address_, data, initial_size);
    Write(0, data + initial_size, size - initial_size);
  }
}

void MemoryBank::Read(int address, uint16_t* data, int size) {
  while (size > 0) {
    const int page = address >> 12;
    const int amount =
        std::min(size, (page + 1) * kMemoryBankPageSize - address);
    const uint16_t page_bit = static_cast<uint16_t>(1 << page);
    if ((mem_masks_.read_mask & page_bit) != 0) {
      std::memcpy(data, mem_.data() + (address - mem_range_.start),
                  amount * sizeof(uint16_t));
    } else if ((map_masks_.read_mask & page_bit) != 0) {
      const PageMap& page_map = page_maps_[page];
      page_map.map->Read(address - page_map.start, data, amount);
    } else {
      std::memset(data, 0, amount * sizeof(uint16_t));
    }
    data += amount;
    address += amount;
    size -= amount;
  }
}

void MemoryBank::Write(int address, const uint16_t* data, int size) {
  while (size > 0) {
    const int page = address >> 12;
    const int amount =
        std::min(size, (page + 1) * kMemoryBankPageSize - address);
    const uint16_t page_bit = static_cast<uint16_t>(1 << page);
    if ((mem_masks_.write_mask & page_bit) != 0) {
      std::memcpy(mem_.data() + (address - mem_range_.start), data,
                  amount * sizeof(uint16_t));
    } else if ((map_masks_.write_mask & page_bit) != 0) {
      const PageMap& page_map = page_maps_[page];
      page_map.map->Write(address - page_map.start, data, amount);
    }
    data += amount;
    address += amount;
    size -= amount;
  }
}

uint16_t MemoryBank::ReadWord() {
  const uint16_t page_bit = (1 << (address_ >> 12));
  if ((mem_masks_.read_mask & page_bit) != 0) {
    return mem_[address_ - mem_range_.start];
  }
  uint16_t value = 0;
  Read(address_, &value, 1);
  return value;
}

void MemoryBank::WriteWord(uint16_t value) {
  const uint16_t page_bit = (1 << (address_ >> 12));
  if ((mem_masks_.write_mask & page_bit) != 0) {
    mem_[address_ - mem_range_.start] = value;
  } else {
    Write(address_, &value, 1);
  }
}

}  // namespace oz3
