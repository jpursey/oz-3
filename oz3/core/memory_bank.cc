// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/memory_bank.h"

#include "absl/memory/memory.h"
#include "glog/logging.h"

namespace oz3 {

//==============================================================================
// MemoryBank
//==============================================================================

MemoryBank::MemoryBank(const MemoryBankConfig& config)
    : mem_range_(config.GetMemPages()),
      mem_masks_(config.GetMemMasks()),
      map_(config.GetMemoryMap()),
      map_masks_(config.GetMemoryMapMasks()) {
  mem_.resize(mem_range_.count);
}

MemoryBank::~MemoryBank() { CHECK(!locked_); }

MemoryLock MemoryBank::Lock() {
  CHECK(!IsLocked());
  return MemoryLock(this);
}

void MemoryBank::AdvanceCycles(Cycles cycles) {
  remaining_cycles_ = std::max<Cycles>(remaining_cycles_ - cycles, 0);
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
  if (size == 0) {
    return;
  }
  const int last_page = (address + size - 1) >> 12;
  int page = address_ >> 12;
  int page_address = page * kMemoryBankPageSize;
  uint16_t page_bit = (1 << page);
  while (true) {
    int amount =
        std::min(address + size, page_address + kMemoryBankPageSize) - address;
    if ((map_masks_.read_mask & page_bit) != 0) {
      map_->Read(address, data, amount);
    } else if ((mem_masks_.read_mask & page_bit) != 0) {
      std::memcpy(data, mem_.data() + (address - mem_range_.start),
                  amount * sizeof(uint16_t));
    } else {
      std::memset(data, 0, amount * sizeof(uint16_t));
    }
    if (++page > last_page) {
      break;
    }
    data += amount;
    address += amount;
    size -= amount;
    page_address += kMemoryBankPageSize;
    page_bit <<= 1;
  }
}

void MemoryBank::Write(int address, const uint16_t* data, int size) {
  if (size == 0) {
    return;
  }
  const int last_page = (address + size - 1) >> 12;
  int page = address_ >> 12;
  int page_address = page * kMemoryBankPageSize;
  uint16_t page_bit = (1 << page);
  while (true) {
    int amount =
        std::min(address + size, page_address + kMemoryBankPageSize) - address;
    if ((map_masks_.write_mask & page_bit) != 0) {
      map_->Write(address, data, amount);
    } else if ((mem_masks_.write_mask & page_bit) != 0) {
      std::memcpy(mem_.data() + (address - mem_range_.start), data,
                  amount * sizeof(uint16_t));
    }
    if (++page > last_page) {
      break;
    }
    data += amount;
    address += amount;
    size -= amount;
    page_address += kMemoryBankPageSize;
    page_bit <<= 1;
  }
}

uint16_t MemoryBank::ReadWord() {
  const uint16_t page_bit = (1 << (address_ >> 12));
  if ((map_masks_.read_mask & page_bit) != 0) {
    uint16_t value = 0;
    map_->Read(address_, &value, 1);
    return value;
  }
  if ((mem_masks_.read_mask & page_bit) != 0) {
    return mem_[address_ - mem_range_.start];
  }
  return 0;
}

void MemoryBank::WriteWord(uint16_t value) {
  const uint16_t page_bit = (1 << (address_ >> 12));
  if ((map_masks_.write_mask & page_bit) != 0) {
    map_->Write(address_, &value, 1);
  } else if ((mem_masks_.write_mask & page_bit) != 0) {
    mem_[address_ - mem_range_.start] = value;
  }
}

}  // namespace oz3
