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

std::unique_ptr<MemoryBank> MemoryBank::Create(Options options) {
  DCHECK(options.mem_page_start >= 0 && options.mem_page_start <= 15);
  DCHECK(options.mem_page_count >= 0 &&
         options.mem_page_start + options.mem_page_count <= 16);
  if (options.mem_page_count == 0) {
    options.mem_read_mask = options.mem_write_mask = 0;
  } else {
    uint16_t mem_page_mask = 0xFFFF;
    if (options.mem_page_start > 0) {
      mem_page_mask &= ~((1 << options.mem_page_start) - 1);
    }
    if (int mem_page_end = options.mem_page_start + options.mem_page_count;
        mem_page_end < 16) {
      mem_page_mask &= (1 << mem_page_end) - 1;
    }
    if (mem_page_mask != 0xFFFF) {
      options.mem_read_mask &= mem_page_mask;
      options.mem_write_mask &= mem_page_mask;
    }
  }

  if (options.map == nullptr) {
    options.map_read_mask = options.map_write_mask = 0;
  }

  return absl::WrapUnique(new MemoryBank(options));
}

MemoryBank::MemoryBank(const Options& options)
    : mem_(options.mem_page_count * kMemoryBankPageSize),
      mem_start_(options.mem_page_start * kMemoryBankPageSize),
      mem_read_mask_(options.mem_read_mask),
      mem_write_mask_(options.mem_write_mask),
      map_(options.map),
      map_read_mask_(options.map_read_mask),
      map_write_mask_(options.map_write_mask) {}

MemoryBank::~MemoryBank() { CHECK(!locked_); }

MemoryLock MemoryBank::Lock() {
  CHECK(!IsLocked());
  return MemoryLock(this);
}

void MemoryBank::AdvanceCycles(Cycles cycles) {
  remaining_cycles_ = std::max<Cycles>(remaining_cycles_ - cycles, 0);
}

absl::Span<uint16_t> MemoryBank::GetMem(int address, int size) {
  if (address < mem_start_ || address + size > mem_start_ + mem_.size()) {
    return {};
  }
  return absl::MakeSpan(mem_.data() + (address - mem_start_), size);
}

absl::Span<const uint16_t> MemoryBank::GetMem(int address, int size) const {
  if (address < mem_start_ || address + size > mem_start_ + mem_.size()) {
    return {};
  }
  return absl::MakeSpan(mem_.data() + (address - mem_start_), size);
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
    if ((map_read_mask_ & page_bit) != 0) {
      map_->Read(address, data, amount);
    } else if ((mem_read_mask_ & page_bit) != 0) {
      std::memcpy(data, mem_.data() + (address - mem_start_),
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
    if ((map_write_mask_ & page_bit) != 0) {
      map_->Write(address, data, amount);
    } else if ((mem_write_mask_ & page_bit) != 0) {
      std::memcpy(mem_.data() + (address - mem_start_), data,
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
  if ((map_read_mask_ & page_bit) != 0) {
    uint16_t value = 0;
    map_->Read(address_, &value, 1);
    return value;
  }
  if ((mem_read_mask_ & page_bit) != 0) {
    return mem_[address_ - mem_start_];
  }
  return 0;
}

void MemoryBank::WriteWord(uint16_t value) {
  const uint16_t page_bit = (1 << (address_ >> 12));
  if ((map_write_mask_ & page_bit) != 0) {
    map_->Write(address_, &value, 1);
  } else if ((mem_write_mask_ & page_bit) != 0) {
    mem_[address_ - mem_start_] = value;
  }
}

}  // namespace oz3
