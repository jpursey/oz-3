// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_MEMORY_BANK_H_
#define OZ3_CORE_MEMORY_BANK_H_

#include <algorithm>
#include <cstring>
#include <memory>
#include <vector>

#include "absl/types/span.h"
#include "glog/logging.h"
#include "oz3/core/component.h"
#include "oz3/core/core_types.h"
#include "oz3/core/memory_bank_config.h"
#include "oz3/core/memory_map.h"

namespace oz3 {

//==============================================================================
// MemoryBank
//==============================================================================

// This class represents a single bank of memory for the OZ3 Processor.
//
// MemoryBanks are 16-bit addressable and are accessible from any OZ3 CpuCore,
// Coprocessor, or Device. Each memory bank is divided into 16 banks of 4096
// 16-bit words of storage. Each page has separate read/write permissions, and
// may be backed by physical memory and/or a memory map callback (typical use is
// for a Coprocessor or Device).
class MemoryBank final : public Component {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Constructs a memory bank with the specified options.
  explicit MemoryBank(const MemoryBankConfig& config = {});

  // MemoryBank is move-only.
  MemoryBank(const MemoryBank&) = delete;
  MemoryBank& operator=(const MemoryBank&) = delete;
  ~MemoryBank();

  //----------------------------------------------------------------------------
  // Lock-required Operations
  //----------------------------------------------------------------------------

  // The current address on the address bus for the MemoryBank. All regular
  // reads and writes through a MemoryLock update based on this address.
  uint16_t GetAddress(const Lock& lock) const;

  // Sets a new read/write address for the MemoryBank. This takes
  // kMemoryBankSetAddressCycles to execute.
  Cycles SetAddress(const Lock& lock, uint16_t address);

  // Reads/Writes the specified buffer of words from/to the memory bank starting
  // at current address. This will automatically "wrap" at the end of address
  // space back to address zero. The address is automatically incremented (with
  // wrapping) by `size`. This takes `size * kMemoryBankAccessWordCycles
  // virtual cycles to execute.
  Cycles ReadWords(const Lock& lock, absl::Span<uint16_t> data);
  Cycles WriteWords(const Lock& lock, absl::Span<const uint16_t> data);

  // Returns the word at the current address and increments the address. This
  // takes kMemoryBankAccessWordCycles to execute.
  Cycles LoadWord(const Lock& lock, uint16_t& value);

  // Writes the word to the current address and increments the address. This
  // takes kMemoryBankAccessWordCycles to execute.
  Cycles StoreWord(const Lock& lock, uint16_t value);

  // Decrements address and writes the word at the new address. This takes
  // kMemoryBankAccessWordCycles to execute.
  Cycles PushWord(const Lock& lock, uint16_t value);

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Returns the start of the memory bank in 16-bit words.
  int GetMemoryStart() const { return mem_range_.start; }

  // Returns the size of the memory bank in 16-bit words.
  int GetMemorySize() const { return mem_range_.count; }

  // Returns direct access to the physical memory of the MemoryBank.
  //
  // Unlike MemoryLock, this does not wrap around at max memory. However, it
  // also consumes no cycles and does not affect the address bus.
  //
  // If the entire range does not exist for the specified range, this will
  // return no data.
  absl::Span<uint16_t> GetMem(int address, int size);
  absl::Span<const uint16_t> GetMem(int address, int size) const;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // Address range for the memory bank.
  struct MemoryRange {
    MemoryRange() = default;
    MemoryRange(const MemoryPageRange& range)
        : start(range.start_page * kMemoryBankPageSize),
          count(range.page_count * kMemoryBankPageSize) {}
    int start = 0;
    int count = 0;
  };

  // Reads/write `size` bytes from current address, wrapping around address
  // space as needed.
  void ReadWrap(uint16_t* data, int size);
  void WriteWrap(const uint16_t* data, int size);

  // Read/write `size` bytes at the specified address. `address + size` cannot
  // exceed the end of address space.
  void Read(int address, uint16_t* data, int size);
  void Write(int address, const uint16_t* data, int size);

  // Read/write a single word at current address.
  uint16_t ReadWord();
  void WriteWord(uint16_t value);

  // Configuration set at initialization.
  const MemoryRange mem_range_;
  const MemoryPageMasks mem_masks_;
  MemoryMap* const map_ = nullptr;
  const MemoryPageMasks map_masks_;

  // Physical memory backing the memory bank.
  std::vector<uint16_t> mem_;

  // Value of address bus for the MemoryBank.
  uint16_t address_ = 0;
};

//==============================================================================
// MemoryBank inlines
//==============================================================================

inline uint16_t MemoryBank::GetAddress(const Lock& lock) const {
  DCHECK(lock.IsLocked(*this));
  return address_;
}

inline Cycles MemoryBank::SetAddress(const Lock& lock,
                                     uint16_t address) {
  DCHECK(lock.IsLocked(*this));
  address_ = address;
  return kMemoryBankSetAddressCycles;
}

inline Cycles MemoryBank::LoadWord(const Lock& lock, uint16_t& value) {
  DCHECK(lock.IsLocked(*this));
  value = ReadWord();
  address_ += 1;
  return kMemoryBankAccessWordCycles;
}

inline Cycles MemoryBank::StoreWord(const Lock& lock, uint16_t value) {
  DCHECK(lock.IsLocked(*this));
  WriteWord(value);
  address_ += 1;
  return kMemoryBankAccessWordCycles;
}

inline Cycles MemoryBank::PushWord(const Lock& lock, uint16_t value) {
  DCHECK(lock.IsLocked(*this));
  address_ -= 1;
  WriteWord(value);
  return kMemoryBankAccessWordCycles;
}

inline Cycles MemoryBank::ReadWords(const Lock& lock,
                                    absl::Span<uint16_t> data) {
  DCHECK(lock.IsLocked(*this));
  DCHECK(data.size() <= kMemoryBankMaxSize);
  const int size = static_cast<int>(data.size());
  ReadWrap(data.data(), size);
  address_ += size;
  return size * kMemoryBankAccessWordCycles;
}

inline Cycles MemoryBank::WriteWords(const Lock& lock,
                                     absl::Span<const uint16_t> data) {
  DCHECK(lock.IsLocked(*this));
  DCHECK(data.size() <= kMemoryBankMaxSize);
  const int size = static_cast<int>(data.size());
  WriteWrap(data.data(), size);
  address_ += size;
  return size * kMemoryBankAccessWordCycles;
}

}  // namespace oz3

#endif  // OZ3_CORE_MEMORY_BANK_H_
