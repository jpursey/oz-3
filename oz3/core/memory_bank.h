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
#include "oz3/core/memory_bank_config.h"
#include "oz3/core/memory_map.h"
#include "oz3/core/types.h"

namespace oz3 {

class MemoryBank;

//==============================================================================
// MemoryLock
//==============================================================================

// The MemoryLock class provides direct access to memory in the MemoryBank.
class MemoryLock final {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------
  MemoryLock(const MemoryLock&) = delete;
  MemoryLock(MemoryLock&& other) noexcept
      : bank_(std::exchange(other.bank_, nullptr)) {}
  MemoryLock& operator=(const MemoryLock&) = delete;
  MemoryLock& operator=(MemoryLock&& other) noexcept {
    if (&other != this) {
      bank_ = std::exchange(other.bank_, nullptr);
    }
    return *this;
  }
  ~MemoryLock();

  //----------------------------------------------------------------------------
  // Operations
  // ----------------------------------------------------------------------------

  // Returns the underlying memory bank for this lock. If this is null,
  // MemoryLock does not reference a MemoryBank and cannot be accessed.
  MemoryBank* GetMemoryBank() { return bank_; }

  // The current address on the address bus for the MemoryBank. All regular
  // reads and writes through a MemoryLock update based on this address.
  uint16_t GetAddress() const;

  // Sets a new read/write address for the MemoryBank. This takes
  // kMemoryBankSetAddressCycles to execute.
  void SetAddress(uint16_t address);

  // Reads/Writes the specified buffer of words from/to the memory bank starting
  // at current address. This will automatically "wrap" at the end of address
  // space back to address zero. The address is automatically incremented (with
  // wrapping) by `size`. This takes `size * kMemoryBankAccessWordCycles
  // virtual cycles to execute.
  void ReadWords(uint16_t* data, int size);
  void WriteWords(const uint16_t* data, int size);

  // Returns the word at the current address and increments the address. This
  // takes kMemoryBankAccessWordCycles to execute.
  uint16_t LoadWord();

  // Writes the word to the current address and increments the address. This
  // takes kMemoryBankAccessWordCycles to execute.
  void StoreWord(uint16_t value);

  // Decrements address and writes the word at the new address. This takes
  // kMemoryBankAccessWordCycles to execute.
  void PushWord(uint16_t value);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------
  friend class MemoryBank;

  MemoryLock(MemoryBank* bank);

  MemoryBank* bank_;
};

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
class MemoryBank final {
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
  // Operations
  //----------------------------------------------------------------------------

  // Returns the start of the memory bank in 16-bit words.
  int GetMemoryStart() const { return mem_range_.start; }

  // Returns the size of the memory bank in 16-bit words.
  int GetMemorySize() const { return mem_range_.count; }

  // Returns true if the memory is locked.
  bool IsLocked() const { return locked_ || remaining_cycles_ > 0; }

  // Returns the size of the memory bank in 16-bit words.
  // Locks memory, returning an exclusive lock object which provides regular
  // simulated access to the memory.
  //
  // When the lock is destroyed, the MemoryBank may be accessed again. All
  // actual memory access is done the lock object.
  //
  // It is invalid to call this if memory is already locked.
  MemoryLock Lock();

  // Returns the number of remaining cycles before the memory bank is available.
  Cycles GetRemainingCycles() const { return remaining_cycles_; }

  // Advances the specified number of cycles.
  void AdvanceCycles(Cycles cycles);

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
  friend class MemoryLock;

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

  // True if the memory bank is currently locked (an active MemoryLock object
  // exists). Only one active (non-moved-from) MemoryLock can exist at any given
  // time.
  bool locked_ = false;

  // Value of address bus for the MemoryBank.
  uint16_t address_ = 0;

  // How many cycles remain before the memory bank is available for locking
  // again.
  Cycles remaining_cycles_ = 0;
};

//==============================================================================
// MemoryLock inline
//==============================================================================

inline MemoryLock::MemoryLock(MemoryBank* bank) : bank_(bank) {
  DCHECK(!bank_->locked_);
  bank_->locked_ = true;
}

inline MemoryLock::~MemoryLock() {
  if (bank_ != nullptr) {
    bank_->locked_ = false;
  }
}

inline uint16_t MemoryLock::GetAddress() const { return bank_->address_; }

inline void MemoryLock::SetAddress(uint16_t address) {
  bank_->address_ = address;
  bank_->remaining_cycles_ += kMemoryBankSetAddressCycles;
}

inline uint16_t MemoryLock::LoadWord() {
  const uint16_t value = bank_->ReadWord();
  bank_->address_ += 1;
  bank_->remaining_cycles_ += kMemoryBankAccessWordCycles;
  return value;
}

inline void MemoryLock::StoreWord(uint16_t value) {
  bank_->WriteWord(value);
  bank_->address_ += 1;
  bank_->remaining_cycles_ += kMemoryBankAccessWordCycles;
}

inline void MemoryLock::PushWord(uint16_t value) {
  bank_->address_ -= 1;
  bank_->WriteWord(value);
  bank_->remaining_cycles_ += kMemoryBankAccessWordCycles;
}

inline void MemoryLock::ReadWords(uint16_t* data, int size) {
  DCHECK(size >= 0 && size <= kMemoryBankMaxSize);
  bank_->ReadWrap(data, size);
  bank_->address_ += size;
  bank_->remaining_cycles_ += size * kMemoryBankAccessWordCycles;
}

inline void MemoryLock::WriteWords(const uint16_t* data, int size) {
  DCHECK(size >= 0 && size <= kMemoryBankMaxSize);
  DCHECK(data != nullptr);
  bank_->WriteWrap(data, size);
  bank_->address_ += size;
  bank_->remaining_cycles_ += size * kMemoryBankAccessWordCycles;
}

}  // namespace oz3

#endif  // OZ3_CORE_MEMORY_BANK_H_
