// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_MEMORY_BANK_CONFIG_H_
#define OZ3_CORE_MEMORY_BANK_CONFIG_H_

#include <cstdint>
#include <limits>

#include "oz3/core/core_types.h"
#include "oz3/core/memory_map.h"

namespace oz3 {

//==============================================================================
// Constants
//==============================================================================

// The maximum number of memory banks that can managed by a single Processor.
inline constexpr int kMaxMemoryBanks = 16;

// Total number of MemoryBank pages.
inline constexpr int kMemoryBankPageCount = 16;

// Size in 16-bit words of a MemoryBank page.
inline constexpr int kMemoryBankPageSize = 4096;

// Max size in 16-bit words of a MemoryBank.
inline constexpr int kMemoryBankMaxSize =
    kMemoryBankPageCount * kMemoryBankPageSize;
static_assert(kMemoryBankMaxSize - 1 == std::numeric_limits<uint16_t>::max());

// Cycle timing constants for MemoryBank access
inline constexpr Cycles kMemoryBankSetAddressCycles = 1;
inline constexpr Cycles kMemoryBankAccessWordCycles = 1;

//==============================================================================
// MemoryPageMasks
//==============================================================================

// Defines the read/write permissions for the memory pages in a memory bank.
struct MemoryPageMasks {
  // Convenience factory methods to generate masks for the common cases.
  static MemoryPageMasks ReadWrite(uint16_t mask = 0xFFFF) {
    return {mask, mask};
  }
  static MemoryPageMasks ReadOnly(uint16_t mask = 0xFFFF) {
    return {mask, 0x0000};
  }
  static MemoryPageMasks WriteOnly(uint16_t mask = 0xFFFF) {
    return {0, 0xFFFF};
  }

  // Restricts reading the memory page to the specified mask. Each set bit
  // indicates the corresponding page is readable.
  uint16_t read_mask = 0xFFFF;

  // Restricts writing the memory page to the specified mask. Each set bit
  // indicates the corresponding page is writable.
  uint16_t write_mask = 0xFFFF;
};

//==============================================================================
// MemoryPageMasks
//==============================================================================

// Defines a range of memory pages within a memory bank.
struct MemoryPageRange {
  // Convenience factory methods to generate ranges for the common cases.
  static MemoryPageRange Max() { return {0, kMemoryBankPageCount}; }

  // Page index of the first page of memory.
  //
  // Should be in the range [0, kMemoryBankPageCount)
  int start_page = 0;

  // Number of pages in the range.
  //
  // Should be in the range [0, kMemoryBankPageCount - start]
  int page_count = kMemoryBankPageCount;
};

//==============================================================================
// MemoryBankConfig
//==============================================================================

// The MemoryBankConfig class defines the configuration needed to create a
// memory bank.
class MemoryBankConfig {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates an empty memory bank configuration with no RAM and no memory
  // mapping.
  static MemoryBankConfig Empty() { return MemoryBankConfig{}; }

  // Creates a memory bank configuration with the maximum possible RAM memory.
  static MemoryBankConfig MaxRam();

  // Creates a memory bank configuration with the specified number of ROM and
  // RAM pages. The ROM pages clamped to a valid range, and RAM pages are
  // clamped to whatever is left.
  static MemoryBankConfig RomRam(int rom_pages,
                                 int ram_pages = kMemoryBankPageCount);

  // Creates a default memory bank configuration which represents an empty
  // memory bank (no physical RAM or memory map).
  MemoryBankConfig() = default;

  // Sets the page range for the physically allocated memory backing this
  // MemoryBank. The page range is clamped to a valid range (which may then be
  // empty), and the read/write masks are clamped to specified page range.
  MemoryBankConfig& SetMemPages(
      MemoryPageRange range,
      MemoryPageMasks masks = MemoryPageMasks::ReadWrite());

  // Sets an optional memory map interface. This will be executed for any
  // read/write attempt that is within the memory map read/write mask. If set,
  // this is called before access to physical memory occurs. The MemoryMap
  // must outlive the MemoryBank.
  MemoryBankConfig& SetMemoryMap(MemoryMap* map,
                                 MemoryPageMasks masks = {0xFFFF, 0xFFFF});

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  const MemoryPageRange& GetMemPages() const { return mem_pages_; }
  const MemoryPageMasks& GetMemMasks() const { return mem_masks_; }
  MemoryMap* GetMemoryMap() const { return map_; }
  const MemoryPageMasks& GetMemoryMapMasks() const { return map_masks_; }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // Physical memory configuration.
  MemoryPageRange mem_pages_ = {0, 0};
  MemoryPageMasks mem_masks_ = {0, 0};

  // Memory map configuration.
  MemoryMap* map_ = nullptr;
  MemoryPageMasks map_masks_ = {0, 0};
};

}  // namespace oz3

#endif  // OZ3_CORE_MEMORY_BANK_CONFIG_H_
