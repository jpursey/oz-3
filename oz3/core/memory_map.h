// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_MEMORY_MAP_H_
#define OZ3_CORE_MEMORY_MAP_H_

#include <cstdint>
#include <memory>

#include "oz3/core/core_types.h"

namespace oz3 {

//==============================================================================
// MemoryMap
//==============================================================================

// The MemoryMap interface provides a way for an external class (usually a
// Coprocessor or Device derived class) to map functionality to a range of pages
// in a MemoryBank. Memory maps always overlay full pages (it is not possible to
// memory map less than a page), and each mapped page belongs wholly to its map.
//
// A map has addresses of its own: address 0 is the start of its first page, so
// it works the same wherever it is placed in the bank.
//
// The bank calls Read and Write under the bank's lock, so whatever the map's
// owner does with the map's storage must take the lock too. A derived map
// enforces this by taking a Lock reference in its accessors, and verifying it
// with IsLocked, as Lockable describes.
class MemoryMap {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  MemoryMap() = default;
  MemoryMap(const MemoryMap&) = delete;
  MemoryMap& operator=(const MemoryMap&) = delete;
  virtual ~MemoryMap() = default;

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Requests a lock on the bank this map is in. The map must be in a bank.
  std::unique_ptr<Lock> RequestLock();

  // Returns true if `lock` is a lock on the bank this map is in.
  bool IsLocked(const Lock& lock) const;

 protected:
  //----------------------------------------------------------------------------
  // Derived class interface
  //----------------------------------------------------------------------------

  // Reads `size` words into `data` from the map's own `address`. This is
  // called by the bank under its lock, and never reads past the end of the
  // map's pages.
  virtual void Read(int address, uint16_t* data, int size);

  // Writes `size` words from `data` to the map's own `address`. This is called
  // by the bank under its lock, and never writes past the end of the map's
  // pages.
  virtual void Write(int address, const uint16_t* data, int size);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The bank attaches itself to the map, and calls Read and Write.
  friend class MemoryBank;

  // The bank this map is in, or null if it is not in one.
  MemoryBank* bank_ = nullptr;
};

}  // namespace oz3

#endif  // OZ3_CORE_MEMORY_MAP_H_
