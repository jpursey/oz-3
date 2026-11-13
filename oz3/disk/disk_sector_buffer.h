// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_SECTOR_BUFFER_H_
#define OZ3_DISK_DISK_SECTOR_BUFFER_H_

#include <array>
#include <cstdint>

#include "oz3/disk/disk_layout.h"

namespace oz3 {

// A buffer holding one sector of a disk.
struct DiskSectorBuffer {
  // Makes the buffer hold no sector, discarding any change, without clearing
  // its words.
  void Reset() {
    sector = -1;
    dirty = false;
  }

  int sector = -1;     // -1 for none.
  bool dirty = false;  // Changed since it was read or written.
  std::array<uint16_t, kDiskSectorWords> words = {};
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_SECTOR_BUFFER_H_
