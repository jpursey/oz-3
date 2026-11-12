// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_media.h"

namespace oz3 {

DiskMedia::DiskMedia(int64_t sector_count, uint16_t flags)
    : sector_count_(sector_count), flags_(flags) {}

DiskStatus DiskMedia::SetWriteProtected(bool write_protected) {
  const uint16_t flags =
      write_protected ? (flags_ | kWriteProtectedFlag)
                      : static_cast<uint16_t>(flags_ & ~kWriteProtectedFlag);
  if (DiskStatus status = WriteFlags(flags); status != DiskStatus::kOk) {
    return status;
  }
  flags_ = flags;
  return DiskStatus::kOk;
}

}  // namespace oz3
