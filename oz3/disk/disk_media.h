// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_MEDIA_H_
#define OZ3_DISK_DISK_MEDIA_H_

#include <cstdint>

#include "oz3/disk/disk_status.h"

namespace oz3 {

// The medium in a drive, as its image's header describes it and the drive's
// MEDIA command reports it: its sector count and its flags, such as write
// protection. It is never visible to the OZ-3 as data.
//
// A DiskImage is the one kind of medium. Drives hand out the medium rather
// than the image, so its sectors are only read and written through them.
class DiskMedia {
 public:
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The header's flags: bit 0 is write protection. The others are reserved.
  static constexpr uint16_t kWriteProtectedFlag = 0x0001;

  // The most sectors a medium can have, as its header counts them in a dword.
  static constexpr int64_t kMaxSectorCount = 0xFFFF'FFFF;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  DiskMedia(const DiskMedia&) = delete;
  DiskMedia& operator=(const DiskMedia&) = delete;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the number of sectors, from 1 to kMaxSectorCount.
  int64_t GetSectorCount() const { return sector_count_; }

  // Returns the header's flags word, as the drive's MEDIA command reports it.
  uint16_t GetFlags() const { return flags_; }

  // Returns whether the medium is write-protected, refusing sector writes.
  bool IsWriteProtected() const { return (flags_ & kWriteProtectedFlag) != 0; }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Sets or clears the header's write-protect bit, which a protected medium
  // still allows. Returns kOk, or kIoError if the header can't be written.
  DiskStatus SetWriteProtected(bool write_protected);

 protected:
  //----------------------------------------------------------------------------
  // Derived class interface
  //----------------------------------------------------------------------------

  // `sector_count` must be 1 to kMaxSectorCount.
  DiskMedia(int64_t sector_count, uint16_t flags);
  ~DiskMedia() = default;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // Writes `flags` to the header. Returns kOk, or kIoError if the write fails.
  virtual DiskStatus WriteFlags(uint16_t flags) = 0;

  const int64_t sector_count_;
  uint16_t flags_;
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_MEDIA_H_
