// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_STATUS_H_
#define OZ3_DISK_DISK_STATUS_H_

#include <cstdint>

namespace oz3 {

// The result of a disk operation: the drive's status codes, which it reports
// to programs as they are. Codes 01h to 0Fh are problems with the request
// itself, 10h to 1Fh with files and slots, and 20h to 2Fh with the disk.
enum class DiskStatus : uint16_t {
  kOk = 0x00,

  // The request.
  kBadCommand = 0x01,       // Unknown command code.
  kBadHandle = 0x02,        // Not a slot, or a slot with nothing open.
  kBadName = 0x03,          // A name field that breaks the name rules.
  kBadPattern = 0x04,       // A pattern that breaks the pattern rules.
  kBadLabel = 0x05,         // A label that breaks the label rules.
  kBadAttributes = 0x06,    // Reserved attribute bits set.
  kBadArgument = 0x07,      // Any other argument out of range.
  kPastEnd = 0x08,          // A seek beyond the file's size.
  kWrongHandleType = 0x09,  // A directory handle for a file, or the reverse.

  // Files and slots.
  kNotFound = 0x10,     // No such file, or no further match.
  kExists = 0x11,       // The name is already taken.
  kInUse = 0x12,        // The file is open, or for a format, any file is.
  kTooManyOpen = 0x13,  // Every slot is in use.
  kReadOnly = 0x14,     // The file's read-only attribute is set.
  kDiskChanged = 0x15,  // Opened on a disk that has since been removed.

  // The disk.
  kNoDisk = 0x20,          // No image is attached.
  kWriteProtected = 0x21,  // The image is write-protected.
  kDiskFull = 0x22,        // No free clusters for data or directory growth.
  kUnformatted = 0x23,     // No valid file system on the disk.
  kCorrupt = 0x24,         // The file system is inconsistent.
  kIoError = 0x25,         // The host failed to read or write the image.
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_STATUS_H_
