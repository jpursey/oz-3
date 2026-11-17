// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_status.h"

#include <string_view>

namespace oz3 {

std::string_view GetDiskStatusText(DiskStatus status) {
  switch (status) {
    case DiskStatus::kOk:
      return "ok";
    case DiskStatus::kBadCommand:
      return "bad command";
    case DiskStatus::kBadHandle:
      return "bad handle";
    case DiskStatus::kBadName:
      return "bad name";
    case DiskStatus::kBadPattern:
      return "bad pattern";
    case DiskStatus::kBadLabel:
      return "bad label";
    case DiskStatus::kBadAttributes:
      return "bad attributes";
    case DiskStatus::kBadArgument:
      return "bad argument";
    case DiskStatus::kPastEnd:
      return "past end";
    case DiskStatus::kWrongHandleType:
      return "wrong handle type";
    case DiskStatus::kNotFound:
      return "not found";
    case DiskStatus::kExists:
      return "exists";
    case DiskStatus::kInUse:
      return "in use";
    case DiskStatus::kTooManyOpen:
      return "too many open";
    case DiskStatus::kReadOnly:
      return "read-only";
    case DiskStatus::kDiskChanged:
      return "disk changed";
    case DiskStatus::kNoDisk:
      return "no disk";
    case DiskStatus::kWriteProtected:
      return "write protected";
    case DiskStatus::kDiskFull:
      return "disk full";
    case DiskStatus::kUnformatted:
      return "unformatted";
    case DiskStatus::kCorrupt:
      return "corrupt";
    case DiskStatus::kIoError:
      return "I/O error";
  }
  return "unknown status";
}

}  // namespace oz3
