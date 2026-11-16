// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/disk_drive_command.h"

namespace oz3 {

namespace {

// The fields commands move, each named for what it holds. A name, pattern, or
// label is 8 words, so 4 fields, and a directory entry is 8 fields.
constexpr int kName = 4;
constexpr int kEntry = 8;
constexpr int kInfoResults = 2;            // Position and size.
constexpr int kMediaResults = 2;           // Sectors and flags.
constexpr int kVolumeResults = kName + 3;  // Label, and three counts.

constexpr DiskCommandKind kLatent = DiskCommandKind::kLatent;
constexpr DiskCommandKind kRead = DiskCommandKind::kRead;
constexpr DiskCommandKind kWrite = DiskCommandKind::kWrite;

// The command table, as the Commands section of docs/disk_design.md has it.
const DiskCommandDef kCommandDefs[] = {
    {.command = DiskDriveCommand::kOpen,
     .kind = kLatent,
     .arguments = kName,
     .completion_results = 1},
    {.command = DiskDriveCommand::kOpenRoot,
     .kind = kLatent,
     .completion_results = 1},
    {.command = DiskDriveCommand::kCreate,
     .kind = kLatent,
     .arguments = kName,
     .completion_results = 1},
    {.command = DiskDriveCommand::kClose,
     .takes_handle = true,
     .kind = kLatent},
    {.command = DiskDriveCommand::kFlush,
     .takes_handle = true,
     .kind = kLatent},
    {.command = DiskDriveCommand::kRead,
     .takes_handle = true,
     .kind = kRead,
     .arguments = 1,
     .accept_results = 1,
     .completion_results = 1},
    {.command = DiskDriveCommand::kWrite,
     .takes_handle = true,
     .kind = kWrite,
     .arguments = 1,
     .accept_results = 1,
     .completion_results = 1},
    {.command = DiskDriveCommand::kReadPacked,
     .takes_handle = true,
     .kind = kRead,
     .packed = true,
     .arguments = 1,
     .accept_results = 1,
     .completion_results = 1},
    {.command = DiskDriveCommand::kWritePacked,
     .takes_handle = true,
     .kind = kWrite,
     .packed = true,
     .arguments = 1,
     .accept_results = 1,
     .completion_results = 1},
    {.command = DiskDriveCommand::kSeek, .takes_handle = true, .arguments = 1},
    {.command = DiskDriveCommand::kInfo,
     .takes_handle = true,
     .accept_results = kInfoResults},
    {.command = DiskDriveCommand::kTruncate,
     .takes_handle = true,
     .kind = kLatent},
    {.command = DiskDriveCommand::kFind,
     .takes_handle = true,
     .kind = kLatent,
     .arguments = kName,
     .completion_results = kEntry},
    {.command = DiskDriveCommand::kDelete, .kind = kLatent, .arguments = kName},
    {.command = DiskDriveCommand::kRename,
     .kind = kLatent,
     .arguments = 2 * kName},
    {.command = DiskDriveCommand::kAttrib,
     .kind = kLatent,
     .arguments = kName + 1},
    {.command = DiskDriveCommand::kMedia, .accept_results = kMediaResults},
    {.command = DiskDriveCommand::kVolume,
     .kind = kLatent,
     .completion_results = kVolumeResults},
    {.command = DiskDriveCommand::kFormat,
     .kind = kLatent,
     .arguments = kName + 1},
    {.command = DiskDriveCommand::kLabel, .kind = kLatent, .arguments = kName},
};

}  // namespace

const DiskCommandDef* GetDiskCommandDef(uint8_t code) {
  for (const DiskCommandDef& def : kCommandDefs) {
    if (static_cast<uint8_t>(def.command) == code) {
      return &def;
    }
  }
  return nullptr;
}

}  // namespace oz3
