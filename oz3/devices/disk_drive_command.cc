// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/disk_drive_command.h"

namespace oz3 {

namespace {

constexpr DiskPortWidth kWord = DiskPortWidth::kWord;
constexpr DiskPortWidth kDword = DiskPortWidth::kDword;

// The fields commands move, each named for what it holds. An 8-word field (a
// name, pattern, or label) is 4 dwords, and a directory entry is 8.
constexpr DiskPortWidth kOneWord[] = {kWord};
constexpr DiskPortWidth kOneDword[] = {kDword};
constexpr DiskPortWidth kName[] = {kDword, kDword, kDword, kDword};
constexpr DiskPortWidth kTwoNames[] = {kDword, kDword, kDword, kDword,
                                       kDword, kDword, kDword, kDword};
constexpr DiskPortWidth kNameAndWord[] = {kDword, kDword, kDword, kDword,
                                          kWord};
constexpr DiskPortWidth kEntry[] = {kDword, kDword, kDword, kDword,
                                    kDword, kDword, kDword, kDword};
constexpr DiskPortWidth kInfoResults[] = {kDword, kDword};
constexpr DiskPortWidth kMediaResults[] = {kDword, kWord};
constexpr DiskPortWidth kVolumeResults[] = {kDword, kDword, kDword, kDword,
                                            kWord,  kWord,  kWord};

constexpr DiskCommandKind kLatent = DiskCommandKind::kLatent;
constexpr DiskCommandKind kRead = DiskCommandKind::kRead;
constexpr DiskCommandKind kWrite = DiskCommandKind::kWrite;

// The command table, as the Commands section of docs/disk_design.md has it.
const DiskCommandDef kCommandDefs[] = {
    {.command = DiskDriveCommand::kOpen,
     .kind = kLatent,
     .arguments = kName,
     .completion_results = kOneWord},
    {.command = DiskDriveCommand::kOpenRoot,
     .kind = kLatent,
     .completion_results = kOneWord},
    {.command = DiskDriveCommand::kCreate,
     .kind = kLatent,
     .arguments = kName,
     .completion_results = kOneWord},
    {.command = DiskDriveCommand::kClose,
     .takes_handle = true,
     .kind = kLatent},
    {.command = DiskDriveCommand::kFlush,
     .takes_handle = true,
     .kind = kLatent},
    {.command = DiskDriveCommand::kRead,
     .takes_handle = true,
     .kind = kRead,
     .arguments = kOneWord,
     .accept_results = kOneWord,
     .completion_results = kOneWord},
    {.command = DiskDriveCommand::kWrite,
     .takes_handle = true,
     .kind = kWrite,
     .arguments = kOneWord,
     .accept_results = kOneWord,
     .completion_results = kOneWord},
    {.command = DiskDriveCommand::kReadPacked,
     .takes_handle = true,
     .kind = kRead,
     .packed = true,
     .arguments = kOneWord,
     .accept_results = kOneWord,
     .completion_results = kOneWord},
    {.command = DiskDriveCommand::kWritePacked,
     .takes_handle = true,
     .kind = kWrite,
     .packed = true,
     .arguments = kOneWord,
     .accept_results = kOneWord,
     .completion_results = kOneWord},
    {.command = DiskDriveCommand::kSeek,
     .takes_handle = true,
     .arguments = kOneDword},
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
     .arguments = kTwoNames},
    {.command = DiskDriveCommand::kAttrib,
     .kind = kLatent,
     .arguments = kNameAndWord},
    {.command = DiskDriveCommand::kMedia, .accept_results = kMediaResults},
    {.command = DiskDriveCommand::kVolume,
     .kind = kLatent,
     .completion_results = kVolumeResults},
    {.command = DiskDriveCommand::kFormat,
     .kind = kLatent,
     .arguments = kNameAndWord},
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
