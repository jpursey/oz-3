// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_DISK_DRIVE_COMMAND_H_
#define OZ3_DEVICES_DISK_DRIVE_COMMAND_H_

#include <cstdint>

#include "absl/types/span.h"

namespace oz3 {

//==============================================================================
// DiskDriveCommand
//==============================================================================

// The disk drive's command codes, which are the high byte of a command word.
// The low nibble numbers commands within a group, and 0 is never a command.
enum class DiskDriveCommand : uint8_t {
  // Files.
  kOpen = 0x10,
  kOpenRoot = 0x11,
  kCreate = 0x12,
  kClose = 0x13,
  kFlush = 0x14,

  // Data.
  kRead = 0x20,
  kWrite = 0x21,
  kReadPacked = 0x22,
  kWritePacked = 0x23,
  kSeek = 0x24,
  kInfo = 0x25,
  kTruncate = 0x26,

  // Directory.
  kFind = 0x30,
  kDelete = 0x31,
  kRename = 0x32,
  kAttrib = 0x33,

  // Disk.
  kMedia = 0x40,
  kVolume = 0x41,
  kFormat = 0x42,
  kLabel = 0x43,
};

//==============================================================================
// DiskDrivePhase
//==============================================================================

// Where the drive's current command is, as the low nibble of its control
// port's state word shows it.
enum class DiskDrivePhase : uint8_t {
  kIdle = 0,       // Waiting for a command word.
  kArguments = 1,  // Waiting for more argument words.
  kWorking = 2,    // Busy, with nothing to send yet.
  kSending = 3,    // Words waiting on the command port.
  kReading = 4,    // Streaming data to the data port.
  kWriting = 5,    // Taking data from the data port.
};

//==============================================================================
// DiskCommandDef
//==============================================================================

// How many of a port's two words one field takes on the drive's command port:
// a word field (a status or a count) is word 0, and a dword field (a position)
// is both, the low word in word 0.
enum class DiskPortWidth : uint8_t {
  kWord,
  kDword,
};

// What follows a command's accept, when its status is OK.
enum class DiskCommandKind : uint8_t {
  kSynchronous,  // Nothing: the accept's results are the command's.
  kLatent,       // A completion, posted later with the interrupt.
  kRead,         // A data phase out of the drive, then a completion.
  kWrite,        // A data phase into the drive, then a completion.
};

// One command's definition: whether it takes a handle in its command word's low
// byte, what follows its accept, and the fields it moves at each step. Each
// step's status is a word field of its own, before the step's results, and an
// error status ends the command's words, so results follow only an OK.
struct DiskCommandDef {
  DiskDriveCommand command = DiskDriveCommand::kMedia;
  bool takes_handle = false;
  DiskCommandKind kind = DiskCommandKind::kSynchronous;
  bool packed = false;  // A data phase moves dwords, not words.
  absl::Span<const DiskPortWidth> arguments;
  absl::Span<const DiskPortWidth> accept_results;
  absl::Span<const DiskPortWidth> completion_results;
};

// Returns the definition of the command with code `code`, or null if there is
// no such command.
const DiskCommandDef* GetDiskCommandDef(uint8_t code);

}  // namespace oz3

#endif  // OZ3_DEVICES_DISK_DRIVE_COMMAND_H_
