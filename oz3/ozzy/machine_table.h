// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_MACHINE_TABLE_H_
#define OZ3_OZZY_MACHINE_TABLE_H_

#include <cstdint>

#include "oz3/core/core_types.h"

namespace oz3 {

//==============================================================================
// The machine table
//
// The machine table describes an Ozzy's hardware to the programs it runs. The
// host writes it into the boot ROM's page at power on, at kMachineTableAddress
// in bank 0. It is words: a header, then an entry for each bank, then an entry
// for each device, with the rest of its kMachineTableSize words 0. Each entry
// is a fixed number of words, and the offsets below are word offsets into the
// header or an entry.
//==============================================================================

// Where the table is in bank 0, and its size in words.
inline constexpr uint16_t kMachineTableAddress = 0x0F00;
inline constexpr int kMachineTableSize = 256;

// The table's version, which is its first word.
inline constexpr uint16_t kMachineTableVersion = 1;

// The most devices the table has room for.
inline constexpr int kMaxMachineDevices = 16;

// A port, interrupt, bank, or page that a device doesn't have.
inline constexpr uint16_t kMachineTableNone = 0xFFFF;

//------------------------------------------------------------------------------
// The header
//------------------------------------------------------------------------------

inline constexpr int kMachineHeaderVersion = 0;
inline constexpr int kMachineHeaderClockHzLow = 1;   // The clock rate in Hz,
inline constexpr int kMachineHeaderClockHzHigh = 2;  // low word first.
inline constexpr int kMachineHeaderCores = 3;
inline constexpr int kMachineHeaderPorts = 4;
inline constexpr int kMachineHeaderBanks = 5;
inline constexpr int kMachineHeaderDevices = 6;
inline constexpr int kMachineHeaderSize = 8;  // The last word is reserved.

//------------------------------------------------------------------------------
// A bank entry
//------------------------------------------------------------------------------

// The bank's ROM and RAM pages, with bit n for page n.
inline constexpr int kMachineBankRom = 0;
inline constexpr int kMachineBankRam = 1;
inline constexpr int kMachineBankSize = 2;

//------------------------------------------------------------------------------
// A device entry
//------------------------------------------------------------------------------

// The type of a device in its entry.
enum class MachineDeviceType : uint16_t {
  kScreen = 1,
  kKeyboard = 2,
  kTeletype = 3,
};

inline constexpr int kMachineDeviceType = 0;
inline constexpr int kMachineDevicePort = 1;
inline constexpr int kMachineDeviceInterrupt = 2;

// The cores the interrupt is raised on, with bit n for core n.
inline constexpr int kMachineDeviceInterruptCores = 3;

// The bank the device's pages are in, and the first of them.
inline constexpr int kMachineDeviceBank = 4;
inline constexpr int kMachineDevicePage = 5;

// Two words that are the type's own: a screen's columns and rows, and
// otherwise 0.
inline constexpr int kMachineDeviceData = 6;
inline constexpr int kMachineDeviceSize = 8;

static_assert(kMachineTableAddress + kMachineTableSize <= kMemoryBankPageSize,
              "The machine table must be in the boot ROM's page");
static_assert(kMachineHeaderSize + kMaxMemoryBanks * kMachineBankSize +
                      kMaxMachineDevices * kMachineDeviceSize <=
                  kMachineTableSize,
              "The machine table must hold every bank and device");

}  // namespace oz3

#endif  // OZ3_OZZY_MACHINE_TABLE_H_
