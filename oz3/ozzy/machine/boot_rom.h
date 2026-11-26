// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_MACHINE_BOOT_ROM_H_
#define OZ3_OZZY_MACHINE_BOOT_ROM_H_

#include <array>
#include <cstdint>

#include "oz3/tools/program_assembler.h"

namespace oz3 {

// Returns the boot ROM, and its map, assembled from boot_rom.oza the first
// time it is called. The boot ROM's code is a ROM segment at page 0 of bank 0,
// which ends before the machine table, and its stack a RAM segment at page 1
// of bank 0.
const AsmProgram& GetBootRom();

// Where the boot ROM's stack ends, in page 1 of bank 0, which the host starts
// it with as BS.
inline constexpr uint16_t kBootRomStackEnd = 0x1200;

// Where the boot ROM reads the loader to, and starts it, in bank 0.
inline constexpr uint16_t kLoaderStart = 0x1000;

// Where a loader that fails jumps to in the boot ROM, in bank 0, with its error
// code in R0.
inline constexpr uint16_t kBootRomExit = 0x0002;

// The boot signature, "BOOT" packed, which the last two words of a disk's boot
// area must be for the boot ROM to start the loader in it.
inline constexpr std::array<uint16_t, 2> kBootSignature = {0x4F42, 0x544F};

}  // namespace oz3

#endif  // OZ3_OZZY_MACHINE_BOOT_ROM_H_
