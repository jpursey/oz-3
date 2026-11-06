// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_BOOT_ROM_H_
#define OZ3_OZZY_BOOT_ROM_H_

#include "oz3/tools/program_assembler.h"

namespace oz3 {

// Returns the boot ROM, and its map, assembled from boot_rom.oza the first
// time it is called. The boot ROM's code is a ROM segment at page 0 of bank 0,
// which ends before the machine table, and its stack a RAM segment in any
// bank.
const AsmProgram& GetBootRom();

}  // namespace oz3

#endif  // OZ3_OZZY_BOOT_ROM_H_
