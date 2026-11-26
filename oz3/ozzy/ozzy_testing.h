// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_OZZY_TESTING_H_
#define OZ3_OZZY_OZZY_TESTING_H_

#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "gb/base/context.h"
#include "oz3/core/cpu_core.h"
#include "oz3/devices/power_control.h"
#include "oz3/devices/screen.h"
#include "oz3/disk/disk_superblock.h"
#include "oz3/ozzy/boot_rom.h"
#include "oz3/ozzy/ozzy.h"
#include "oz3/ozzy/ozzy_settings.h"
#include "oz3/tools/program_assembler.h"
#include "oz3/tools/program_map.h"

namespace oz3 {

//==============================================================================
// The host
//==============================================================================

// Returns `context` with a power request callback, as every Ozzy's context
// needs one. It adds each request to `requests` if it isn't null, which must
// outlive the Ozzy, and otherwise ignores them.
gb::Context WithPowerCallback(gb::Context context = {},
                              std::vector<PowerRequest>* requests = nullptr);

// Returns the settings of the default Ozzy with a teletype at `baud` in place
// of the screen.
OzzySettings GetTeletypeSettings(int baud);

//==============================================================================
// Programs
//==============================================================================

// Returns `source` assembled in the default instruction set, adding a test
// failure for each error, or null if it doesn't assemble.
std::unique_ptr<AsmProgram> AssembleTestProgram(std::string_view source);

// Returns `program`'s bare program in a boot area, as `oz3dsk boot` writes it,
// adding a test failure if it isn't a bare program that fills the boot area.
DiskSuperblock::BootArea GetBootArea(const AsmProgram& program);

// Expects `map` to have each of `expected`'s constants, with its value.
void ExpectSourceConstants(const ProgramMap& map,
                           std::map<std::string_view, uint32_t> expected);

//==============================================================================
// The boot ROM
//==============================================================================

// The prompt the boot ROM shows.
inline constexpr std::string_view kBootPrompt = "Press Enter to Boot...";

// Where it shows the prompt and the status on the screen, after the banner's
// four rows, in a box, and a blank row.
inline constexpr int kBootPromptRow = 5;
inline constexpr int kBootStatusRow = 7;

// Where the boot ROM keeps the loader's error code: its ninth variable from the
// end of its stack (the boot ROM's V_ERROR).
inline constexpr int kLoaderErrorAddress = kBootRomStackEnd - 9;

// Returns the status the boot ROM shows after a loader fails with `code`.
std::string GetLoaderErrorStatus(int code);

//==============================================================================
// Core 0 and the screen
//==============================================================================

// Returns core 0's state.
CpuCore::State GetState(Ozzy& ozzy);

// Returns core 0's registers.
CpuCore::Registers GetRegisters(Ozzy& ozzy);

// Returns whether core 0 has halted.
bool IsHalted(Ozzy& ozzy);

// Returns the text of row `row` of `picture`, without its cells' attributes,
// and without the spaces at its end.
std::string GetRowText(const ScreenPicture& picture, int row);

}  // namespace oz3

#endif  // OZ3_OZZY_OZZY_TESTING_H_
