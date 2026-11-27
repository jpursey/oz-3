// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_MACHINE_OZZY_TESTING_H_
#define OZ3_OZZY_MACHINE_OZZY_TESTING_H_

#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/time/time.h"
#include "gb/base/context.h"
#include "gb/file/file_system.h"
#include "oz3/asm/program_assembler.h"
#include "oz3/core/cpu_core.h"
#include "oz3/devices/power_control/power_control.h"
#include "oz3/devices/screen/screen.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_superblock.h"
#include "oz3/ozzy/machine/boot_rom.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"
#include "oz3/program/program_map.h"

namespace oz3 {

//==============================================================================
// The host
//==============================================================================

// Returns `context` with a power request callback, as every Ozzy's context
// needs one. It adds each request to `requests` if it isn't null, which must
// outlive the Ozzy, and otherwise ignores them.
gb::Context WithPowerCallback(gb::Context context = {},
                              std::vector<PowerRequest>* requests = nullptr);

// Returns an Ozzy with `settings`, created in `context` with a power request
// callback that ignores requests (see WithPowerCallback), or null after failing
// the test.
std::unique_ptr<Ozzy> CreateTestOzzy(const OzzySettings& settings = {},
                                     gb::Context context = {});

// Returns the settings of the default Ozzy with a teletype at `baud` in place
// of the screen.
OzzySettings GetTeletypeSettings(int baud);

//==============================================================================
// Programs
//==============================================================================

// Returns `source` assembled in the default instruction set, adding a test
// failure for each error, or null if it doesn't assemble.
std::unique_ptr<AsmProgram> AssembleTestProgram(std::string_view source);

// Assembles `source` in the default instruction set, adding a test failure for
// each error, loads it into `ozzy`, and resets core 0 to run it, in place of
// the boot ROM. Returns whether it did.
bool LoadOnCore0(Ozzy& ozzy, std::string_view source);

// Returns `program`'s bare program in a boot area, as `oz3dsk boot` writes it,
// adding a test failure if it isn't a bare program that fills the boot area.
DiskSuperblock::BootArea GetBootArea(const AsmProgram& program);

// Expects `map` to have each of `expected`'s constants, with its value.
void ExpectSourceConstants(const ProgramMap& map,
                           std::map<std::string_view, uint32_t> expected);

//==============================================================================
// Disk images
//==============================================================================

// Makes a formatted image at `path` in `host`, with a 360K floppy's sectors,
// adding a test failure if it can't.
void MakeTestImage(gb::FileSystem* host, std::string_view path);

// Returns the root's entry for `name` on the image at `path` in `host`, and
// its words, or nullopt if there is no such file. Nothing else may have the
// image open.
std::optional<std::pair<DiskDirEntry, std::vector<uint16_t>>> ReadTestFile(
    gb::FileSystem* host, std::string_view path, std::string_view name);

// A program for core 0 (see LoadOnCore0) that creates the file LOG on drive A:,
// on its ports and interrupt, and writes the words 1 to 4 to it, without
// closing it, so they reach the image only once the drive commits them. It
// halts with R5 0, or the step that failed.
inline constexpr std::string_view kWriteLogProgram = R"---(
    .STACK 16

    .SEGMENT main CODE
        SETI 2, handler         ; Reads a completion into R2, R3 fields
        MVQ R6, 0               ; Completions handled
        EI

    idle_create:
        IN 18, R0
        AND R0, 15              ; Phase 0
        JC NZ, idle_create
        MOV R2, create_completion
        MVQ R3, 2
        MOV R1, create          ; The command word, then the name's 4 dwords
        MVQ R7, 1
    send_create:
        OUTR.IW 16, (R1)
        JC NS, send_create
        MVQ R7, 4
    send_name:
        OUTR.ID 16, [R1]
        JC NS, send_name
    wait_create:
        IN 18, R0
        AND R0, 14              ; Phase 2 or higher
        JC Z, wait_create
        MOV R1, accept
        MVQ R7, 1
    read_create_accept:
        INR.IW 16, (R1)
        JC NS, read_create_accept
        MVQ R5, 1
        MOV R1, accept
        MOV R0, (R1)
        CMP R0, 0               ; OK
        JC NZ, fail
    wait_create_completion:
        CMP R6, 1
        JC NZ, wait_create_completion
        MVQ R5, 2
        MOV R1, create_completion
        MOV R0, (R1)
        CMP R0, 0               ; OK, in slot 0
        JC NZ, fail
        ADQ R1, 1
        MOV R0, (R1)
        CMP R0, 0
        JC NZ, fail

    idle_write:
        IN 18, R0
        AND R0, 15
        JC NZ, idle_write
        MOV R2, write_completion
        MVQ R3, 2
        MOV R1, write           ; WRITE 0 and its count
        MVQ R7, 2
    send_write:
        OUTR.IW 16, (R1)
        JC NS, send_write
    wait_write:
        IN 18, R0
        AND R0, 14
        JC Z, wait_write
        MOV R1, accept
        MVQ R7, 2
    read_write_accept:
        INR.IW 16, (R1)
        JC NS, read_write_accept
        MVQ R5, 3
        MOV R1, accept
        MOV R0, (R1)
        CMP R0, 0               ; OK, for 4 words
        JC NZ, fail
        ADQ R1, 1
        MOV R0, (R1)
        CMP R0, 4
        JC NZ, fail
        MOV R1, words
        MVQ R7, 4
    send_words:
        OUTR.IW 17, (R1)
        JC NS, send_words
    wait_write_completion:
        CMP R6, 2
        JC NZ, wait_write_completion
        MVQ R5, 4
        MOV R1, write_completion
        MOV R0, (R1)
        CMP R0, 0               ; OK, 4 written
        JC NZ, fail
        ADQ R1, 1
        MOV R0, (R1)
        CMP R0, 4
        JC NZ, fail
        MVQ R5, 0
    fail:
        HALT

    handler:
        MOV R7, R3
    read_completion:
        INR.IW 16, (R2)
        JC NS, read_completion
        ADQ R6, 1
        IRT

    .SEGMENT vars DATA
    create:
        .DW 1200h
        .DW SIZE(6) "LOG"
        .DW 0, 0
    write:
        .DW 2100h, 4
    words:
        .DW 1, 2, 3, 4
    accept:
        .DW 0FFFFh, 0FFFFh
    create_completion:
        .DW 0FFFFh, 0FFFFh
    write_completion:
        .DW 0FFFFh, 0FFFFh

    .SEGMENT stack STACK
)---";

//==============================================================================
// The boot ROM
//==============================================================================

// The first line of the boot ROM's banner.
inline constexpr std::string_view kBootVersion = "Ozzy 1.0";

// Long enough for the boot ROM to show its banner and prompt.
inline constexpr absl::Duration kBootTime = absl::Seconds(1);

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

// Runs `ozzy` a millisecond at a time until core 0 halts, for at most 10
// seconds, adding a test failure if it doesn't. Returns whether it halted.
bool RunUntilCore0Halts(Ozzy& ozzy);

// Returns the text of row `row` of `picture`, without its cells' attributes,
// and without the spaces at its end.
std::string GetRowText(const ScreenPicture& picture, int row);

}  // namespace oz3

#endif  // OZ3_OZZY_MACHINE_OZZY_TESTING_H_
