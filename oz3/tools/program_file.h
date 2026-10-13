// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_FILE_H_
#define OZ3_TOOLS_PROGRAM_FILE_H_

#include <cstdint>
#include <string_view>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "oz3/tools/program.h"

namespace oz3 {

//==============================================================================
// Program files
//
// A program file holds one Program as a sequence of words, so a program can be
// assembled once and loaded many times. It is the OZ-3's executable format,
// which the wiki specifies (Program Files), so a loader written in OZ-3 code
// can read it as well as ProgramLoader can.
//
// A program file only ever holds a valid program (see ValidateProgram), so
// writing an invalid program fails, as does reading one.
//==============================================================================

// The first word of every program file. On a host, its bytes are "OZ".
inline constexpr uint16_t kProgramFileId = 0x5A4F;

// The version of the program file format that this writes and reads.
inline constexpr uint16_t kProgramFileVersion = 1;

// Returns the words of a program file holding `program`, or the error from
// ValidateProgram if it isn't valid.
absl::StatusOr<std::vector<uint16_t>> WriteProgramWords(const Program& program);

// Returns the Program held in the words of a program file.
//
// This fails if the words are not a program file of a version this reads, if
// they end before the program does, if any words follow it, or if the program
// they hold isn't valid.
absl::StatusOr<Program> ReadProgramWords(absl::Span<const uint16_t> words);

// Writes `program` as a program file at `path`, each word as two bytes, low
// byte first.
absl::Status WriteProgramFile(gb::FileSystem& file_system,
                              std::string_view path, const Program& program);

// Reads the Program in the program file at `path`, as written by
// WriteProgramFile.
absl::StatusOr<Program> ReadProgramFile(gb::FileSystem& file_system,
                                        std::string_view path);

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_FILE_H_
