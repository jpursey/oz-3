// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_MAP_FILE_H_
#define OZ3_TOOLS_PROGRAM_MAP_FILE_H_

#include <string_view>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "gb/file/file_system.h"
#include "oz3/tools/program_map.h"

namespace oz3 {

//==============================================================================
// Program map files
//
// A map file holds one ProgramMap, written beside its program file (as
// `.oz3map`), for a debugger. Only host tools read it, so unlike a program
// file, it is a Game Bits chunk file, and not part of the specification. Its
// chunks are in program_map_chunks.h.
//==============================================================================

// Writes `map` as a map file at `path`.
absl::Status WriteProgramMapFile(gb::FileSystem& file_system,
                                 std::string_view path, const ProgramMap& map);

// Reads the map in the map file at `path`, as written by WriteProgramMapFile.
//
// This fails if the file isn't a map file, if it has a chunk of the map twice,
// if a chunk is a version this doesn't read or is corrupt, or if
// ProgramMap::Create rejects the map it holds. A chunk of the map that the file
// doesn't have is empty, and chunks of other types are skipped, so a later
// version can add chunks that this ignores.
absl::StatusOr<ProgramMap> ReadProgramMapFile(gb::FileSystem& file_system,
                                              std::string_view path);

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_MAP_FILE_H_
