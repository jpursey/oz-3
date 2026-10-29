// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_MAP_CHUNKS_H_
#define OZ3_TOOLS_PROGRAM_MAP_CHUNKS_H_

#include <cstdint>

#include "gb/file/chunk_types.h"

namespace oz3 {

//==============================================================================
// Program map file format
//
// The chunks of a map file, which WriteProgramMapFile and ReadProgramMapFile
// (see program_map_file.h) write and read, and which only they and their tests
// use.
//
// A map file is a chunk file of type kChunkTypeProgramMap, with at most one
// list chunk of each type below, at kProgramMapChunkVersion. A chunk the file
// doesn't have has no entries. Each entry is the matching part of the
// ProgramMap, in the map's order. A string is null terminated, and a null
// ChunkPtr is an empty string.
//==============================================================================

inline constexpr gb::ChunkType kChunkTypeProgramMap = {'O', 'Z', 'M', 'P'};
inline constexpr int32_t kProgramMapChunkVersion = 1;

// The source files (GetFiles) and segment names (GetSegments), each a
// ProgramMapFileString.
inline constexpr gb::ChunkType kChunkTypeProgramMapFiles = {'F', 'I', 'L', 'E'};
inline constexpr gb::ChunkType kChunkTypeProgramMapSegments = {'S', 'E', 'G',
                                                               'M'};
struct ProgramMapFileString {
  gb::ChunkPtr<const char> value;
};

// The statements (GetStatements).
inline constexpr gb::ChunkType kChunkTypeProgramMapStatements = {'S', 'T', 'M',
                                                                 'T'};
struct ProgramMapFileStatement {
  int32_t segment;
  int32_t offset;
  int32_t size;
  int32_t file;
  int32_t line;
  int32_t reserved;
};

// The labels (GetLabels).
inline constexpr gb::ChunkType kChunkTypeProgramMapLabels = {'L', 'A', 'B',
                                                             'L'};
struct ProgramMapFileLabel {
  gb::ChunkPtr<const char> name;
  int32_t segment;
  int32_t offset;
  int32_t file;
  int32_t line;
};

// The constants (GetConstants). `has_word_value` is 1 if the constant has a
// word value, which is `word_value`, and 0 if it doesn't.
inline constexpr gb::ChunkType kChunkTypeProgramMapConstants = {'C', 'N', 'S',
                                                                'T'};
struct ProgramMapFileConstant {
  gb::ChunkPtr<const char> name;
  uint32_t dword_value;
  uint16_t word_value;
  uint16_t has_word_value;
  int32_t file;
  int32_t line;
};

// Entries are read and written as they are in memory, so their layout is the
// file's.
static_assert(sizeof(ProgramMapFileString) == 8);
static_assert(sizeof(ProgramMapFileStatement) == 24);
static_assert(sizeof(ProgramMapFileLabel) == 24);
static_assert(sizeof(ProgramMapFileConstant) == 24);

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_MAP_CHUNKS_H_
