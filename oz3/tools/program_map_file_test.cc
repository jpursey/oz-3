// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_map_file.h"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "gb/file/chunk_types.h"
#include "gb/file/chunk_writer.h"
#include "gb/file/file.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/tools/program_map.h"
#include "oz3/tools/program_map_chunks.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::HasSubstr;

constexpr std::string_view kPath = "mem:/program.oz3map";

// The index of each of a map's chunks in MakeEmptyChunks().
enum ChunkIndex { kFiles, kSegments, kStatements, kLabels, kConstants };

void ExpectSameMap(const ProgramMap& map, const ProgramMap& expected) {
  EXPECT_THAT(map.GetFiles(), ElementsAreArray(expected.GetFiles()));
  EXPECT_THAT(map.GetSegments(), ElementsAreArray(expected.GetSegments()));
  EXPECT_THAT(map.GetStatements(), ElementsAreArray(expected.GetStatements()));
  EXPECT_THAT(map.GetLabels(), ElementsAreArray(expected.GetLabels()));
  EXPECT_THAT(map.GetConstants(), ElementsAreArray(expected.GetConstants()));
}

class ProgramMapFileTest : public ::testing::Test {
 protected:
  ProgramMapFileTest() {
    file_system_.Register(std::make_unique<gb::MemoryFileProtocol>());
  }

  // Expects `map` to be written at kPath, and read back the same.
  void ExpectRoundTrip(const ProgramMap& map) {
    absl::Status status = WriteProgramMapFile(file_system_, kPath, map);
    ASSERT_TRUE(status.ok()) << status;
    absl::StatusOr<ProgramMap> read = ReadProgramMapFile(file_system_, kPath);
    ASSERT_TRUE(read.ok()) << read.status();
    ExpectSameMap(*read, map);
  }

  // Writes a chunk file of `type` at kPath, with `chunks`.
  void WriteChunks(const gb::ChunkType& type,
                   const std::vector<gb::ChunkWriter>& chunks) {
    std::unique_ptr<gb::File> file =
        file_system_.OpenFile(kPath, gb::kNewFileFlags);
    ASSERT_NE(file, nullptr);
    ASSERT_TRUE(gb::WriteChunkFile(file.get(), type, chunks));
    ASSERT_TRUE(file->Close());
  }

  // Expects reading kPath to fail with an error containing `message`.
  void ExpectReadFails(std::string_view message) {
    absl::StatusOr<ProgramMap> map = ReadProgramMapFile(file_system_, kPath);
    ASSERT_FALSE(map.ok());
    EXPECT_THAT(map.status().message(), HasSubstr(message));
  }

  gb::FileSystem file_system_;
};

// Returns a chunk of `type` with no entries.
gb::ChunkWriter MakeEmptyChunk(const gb::ChunkType& type,
                               int32_t version = kProgramMapChunkVersion) {
  return gb::ChunkWriter::New<uint64_t>(type, version, 0);
}

// Returns each of a map's chunks, with no entries, which is an empty map.
std::vector<gb::ChunkWriter> MakeEmptyChunks() {
  std::vector<gb::ChunkWriter> chunks;
  chunks.push_back(MakeEmptyChunk(kChunkTypeProgramMapFiles));
  chunks.push_back(MakeEmptyChunk(kChunkTypeProgramMapSegments));
  chunks.push_back(MakeEmptyChunk(kChunkTypeProgramMapStatements));
  chunks.push_back(MakeEmptyChunk(kChunkTypeProgramMapLabels));
  chunks.push_back(MakeEmptyChunk(kChunkTypeProgramMapConstants));
  return chunks;
}

ProgramMap MakeMap() {
  absl::StatusOr<ProgramMap> map = ProgramMap::Create(
      {"main.oza", "lib/include.oza"}, {"code", "data"},
      {{.segment = 0,
        .offset = 0,
        .size = 2,
        .location = {.file = 0, .line = 4}},
       {.segment = 0,
        .offset = 2,
        .size = 1,
        .location = {.file = 1, .line = 0}},
       {.segment = 1,
        .offset = 6,
        .size = 3,
        .location = {.file = 0, .line = 9}}},
      {{.name = "START",
        .segment = 0,
        .offset = 0,
        .location = {.file = 0, .line = 3}},
       {.name = "START@LOOP",
        .segment = 0,
        .offset = 2,
        .location = {.file = 1, .line = 0}},
       {.name = "TABLE",
        .segment = 1,
        .offset = 6,
        .location = {.file = 0, .line = 8}}},
      {{.name = "BIG", .dword_value = 0x10000, .location = {.file = 0}},
       {.name = "HALF",
        .word_value = 0,
        .dword_value = 0x8000,
        .location = {.file = 0, .line = 1}},
       {.name = "MASK",
        .word_value = 0xFFFE,
        .dword_value = 0xFFFFFFFE,
        .location = {.file = 1, .line = 2}}});
  CHECK(map.ok()) << map.status();
  return *std::move(map);
}

TEST_F(ProgramMapFileTest, RoundTrip) { ExpectRoundTrip(MakeMap()); }

TEST_F(ProgramMapFileTest, RoundTripEmptyMap) { ExpectRoundTrip(ProgramMap()); }

TEST_F(ProgramMapFileTest, RoundTripDefaultProgramSegment) {
  absl::StatusOr<ProgramMap> map = ProgramMap::Create(
      {"program"}, {""}, {{.segment = 0, .offset = 0, .size = 1}}, {}, {});
  ASSERT_TRUE(map.ok()) << map.status();
  ExpectRoundTrip(*map);
}

TEST_F(ProgramMapFileTest, WriteFailsForPathThatCantBeOpened) {
  absl::Status status =
      WriteProgramMapFile(file_system_, "none:/program.oz3map", MakeMap());
  EXPECT_EQ(status.code(), absl::StatusCode::kUnavailable);
  EXPECT_THAT(status.message(), HasSubstr("Could not write file"));
}

TEST_F(ProgramMapFileTest, ReadMissingFile) {
  absl::StatusOr<ProgramMap> map = ReadProgramMapFile(file_system_, kPath);
  EXPECT_EQ(map.status().code(), absl::StatusCode::kNotFound);
}

TEST_F(ProgramMapFileTest, ReadFileThatIsNotAChunkFile) {
  ASSERT_TRUE(file_system_.WriteFile(kPath, "Not a chunk file"));
  ExpectReadFails("isn't a chunk file, or is corrupt");
}

TEST_F(ProgramMapFileTest, ReadOtherChunkFileType) {
  WriteChunks({'X', 'Y', 'Z', 'W'}, MakeEmptyChunks());
  ExpectReadFails("is a chunk file of type XYZW, not a map file");
}

TEST_F(ProgramMapFileTest, ReadTruncatedFile) {
  ASSERT_TRUE(WriteProgramMapFile(file_system_, kPath, MakeMap()).ok());
  std::vector<std::byte> bytes;
  ASSERT_TRUE(file_system_.ReadFile(kPath, &bytes));
  bytes.resize(bytes.size() - 8);
  ASSERT_TRUE(file_system_.WriteFile(kPath, bytes));
  ExpectReadFails("isn't a chunk file, or is corrupt");
}

TEST_F(ProgramMapFileTest, ReadEmptyChunks) {
  WriteChunks(kChunkTypeProgramMap, MakeEmptyChunks());
  absl::StatusOr<ProgramMap> map = ReadProgramMapFile(file_system_, kPath);
  ASSERT_TRUE(map.ok()) << map.status();
  ExpectSameMap(*map, ProgramMap());
}

TEST_F(ProgramMapFileTest, ReadSkipsOtherChunks) {
  std::vector<gb::ChunkWriter> chunks = MakeEmptyChunks();
  gb::ChunkWriter other =
      gb::ChunkWriter::New<uint64_t>({'X', 'T', 'R', 'A'}, 2, 1);
  *other.GetChunkData<uint64_t>() = 42;
  chunks.insert(chunks.begin() + kStatements, std::move(other));
  WriteChunks(kChunkTypeProgramMap, chunks);
  EXPECT_TRUE(ReadProgramMapFile(file_system_, kPath).ok());
}

TEST_F(ProgramMapFileTest, ReadNoChunks) {
  WriteChunks(kChunkTypeProgramMap, {});
  absl::StatusOr<ProgramMap> map = ReadProgramMapFile(file_system_, kPath);
  ASSERT_TRUE(map.ok()) << map.status();
  ExpectSameMap(*map, ProgramMap());
}

TEST_F(ProgramMapFileTest, ReadMissingChunksAreEmpty) {
  std::vector<gb::ChunkWriter> chunks;
  chunks.push_back(gb::ChunkWriter::New<ProgramMapFileString>(
      kChunkTypeProgramMapFiles, kProgramMapChunkVersion, 1));
  gb::ChunkWriter& files = chunks.back();
  files.GetChunkData<ProgramMapFileString>()->value =
      files.AddString("main.oza");
  WriteChunks(kChunkTypeProgramMap, chunks);
  absl::StatusOr<ProgramMap> map = ReadProgramMapFile(file_system_, kPath);
  ASSERT_TRUE(map.ok()) << map.status();
  ExpectSameMap(*map, *ProgramMap::Create({"main.oza"}, {}, {}, {}, {}));
}

TEST_F(ProgramMapFileTest, ReadRepeatedChunk) {
  std::vector<gb::ChunkWriter> chunks = MakeEmptyChunks();
  chunks.push_back(MakeEmptyChunk(kChunkTypeProgramMapSegments));
  WriteChunks(kChunkTypeProgramMap, chunks);
  ExpectReadFails("has more than one SEGM chunk");
}

TEST_F(ProgramMapFileTest, ReadChunkOfOtherVersion) {
  std::vector<gb::ChunkWriter> chunks = MakeEmptyChunks();
  chunks[kStatements] = MakeEmptyChunk(kChunkTypeProgramMapStatements, 2);
  WriteChunks(kChunkTypeProgramMap, chunks);
  ExpectReadFails("has a STMT chunk of version 2, which this doesn't read");
}

TEST_F(ProgramMapFileTest, ReadChunkTooSmallForItsEntries) {
  std::vector<gb::ChunkWriter> chunks = MakeEmptyChunks();

  // Three entries of one byte each, rather than of a statement each.
  chunks[kStatements] = gb::ChunkWriter::New<char>(
      kChunkTypeProgramMapStatements, kProgramMapChunkVersion, 3);
  WriteChunks(kChunkTypeProgramMap, chunks);
  ExpectReadFails("has a STMT chunk too small for its 3 entries");
}

TEST_F(ProgramMapFileTest, ReadStringPastEndOfChunk) {
  std::vector<gb::ChunkWriter> chunks = MakeEmptyChunks();
  chunks[kFiles] = gb::ChunkWriter::New<ProgramMapFileString>(
      kChunkTypeProgramMapFiles, kProgramMapChunkVersion, 1);
  chunks[kFiles].GetChunkData<ProgramMapFileString>()->value.offset = 1024;
  WriteChunks(kChunkTypeProgramMap, chunks);
  ExpectReadFails("has a corrupt string in its FILE chunk");
}

TEST_F(ProgramMapFileTest, ReadStringWithNoTerminator) {
  std::vector<gb::ChunkWriter> chunks = MakeEmptyChunks();
  chunks[kLabels] = gb::ChunkWriter::New<ProgramMapFileLabel>(
      kChunkTypeProgramMapLabels, kProgramMapChunkVersion, 1);
  gb::ChunkWriter& labels = chunks[kLabels];

  // Eight characters fill the chunk's last 8 bytes, with no terminator.
  labels.GetChunkData<ProgramMapFileLabel>()->name =
      labels.AddData<const char>("ABCDEFGH", 8);
  WriteChunks(kChunkTypeProgramMap, chunks);
  ExpectReadFails("has a corrupt string in its LABL chunk");
}

TEST_F(ProgramMapFileTest, ReadNullStringIsEmpty) {
  std::vector<gb::ChunkWriter> chunks = MakeEmptyChunks();
  chunks[kSegments] = gb::ChunkWriter::New<ProgramMapFileString>(
      kChunkTypeProgramMapSegments, kProgramMapChunkVersion, 1);
  WriteChunks(kChunkTypeProgramMap, chunks);
  absl::StatusOr<ProgramMap> map = ReadProgramMapFile(file_system_, kPath);
  ASSERT_TRUE(map.ok()) << map.status();
  EXPECT_THAT(map->GetSegments(), ElementsAre(""));
}

TEST_F(ProgramMapFileTest, ReadConstantWithBadWordValueFlag) {
  std::vector<gb::ChunkWriter> chunks = MakeEmptyChunks();
  chunks[kConstants] = gb::ChunkWriter::New<ProgramMapFileConstant>(
      kChunkTypeProgramMapConstants, kProgramMapChunkVersion, 1);
  gb::ChunkWriter& constants = chunks[kConstants];
  *constants.GetChunkData<ProgramMapFileConstant>() = {
      .name = constants.AddString("N"), .has_word_value = 2};
  WriteChunks(kChunkTypeProgramMap, chunks);
  ExpectReadFails("has the constant N with has_word_value 2");
}

TEST_F(ProgramMapFileTest, ReadMapThatIsNotValid) {
  // The statement is in segment 0, but there are no segments.
  std::vector<gb::ChunkWriter> chunks = MakeEmptyChunks();
  chunks[kStatements] = gb::ChunkWriter::New<ProgramMapFileStatement>(
      kChunkTypeProgramMapStatements, kProgramMapChunkVersion, 1);
  *chunks[kStatements].GetChunkData<ProgramMapFileStatement>() = {.size = 1};
  WriteChunks(kChunkTypeProgramMap, chunks);
  ExpectReadFails(
      "holds a map that isn't valid: Statement 0 is in segment 0, but the map "
      "has 0");
}

}  // namespace
}  // namespace oz3
