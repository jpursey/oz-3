// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_map_file.h"

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/string_view.h"
#include "absl/types/span.h"
#include "gb/file/chunk_reader.h"
#include "gb/file/chunk_types.h"
#include "gb/file/chunk_writer.h"
#include "gb/file/file.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
#include "oz3/tools/program_map.h"
#include "oz3/tools/program_map_chunks.h"

namespace oz3 {

namespace {

gb::ChunkWriter WriteStrings(const gb::ChunkType& type,
                             absl::Span<const std::string> strings) {
  gb::ChunkWriter writer = gb::ChunkWriter::New<ProgramMapFileString>(
      type, kProgramMapChunkVersion, static_cast<int32_t>(strings.size()));
  ProgramMapFileString* entries = writer.GetChunkData<ProgramMapFileString>();
  for (int i = 0; i < static_cast<int>(strings.size()); ++i) {
    entries[i].value = writer.AddString(strings[i]);
  }
  return writer;
}

gb::ChunkWriter WriteStatements(
    absl::Span<const ProgramMap::Statement> statements) {
  gb::ChunkWriter writer = gb::ChunkWriter::New<ProgramMapFileStatement>(
      kChunkTypeProgramMapStatements, kProgramMapChunkVersion,
      static_cast<int32_t>(statements.size()));
  ProgramMapFileStatement* entries =
      writer.GetChunkData<ProgramMapFileStatement>();
  for (int i = 0; i < static_cast<int>(statements.size()); ++i) {
    const ProgramMap::Statement& statement = statements[i];
    entries[i] = {.segment = statement.segment,
                  .offset = statement.offset,
                  .size = statement.size,
                  .file = statement.location.file,
                  .line = statement.location.line};
  }
  return writer;
}

gb::ChunkWriter WriteLabels(absl::Span<const ProgramMap::Label> labels) {
  gb::ChunkWriter writer = gb::ChunkWriter::New<ProgramMapFileLabel>(
      kChunkTypeProgramMapLabels, kProgramMapChunkVersion,
      static_cast<int32_t>(labels.size()));
  ProgramMapFileLabel* entries = writer.GetChunkData<ProgramMapFileLabel>();
  for (int i = 0; i < static_cast<int>(labels.size()); ++i) {
    const ProgramMap::Label& label = labels[i];
    entries[i] = {.name = writer.AddString(label.name),
                  .segment = label.segment,
                  .offset = label.offset,
                  .file = label.location.file,
                  .line = label.location.line};
  }
  return writer;
}

gb::ChunkWriter WriteConstants(
    absl::Span<const ProgramMap::Constant> constants) {
  gb::ChunkWriter writer = gb::ChunkWriter::New<ProgramMapFileConstant>(
      kChunkTypeProgramMapConstants, kProgramMapChunkVersion,
      static_cast<int32_t>(constants.size()));
  ProgramMapFileConstant* entries =
      writer.GetChunkData<ProgramMapFileConstant>();
  for (int i = 0; i < static_cast<int>(constants.size()); ++i) {
    const ProgramMap::Constant& constant = constants[i];
    entries[i] = {.name = writer.AddString(constant.name),
                  .dword_value = constant.dword_value,
                  .word_value = constant.word_value.value_or(0),
                  .has_word_value =
                      static_cast<uint16_t>(constant.word_value.has_value()),
                  .file = constant.location.file,
                  .line = constant.location.line};
  }
  return writer;
}

// Reads the map in one map file.
class MapFileReader final {
 public:
  explicit MapFileReader(std::string_view path) : path_(path) {}

  absl::StatusOr<ProgramMap> Read(gb::FileSystem& file_system);

 private:
  // Sets the error to `message`, after the file's path, and returns false.
  bool Fail(std::string_view message);

  // Reads every chunk of the file, which must be a map file.
  bool ReadChunkFile(gb::FileSystem& file_system,
                     std::vector<gb::ChunkReader>& chunks);

  // Sets `entries` to the entries of `chunk`, which are none if its count is
  // 0, and sets `has_chunk`. This validates that the file had no chunk of this
  // type before, as `has_chunk` says, that the chunk is of a version this
  // reads, and that its entries fit in it.
  template <typename Entry>
  bool GetEntries(gb::ChunkReader& chunk, bool& has_chunk,
                  absl::Span<Entry>& entries);

  // Sets `value` to the string `ptr` in `chunk`.
  bool ReadString(gb::ChunkReader& chunk, gb::ChunkPtr<const char>& ptr,
                  std::string& value);

  // Sets `strings` to the strings in `chunk` (see GetEntries).
  bool ReadStrings(gb::ChunkReader& chunk, bool& has_chunk,
                   std::vector<std::string>& strings);

  // Each reads a chunk of the map into its part of the map.
  bool ReadFiles(gb::ChunkReader& chunk);
  bool ReadSegments(gb::ChunkReader& chunk);
  bool ReadStatements(gb::ChunkReader& chunk);
  bool ReadLabels(gb::ChunkReader& chunk);
  bool ReadConstants(gb::ChunkReader& chunk);

  const std::string_view path_;
  absl::Status error_;

  // Each part of the map, which is in at most one chunk, and is empty if the
  // file doesn't have it, and whether the file has had its chunk so far.
  std::vector<std::string> files_;
  std::vector<std::string> segments_;
  std::vector<ProgramMap::Statement> statements_;
  std::vector<ProgramMap::Label> labels_;
  std::vector<ProgramMap::Constant> constants_;
  bool has_files_ = false;
  bool has_segments_ = false;
  bool has_statements_ = false;
  bool has_labels_ = false;
  bool has_constants_ = false;
};

absl::StatusOr<ProgramMap> MapFileReader::Read(gb::FileSystem& file_system) {
  std::vector<gb::ChunkReader> chunks;
  if (!ReadChunkFile(file_system, chunks)) {
    return error_;
  }

  // Chunks of other types are skipped.
  for (gb::ChunkReader& chunk : chunks) {
    const gb::ChunkType& type = chunk.GetType();
    if (type == kChunkTypeProgramMapFiles) {
      if (!ReadFiles(chunk)) {
        return error_;
      }
    } else if (type == kChunkTypeProgramMapSegments) {
      if (!ReadSegments(chunk)) {
        return error_;
      }
    } else if (type == kChunkTypeProgramMapStatements) {
      if (!ReadStatements(chunk)) {
        return error_;
      }
    } else if (type == kChunkTypeProgramMapLabels) {
      if (!ReadLabels(chunk)) {
        return error_;
      }
    } else if (type == kChunkTypeProgramMapConstants) {
      if (!ReadConstants(chunk)) {
        return error_;
      }
    }
  }

  absl::StatusOr<ProgramMap> map = ProgramMap::Create(
      std::move(files_), std::move(segments_), std::move(statements_),
      std::move(labels_), std::move(constants_));
  if (!map.ok()) {
    Fail(
        absl::StrCat("holds a map that isn't valid: ", map.status().message()));
    return error_;
  }
  return map;
}

bool MapFileReader::Fail(std::string_view message) {
  error_ = absl::InvalidArgumentError(
      absl::StrCat("Map file ", path_, " ", message));
  return false;
}

bool MapFileReader::ReadChunkFile(gb::FileSystem& file_system,
                                  std::vector<gb::ChunkReader>& chunks) {
  std::unique_ptr<gb::File> file =
      file_system.OpenFile(path_, gb::kReadFileFlags);
  if (file == nullptr) {
    error_ = absl::NotFoundError(absl::StrCat("Could not read file: ", path_));
    return false;
  }
  gb::ChunkType type = {};
  if (!gb::ReadChunkFile(file.get(), &type, &chunks)) {
    return Fail("isn't a chunk file, or is corrupt");
  }
  if (type != kChunkTypeProgramMap) {
    return Fail(absl::StrCat("is a chunk file of type ", type.ToString(),
                             ", not a map file"));
  }
  return true;
}

template <typename Entry>
bool MapFileReader::GetEntries(gb::ChunkReader& chunk, bool& has_chunk,
                               absl::Span<Entry>& entries) {
  if (has_chunk) {
    return Fail(absl::StrCat("has more than one ", chunk.GetType().ToString(),
                             " chunk"));
  }
  has_chunk = true;
  if (chunk.GetVersion() != kProgramMapChunkVersion) {
    return Fail(absl::StrCat("has a ", chunk.GetType().ToString(),
                             " chunk of version ", chunk.GetVersion(),
                             ", which this doesn't read"));
  }
  if (!chunk.FitsEntriesOfSize(gb::GetChunkTypeSize<Entry>())) {
    return Fail(absl::StrCat("has a ", chunk.GetType().ToString(),
                             " chunk too small for its ", chunk.GetCount(),
                             " entries"));
  }

  // The data is null if there are no entries.
  entries = absl::MakeSpan(chunk.GetChunkData<Entry>(), chunk.GetCount());
  return true;
}

bool MapFileReader::ReadString(gb::ChunkReader& chunk,
                               gb::ChunkPtr<const char>& ptr,
                               std::string& value) {
  if (!chunk.ConvertToPtr(&ptr)) {
    return Fail(absl::StrCat("has a corrupt string in its ",
                             chunk.GetType().ToString(), " chunk"));
  }
  value = absl::NullSafeStringView(ptr.ptr);
  return true;
}

bool MapFileReader::ReadStrings(gb::ChunkReader& chunk, bool& has_chunk,
                                std::vector<std::string>& strings) {
  absl::Span<ProgramMapFileString> entries;
  if (!GetEntries(chunk, has_chunk, entries)) {
    return false;
  }
  strings.resize(entries.size());
  for (int i = 0; i < static_cast<int>(entries.size()); ++i) {
    if (!ReadString(chunk, entries[i].value, strings[i])) {
      return false;
    }
  }
  return true;
}

bool MapFileReader::ReadFiles(gb::ChunkReader& chunk) {
  return ReadStrings(chunk, has_files_, files_);
}

bool MapFileReader::ReadSegments(gb::ChunkReader& chunk) {
  return ReadStrings(chunk, has_segments_, segments_);
}

bool MapFileReader::ReadStatements(gb::ChunkReader& chunk) {
  absl::Span<ProgramMapFileStatement> entries;
  if (!GetEntries(chunk, has_statements_, entries)) {
    return false;
  }
  statements_.resize(entries.size());
  for (int i = 0; i < static_cast<int>(entries.size()); ++i) {
    const ProgramMapFileStatement& entry = entries[i];
    ProgramMap::Statement& statement = statements_[i];
    statement.segment = entry.segment;
    statement.offset = entry.offset;
    statement.size = entry.size;
    statement.location = {.file = entry.file, .line = entry.line};
  }
  return true;
}

bool MapFileReader::ReadLabels(gb::ChunkReader& chunk) {
  absl::Span<ProgramMapFileLabel> entries;
  if (!GetEntries(chunk, has_labels_, entries)) {
    return false;
  }
  labels_.resize(entries.size());
  for (int i = 0; i < static_cast<int>(entries.size()); ++i) {
    ProgramMapFileLabel& entry = entries[i];
    ProgramMap::Label& label = labels_[i];
    if (!ReadString(chunk, entry.name, label.name)) {
      return false;
    }
    label.segment = entry.segment;
    label.offset = entry.offset;
    label.location = {.file = entry.file, .line = entry.line};
  }
  return true;
}

bool MapFileReader::ReadConstants(gb::ChunkReader& chunk) {
  absl::Span<ProgramMapFileConstant> entries;
  if (!GetEntries(chunk, has_constants_, entries)) {
    return false;
  }
  constants_.resize(entries.size());
  for (int i = 0; i < static_cast<int>(entries.size()); ++i) {
    ProgramMapFileConstant& entry = entries[i];
    ProgramMap::Constant& constant = constants_[i];
    if (!ReadString(chunk, entry.name, constant.name)) {
      return false;
    }
    if (entry.has_word_value > 1) {
      return Fail(absl::StrCat("has the constant ", constant.name,
                               " with has_word_value ", entry.has_word_value,
                               ", which must be 0 or 1"));
    }
    if (entry.has_word_value == 1) {
      constant.word_value = entry.word_value;
    }
    constant.dword_value = entry.dword_value;
    constant.location = {.file = entry.file, .line = entry.line};
  }
  return true;
}

}  // namespace

absl::Status WriteProgramMapFile(gb::FileSystem& file_system,
                                 std::string_view path, const ProgramMap& map) {
  std::vector<gb::ChunkWriter> chunks;
  chunks.push_back(WriteStrings(kChunkTypeProgramMapFiles, map.GetFiles()));
  chunks.push_back(
      WriteStrings(kChunkTypeProgramMapSegments, map.GetSegments()));
  chunks.push_back(WriteStatements(map.GetStatements()));
  chunks.push_back(WriteLabels(map.GetLabels()));
  chunks.push_back(WriteConstants(map.GetConstants()));
  std::unique_ptr<gb::File> file =
      file_system.OpenFile(path, gb::kNewFileFlags);
  if (file == nullptr ||
      !gb::WriteChunkFile(file.get(), kChunkTypeProgramMap, chunks) ||
      !file->Close()) {
    return absl::UnavailableError(absl::StrCat("Could not write file: ", path));
  }
  return absl::OkStatus();
}

absl::StatusOr<ProgramMap> ReadProgramMapFile(gb::FileSystem& file_system,
                                              std::string_view path) {
  return MapFileReader(path).Read(file_system);
}

}  // namespace oz3
