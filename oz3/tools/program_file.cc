// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_file.h"

#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"

namespace oz3 {

namespace {

// Data blocks' offsets and sizes are in units of this many words.
constexpr int kUnitSize = Program::Segment::Data::kAlignment;

// The largest offset of a data block, in units, below the end of a bank.
constexpr int kMaxOffsetUnits = 0x10000 / kUnitSize - 1;

// A file format's name, as it starts a sentence, and the first two words of
// its files.
struct FileFormat {
  std::string_view name;
  uint16_t id;
  uint16_t version;
};

constexpr FileFormat kProgramFormat = {
    .name = "Program file",
    .id = kProgramFileId,
    .version = kProgramFileVersion,
};

constexpr FileFormat kBareProgramFormat = {
    .name = "Bare program file",
    .id = kBareProgramFileId,
    .version = kBareProgramFileVersion,
};

// Every format, so reading a file of one as another says what it is.
constexpr FileFormat kFileFormats[] = {kProgramFormat, kBareProgramFormat};

//==============================================================================
// Writing
//==============================================================================

// Appends `blocks` to `words`: each one's offset and size in units, then its
// words.
void WriteBlocks(absl::Span<const Program::Segment::Data> blocks,
                 std::vector<uint16_t>& words) {
  for (const Program::Segment::Data& data : blocks) {
    words.push_back(static_cast<uint16_t>(data.offset / kUnitSize));
    words.push_back(static_cast<uint16_t>(data.data.size() / kUnitSize));
    words.insert(words.end(), data.data.begin(), data.data.end());
  }
}

// Returns the words of a program file holding `program`, which must be valid,
// so that every count fits in a word.
std::vector<uint16_t> WriteValidProgramWords(const Program& program) {
  std::vector<uint16_t> words = {kProgramFileId, kProgramFileVersion};

  words.push_back(static_cast<uint16_t>(program.banks.size()));
  for (const Program::Bank& bank : program.banks) {
    words.push_back(bank.assign_mask);
  }

  words.push_back(static_cast<uint16_t>(program.segments.size()));
  for (const Program::Segment& segment : program.segments) {
    words.push_back(segment.bank);
    words.push_back(static_cast<uint16_t>(segment.access));
    words.push_back(segment.page_start);
    words.push_back(segment.page_count);
    words.push_back(static_cast<uint16_t>(segment.data.size()));
  }

  words.push_back(program.code_segment);
  words.push_back(program.stack_segment);
  words.push_back(program.data_segment);
  words.push_back(program.extra_segment);

  words.push_back(static_cast<uint16_t>(program.instances.size()));
  for (const Program::Instance& instance : program.instances) {
    words.push_back(instance.code);
    words.push_back(instance.stack);
    words.push_back(instance.data);
    words.push_back(instance.extra);
  }

  for (const Program::Segment& segment : program.segments) {
    WriteBlocks(segment.data, words);
  }
  return words;
}

// Returns the words of a bare program file holding `program`, which must be
// valid, so that every size fits in a word.
std::vector<uint16_t> WriteValidBareProgramWords(const BareProgram& program) {
  std::vector<uint16_t> words = {
      kBareProgramFileId,
      kBareProgramFileVersion,
      static_cast<uint16_t>(program.contents_words),
      static_cast<uint16_t>(program.stack_words),
      static_cast<uint16_t>(program.data.size()),
      0,  // Reserved.
  };
  WriteBlocks(program.data, words);
  return words;
}

// Writes `words` to the file at `path`, each as two bytes, low byte first.
absl::Status WriteWordsFile(gb::FileSystem& file_system, std::string_view path,
                            absl::Span<const uint16_t> words) {
  std::vector<std::byte> bytes;
  bytes.reserve(words.size() * 2);
  for (uint16_t word : words) {
    bytes.push_back(static_cast<std::byte>(word & 0xFF));
    bytes.push_back(static_cast<std::byte>(word >> 8));
  }
  if (!file_system.WriteFile(path, bytes)) {
    return absl::UnavailableError(absl::StrCat("Could not write file: ", path));
  }
  return absl::OkStatus();
}

//==============================================================================
// Reading
//==============================================================================

// Returns the words of the file at `path`, written by WriteWordsFile, or an
// error naming it as a file of `format`.
absl::StatusOr<std::vector<uint16_t>> ReadWordsFile(gb::FileSystem& file_system,
                                                    std::string_view path,
                                                    const FileFormat& format) {
  std::vector<std::byte> bytes;
  if (!file_system.ReadFile(path, &bytes)) {
    return absl::NotFoundError(absl::StrCat("Could not read file: ", path));
  }
  if (bytes.size() % 2 != 0) {
    return absl::InvalidArgumentError(
        absl::StrCat(format.name, " ", path, " has an odd number of bytes"));
  }
  std::vector<uint16_t> words(bytes.size() / 2);
  for (int i = 0; i < static_cast<int>(words.size()); ++i) {
    const int low = std::to_integer<int>(bytes[i * 2]);
    const int high = std::to_integer<int>(bytes[i * 2 + 1]);
    words[i] = static_cast<uint16_t>(low | (high << 8));
  }
  return words;
}

// Reads the words of a file of one format, in order.
class WordReader {
 public:
  WordReader(absl::Span<const uint16_t> words, FileFormat format)
      : words_(words), format_(format) {}

  // Reads the identifier and version, returning an error if they aren't the
  // format's.
  absl::Status ReadHeader();

  // Returns the next word, or 0 once the words run out, which marks them
  // truncated.
  uint16_t ReadWord();

  // Returns the next word as a count of entries, each `entry_size` words. If
  // fewer words remain than that many entries need, this returns 0 and marks
  // the words truncated, so nothing is allocated for entries that aren't there.
  int ReadCount(int entry_size);

  // Reads `count` data blocks, as WriteBlocks writes them, into `blocks`, or
  // returns an error if one's offset is past the end of a bank. This stops if
  // the words are truncated.
  absl::Status ReadBlocks(int count,
                          std::vector<Program::Segment::Data>& blocks);

  // Returns an error if the words were truncated, or if any remain unread.
  absl::Status ValidateEnd() const;

 private:
  // Returns the number of words not yet read.
  int GetRemaining() const;

  // Reads the next `count` words into `words`, or marks the words truncated if
  // there aren't that many.
  void ReadWords(int count, std::vector<uint16_t>& words);

  absl::Span<const uint16_t> words_;
  const FileFormat format_;
  int index_ = 0;
  bool truncated_ = false;
};

absl::Status WordReader::ReadHeader() {
  const uint16_t id = ReadWord();
  if (id != format_.id) {
    std::string message =
        absl::StrCat("Not a ", absl::AsciiStrToLower(format_.name));
    for (const FileFormat& other : kFileFormats) {
      if (id == other.id) {
        absl::StrAppend(&message, ", but a ",
                        absl::AsciiStrToLower(other.name));
      }
    }
    return absl::InvalidArgumentError(message);
  }
  const uint16_t version = ReadWord();
  if (!truncated_ && version != format_.version) {
    return absl::InvalidArgumentError(
        absl::StrCat(format_.name, " version ", version, " is not supported"));
  }
  return absl::OkStatus();
}

uint16_t WordReader::ReadWord() {
  if (GetRemaining() == 0) {
    truncated_ = true;
    return 0;
  }
  return words_[index_++];
}

int WordReader::ReadCount(int entry_size) {
  const int count = ReadWord();
  if (count * entry_size > GetRemaining()) {
    truncated_ = true;
    return 0;
  }
  return count;
}

absl::Status WordReader::ReadBlocks(
    int count, std::vector<Program::Segment::Data>& blocks) {
  for (int b = 0; b < count && !truncated_; ++b) {
    const uint16_t offset_units = ReadWord();
    const uint16_t size_units = ReadWord();
    if (!truncated_ && offset_units > kMaxOffsetUnits) {
      return absl::InvalidArgumentError(
          absl::StrCat(format_.name, " has a data block at offset ",
                       offset_units, " units, past the end of a bank"));
    }
    Program::Segment::Data& data = blocks.emplace_back();
    data.offset = static_cast<uint16_t>(offset_units * kUnitSize);
    ReadWords(size_units * kUnitSize, data.data);
  }
  return absl::OkStatus();
}

absl::Status WordReader::ValidateEnd() const {
  if (truncated_) {
    return absl::InvalidArgumentError(
        absl::StrCat(format_.name, " ends before its program does"));
  }
  if (GetRemaining() > 0) {
    return absl::InvalidArgumentError(absl::StrCat(
        format_.name, " has ", GetRemaining(), " words after its program"));
  }
  return absl::OkStatus();
}

int WordReader::GetRemaining() const {
  return static_cast<int>(words_.size()) - index_;
}

void WordReader::ReadWords(int count, std::vector<uint16_t>& words) {
  if (count > GetRemaining()) {
    truncated_ = true;
    return;
  }
  words.assign(words_.begin() + index_, words_.begin() + index_ + count);
  index_ += count;
}

}  // namespace

//==============================================================================
// Program files
//==============================================================================

absl::StatusOr<std::vector<uint16_t>> WriteProgramWords(
    const Program& program) {
  if (absl::Status status = ValidateProgram(program); !status.ok()) {
    return status;
  }
  return WriteValidProgramWords(program);
}

absl::StatusOr<Program> ReadProgramWords(absl::Span<const uint16_t> words) {
  WordReader reader(words, kProgramFormat);
  if (absl::Status status = reader.ReadHeader(); !status.ok()) {
    return status;
  }

  Program program;
  program.banks.resize(reader.ReadCount(1));
  for (Program::Bank& bank : program.banks) {
    bank.assign_mask = reader.ReadWord();
  }

  program.segments.resize(reader.ReadCount(5));
  std::vector<int> block_counts;
  block_counts.reserve(program.segments.size());
  for (Program::Segment& segment : program.segments) {
    segment.bank = reader.ReadWord();
    segment.access = static_cast<Program::Segment::Access>(reader.ReadWord());
    segment.page_start = reader.ReadWord();
    segment.page_count = reader.ReadWord();
    block_counts.push_back(reader.ReadWord());
  }

  program.code_segment = reader.ReadWord();
  program.stack_segment = reader.ReadWord();
  program.data_segment = reader.ReadWord();
  program.extra_segment = reader.ReadWord();

  program.instances.resize(reader.ReadCount(4));
  for (Program::Instance& instance : program.instances) {
    instance.code = reader.ReadWord();
    instance.stack = reader.ReadWord();
    instance.data = reader.ReadWord();
    instance.extra = reader.ReadWord();
  }

  for (int i = 0; i < static_cast<int>(program.segments.size()); ++i) {
    if (absl::Status status =
            reader.ReadBlocks(block_counts[i], program.segments[i].data);
        !status.ok()) {
      return status;
    }
  }

  if (absl::Status status = reader.ValidateEnd(); !status.ok()) {
    return status;
  }
  if (absl::Status status = ValidateProgram(program); !status.ok()) {
    return status;
  }
  return program;
}

absl::Status WriteProgramFile(gb::FileSystem& file_system,
                              std::string_view path, const Program& program) {
  absl::StatusOr<std::vector<uint16_t>> words = WriteProgramWords(program);
  if (!words.ok()) {
    return words.status();
  }
  return WriteWordsFile(file_system, path, *words);
}

absl::StatusOr<Program> ReadProgramFile(gb::FileSystem& file_system,
                                        std::string_view path) {
  absl::StatusOr<std::vector<uint16_t>> words =
      ReadWordsFile(file_system, path, kProgramFormat);
  if (!words.ok()) {
    return words.status();
  }
  return ReadProgramWords(*words);
}

//==============================================================================
// Bare program files
//==============================================================================

absl::StatusOr<std::vector<uint16_t>> WriteBareProgramWords(
    const BareProgram& program) {
  if (absl::Status status = ValidateBareProgram(program); !status.ok()) {
    return status;
  }
  return WriteValidBareProgramWords(program);
}

absl::StatusOr<BareProgram> ReadBareProgramWords(
    absl::Span<const uint16_t> words) {
  WordReader reader(words, kBareProgramFormat);
  if (absl::Status status = reader.ReadHeader(); !status.ok()) {
    return status;
  }

  BareProgram program;
  program.contents_words = reader.ReadWord();
  program.stack_words = reader.ReadWord();
  const int block_count = reader.ReadWord();
  const uint16_t reserved = reader.ReadWord();
  if (absl::Status status = reader.ReadBlocks(block_count, program.data);
      !status.ok()) {
    return status;
  }

  if (absl::Status status = reader.ValidateEnd(); !status.ok()) {
    return status;
  }
  if (reserved != 0) {
    return absl::InvalidArgumentError(
        absl::StrCat(kBareProgramFormat.name, " has ", reserved,
                     " in its reserved word, which must be 0"));
  }
  if (absl::Status status = ValidateBareProgram(program); !status.ok()) {
    return status;
  }
  return program;
}

absl::Status WriteBareProgramFile(gb::FileSystem& file_system,
                                  std::string_view path,
                                  const BareProgram& program) {
  absl::StatusOr<std::vector<uint16_t>> words = WriteBareProgramWords(program);
  if (!words.ok()) {
    return words.status();
  }
  return WriteWordsFile(file_system, path, *words);
}

absl::StatusOr<BareProgram> ReadBareProgramFile(gb::FileSystem& file_system,
                                                std::string_view path) {
  absl::StatusOr<std::vector<uint16_t>> words =
      ReadWordsFile(file_system, path, kBareProgramFormat);
  if (!words.ok()) {
    return words.status();
  }
  return ReadBareProgramWords(*words);
}

}  // namespace oz3
