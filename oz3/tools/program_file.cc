// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_file.h"

#include <cstddef>
#include <vector>

#include "absl/strings/str_cat.h"

namespace oz3 {

namespace {

// Data blocks' offsets and sizes are in units of this many words.
constexpr int kUnitSize = Program::Segment::Data::kAlignment;

// The largest offset of a data block, in units, below the end of a bank.
constexpr int kMaxOffsetUnits = 0x10000 / kUnitSize - 1;

//==============================================================================
// Writing
//==============================================================================

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
    for (const Program::Segment::Data& data : segment.data) {
      words.push_back(static_cast<uint16_t>(data.offset / kUnitSize));
      words.push_back(static_cast<uint16_t>(data.data.size() / kUnitSize));
      words.insert(words.end(), data.data.begin(), data.data.end());
    }
  }
  return words;
}

//==============================================================================
// ProgramWordReader
//==============================================================================

class ProgramWordReader {
 public:
  explicit ProgramWordReader(absl::Span<const uint16_t> words)
      : words_(words) {}

  absl::StatusOr<Program> Read();

 private:
  // Returns the number of words not yet read.
  int GetRemaining() const;

  // Returns the next word, or 0 once the words run out, which marks them
  // truncated.
  uint16_t ReadWord();

  // Returns the next word as a count of entries, each `entry_size` words. If
  // fewer words remain than that many entries need, this returns 0 and marks
  // the words truncated, so nothing is allocated for entries that aren't there.
  int ReadCount(int entry_size);

  // Reads the next `count` words into `words`, or marks the words truncated if
  // there aren't that many.
  void ReadWords(int count, std::vector<uint16_t>& words);

  absl::Span<const uint16_t> words_;
  int index_ = 0;
  bool truncated_ = false;
};

absl::StatusOr<Program> ProgramWordReader::Read() {
  if (ReadWord() != kProgramFileId) {
    return absl::InvalidArgumentError("Not a program file");
  }
  const uint16_t version = ReadWord();
  if (!truncated_ && version != kProgramFileVersion) {
    return absl::InvalidArgumentError(
        absl::StrCat("Program file version ", version, " is not supported"));
  }

  Program program;
  program.banks.resize(ReadCount(1));
  for (Program::Bank& bank : program.banks) {
    bank.assign_mask = ReadWord();
  }

  program.segments.resize(ReadCount(5));
  std::vector<int> block_counts;
  block_counts.reserve(program.segments.size());
  for (Program::Segment& segment : program.segments) {
    segment.bank = ReadWord();
    segment.access = static_cast<Program::Segment::Access>(ReadWord());
    segment.page_start = ReadWord();
    segment.page_count = ReadWord();
    block_counts.push_back(ReadWord());
  }

  program.code_segment = ReadWord();
  program.stack_segment = ReadWord();
  program.data_segment = ReadWord();
  program.extra_segment = ReadWord();

  program.instances.resize(ReadCount(4));
  for (Program::Instance& instance : program.instances) {
    instance.code = ReadWord();
    instance.stack = ReadWord();
    instance.data = ReadWord();
    instance.extra = ReadWord();
  }

  for (int i = 0; i < static_cast<int>(program.segments.size()); ++i) {
    std::vector<Program::Segment::Data>& blocks = program.segments[i].data;
    for (int b = 0; b < block_counts[i] && !truncated_; ++b) {
      const uint16_t offset_units = ReadWord();
      const uint16_t size_units = ReadWord();
      if (!truncated_ && offset_units > kMaxOffsetUnits) {
        return absl::InvalidArgumentError(
            absl::StrCat("Program file has a data block at offset ",
                         offset_units, " units, past the end of a bank"));
      }
      Program::Segment::Data& data = blocks.emplace_back();
      data.offset = static_cast<uint16_t>(offset_units * kUnitSize);
      ReadWords(size_units * kUnitSize, data.data);
    }
  }

  if (truncated_) {
    return absl::InvalidArgumentError(
        "Program file ends before its program does");
  }
  if (GetRemaining() > 0) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Program file has ", GetRemaining(), " words after its program"));
  }
  if (absl::Status status = ValidateProgram(program); !status.ok()) {
    return status;
  }
  return program;
}

int ProgramWordReader::GetRemaining() const {
  return static_cast<int>(words_.size()) - index_;
}

uint16_t ProgramWordReader::ReadWord() {
  if (GetRemaining() == 0) {
    truncated_ = true;
    return 0;
  }
  return words_[index_++];
}

int ProgramWordReader::ReadCount(int entry_size) {
  const int count = ReadWord();
  if (count * entry_size > GetRemaining()) {
    truncated_ = true;
    return 0;
  }
  return count;
}

void ProgramWordReader::ReadWords(int count, std::vector<uint16_t>& words) {
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
  return ProgramWordReader(words).Read();
}

absl::Status WriteProgramFile(gb::FileSystem& file_system,
                              std::string_view path, const Program& program) {
  absl::StatusOr<std::vector<uint16_t>> words = WriteProgramWords(program);
  if (!words.ok()) {
    return words.status();
  }
  std::vector<std::byte> bytes;
  bytes.reserve(words->size() * 2);
  for (uint16_t word : *words) {
    bytes.push_back(static_cast<std::byte>(word & 0xFF));
    bytes.push_back(static_cast<std::byte>(word >> 8));
  }
  if (!file_system.WriteFile(path, bytes)) {
    return absl::UnavailableError(absl::StrCat("Could not write file: ", path));
  }
  return absl::OkStatus();
}

absl::StatusOr<Program> ReadProgramFile(gb::FileSystem& file_system,
                                        std::string_view path) {
  std::vector<std::byte> bytes;
  if (!file_system.ReadFile(path, &bytes)) {
    return absl::NotFoundError(absl::StrCat("Could not read file: ", path));
  }
  if (bytes.size() % 2 != 0) {
    return absl::InvalidArgumentError(
        absl::StrCat("Program file ", path, " has an odd number of bytes"));
  }
  std::vector<uint16_t> words(bytes.size() / 2);
  for (int i = 0; i < static_cast<int>(words.size()); ++i) {
    const int low = std::to_integer<int>(bytes[i * 2]);
    const int high = std::to_integer<int>(bytes[i * 2 + 1]);
    words[i] = static_cast<uint16_t>(low | (high << 8));
  }
  return ReadProgramWords(words);
}

}  // namespace oz3
