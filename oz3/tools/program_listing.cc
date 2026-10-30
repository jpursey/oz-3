// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_listing.h"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_replace.h"
#include "absl/strings/str_split.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gb/file/path.h"
#include "oz3/tools/program.h"
#include "oz3/tools/program_map.h"

namespace oz3 {

namespace {

// The most words a row of the listing has.
constexpr int kWordsPerRow = 4;

// The width of a row's words: four hex digits each, with spaces between.
constexpr int kWordsWidth = kWordsPerRow * 5 - 1;

// The most characters of a segment's name a location has. A longer name is
// cut short, ending in kSegmentNameCut, which no name has.
constexpr int kMaxSegmentNameWidth = 8;
constexpr char kSegmentNameCut = '~';

// Formats the listing of one program (see FormatProgramListing).
class ProgramLister final {
 public:
  // `program` and `map` must outlive this.
  ProgramLister(const Program* program, const ProgramMap* map)
      : program_(program), map_(map) {}

  absl::StatusOr<std::string> Format(absl::Span<const std::string> sources);

 private:
  // A source line, and what it assembled to.
  struct Line {
    std::string_view text;
    std::vector<const ProgramMap::Statement*> statements;
    const ProgramMap::Label* label = nullptr;
  };

  // Splits each source into its lines, and puts each statement and label on
  // its line.
  absl::Status AddLines(absl::Span<const std::string> sources);

  // Returns the line at `location`, or null if its file doesn't have it.
  Line* FindLine(const ProgramMap::Location& location);

  // Returns where `location` is, as an error message names it.
  std::string FormatLine(const ProgramMap::Location& location) const;

  // Appends the rows of a file, a statement, or a row of the listing.
  absl::Status AddFileRows(int file);
  absl::Status AddStatementRows(const ProgramMap::Statement& statement,
                                std::string_view text);
  void AddRow(std::string_view location, absl::Span<const uint16_t> words,
              std::string_view text);

  // Returns an offset in a segment as a row's location.
  std::string FormatLocation(int segment, int offset) const;

  // Returns the word at `offset` in a segment's data, or nullopt if no data
  // block has it.
  std::optional<uint16_t> GetWord(int segment, int offset) const;

  const Program* const program_;
  const ProgramMap* const map_;
  std::vector<std::string> sources_;      // With line breaks as '\n'.
  std::vector<std::vector<Line>> files_;  // By map file, then line.
  int location_width_ = 4;
  std::string listing_;
};

absl::StatusOr<std::string> ProgramLister::Format(
    absl::Span<const std::string> sources) {
  if (sources.size() != map_->GetFiles().size()) {
    return absl::InvalidArgumentError(
        absl::StrCat("Listing has ", sources.size(), " sources for ",
                     map_->GetFiles().size(), " files"));
  }
  if (map_->GetSegments().size() != program_->segments.size()) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Map has ", map_->GetSegments().size(), " segments for a program with ",
        program_->segments.size()));
  }
  if (absl::Status status = AddLines(sources); !status.ok()) {
    return status;
  }

  for (int segment = 0; segment < static_cast<int>(program_->segments.size());
       ++segment) {
    location_width_ = std::max(
        location_width_, static_cast<int>(FormatLocation(segment, 0).size()));
  }

  // A label at the end of a full segment has a fifth digit.
  for (const ProgramMap::Label& label : map_->GetLabels()) {
    location_width_ = std::max(
        location_width_,
        static_cast<int>(FormatLocation(label.segment, label.offset).size()));
  }

  for (int file = 0; file < static_cast<int>(files_.size()); ++file) {
    if (file > 0) {
      absl::StrAppend(&listing_, "\n");
    }
    if (absl::Status status = AddFileRows(file); !status.ok()) {
      return status;
    }
  }
  return std::move(listing_);
}

absl::Status ProgramLister::AddLines(absl::Span<const std::string> sources) {
  sources_.assign(sources.begin(), sources.end());
  files_.resize(sources.size());
  for (int file = 0; file < static_cast<int>(sources.size()); ++file) {
    // Lines are split as gb::Lexer splits them, which numbers the map's lines.
    absl::StrReplaceAll({{"\r\n", "\n"}, {"\r", "\n"}}, &sources_[file]);
    std::vector<std::string_view> texts = absl::StrSplit(sources_[file], '\n');
    if (texts.back().empty()) {
      texts.pop_back();
    }
    std::vector<Line>& lines = files_[file];
    lines.resize(texts.size());
    for (int i = 0; i < static_cast<int>(texts.size()); ++i) {
      lines[i].text = texts[i];
    }
  }
  for (const ProgramMap::Statement& statement : map_->GetStatements()) {
    Line* line = FindLine(statement.location);
    if (line == nullptr) {
      return absl::InvalidArgumentError(
          absl::StrCat("No source line for the statement at ",
                       FormatLine(statement.location)));
    }
    line->statements.push_back(&statement);
  }
  for (const ProgramMap::Label& label : map_->GetLabels()) {
    Line* line = FindLine(label.location);
    if (line == nullptr) {
      return absl::InvalidArgumentError(
          absl::StrCat("No source line for label ", label.name, " at ",
                       FormatLine(label.location)));
    }
    line->label = &label;
  }
  return absl::OkStatus();
}

ProgramLister::Line* ProgramLister::FindLine(
    const ProgramMap::Location& location) {
  std::vector<Line>& lines = files_[location.file];
  if (location.line >= static_cast<int>(lines.size())) {
    return nullptr;
  }
  return &lines[location.line];
}

std::string ProgramLister::FormatLine(
    const ProgramMap::Location& location) const {
  return absl::StrCat(gb::RemoveProtocol(map_->GetFiles()[location.file]), ":",
                      location.line + 1);
}

absl::Status ProgramLister::AddFileRows(int file) {
  absl::StrAppend(&listing_, gb::RemoveProtocol(map_->GetFiles()[file]), "\n");
  for (const Line& line : files_[file]) {
    if (line.statements.empty()) {
      std::string location;
      if (line.label != nullptr) {
        location = FormatLocation(line.label->segment, line.label->offset);
      }
      AddRow(location, {}, line.text);
      continue;
    }
    std::string_view text = line.text;
    for (const ProgramMap::Statement* statement : line.statements) {
      if (absl::Status status = AddStatementRows(*statement, text);
          !status.ok()) {
        return status;
      }
      text = {};
    }
  }
  return absl::OkStatus();
}

absl::Status ProgramLister::AddStatementRows(
    const ProgramMap::Statement& statement, std::string_view text) {
  std::vector<uint16_t> words;
  words.reserve(statement.size);
  for (int i = 0; i < statement.size; ++i) {
    std::optional<uint16_t> word =
        GetWord(statement.segment, statement.offset + i);
    if (!word.has_value()) {
      return absl::InvalidArgumentError(
          absl::StrCat("Program has no data for the statement at ",
                       FormatLine(statement.location)));
    }
    words.push_back(*word);
  }
  for (int i = 0; i < statement.size; i += kWordsPerRow) {
    AddRow(FormatLocation(statement.segment, statement.offset + i),
           absl::MakeConstSpan(words).subspan(i, kWordsPerRow),
           i == 0 ? text : "");
  }
  return absl::OkStatus();
}

void ProgramLister::AddRow(std::string_view location,
                           absl::Span<const uint16_t> words,
                           std::string_view text) {
  std::string words_text;
  for (const uint16_t word : words) {
    absl::StrAppendFormat(&words_text, "%s%04X", words_text.empty() ? "" : " ",
                          word);
  }
  const std::string row =
      absl::StrFormat("%-*s  %-*s  %s", location_width_, location, kWordsWidth,
                      words_text, text);
  absl::StrAppend(&listing_, absl::StripTrailingAsciiWhitespace(row), "\n");
}

std::string ProgramLister::FormatLocation(int segment, int offset) const {
  const std::string& name = map_->GetSegments()[segment];
  if (name.empty()) {
    return absl::StrFormat("%04X", offset);
  }
  if (static_cast<int>(name.size()) > kMaxSegmentNameWidth) {
    return absl::StrFormat(
        "%s%c:%04X", std::string_view(name).substr(0, kMaxSegmentNameWidth - 1),
        kSegmentNameCut, offset);
  }
  return absl::StrFormat("%s:%04X", name, offset);
}

std::optional<uint16_t> ProgramLister::GetWord(int segment, int offset) const {
  // The block that has `offset` is the last that starts at or before it.
  const std::vector<Program::Segment::Data>& blocks =
      program_->segments[segment].data;
  auto it = absl::c_upper_bound(
      blocks, offset, [](int value, const Program::Segment::Data& block) {
        return value < block.offset;
      });
  if (it == blocks.begin()) {
    return std::nullopt;
  }
  --it;
  const int index = offset - it->offset;
  if (index >= static_cast<int>(it->data.size())) {
    return std::nullopt;
  }
  return it->data[index];
}

}  // namespace

absl::StatusOr<std::string> FormatProgramListing(
    const Program& program, const ProgramMap& map,
    absl::Span<const std::string> sources) {
  return ProgramLister(&program, &map).Format(sources);
}

absl::Status WriteProgramListingFile(gb::FileSystem& file_system,
                                     std::string_view path,
                                     const Program& program,
                                     const ProgramMap& map) {
  std::vector<std::string> sources;
  for (const std::string& file : map.GetFiles()) {
    if (!file_system.ReadFile(file, &sources.emplace_back())) {
      return absl::NotFoundError(absl::StrCat("Could not read file: ", file));
    }
  }
  absl::StatusOr<std::string> listing =
      FormatProgramListing(program, map, sources);
  if (!listing.ok()) {
    return listing.status();
  }
  if (!file_system.WriteFile(path, *listing)) {
    return absl::UnavailableError(absl::StrCat("Could not write file: ", path));
  }
  return absl::OkStatus();
}

}  // namespace oz3
