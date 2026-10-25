// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_MAP_H_
#define OZ3_TOOLS_PROGRAM_MAP_H_

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/types/span.h"

namespace oz3 {

// Where the words of an assembled program came from, and its symbols, for a
// debugger.
//
// A map works in segment offsets, as every address in a program is one, and
// its segments are the program's, by index. So a map is only meaningful with
// the program it was assembled with.
class ProgramMap final {
 public:
  // A line in a source file, numbered from 0 as gb::LexerLocation is.
  struct Location {
    int file = 0;  // An index into GetFiles().
    int line = 0;

    bool operator==(const Location&) const = default;
  };

  // A statement that wrote words: an instruction, .DW, or .DD.
  struct Statement {
    int segment = 0;  // An index into GetSegments().
    int offset = 0;
    int size = 0;  // In words, at least 1.
    Location location;

    bool operator==(const Statement&) const = default;
  };

  // A label or local label, whose value is its offset in its segment. A local
  // label's name is its scope's label and then its own ("START@LOOP"), or only
  // its own before the program's first label ("@LOOP").
  struct Label {
    std::string name;
    int segment = 0;    // An index into GetSegments().
    int offset = 0;     // Up to the end of a segment, which a label can be at.
    Location location;  // Where it is defined.

    bool operator==(const Label&) const = default;
  };

  // A constant, which has a value in each width it can be evaluated in, as
  // where it is used sets its width. The word value isn't always the dword
  // value's low word, and a constant may have no word value at all.
  struct Constant {
    std::string name;
    std::optional<uint16_t> word_value;
    uint32_t dword_value = 0;
    Location location;  // Where it is defined.

    bool operator==(const Constant&) const = default;
  };

  // Returns a map of these, which can be in any order, or an error naming
  // something wrong with them:
  // - A location's file is out of range, or its line is negative.
  // - A statement's segment is out of range, its size is less than 1, it
  //   starts before 0 or passes the end of a segment, or it overlaps another
  //   statement in its segment.
  // - A label's segment is out of range, or its offset is before 0 or past the
  //   end of a segment.
  // - A label or constant has no name, or the name of another label or
  //   constant.
  // - A segment has the name of another, or has no name and isn't the only
  //   segment (as the default program's one segment is).
  static absl::StatusOr<ProgramMap> Create(std::vector<std::string> files,
                                           std::vector<std::string> segments,
                                           std::vector<Statement> statements,
                                           std::vector<Label> labels,
                                           std::vector<Constant> constants);

  // Constructs an empty map.
  ProgramMap() = default;

  ProgramMap(const ProgramMap&) = default;
  ProgramMap(ProgramMap&&) = default;
  ProgramMap& operator=(const ProgramMap&) = default;
  ProgramMap& operator=(ProgramMap&&) = default;
  ~ProgramMap() = default;

  // Returns the paths of the source files, the main file first.
  absl::Span<const std::string> GetFiles() const { return files_; }

  // Returns the segments' names, in the program's order.
  absl::Span<const std::string> GetSegments() const { return segments_; }

  // Returns the statements by segment, then offset.
  absl::Span<const Statement> GetStatements() const { return statements_; }

  // Returns the labels by segment, then offset, then name, and the constants
  // by name.
  absl::Span<const Label> GetLabels() const { return labels_; }
  absl::Span<const Constant> GetConstants() const { return constants_; }

  // Returns the statement that wrote the word at `offset` in `segment`, or null
  // if no statement did (such as words .DS reserves).
  const Statement* FindStatement(int segment, int offset) const;

  // Returns the label in `segment` nearest at or before `offset`, for writing
  // an address as "label+n", or null if there is none. Of several at the same
  // offset, it is the first by name, so a label comes before the local labels
  // in its scope.
  const Label* FindLabel(int segment, int offset) const;

 private:
  std::vector<std::string> files_;
  std::vector<std::string> segments_;
  std::vector<Statement> statements_;
  std::vector<Label> labels_;
  std::vector<Constant> constants_;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_MAP_H_
