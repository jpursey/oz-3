// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_map.h"

#include <algorithm>
#include <compare>
#include <iterator>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/container/flat_hash_set.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "oz3/core/core_types.h"

namespace oz3 {

namespace {

// A segment and an offset in it, which statements and labels are sorted by.
struct SegmentOffset {
  int segment = 0;
  int offset = 0;

  auto operator<=>(const SegmentOffset&) const = default;
};

// Each error names what is wrong by its `kind` and `id` ("Statement 2", or
// "Label START"), which are only formatted for an error.

// Returns an error if `location` isn't in one of `file_count` files.
absl::Status ValidateLocation(const ProgramMap::Location& location,
                              int file_count, std::string_view kind,
                              const absl::AlphaNum& id) {
  if (location.file < 0 || location.file >= file_count) {
    return absl::InvalidArgumentError(
        absl::StrCat(kind, " ", id, " is in file ", location.file,
                     ", but the map has ", file_count));
  }
  if (location.line < 0) {
    return absl::InvalidArgumentError(
        absl::StrCat(kind, " ", id, " is on line ", location.line));
  }
  return absl::OkStatus();
}

// Returns an error if `segment` isn't one of `segment_count`.
absl::Status ValidateSegment(int segment, int segment_count,
                             std::string_view kind, const absl::AlphaNum& id) {
  if (segment < 0 || segment >= segment_count) {
    return absl::InvalidArgumentError(
        absl::StrCat(kind, " ", id, " is in segment ", segment,
                     ", but the map has ", segment_count));
  }
  return absl::OkStatus();
}

absl::Status ValidateSegmentNames(const std::vector<std::string>& segments) {
  absl::flat_hash_set<std::string_view> names;
  for (int i = 0; i < static_cast<int>(segments.size()); ++i) {
    const std::string& name = segments[i];
    if (name.empty() && segments.size() != 1) {
      return absl::InvalidArgumentError(absl::StrCat(
          "Segment ", i, " has no name, but it isn't the only segment"));
    }
    if (!names.insert(name).second) {
      return absl::InvalidArgumentError(
          absl::StrCat("Segment ", i, " is named ", name,
                       ", which another segment is already named"));
    }
  }
  return absl::OkStatus();
}

absl::Status ValidateStatement(const ProgramMap::Statement& statement,
                               int index, int file_count, int segment_count) {
  if (absl::Status status =
          ValidateSegment(statement.segment, segment_count, "Statement", index);
      !status.ok()) {
    return status;
  }
  if (statement.size < 1) {
    return absl::InvalidArgumentError(
        absl::StrCat("Statement ", index, " has ", statement.size,
                     " words, but it must have at least 1"));
  }
  // The size is at least 1, so this can't overflow, as adding it could.
  if (statement.offset < 0 ||
      statement.offset > kMemoryBankMaxSize - statement.size) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Statement ", index, " has ", statement.size, " words at offset ",
        statement.offset, ", which isn't in a segment of ", kMemoryBankMaxSize,
        " words"));
  }
  return ValidateLocation(statement.location, file_count, "Statement", index);
}

absl::Status ValidateLabel(const ProgramMap::Label& label, int file_count,
                           int segment_count) {
  if (absl::Status status =
          ValidateSegment(label.segment, segment_count, "Label", label.name);
      !status.ok()) {
    return status;
  }
  if (label.offset < 0 || label.offset > kMemoryBankMaxSize) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Label ", label.name, " is at offset ", label.offset,
        ", which isn't in a segment of ", kMemoryBankMaxSize, " words"));
  }
  return ValidateLocation(label.location, file_count, "Label", label.name);
}

// Returns an error if `name` is empty, or already in `names`, and adds it to
// `names` otherwise. `what` is "Label" or "Constant", and `index` is its
// index in the labels or constants.
absl::Status AddName(std::string_view name, std::string_view what, int index,
                     absl::flat_hash_set<std::string_view>& names) {
  if (name.empty()) {
    return absl::InvalidArgumentError(
        absl::StrCat(what, " ", index, " has no name"));
  }
  if (!names.insert(name).second) {
    return absl::InvalidArgumentError(absl::StrCat(
        what, " ", name, " has the name of another label or constant"));
  }
  return absl::OkStatus();
}

SegmentOffset GetSegmentOffset(const ProgramMap::Statement& statement) {
  return {statement.segment, statement.offset};
}

SegmentOffset GetSegmentOffset(const ProgramMap::Label& label) {
  return {label.segment, label.offset};
}

}  // namespace

absl::StatusOr<ProgramMap> ProgramMap::Create(std::vector<std::string> files,
                                              std::vector<std::string> segments,
                                              std::vector<Statement> statements,
                                              std::vector<Label> labels,
                                              std::vector<Constant> constants) {
  const int file_count = static_cast<int>(files.size());
  const int segment_count = static_cast<int>(segments.size());
  if (absl::Status status = ValidateSegmentNames(segments); !status.ok()) {
    return status;
  }
  for (int i = 0; i < static_cast<int>(statements.size()); ++i) {
    if (absl::Status status =
            ValidateStatement(statements[i], i, file_count, segment_count);
        !status.ok()) {
      return status;
    }
  }
  absl::flat_hash_set<std::string_view> names;
  names.reserve(labels.size() + constants.size());
  for (int i = 0; i < static_cast<int>(labels.size()); ++i) {
    const Label& label = labels[i];
    if (absl::Status status = AddName(label.name, "Label", i, names);
        !status.ok()) {
      return status;
    }
    if (absl::Status status = ValidateLabel(label, file_count, segment_count);
        !status.ok()) {
      return status;
    }
  }
  for (int i = 0; i < static_cast<int>(constants.size()); ++i) {
    const Constant& constant = constants[i];
    if (absl::Status status = AddName(constant.name, "Constant", i, names);
        !status.ok()) {
      return status;
    }
    if (absl::Status status = ValidateLocation(constant.location, file_count,
                                               "Constant", constant.name);
        !status.ok()) {
      return status;
    }
  }

  // Statements can only be checked for overlaps once they are in order.
  absl::c_sort(statements, [](const Statement& a, const Statement& b) {
    return GetSegmentOffset(a) < GetSegmentOffset(b);
  });
  for (int i = 1; i < static_cast<int>(statements.size()); ++i) {
    const Statement& before = statements[i - 1];
    const Statement& statement = statements[i];
    if (before.segment == statement.segment &&
        before.offset + before.size > statement.offset) {
      return absl::InvalidArgumentError(absl::StrCat(
          "The statements at offsets ", before.offset, " and ",
          statement.offset, " in segment ", statement.segment, " overlap"));
    }
  }
  absl::c_sort(labels, [](const Label& a, const Label& b) {
    return std::tie(a.segment, a.offset, a.name) <
           std::tie(b.segment, b.offset, b.name);
  });
  absl::c_sort(constants, [](const Constant& a, const Constant& b) {
    return a.name < b.name;
  });

  ProgramMap map;
  map.files_ = std::move(files);
  map.segments_ = std::move(segments);
  map.statements_ = std::move(statements);
  map.labels_ = std::move(labels);
  map.constants_ = std::move(constants);
  return map;
}

const ProgramMap::Statement* ProgramMap::FindStatement(int segment,
                                                       int offset) const {
  // The statement that starts last at or before the offset is the only one
  // that can hold it, as statements don't overlap.
  auto it = absl::c_upper_bound(
      statements_, SegmentOffset{segment, offset},
      [](const SegmentOffset& key, const Statement& statement) {
        return key < GetSegmentOffset(statement);
      });
  if (it == statements_.begin()) {
    return nullptr;
  }
  --it;
  if (it->segment != segment || offset >= it->offset + it->size) {
    return nullptr;
  }
  return &*it;
}

const ProgramMap::Label* ProgramMap::FindLabel(int segment, int offset) const {
  // The last label at or before the offset is at the nearest offset, but it is
  // the last there by name, so this then finds the first there.
  auto it =
      absl::c_upper_bound(labels_, SegmentOffset{segment, offset},
                          [](const SegmentOffset& key, const Label& label) {
                            return key < GetSegmentOffset(label);
                          });
  if (it == labels_.begin()) {
    return nullptr;
  }
  const SegmentOffset nearest = GetSegmentOffset(*std::prev(it));
  if (nearest.segment != segment) {
    return nullptr;
  }
  it = std::lower_bound(labels_.begin(), it, nearest,
                        [](const Label& label, const SegmentOffset& key) {
                          return GetSegmentOffset(label) < key;
                        });
  return &*it;
}

}  // namespace oz3
