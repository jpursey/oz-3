// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program.h"

#include <string_view>

#include "absl/strings/str_cat.h"
#include "oz3/core/core_types.h"

namespace oz3 {

namespace {

// Returns the number of words in `segment`.
int GetSegmentSize(const Program::Segment& segment) {
  return segment.page_count * kMemoryBankPageSize;
}

absl::Status ValidateSegment(const Program& program, int index) {
  const Program::Segment& segment = program.segments[index];
  if (segment.bank >= program.banks.size()) {
    return absl::InvalidArgumentError(
        absl::StrCat("Segment ", index, " is in bank ", segment.bank,
                     ", but the program has ", program.banks.size()));
  }
  if (segment.access != Program::Segment::Access::kReadOnly &&
      segment.access != Program::Segment::Access::kReadWrite) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Segment ", index, " has access ", static_cast<int>(segment.access),
        ", which is neither read-only (0) nor read-write (1)"));
  }
  if (segment.page_count == 0 || segment.page_count > kMemoryBankPageCount) {
    return absl::InvalidArgumentError(
        absl::StrCat("Segment ", index, " has ", segment.page_count,
                     " pages, which must be from 1 to ", kMemoryBankPageCount));
  }
  if (segment.page_start != 0xFFFF &&
      segment.page_start + segment.page_count > kMemoryBankPageCount) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Segment ", index, " starts at page ", segment.page_start, " with ",
        segment.page_count, " pages, past the end of its bank"));
  }

  constexpr int kAlignment = Program::Segment::Data::kAlignment;
  int data_end = 0;
  for (int b = 0; b < static_cast<int>(segment.data.size()); ++b) {
    const Program::Segment::Data& data = segment.data[b];
    const int size = static_cast<int>(data.data.size());
    if (size == 0 || data.offset % kAlignment != 0 || size % kAlignment != 0) {
      return absl::InvalidArgumentError(absl::StrCat(
          "Segment ", index, " has a data block at offset ", data.offset,
          " of ", size, " words, but both must be multiples of ", kAlignment,
          ", and the size can't be 0"));
    }
    if (data.offset < data_end) {
      return absl::InvalidArgumentError(absl::StrCat(
          "Segment ", index, " has a data block at offset ", data.offset,
          ", before the end of the block before it (", data_end, ")"));
    }
    data_end = data.offset + size;
    if (data_end > GetSegmentSize(segment)) {
      return absl::InvalidArgumentError(absl::StrCat(
          "Segment ", index, " has a data block at offset ", data.offset,
          " of ", size, " words, past the end of the segment (",
          GetSegmentSize(segment), " words)"));
    }
  }
  return absl::OkStatus();
}

absl::Status ValidateBankPages(const Program& program) {
  int page_counts[kMaxMemoryBanks] = {};
  uint16_t fixed_pages[kMaxMemoryBanks] = {};
  for (int s = 0; s < static_cast<int>(program.segments.size()); ++s) {
    const Program::Segment& segment = program.segments[s];
    page_counts[segment.bank] += segment.page_count;
    if (page_counts[segment.bank] > kMemoryBankPageCount) {
      return absl::InvalidArgumentError(
          absl::StrCat("Bank ", segment.bank, " has more than ",
                       kMemoryBankPageCount, " pages of segments"));
    }
    if (segment.page_start == 0xFFFF) {
      continue;
    }
    const uint16_t pages = static_cast<uint16_t>(((1 << segment.page_count) - 1)
                                                 << segment.page_start);
    if ((fixed_pages[segment.bank] & pages) != 0) {
      return absl::InvalidArgumentError(
          absl::StrCat("Segment ", s, " overlaps the pages of another segment",
                       " in bank ", segment.bank));
    }
    fixed_pages[segment.bank] |= pages;
  }
  return absl::OkStatus();
}

absl::Status ValidateRole(const Program& program, std::string_view role,
                          uint16_t segment,
                          uint16_t Program::Instance::* offset) {
  if (segment == 0xFFFF) {
    return absl::OkStatus();
  }
  if (segment >= program.segments.size()) {
    return absl::InvalidArgumentError(
        absl::StrCat("The ", role, " segment is ", segment,
                     ", but the program has ", program.segments.size()));
  }
  const int size = GetSegmentSize(program.segments[segment]);
  for (int i = 0; i < static_cast<int>(program.instances.size()); ++i) {
    if (program.instances[i].*offset > size) {
      return absl::InvalidArgumentError(
          absl::StrCat("Instance ", i, "'s ", role, " offset is ",
                       program.instances[i].*offset,
                       ", past the end of its segment (", size, " words)"));
    }
  }
  return absl::OkStatus();
}

}  // namespace

absl::Status ValidateProgram(const Program& program) {
  if (program.banks.size() > kMaxMemoryBanks) {
    return absl::InvalidArgumentError(
        absl::StrCat("Program has ", program.banks.size(), " banks, more than ",
                     kMaxMemoryBanks));
  }
  if (program.segments.size() > kMaxMemoryBanks * kMemoryBankPageCount) {
    return absl::InvalidArgumentError(absl::StrCat(
        "Program has ", program.segments.size(), " segments, more than ",
        kMaxMemoryBanks * kMemoryBankPageCount));
  }
  if (program.instances.size() > kMaxCores) {
    return absl::InvalidArgumentError(
        absl::StrCat("Program has ", program.instances.size(),
                     " instances, more than ", kMaxCores));
  }
  for (int s = 0; s < static_cast<int>(program.segments.size()); ++s) {
    if (absl::Status status = ValidateSegment(program, s); !status.ok()) {
      return status;
    }
  }
  if (absl::Status status = ValidateBankPages(program); !status.ok()) {
    return status;
  }
  if (absl::Status status = ValidateRole(program, "code", program.code_segment,
                                         &Program::Instance::code);
      !status.ok()) {
    return status;
  }
  if (absl::Status status = ValidateRole(
          program, "stack", program.stack_segment, &Program::Instance::stack);
      !status.ok()) {
    return status;
  }
  if (absl::Status status = ValidateRole(program, "data", program.data_segment,
                                         &Program::Instance::data);
      !status.ok()) {
    return status;
  }
  return ValidateRole(program, "extra", program.extra_segment,
                      &Program::Instance::extra);
}

}  // namespace oz3
