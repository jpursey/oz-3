// Copyright (c) 2025 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/program_loader.h"

#include "absl/log/check.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/processor.h"

namespace oz3 {

ProgramLoader::ProgramLoader(Processor* processor) : processor_(processor) {
  DCHECK(processor_ != nullptr);
  free_pages_.resize(kMaxMemoryBanks);
  for (int b = 0; b < kMaxMemoryBanks; ++b) {
    BankPages& pages = free_pages_[b];
    pages.bank = b;

    const MemoryBank* memory_bank = processor_->GetMemory(b);
    const MemoryPageMasks masks = memory_bank->GetMemPageMasks();
    const MemoryPageMasks map_masks = memory_bank->GetMapPageMasks();
    const uint16_t read_mask = masks.read_mask & ~map_masks.read_mask;
    const uint16_t read_write_mask =
        read_mask & masks.write_mask & ~map_masks.write_mask;
    const uint16_t read_only_mask = read_mask & ~read_write_mask;
    if (read_only_mask == 0 && read_write_mask == 0) {
      continue;
    }
    AddPageSpans(pages.read_only, read_only_mask);
    AddPageSpans(pages.read_write, read_write_mask);
    unused_banks_.insert(b);
  }
}

void ProgramLoader::AddPageSpans(std::vector<PageSpan>& spans, uint16_t mask) {
  if (mask == 0) {
    return;
  }
  std::optional<PageSpan> span;
  for (uint16_t p = 0; p < kMemoryBankPageCount; ++p) {
    if ((mask & (1 << p)) != 0) {
      if (!span.has_value()) {
        span = PageSpan{.begin = p, .end = static_cast<uint16_t>(p + 1)};
      } else {
        ++span->end;
      }
    } else if (span.has_value()) {
      spans.push_back(*span);
      span.reset();
    }
  }
  if (span.has_value()) {
    spans.push_back(*span);
  }
}

ProgramLoader::Handle ProgramLoader::Load(const Program& program) {
  BankReqs bank_reqs;
  if (!GetRequirements(program, bank_reqs)) {
    return kInvalidHandle;
  }
  LoadedProgram& loaded_program = loaded_[next_handle_];

  auto GetSegment = [&program](int segment) -> const Program::Segment* {
    if (segment == 0xFFFF) {
      return nullptr;
    }
    if (segment < 0 || segment >= program.segments.size()) {
      return nullptr;
    }
    return &program.segments[segment];
  };

  SegmentAssign segments[4];
  segments[CpuCore::CODE] = {GetSegment(program.code_segment)};
  segments[CpuCore::STACK] = {GetSegment(program.stack_segment)};
  segments[CpuCore::DATA] = {GetSegment(program.data_segment)};
  segments[CpuCore::EXTRA] = {GetSegment(program.extra_segment)};

  // Creaste a working copy of the current free pages, so we can undo if we
  // fail.
  FreePages free_pages = free_pages_;

  // Start by allocating from unused banks first, and then proceed to the
  // remaining pages.
  std::vector<int> banks;
  banks.reserve(free_pages.size());
  for (int b : unused_banks_) {
    banks.push_back(b);
  }
  for (const auto& pages : free_pages) {
    if (!unused_banks_.contains(pages.bank) &&
        (!pages.read_only.empty() || !pages.read_write.empty())) {
      banks.push_back(pages.bank);
    }
  }

  // Attempt to allocate from the banks.
  for (const BankReq& bank_req : bank_reqs) {
    bool found = false;
    for (int b : banks) {
      if ((bank_req.bank_mask & (1 << b)) == 0) {
        continue;
      }
      if (AllocateBankPages(loaded_program, segments, free_pages[b],
                            bank_req)) {
        found = true;
        break;
      }
    }
    if (!found) {
      loaded_.erase(next_handle_);
      return kInvalidHandle;
    }
  }

  // Set ResetParams for each instance.
  loaded_program.instances.reserve(program.instances.size());
  for (const auto& instance : program.instances) {
    CpuCore::ResetParams& reset_params =
        loaded_program.instances.emplace_back();
    reset_params.mask = 0;
    if (auto& code = segments[CpuCore::CODE]; code.segment != nullptr) {
      reset_params.mask |= CpuCore::ResetParams::MC | CpuCore::ResetParams::RBC;
      reset_params.mb |= CpuCore::Banks::Default().SetCode(code.bank).ToWord();
      reset_params.bc = (code.span.begin * kMemoryBankPageSize) + instance.code;
    }
    if (auto& stack = segments[CpuCore::STACK]; stack.segment != nullptr) {
      reset_params.mask |= CpuCore::ResetParams::MS | CpuCore::ResetParams::RBS;
      reset_params.mb |=
          CpuCore::Banks::Default().SetStack(stack.bank).ToWord();
      reset_params.bs = (stack.span.end * kMemoryBankPageSize) - instance.stack;
    }
    if (auto& data = segments[CpuCore::DATA]; data.segment != nullptr) {
      reset_params.mask |= CpuCore::ResetParams::MD | CpuCore::ResetParams::RBD;
      reset_params.mb |= CpuCore::Banks::Default().SetData(data.bank).ToWord();
      reset_params.bd = (data.span.begin * kMemoryBankPageSize) + instance.data;
    }
    if (auto& extra = segments[CpuCore::EXTRA]; extra.segment != nullptr) {
      reset_params.mask |= CpuCore::ResetParams::ME | CpuCore::ResetParams::RBE;
      reset_params.mb |=
          CpuCore::Banks::Default().SetExtra(extra.bank).ToWord();
      reset_params.be =
          (extra.span.begin * kMemoryBankPageSize) + instance.extra;
    }
  }

  // Propagate changes back to state.
  free_pages_ = std::move(free_pages);
  for (const BankPages& pages : loaded_program.pages) {
    unused_banks_.erase(pages.bank);
    ++used_bank_counts[pages.bank];
  }
  return next_handle_++;
}

bool ProgramLoader::GetRequirements(const Program& program,
                                    BankReqs& bank_reqs) {
  // Validate limits.
  if (program.banks.size() > kMaxMemoryBanks ||
      program.segments.size() > kMaxMemoryBanks * kMemoryBankPageCount ||
      program.instances.size() > kMaxCores) {
    return false;
  }

  // Set up base banks requirements.
  BankReqs reqs(program.banks.size());
  for (int b = 0; b < program.banks.size(); ++b) {
    reqs[b].bank_mask = program.banks[b].assign_mask;
  }

  // Add Segment requirements
  static_assert(kMemoryBankPageCount <= 16);
  uint16_t bank_masks[kMaxMemoryBanks] = {};
  uint16_t page_counts[kMaxMemoryBanks] = {};
  for (const auto& segment : program.segments) {
    // Ensure the bank index is valid.
    if (segment.bank >= program.banks.size()) {
      return false;
    }

    if (segment.page_start == 0xFFFF) {
      if (segment.page_count == 0 ||
          segment.page_count > kMemoryBankPageCount) {
        return false;
      }

      (segment.access == Program::Segment::Access::kReadOnly
           ? reqs[segment.bank].any_read_only
           : reqs[segment.bank].any_read_write)
          .push_back(&segment);
    } else {
      if (segment.page_count == 0 ||
          segment.page_start + segment.page_count > kMemoryBankPageCount) {
        return false;
      }

      // Mark bank pages as reserved, to ensure no two segments are requiring
      // overlapping ranges.
      for (int p = 0; p < segment.page_count; ++p) {
        uint16_t mask = (1 << (segment.page_start + p));
        if ((bank_masks[segment.bank] & mask) != 0) {
          return false;
        }
        bank_masks[segment.bank] |= mask;
      }

      (segment.access == Program::Segment::Access::kReadOnly
           ? reqs[segment.bank].fixed_read_only
           : reqs[segment.bank].fixed_read_write)
          .push_back(&segment);
    }

    // Validate total page count does not exceed the max for a single memory
    // bank.
    page_counts[segment.bank] += segment.page_count;
    if (page_counts[segment.bank] > kMemoryBankPageCount) {
      return false;
    }

    // Validate all initial data fits within the segment.
    for (const auto& data : segment.data) {
      if (data.offset + data.data.size() >
          segment.page_count * kMemoryBankPageSize) {
        return false;
      }
    }
  }

  // Validate instance specifications.
  if (program.code_segment != 0xFFFF) {
    if (program.code_segment >= program.segments.size()) {
      return false;
    }
    for (const auto& offsets : program.instances) {
      if (offsets.code > program.segments[program.code_segment].page_count *
                             kMemoryBankPageSize) {
        return false;
      }
    }
  }
  if (program.stack_segment != 0xFFFF) {
    if (program.stack_segment >= program.segments.size()) {
      return false;
    }
    for (const auto& offsets : program.instances) {
      if (offsets.stack > program.segments[program.stack_segment].page_count *
                              kMemoryBankPageSize) {
        return false;
      }
    }
  }
  if (program.data_segment != 0xFFFF) {
    if (program.data_segment >= program.segments.size()) {
      return false;
    }
    for (const auto& offsets : program.instances) {
      if (offsets.data > program.segments[program.data_segment].page_count *
                             kMemoryBankPageSize) {
        return false;
      }
    }
  }
  if (program.extra_segment != 0xFFFF) {
    if (program.extra_segment >= program.segments.size()) {
      return false;
    }
    for (const auto& offsets : program.instances) {
      if (offsets.extra > program.segments[program.extra_segment].page_count *
                              kMemoryBankPageSize) {
        return false;
      }
    }
  }

  // Success!
  bank_reqs = std::move(reqs);
  return true;
}

bool ProgramLoader::AllocateBankPages(LoadedProgram& program,
                                      SegmentAssign segments[4],
                                      BankPages& pages, const BankReq& reqs) {
  absl::flat_hash_map<const Program::Segment*, PageSpan> assigned;
  BankPages new_pages = pages;
  BankPages new_program_pages;
  new_program_pages.bank = pages.bank;

  // Attempt to allocate all the pages. Fixed pages before dynamic pages, and
  // read-write pages before read-only pages.
  for (const Program::Segment* segment : reqs.fixed_read_write) {
    std::optional<PageSpan> program_span = AllocateFixedSpan(
        new_pages.read_write, segment->page_start, segment->page_count);
    if (!program_span.has_value()) {
      return false;
    }
    new_program_pages.read_write.push_back(*program_span);
    UpdateSegmentAssignments(segment, pages.bank, *program_span, segments);
    WriteSegmentData(pages.bank, *program_span, segment);
  }

  for (const Program::Segment* segment : reqs.fixed_read_only) {
    std::optional<PageSpan> program_span = AllocateFixedSpan(
        new_pages.read_only, segment->page_start, segment->page_count);
    if (program_span.has_value()) {
      new_program_pages.read_only.push_back(*program_span);
    } else {
      program_span = AllocateFixedSpan(
          new_pages.read_write, segment->page_start, segment->page_count);
      if (!program_span.has_value()) {
        return false;
      }
      new_program_pages.read_write.push_back(*program_span);
    }
    UpdateSegmentAssignments(segment, pages.bank, *program_span, segments);
    WriteSegmentData(pages.bank, *program_span, segment);
  }

  for (const Program::Segment* segment : reqs.any_read_write) {
    std::optional<PageSpan> program_span =
        AllocateDynamicSpan(new_pages.read_write, segment->page_count);
    if (!program_span.has_value()) {
      return false;
    }
    new_program_pages.read_write.push_back(*program_span);
    UpdateSegmentAssignments(segment, pages.bank, *program_span, segments);
    WriteSegmentData(pages.bank, *program_span, segment);
  }

  for (const Program::Segment* segment : reqs.any_read_only) {
    std::optional<PageSpan> program_span =
        AllocateDynamicSpan(new_pages.read_only, segment->page_count);
    if (program_span.has_value()) {
      new_program_pages.read_only.push_back(*program_span);
    } else {
      program_span =
          AllocateDynamicSpan(new_pages.read_write, segment->page_count);
      if (!program_span.has_value()) {
        return false;
      }
      new_program_pages.read_write.push_back(*program_span);
    }
    UpdateSegmentAssignments(segment, pages.bank, *program_span, segments);
    WriteSegmentData(pages.bank, *program_span, segment);
  }

  pages = std::move(new_pages);
  SortAndMergePages(new_program_pages.read_only);
  SortAndMergePages(new_program_pages.read_write);
  program.pages.emplace_back(std::move(new_program_pages));
  return true;
}

void ProgramLoader::UpdateSegmentAssignments(const Program::Segment* segment,
                                             int bank, PageSpan span,
                                             SegmentAssign segments[4]) {
  for (int i = 0; i < 4; ++i) {
    if (segments[i].segment == segment) {
      segments[i].bank = bank;
      segments[i].span = span;
    }
  }
}

void ProgramLoader::WriteSegmentData(int bank, PageSpan span,
                                     const Program::Segment* segment) {
  MemoryBank* memory = processor_->GetMemory(bank);
  int start_address = span.begin * kMemoryBankPageSize;
  for (const auto& data : segment->data) {
    auto dst = memory->GetMem(start_address + data.offset, data.data.size());
    DCHECK(dst.size() == data.data.size());
    std::memcpy(dst.data(), data.data.data(),
                data.data.size() * sizeof(uint16_t));
  }
}

std::optional<ProgramLoader::PageSpan> ProgramLoader::AllocateFixedSpan(
    std::vector<PageSpan>& pages, uint16_t page_start, uint16_t page_count) {
  for (auto it = pages.begin(); it != pages.end(); ++it) {
    PageSpan& span = *it;
    if (page_start < span.begin || page_start + page_count > span.end) {
      continue;
    }
    if (span.begin == page_start) {
      span.begin += page_count;
      if (span.begin == span.end) {
        pages.erase(it);
      }
    } else if (span.end == page_start + page_count) {
      span.end -= page_count;
    } else {
      PageSpan post_span = {static_cast<uint16_t>(page_start + page_count),
                            span.end};
      span = {span.begin, page_start};
      pages.insert(++it, post_span);
    }
    return PageSpan{page_start, static_cast<uint16_t>(page_start + page_count)};
  }
  return {};
}

std::optional<ProgramLoader::PageSpan> ProgramLoader::AllocateDynamicSpan(
    std::vector<PageSpan>& pages, uint16_t page_count) {
  for (auto it = pages.begin(); it != pages.end(); ++it) {
    PageSpan& span = *it;
    if (page_count > span.end - span.begin) {
      continue;
    }
    const uint16_t page_start = span.begin;
    span.begin += page_count;
    if (span.begin == span.end) {
      pages.erase(it);
    }
    return PageSpan{page_start, static_cast<uint16_t>(page_start + page_count)};
  }
  return {};
}

void ProgramLoader::Unload(Handle handle) {
  auto it = loaded_.find(handle);
  if (it == loaded_.end()) {
    return;
  }
  LoadedProgram program = std::move(it->second);
  loaded_.erase(it);

  for (const BankPages& pages : program.pages) {
    ReturnPages(free_pages_[pages.bank].read_only, pages.read_only);
    ReturnPages(free_pages_[pages.bank].read_write, pages.read_write);
    DCHECK(used_bank_counts[pages.bank] > 0);
    if (--used_bank_counts[pages.bank] == 0) {
      unused_banks_.insert(pages.bank);
    }
  }
}

void ProgramLoader::ReturnPages(std::vector<PageSpan>& pages,
                                const std::vector<PageSpan>& program_pages) {
  if (program_pages.empty()) {
    return;
  }
  for (const PageSpan& span : program_pages) {
    pages.push_back(span);
  }
  SortAndMergePages(pages);
}

void ProgramLoader::SortAndMergePages(std::vector<PageSpan>& pages) {
  std::sort(pages.begin(), pages.end());
  for (auto it = pages.begin(); it != pages.end();) {
    auto next = it + 1;
    if (next != pages.end() && next->begin == it->end) {
      it->end = next->end;
      pages.erase(next);
    } else {
      ++it;
    }
  }
}

CpuCore::ResetParams ProgramLoader::GetResetParams(Handle handle,
                                                   int instance) const {
  auto it = loaded_.find(handle);
  if (it == loaded_.end()) {
    return {};
  }
  const LoadedProgram& program = it->second;
  if (instance < 0 || instance >= program.instances.size()) {
    return {};
  }
  return program.instances[instance];
}

}  // namespace oz3
