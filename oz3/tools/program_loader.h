// Copyright (c) 2025 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_LOADER_H_
#define OZ3_TOOLS_PROGRAM_LOADER_H_

#include <map>
#include <optional>
#include <set>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_format.h"
#include "absl/types/span.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/processor.h"
#include "oz3/tools/program.h"

namespace oz3 {

// A ProgramLoader is a helper class that manages loading one or more programs
// into memory for a specified Processor.
//
// Programs may be manually defined (see program.h), or loaded from disk using
// the defined Program format (see program_exporter.h).
//
// Each program is assigned a program handle when it is successfully loaded (all
// the program's memory requirements can be fulfilled). The program handle can
// be used to retrieve the CpuCore::ResetParams needed to start the program, or
// to unload a program that is no longer running.
//
// Loading and unloading programs via this class are *not* simulated operations,
// and do not consume any cycles on the Processor. As such, this class is
// intended for use where program loading is a fully external operation (for
// instance by the ProgramDebugger).
//
// For simulated program loading, the Ozzy virtual computer supports this as
// part of its ROM program (which may also be loaded via this class), or its
// assembly definition may be copied or otherwise used as a reference.
class ProgramLoader final {
 public:
  // Defines a contiguous range of pages [begin,end) in a memory bank.
  struct PageSpan {
    uint16_t begin = 0;
    uint16_t end = 0;

    auto operator<=>(const PageSpan&) const = default;

    template <typename Sink>
    friend void AbslStringify(Sink& sink, const PageSpan& span) {
      absl::Format(&sink, "(%v,%v)", span.begin, span.end);
    }
  };

  // Defines a set of pages for a memory bank.
  struct BankPages {
    int bank = 0;
    std::vector<PageSpan> read_only;   // Sorted
    std::vector<PageSpan> read_write;  // Sorted

    auto operator<=>(const BankPages&) const = default;

    template <typename Sink>
    friend void AbslStringify(Sink& sink, const BankPages& pages) {
      absl::Format(&sink, "{bank=%v, read_only={", pages.bank);
      for (int i = 0; i < pages.read_only.size(); ++i) {
        absl::Format(&sink, "%v%s", pages.read_only[i],
                     (i < pages.read_only.size() - 1 ? "," : ""));
      }
      absl::Format(&sink, "}, read_write={");
      for (int i = 0; i < pages.read_write.size(); ++i) {
        absl::Format(&sink, "%v%s", pages.read_write[i],
                     (i < pages.read_write.size() - 1 ? "," : ""));
      }
      absl::Format(&sink, "}}");
    }
  };

  // Defines a loaded program.
  struct LoadedProgram {
    // This contains one set of pages for each program bank, which maps each one
    // to an actual bank.
    std::vector<BankPages> pages;

    // This contains the ResetParams for every defined instance of the program.
    std::vector<CpuCore::ResetParams> instances;
  };

  // Handle for a loaded program;
  using Handle = int;

  // Invalid handle value.
  static constexpr Handle kInvalidHandle = 0;

  // Constructs a new ProgramLoader for the specified Processor.
  //
  // The processor must be valid and have a lifetime equal or greater to the
  // ProgramLoader. Only one ProgramLoader should be associated with a
  // Processor, as otherwise they will conflict over memory allocations.
  explicit ProgramLoader(Processor* processor);

  ProgramLoader(const ProgramLoader&) = delete;
  ProgramLoader& operator=(const ProgramLoader&) = delete;
  ~ProgramLoader() = default;

  // Loads a program into memory for the Processor.
  //
  // If the program is invalidly specified or its requirements for memory
  // allocation cannot be met, then this will return kInvalidHandle. There are
  // no guarantees around changes to memory for regions that are not allocated
  // to any currently loaded program.
  Handle Load(const Program& program);

  // Unloads a previously loaded program from memory.
  //
  // This does not actually modify any memory in the Processor, but just
  // unregisters the memory in the loader so it can be reallocated by another
  // program.
  void Unload(Handle handle);

  // Retrieves the ResetParams for the specified program handle.
  //
  // If the program handle does not reference a currently loaded program, then
  // this will return empty ResetParams.
  //
  // The instance index should be specified if the program will be executed
  // simultaneously by more than one core. If specified, it is the index into
  // the corresponding instance array defined in program. Each program defines a
  // maximum number of instances, and if instance not in this range, then this
  // will return empty ResetParams.
  CpuCore::ResetParams GetResetParams(Handle handle, int instance = 0) const;

  // Retrieves a loaded program.
  //
  // Generally, this is not needed and should not be used, except for tests and
  // debugging purposes.
  //
  // If the program handle does not reference a currently loaded program, then
  // this will return nullptr.
  const LoadedProgram* GetProgram(Handle handle) const {
    auto it = loaded_.find(handle);
    return it != loaded_.end() ? &it->second : nullptr;
  }

  // Retrieves all free pages per-bank on the Processor.
  //
  // Generally, this is not needed and should not be used, except for tests and
  // debugging purposes.
  absl::Span<const BankPages> GetFreePages() const {
    return absl::MakeSpan(free_pages_);
  }

 private:
  // Tracks a segment assignment in a loaded program.
  struct SegmentAssign {
    const Program::Segment* segment = nullptr;
    int bank = 0;
    PageSpan span;
  };

  // Set of requirements for a bank allocated to a program being loaded.
  struct BankReq {
    uint32_t bank_mask = 0;
    std::vector<const Program::Segment*> fixed_read_only;
    std::vector<const Program::Segment*> fixed_read_write;
    std::vector<const Program::Segment*> any_read_only;
    std::vector<const Program::Segment*> any_read_write;
  };
  using BankReqs = std::vector<BankReq>;

  // Sorted containers indexed by bank. This results in well-defined allocation
  // behavior when loading programs.
  using BankIndexes = std::set<int>;
  using FreePages = std::vector<BankPages>;

  // Adds pages spans that correspond to the specified mask
  void AddPageSpans(std::vector<PageSpan>& spans, uint16_t mask);

  // Determines the memory requirements for a program, returning false if the
  // Program has invalid or incompatible requirements.
  bool GetRequirements(const Program& program, BankReqs& bank_reqs);

  // Attempts to allocate pages for a program bank into the speciied bank pages.
  bool AllocateBankPages(LoadedProgram& program, SegmentAssign segments[4],
                         BankPages& pages, const BankReq& bank_req);

  // Removes the page requiest from the ordered set of page spans, returning the
  // resulting page span on success.
  std::optional<PageSpan> AllocateFixedSpan(std::vector<PageSpan>& pages,
                                            uint16_t page_start,
                                            uint16_t page_count);
  std::optional<PageSpan> AllocateDynamicSpan(std::vector<PageSpan>& pages,
                                              uint16_t page_count);

  // Updates assignments for the program banks (CODE, STACK, DATA, and EXTRA)
  // tied to specific program segments.
  void UpdateSegmentAssignments(const Program::Segment* segment, int bank,
                                PageSpan span, SegmentAssign segments[4]);

  // Writes any specified segment data to the allocated page span.
  void WriteSegmentData(int bank, PageSpan span,
                        const Program::Segment* segment);

  // Returns memory pages allocated to a program.
  void ReturnPages(std::vector<PageSpan>& pages,
                   const std::vector<PageSpan>& program_pages);

  // Sort and merge adjacent pages.
  void SortAndMergePages(std::vector<PageSpan>& pages);

  // Processor that this ProgramLoader is managing programs for.
  Processor* const processor_;

  // List of banks that have available page spans and are currently unused.
  // Loaded programs are preferentially assigned to unused banks, to maximize
  // parallel execution.
  BankIndexes unused_banks_;

  // A count of how many programs are currently allocated to each bank. This is
  // used to determine when a bank becomes unused again.
  uint16_t used_bank_counts[kMaxMemoryBanks] = {};

  // Available pages for read-only and read-write use, keyed by bank index.
  FreePages free_pages_;

  // All loaded programs
  absl::flat_hash_map<Handle, LoadedProgram> loaded_;

  // Handle allocation. This continually increments for each loaded program.
  int next_handle_ = 1;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_LOADER_H_
