// Copyright (c) 2025 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_PROGRAM_H_
#define OZ3_TOOLS_PROGRAM_H_

#include <cstdint>
#include <vector>

namespace oz3 {

// A Program defines a complete set of Segments and corresponding bank
// assignments that make up a loadable program for a Processor.
//
// A program can be a complete executable program (CODE, STACK, DATA, and
// optionally EXTRA banks defined), or it may be a partial program or overlay
// with only a few (or none) of the banks defined.
//
// Programs may be defined to load into explicit memory banks and address
// offsets, or can have these dynamically assigned by the Processor.
//
// Programs also may define one or more instances which determine how a program
// is assigned to a CpuCore for processing. The number of instances determines
// the number of cores that may simultaneously execute any code in the program.
// However, programs may be data only (there are no instances defined).
struct Program {
  // A Segment specifies an address range within a Bank and any initial data
  // that should be set.
  struct Segment {
    // Access permissions for the Segment.
    enum class Access : uint16_t {
      kReadOnly,   // Segment is read-only. This is used for loading into ROM.
      kReadWrite,  // Segment is read-write. This is used for loading into RAM.
    };

    // Defines a block of initialized data within the Segment
    struct Data {
      // Offset from the beginning of the Segment where the data starts. This
      // must be less than the Segment size.
      uint16_t offset = 0;

      // Data values to be loaded into the Segment at the specified offset. The
      // offset + data.size() must be less than or equal to Segment size.
      std::vector<uint16_t> data;
    };

    // Bank index for the Segment. Must be in the range [0, banks.size()].
    uint16_t bank = 0;

    // Access permissions for the Segment.
    Access access = Access::kReadWrite;

    // Start page for the Segment within the bank (must be in the range [0,
    // kMemoryBankPageCount)). If this is set to 0xFFFF (-1), then the Segment
    // may be loaded into any address within the bank.
    uint16_t page_start = 0xFFFF;

    // Number of pages allocated to the Segment. Page start + page count must be
    // less than or equal to kMemoryBankPageCount.
    uint16_t page_count = 1;

    // Data blocks within the Segment.
    std::vector<Data> data;
  };

  // A Bank specifies a physical memory bank that one or more program segments
  // are assigned to.
  struct Bank {
    // Explicit mask of physical memory banks the Bank may be assigned to. By
    // default, any bank may be used.
    uint16_t assign_mask = 0xFFFF;
  };

  // An instance defines offsets relative to the segment start (for CODE, DATA,
  // and EXTRA) or segment end (for STACK) for the corresponding base address
  // registers (BC, BS, BD, and BE) of a CpuCore. Since the stack starts at the
  // end, the offset should be negative (in two's compliement) if overriden.
  struct Instance {
    uint16_t code = 0;
    uint16_t stack = 0;
    uint16_t data = 0;
    uint16_t extra = 0;
  };

  // List of Banks for the program.
  //
  // There can be at most kMaxMemoryBanks banks defined for a program. They are
  // allocated greedily by the program loader from first to last, so banks with
  // more restrictive assignment masks should be defined first.
  std::vector<Bank> banks;

  // List of Segments for the program.
  //
  // There can be at most kMaxMemoryBanks * kMemoryBankPageCount segments
  // defined for a program. No more than kMemoryBankPageCount pages are allowed
  // to be defined within a single bank, and all explicit page ranges
  // (`page_start` is not 0xFFFF) within a bank cannot overlap.
  //
  // Segments with explicit page ranges are allocated first, followed by
  // relocatable pages. Relocatable pages are allocated greedily by the program
  // loader, so segments with larger page counts generally should be defined
  // first. Within each set of fixed or dynamic pages, read-write pages are
  // allocated before read-only (as read-only pages can be allocated to either
  // read-write or read-only memory).
  std::vector<Segment> segments;

  // Index into the Segment array in a Program specifying the Segment assigned
  // to the bank. If this is set to 0xFFFF (-1), then no bank assignment is
  // made.
  uint16_t code_segment = 0xFFFF;
  uint16_t stack_segment = 0xFFFF;
  uint16_t data_segment = 0xFFFF;
  uint16_t extra_segment = 0xFFFF;

  // Default base address offsets for a program instance. If multiple cores will
  // execute the same program, there must be enough instances defined for the
  // maximum simultaneous executions (up to kMaxCores).
  std::vector<Instance> instances;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_PROGRAM_H_
