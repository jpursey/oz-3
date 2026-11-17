// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_TESTING_H_
#define OZ3_DISK_DISK_TESTING_H_

#include <cstdint>
#include <vector>

#include "absl/types/span.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/faulty_file_protocol.h"

namespace oz3 {

//==============================================================================
// For tests: what a disk image holds and what is written to it, worked out
// without the disk library, so tests can check what it does.
//==============================================================================

// Returns the sectors written to disk images through `protocol` since the
// last call, in order, with -1 for a write to an image's header. It takes
// every file of `protocol` to be an image, and forgets the writes, as
// FaultyFileProtocol::CollectWriteOffsets does.
std::vector<int> CollectSectorWrites(FaultyFileProtocol* protocol);

// A chain FollowFatChain follows.
struct FatChain {
  std::vector<int> clusters;  // In order.
  bool broken = false;        // It ends at a link past the last cluster.
};

// Returns the chain from `first` in `fat`, the FAT of a disk of `layout`:
// none if `first` is 0, and otherwise each cluster to the chain's end, so a
// long chain's tail is part of it. It stops at a link past the last cluster,
// or a `first` past it, and after as many clusters as the disk has, so a loop
// repeats clusters. `fat` must have a word for every cluster.
FatChain FollowFatChain(const DiskLayout& layout,
                        absl::Span<const uint16_t> fat, int first);

}  // namespace oz3

#endif  // OZ3_DISK_DISK_TESTING_H_
