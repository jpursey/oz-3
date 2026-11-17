// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_testing.h"

#include <cstdint>
#include <optional>
#include <vector>

#include "absl/log/check.h"

namespace oz3 {

namespace {

// Where an image's sectors are in its file, after the header.
constexpr int64_t kHeaderBytes = 16;
constexpr int64_t kSectorBytes = kDiskSectorWords * 2;

}  // namespace

std::vector<int> CollectSectorWrites(FaultyFileProtocol* protocol) {
  std::vector<int> sectors;
  for (int64_t offset : protocol->CollectWriteOffsets()) {
    sectors.push_back(
        offset < kHeaderBytes
            ? -1
            : static_cast<int>((offset - kHeaderBytes) / kSectorBytes));
  }
  return sectors;
}

FatChain FollowFatChain(const DiskLayout& layout,
                        absl::Span<const uint16_t> fat, int first) {
  CHECK_GT(static_cast<int>(fat.size()), layout.cluster_count);
  FatChain chain;
  for (std::optional<int> cluster = first; cluster != 0;
       cluster = layout.GetNextCluster(*cluster, fat[*cluster])) {
    if (!cluster.has_value() || *cluster > layout.cluster_count) {
      chain.broken = true;
      break;
    }
    if (static_cast<int>(chain.clusters.size()) == layout.cluster_count) {
      break;
    }
    chain.clusters.push_back(*cluster);
  }
  return chain;
}

}  // namespace oz3
