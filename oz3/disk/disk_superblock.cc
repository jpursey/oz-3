// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_superblock.h"

#include <algorithm>
#include <iterator>

#include "absl/log/check.h"
#include "absl/types/span.h"

namespace oz3 {

namespace {

// "OZFS", packed.
constexpr uint16_t kFormatId[] = {'O' | 'Z' << 8, 'F' | 'S' << 8};
constexpr uint16_t kVersion = 1;

// Returns `value` as a word, which it must fit.
uint16_t ToWord(int value) {
  CHECK_GE(value, 0);
  CHECK_LE(value, 0xFFFF);
  return static_cast<uint16_t>(value);
}

}  // namespace

DiskSuperblock::DiskSuperblock(const DiskLayout& layout, int root_cluster,
                               const DiskLabel& label, DateTime formatted) {
  std::copy(std::begin(kFormatId), std::end(kFormatId),
            words_.begin() + kFormatIdWord);
  words_[kVersionWord] = kVersion;
  words_[kSectorsPerClusterWord] = ToWord(layout.sectors_per_cluster);
  words_[kClusterCountWord] = ToWord(layout.cluster_count);
  words_[kFatSectorsWord] = ToWord(layout.fat_sectors);
  words_[kRootClusterWord] = ToWord(root_cluster);
  SetLabel(label);
  const DateTime::Words& formatted_words = formatted.GetWords();
  std::copy(formatted_words.begin(), formatted_words.end(),
            words_.begin() + kFormattedWord);
}

bool DiskSuperblock::IsValid(int sector_count) const {
  if (!std::equal(std::begin(kFormatId), std::end(kFormatId),
                  words_.begin() + kFormatIdWord) ||
      words_[kVersionWord] != kVersion) {
    return false;
  }
  const DiskLayout layout = GetLayout();
  const int root_cluster = GetRootCluster();
  return layout.IsValid(sector_count) && root_cluster >= 1 &&
         root_cluster <= layout.cluster_count && GetLabel().has_value();
}

std::optional<DiskLabel> DiskSuperblock::GetLabel() const {
  return DiskLabel::FromWords(
      absl::MakeConstSpan(words_).subspan(kLabelWord, DiskLabel::kWordCount));
}

void DiskSuperblock::SetLabel(const DiskLabel& label) {
  const DiskLabel::Words label_words = label.GetWords();
  std::copy(label_words.begin(), label_words.end(),
            words_.begin() + kLabelWord);
}

}  // namespace oz3
