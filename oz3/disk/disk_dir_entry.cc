// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_dir_entry.h"

#include <algorithm>

#include "absl/log/check.h"

namespace oz3 {

DiskDirEntry DiskDirEntry::FromWords(absl::Span<const uint16_t> words) {
  CHECK_EQ(static_cast<int>(words.size()), kWordCount);
  Words entry_words;
  std::copy(words.begin(), words.end(), entry_words.begin());
  return DiskDirEntry(entry_words);
}

std::optional<DiskFilename> DiskDirEntry::GetName() const {
  return DiskFilename::FromWords(
      absl::MakeConstSpan(words_).subspan(kNameWord, DiskFilename::kWordCount));
}

void DiskDirEntry::SetName(const DiskFilename& name) {
  CHECK(!name.IsPattern());
  const DiskFilename::Words name_words = name.GetWords();
  std::copy(name_words.begin(), name_words.end(), words_.begin() + kNameWord);
}

void DiskDirEntry::SetFirstCluster(int cluster) {
  CHECK_GE(cluster, 0);
  CHECK_LE(cluster, kMaxFirstCluster);
  words_[kFirstClusterWord] = static_cast<uint16_t>(cluster);
}

int64_t DiskDirEntry::GetSize() const {
  return static_cast<int64_t>(words_[kSizeWord]) |
         (static_cast<int64_t>(words_[kSizeWord + 1]) << 16);
}

void DiskDirEntry::SetSize(int64_t size) {
  CHECK_GE(size, 0);
  CHECK_LE(size, kMaxSize);
  words_[kSizeWord] = static_cast<uint16_t>(size);
  words_[kSizeWord + 1] = static_cast<uint16_t>(size >> 16);
}

DateTime DiskDirEntry::GetModified() const {
  DateTime::Words modified;
  std::copy_n(words_.begin() + kModifiedWord, modified.size(),
              modified.begin());
  return DateTime::FromWords(modified);
}

void DiskDirEntry::SetModified(const DateTime& modified) {
  const DateTime::Words& modified_words = modified.GetWords();
  std::copy(modified_words.begin(), modified_words.end(),
            words_.begin() + kModifiedWord);
}

}  // namespace oz3
