// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_DIR_ENTRY_H_
#define OZ3_DISK_DISK_DIR_ENTRY_H_

#include <array>
#include <cstdint>
#include <optional>

#include "absl/types/span.h"
#include "oz3/core/date_time.h"
#include "oz3/disk/disk_filename.h"

namespace oz3 {

// A directory entry's attribute bits. The others are reserved.
inline constexpr uint16_t kDiskAttribReadOnly = 1 << 0;
inline constexpr uint16_t kDiskAttribHidden = 1 << 1;
inline constexpr uint16_t kDiskAttribDirectory = 1 << 2;  // Always 0 for now.

// The attribute bits ATTRIB sets. Any other is bad attributes.
inline constexpr uint16_t kDiskAttribSettable =
    kDiskAttribReadOnly | kDiskAttribHidden;

// A directory entry: its name (words 0-7), attributes (8), first cluster (9),
// size in words (10 and 11, the low word first), and modified time (12-15).
// An entry whose first word is 0 is free.
//
// It holds its words exactly, as the disk has them, so an entry read from a
// disk may break the rules. The getters say what it holds, and the setters
// change only their own words.
class DiskDirEntry final {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // The words of an entry, in order.
  using Words = std::array<uint16_t, 16>;

  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The words in a directory entry.
  static constexpr int kWordCount = static_cast<int>(Words().size());

  // The largest size, in words.
  static constexpr int64_t kMaxSize = 0xFFFF'FFFF;

  // The largest first cluster word.
  static constexpr int kMaxFirstCluster = 0xFFFF;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // A free entry, all zeros.
  DiskDirEntry() = default;

  // Returns `words` exactly as given. `words` must be kWordCount long.
  static DiskDirEntry FromWords(absl::Span<const uint16_t> words);

  //----------------------------------------------------------------------------
  // Operators
  //----------------------------------------------------------------------------

  bool operator==(const DiskDirEntry&) const = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns whether the entry is free: its first word is 0.
  bool IsFree() const { return words_[kNameWord] == 0; }

  // Returns the name, or nullopt if the entry is free or its name breaks the
  // name rules. Lower-case letters are upper-cased, as DiskFilename::FromWords
  // does.
  std::optional<DiskFilename> GetName() const;

  // Sets the name. `name` must not be a pattern.
  void SetName(const DiskFilename& name);

  // Returns or sets the attribute bits, all 16, reserved bits included.
  uint16_t GetAttributes() const { return words_[kAttributesWord]; }
  void SetAttributes(uint16_t attributes) {
    words_[kAttributesWord] = attributes;
  }

  // Returns or sets the first cluster, 0 for an empty file. `cluster` must be
  // 0 to kMaxFirstCluster.
  int GetFirstCluster() const { return words_[kFirstClusterWord]; }
  void SetFirstCluster(int cluster);

  // Returns or sets the size, in words. `size` must be 0 to kMaxSize.
  int64_t GetSize() const;
  void SetSize(int64_t size);

  // Returns or sets the modified time.
  DateTime GetModified() const;
  void SetModified(const DateTime& modified);

  // Returns the words.
  const Words& GetWords() const { return words_; }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // Where each field starts.
  static constexpr int kNameWord = 0;
  static constexpr int kAttributesWord = 8;
  static constexpr int kFirstClusterWord = 9;
  static constexpr int kSizeWord = 10;
  static constexpr int kModifiedWord = 12;

  explicit DiskDirEntry(const Words& words) : words_(words) {}

  Words words_ = {};
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_DIR_ENTRY_H_
