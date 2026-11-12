// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_LABEL_H_
#define OZ3_DISK_DISK_LABEL_H_

#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "absl/types/span.h"

namespace oz3 {

// A volume label: one field of 0-16 characters of packed text, the first
// character of a pair in the low byte, and zero-padded, with no zero
// before a character. Its characters are a name's (see DiskFilename), with
// letters upper-cased as a label is made. It has no extension, no dot, and no
// patterns.
class DiskLabel final {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // The words of a label, in order.
  using Words = std::array<uint16_t, 8>;

  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The words in a label field.
  static constexpr int kWordCount = static_cast<int>(Words().size());

  // The most characters in a label.
  static constexpr int kMaxLength = kWordCount * 2;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // The empty label.
  DiskLabel() = default;

  // Parses `text` ("work"), returning nullopt for anything that breaks the
  // label rules.
  static std::optional<DiskLabel> Parse(std::string_view text);

  // Returns the label in `words`, as a command argument or superblock holds
  // it, or nullopt if it breaks the label rules. `words` must be
  // kWordCount long.
  static std::optional<DiskLabel> FromWords(absl::Span<const uint16_t> words);

  //----------------------------------------------------------------------------
  // Operators
  //----------------------------------------------------------------------------

  bool operator==(const DiskLabel&) const = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the text, "WORK", or "" for the empty label.
  std::string ToString() const;

  // Returns the words.
  Words GetWords() const;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The field's characters, in reading order.
  using Chars = std::array<char, kMaxLength>;

  explicit DiskLabel(const Chars& chars) : chars_(chars) {}

  // Returns the label in `chars`, or nullopt.
  static std::optional<DiskLabel> FromChars(Chars chars);

  Chars chars_ = {};
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_LABEL_H_
