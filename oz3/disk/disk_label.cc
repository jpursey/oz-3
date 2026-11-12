// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_label.h"

#include "oz3/disk/disk_packed_text.h"

namespace oz3 {

std::optional<DiskLabel> DiskLabel::Parse(std::string_view text) {
  Chars chars = {};
  if (!internal::CopyPackedText(text, absl::MakeSpan(chars))) {
    return std::nullopt;
  }
  return FromChars(chars);
}

std::optional<DiskLabel> DiskLabel::FromWords(
    absl::Span<const uint16_t> words) {
  Chars chars;
  internal::UnpackWords(words, absl::MakeSpan(chars));
  return FromChars(chars);
}

std::string DiskLabel::ToString() const {
  return std::string(internal::GetPackedText(chars_));
}

DiskLabel::Words DiskLabel::GetWords() const {
  Words words;
  internal::PackWords(chars_, absl::MakeSpan(words));
  return words;
}

std::optional<DiskLabel> DiskLabel::FromChars(Chars chars) {
  if (!internal::CheckPackedText(absl::MakeSpan(chars), /*pattern=*/false)) {
    return std::nullopt;
  }
  return DiskLabel(chars);
}

}  // namespace oz3
