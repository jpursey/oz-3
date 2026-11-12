// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_filename.h"

#include <algorithm>

#include "oz3/disk/disk_packed_text.h"

namespace oz3 {

namespace {

// Returns the text of a pattern's field, with ?s that run to the end of the
// field shown as the * they could have been written as.
std::string GetPatternText(absl::Span<const char> field) {
  std::string text(internal::GetPackedText(field));
  if (text.size() == field.size() && text.back() == '?') {
    text.erase(text.find_last_not_of('?') + 1);
    text += '*';
  }
  return text;
}

}  // namespace

std::optional<DiskFilename> DiskFilename::Parse(std::string_view text) {
  return FromText(text, /*pattern=*/false);
}

std::optional<DiskFilename> DiskFilename::ParsePattern(std::string_view text) {
  return FromText(text, /*pattern=*/true);
}

std::optional<DiskFilename> DiskFilename::FromWords(
    absl::Span<const uint16_t> words) {
  Chars chars;
  internal::UnpackWords(words, absl::MakeSpan(chars));
  return FromChars(chars, /*pattern=*/false);
}

std::optional<DiskFilename> DiskFilename::PatternFromWords(
    absl::Span<const uint16_t> words) {
  Chars chars;
  internal::UnpackWords(words, absl::MakeSpan(chars));
  return FromChars(chars, /*pattern=*/true);
}

bool DiskFilename::IsPattern() const {
  return std::find(chars_.begin(), chars_.end(), '?') != chars_.end();
}

bool DiskFilename::Matches(const DiskFilename& name) const {
  for (int i = 0; i < static_cast<int>(chars_.size()); ++i) {
    if (chars_[i] != '?' && chars_[i] != name.chars_[i]) {
      return false;
    }
  }
  return true;
}

std::string DiskFilename::ToString() const {
  const absl::Span<const char> chars(chars_);
  std::string text = GetPatternText(chars.subspan(0, kMaxNameLength));
  const std::string extension = GetPatternText(chars.subspan(kMaxNameLength));
  if (!extension.empty()) {
    text += '.';
    text += extension;
  }
  return text;
}

DiskFilename::Words DiskFilename::GetWords() const {
  Words words;
  internal::PackWords(chars_, absl::MakeSpan(words));
  return words;
}

std::optional<DiskFilename> DiskFilename::FromChars(Chars chars, bool pattern) {
  const absl::Span<char> span(chars);
  if (!internal::CheckPackedText(span.subspan(0, kMaxNameLength), pattern) ||
      !internal::CheckPackedText(span.subspan(kMaxNameLength), pattern) ||
      chars[0] == 0) {
    return std::nullopt;
  }
  return DiskFilename(chars);
}

std::optional<DiskFilename> DiskFilename::FromText(std::string_view text,
                                                   bool pattern) {
  std::string_view extension;
  if (const auto dot = text.find('.'); dot != std::string_view::npos) {
    extension = text.substr(dot + 1);
    text = text.substr(0, dot);
  }
  Chars chars = {};
  const absl::Span<char> span(chars);
  if (!internal::CopyPackedText(text, span.subspan(0, kMaxNameLength)) ||
      !internal::CopyPackedText(extension, span.subspan(kMaxNameLength))) {
    return std::nullopt;
  }
  return FromChars(chars, pattern);
}

}  // namespace oz3
