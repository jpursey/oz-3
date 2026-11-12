// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_packed_text.h"

#include <algorithm>

#include "absl/log/check.h"

namespace oz3 {
namespace internal {

namespace {

// The punctuation allowed in names and labels, besides letters and digits.
constexpr std::string_view kPunctuation = "!#$%&'()-@^_`{}~";

}  // namespace

bool IsNameCharacter(char c) {
  return (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
         kPunctuation.find(c) != std::string_view::npos;
}

bool CheckPackedText(absl::Span<char> field, bool pattern) {
  bool padding = false;
  bool star = false;
  for (char& c : field) {
    if (star) {
      if (c != 0) {
        return false;
      }
      c = '?';
    } else if (c == 0) {
      padding = true;
    } else if (padding) {
      return false;
    } else if (pattern && c == '*') {
      star = true;
      c = '?';
    } else if (pattern && c == '?') {
      // Kept as it is.
    } else {
      if (c >= 'a' && c <= 'z') {
        c = static_cast<char>(c - 'a' + 'A');
      }
      if (!IsNameCharacter(c)) {
        return false;
      }
    }
  }
  return true;
}

bool CopyPackedText(std::string_view text, absl::Span<char> field) {
  if (text.size() > field.size() || text.find('\0') != std::string_view::npos) {
    return false;
  }
  std::copy(text.begin(), text.end(), field.begin());
  return true;
}

std::string_view GetPackedText(absl::Span<const char> field) {
  const std::string_view text(field.data(), field.size());
  return text.substr(0, text.find('\0'));
}

void UnpackWords(absl::Span<const uint16_t> words, absl::Span<char> text) {
  CHECK_EQ(text.size(), words.size() * 2);
  for (int i = 0; i < static_cast<int>(words.size()); ++i) {
    text[i * 2] = static_cast<char>(words[i] & 0xFF);
    text[i * 2 + 1] = static_cast<char>(words[i] >> 8);
  }
}

void PackWords(absl::Span<const char> text, absl::Span<uint16_t> words) {
  CHECK_EQ(text.size(), words.size() * 2);
  for (int i = 0; i < static_cast<int>(words.size()); ++i) {
    words[i] =
        static_cast<uint16_t>(static_cast<uint8_t>(text[i * 2]) |
                              (static_cast<uint8_t>(text[i * 2 + 1]) << 8));
  }
}

}  // namespace internal
}  // namespace oz3
