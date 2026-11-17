// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/disk_transfer.h"

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "absl/strings/str_replace.h"
#include "absl/types/span.h"

namespace oz3 {

std::vector<uint16_t> WordsFromBytes(absl::Span<const std::byte> bytes) {
  const int byte_count = static_cast<int>(bytes.size());
  std::vector<uint16_t> words((byte_count + 1) / 2, 0);
  for (int i = 0; i < byte_count; ++i) {
    words[i / 2] |= static_cast<uint16_t>(std::to_integer<uint16_t>(bytes[i])
                                          << (i % 2 * 8));
  }
  return words;
}

std::vector<std::byte> BytesFromWords(absl::Span<const uint16_t> words) {
  std::vector<std::byte> bytes;
  bytes.reserve(words.size() * 2);
  for (const uint16_t word : words) {
    bytes.push_back(static_cast<std::byte>(word & 0xFF));
    bytes.push_back(static_cast<std::byte>(word >> 8));
  }
  return bytes;
}

std::vector<uint16_t> WordsFromText(std::string_view text) {
  const std::string lf_text =
      absl::StrReplaceAll(text, {{"\r\n", "\n"}, {"\r", "\n"}});
  return WordsFromBytes(absl::MakeConstSpan(
      reinterpret_cast<const std::byte*>(lf_text.data()), lf_text.size()));
}

std::string TextFromWords(absl::Span<const uint16_t> words) {
  std::string text;
  text.reserve(words.size() * 2);
  for (const uint16_t word : words) {
    const char low = static_cast<char>(word & 0xFF);
    const char high = static_cast<char>(word >> 8);
    if (low != '\0') {
      text.push_back(low);
    }
    if (high != '\0') {
      text.push_back(high);
    }
  }
  return text;
}

}  // namespace oz3
