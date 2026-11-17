// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_DISK_TRANSFER_H_
#define OZ3_TOOLS_DISK_TRANSFER_H_

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "absl/types/span.h"

namespace oz3 {

//==============================================================================
// Host transfer
//
// Host bytes and text to and from the words of an OZ-3 file. A word is two
// bytes, low byte first, as in .oz3 files and as strings are packed, so text
// never needs reordering.
//==============================================================================

// Returns `bytes` as words, two bytes each, low byte first, with an odd last
// byte padded with a zero high byte.
std::vector<uint16_t> WordsFromBytes(absl::Span<const std::byte> bytes);

// Returns `words` as bytes, two each, low byte first.
std::vector<std::byte> BytesFromWords(absl::Span<const uint16_t> words);

// Returns host text as words, as WordsFromBytes does, after turning each CRLF
// and lone CR into LF.
std::vector<uint16_t> WordsFromText(std::string_view text);

// Returns `words` as host text, as BytesFromWords does, dropping every zero
// byte, such as the padding at the end of an odd length. Line endings stay LF.
std::string TextFromWords(absl::Span<const uint16_t> words);

}  // namespace oz3

#endif  // OZ3_TOOLS_DISK_TRANSFER_H_
