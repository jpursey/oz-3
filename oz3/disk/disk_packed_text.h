// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_PACKED_TEXT_H_
#define OZ3_DISK_DISK_PACKED_TEXT_H_

#include <cstdint>
#include <string_view>

#include "absl/types/span.h"

namespace oz3 {
namespace internal {

// The rules names and labels share for their fields of packed text: characters
// in reading order, packed into words low byte first, from the name characters,
// with letters upper-cased, and zero-padded with no zero before a
// character. DiskFilename and DiskLabel use these, and nothing else should.

// Returns whether `c` is a name character, once upper-cased: a letter, a
// digit, or ! # $ % & ' ( ) - @ ^ _ ` { } ~.
bool IsNameCharacter(char c);

// Checks a field in place, upper-casing its letters, and for a `pattern`,
// allowing ? and expanding * into ?s to the end of the field. Returns whether
// it keeps the rules. It may be empty, all padding.
bool CheckPackedText(absl::Span<char> field, bool pattern);

// Copies `text` into `field`, which is zero already. Returns false if it
// doesn't fit, or holds a zero, which would read as padding.
bool CopyPackedText(std::string_view text, absl::Span<char> field);

// Returns the text of a field, up to its first zero.
std::string_view GetPackedText(absl::Span<const char> field);

// Unpacks `words` into `text`, low byte first. `text` must be twice as long.
void UnpackWords(absl::Span<const uint16_t> words, absl::Span<char> text);

// Packs `text` into `words`, low byte first. `text` must be twice as long.
void PackWords(absl::Span<const char> text, absl::Span<uint16_t> words);

}  // namespace internal
}  // namespace oz3

#endif  // OZ3_DISK_DISK_PACKED_TEXT_H_
