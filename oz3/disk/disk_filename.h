// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_FILENAME_H_
#define OZ3_DISK_DISK_FILENAME_H_

#include <array>
#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "absl/types/span.h"

namespace oz3 {

// A file's name: the 8-word field directory entries and commands hold, of a
// 1-12 character name (words 0-5) and a 0-4 character extension (words 6-7).
// Each is packed text, the first character of a pair in the low byte, and
// zero-padded, with no zero before a character. No dot is stored.
//
// Characters are letters, digits, and ! # $ % & ' ( ) - @ ^ _ ` { } ~. Letters
// are upper-cased as a name is made, so names are matched without regard to
// case.
//
// A pattern is a name that may also hold ? and *. A * fills the rest of its
// field with ?, as the pattern is made, and nothing may follow it in the field.
// A ? matches any one character, padding included. A name without ? or * is
// never a pattern, and a pattern is only made by the Pattern functions, so one
// can't be mistaken for the other.
class DiskFilename final {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // The words of a name, in order.
  using Words = std::array<uint16_t, 8>;

  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // The words in a name or pattern field.
  static constexpr int kWordCount = static_cast<int>(Words().size());

  // The most characters in the name and the extension.
  static constexpr int kMaxNameLength = 12;
  static constexpr int kMaxExtensionLength = 4;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Parses dotted text ("hello.txt"), or text with no dot and no extension
  // ("hello", or "hello."). Returns nullopt for anything that breaks the name
  // rules, ? and * included.
  static std::optional<DiskFilename> Parse(std::string_view text);

  // Parses a dotted pattern ("*.TXT"), as Parse does a name, expanding each *.
  static std::optional<DiskFilename> ParsePattern(std::string_view text);

  // Returns the name in `words`, as a command argument or directory entry
  // holds it, or nullopt if it breaks the name rules. `words` must be
  // kWordCount long.
  static std::optional<DiskFilename> FromWords(
      absl::Span<const uint16_t> words);

  // Returns the pattern in `words`, as FromWords does a name, expanding each *.
  static std::optional<DiskFilename> PatternFromWords(
      absl::Span<const uint16_t> words);

  //----------------------------------------------------------------------------
  // Operators
  //----------------------------------------------------------------------------

  // Names order by their 16 characters in reading order, which is by name and
  // then extension, as code sorts them with CMPT.
  auto operator<=>(const DiskFilename&) const = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns whether this is a pattern: it holds a ?.
  bool IsPattern() const;

  // Returns whether `name` matches this: each character is the same as this
  // one's, or this one's is ?. A name matches only itself.
  bool Matches(const DiskFilename& name) const;

  // Returns the dotted text: "HELLO.TXT", or "HELLO" with no extension. A
  // pattern shows ?s that run to the end of a field as a *, so it parses back
  // to itself: "*.TXT" is "*.TXT", and "A?.T?" is "A?.T?".
  std::string ToString() const;

  // Returns the words.
  Words GetWords() const;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The field's characters, in reading order. Each is below 80h, as a name
  // character, ?, or a zero, so a signed char orders them as unsigned values
  // do.
  using Chars = std::array<char, kWordCount * 2>;

  explicit DiskFilename(const Chars& chars) : chars_(chars) {}

  // Returns the name or pattern in `chars`, or nullopt.
  static std::optional<DiskFilename> FromChars(Chars chars, bool pattern);

  // Returns the name or pattern in dotted `text`, or nullopt.
  static std::optional<DiskFilename> FromText(std::string_view text,
                                              bool pattern);

  Chars chars_;
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_FILENAME_H_
