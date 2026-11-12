// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_filename.h"

#include <optional>
#include <string>
#include <string_view>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;

// Returns the name `text` parses to, which must parse.
DiskFilename Name(std::string_view text) {
  std::optional<DiskFilename> name = DiskFilename::Parse(text);
  EXPECT_TRUE(name.has_value()) << text;
  return name.value_or(*DiskFilename::Parse("BAD"));
}

// Returns the pattern `text` parses to, which must parse.
DiskFilename Pattern(std::string_view text) {
  std::optional<DiskFilename> pattern = DiskFilename::ParsePattern(text);
  EXPECT_TRUE(pattern.has_value()) << text;
  return pattern.value_or(*DiskFilename::Parse("BAD"));
}

// Returns the dotted text `text` parses to, or "<none>".
std::string Parsed(std::string_view text) {
  std::optional<DiskFilename> name = DiskFilename::Parse(text);
  return name.has_value() ? name->ToString() : "<none>";
}

//------------------------------------------------------------------------------
// DiskFilename
//------------------------------------------------------------------------------

TEST(DiskFilenameTest, ParsesNameAndExtension) {
  const DiskFilename name = Name("HELLO.TXT");
  EXPECT_EQ(name.ToString(), "HELLO.TXT");
  EXPECT_FALSE(name.IsPattern());
  EXPECT_THAT(name.GetWords(), ElementsAre('H' | 'E' << 8, 'L' | 'L' << 8, 'O',
                                           0, 0, 0, 'T' | 'X' << 8, 'T'));
}

TEST(DiskFilenameTest, ParsesLimits) {
  EXPECT_EQ(Parsed("ABCDEFGHIJKL.MNOP"), "ABCDEFGHIJKL.MNOP");
  EXPECT_EQ(Parsed("ABCDEFGHIJKLM.TXT"), "<none>");
  EXPECT_EQ(Parsed("A.MNOPQ"), "<none>");
  EXPECT_EQ(Parsed("A"), "A");
  EXPECT_EQ(Parsed(""), "<none>");
  EXPECT_EQ(Parsed(".TXT"), "<none>");
}

TEST(DiskFilenameTest, EmptyExtension) {
  EXPECT_EQ(Parsed("HELLO"), "HELLO");
  EXPECT_EQ(Parsed("HELLO."), "HELLO");
  EXPECT_EQ(Name("HELLO."), Name("HELLO"));
  EXPECT_THAT(Name("HELLO").GetWords(),
              ElementsAre('H' | 'E' << 8, 'L' | 'L' << 8, 'O', 0, 0, 0, 0, 0));
}

TEST(DiskFilenameTest, SecondDotIsRefused) {
  EXPECT_EQ(Parsed("A.B.C"), "<none>");
  EXPECT_EQ(Parsed("A..B"), "<none>");
  EXPECT_EQ(Parsed("A.."), "<none>");
}

TEST(DiskFilenameTest, AllowedCharacters) {
  const std::string allowed =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&'()-@^_`{}~";
  for (char c : allowed) {
    const std::string text(1, c);
    EXPECT_EQ(Parsed(text), text) << "Character " << c;
    EXPECT_EQ(Parsed("A." + text), "A." + text) << "Character " << c;
  }
}

TEST(DiskFilenameTest, RefusedCharacters) {
  const std::string allowed =
      "abcdefghijklmnopqrstuvwxyz"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&'()-@^_`{}~.";
  for (int i = 0; i < 256; ++i) {
    const char c = static_cast<char>(i);
    if (allowed.find(c) != std::string::npos) {
      continue;
    }
    const std::string text(1, c);
    EXPECT_EQ(Parsed(text), "<none>") << "Character " << i;
    EXPECT_EQ(Parsed("A" + text), "<none>") << "Character " << i;
    EXPECT_EQ(Parsed("A." + text), "<none>") << "Character " << i;
    EXPECT_EQ(Parsed("A.B" + text), "<none>") << "Character " << i;
  }
}

TEST(DiskFilenameTest, PatternCharactersAreRefusedInNames) {
  EXPECT_EQ(Parsed("*.TXT"), "<none>");
  EXPECT_EQ(Parsed("A?"), "<none>");
  EXPECT_EQ(Parsed("A.T?T"), "<none>");
}

TEST(DiskFilenameTest, UpperCasesLetters) {
  EXPECT_EQ(Parsed("hello.txt"), "HELLO.TXT");
  EXPECT_EQ(Parsed("HeLLo.tXt"), "HELLO.TXT");
  EXPECT_EQ(Name("hello.txt"), Name("HELLO.TXT"));
}

TEST(DiskFilenameTest, FromWordsRoundTrips) {
  const DiskFilename name = Name("README.MD");
  const std::optional<DiskFilename> from_words =
      DiskFilename::FromWords(name.GetWords());
  ASSERT_TRUE(from_words.has_value());
  EXPECT_EQ(*from_words, name);
  EXPECT_EQ(from_words->ToString(), "README.MD");
}

TEST(DiskFilenameTest, FromWordsUpperCasesLetters) {
  const DiskFilename::Words words = {'h' | 'i' << 8, 0, 0, 0, 0, 0, 't', 0};
  const std::optional<DiskFilename> name = DiskFilename::FromWords(words);
  ASSERT_TRUE(name.has_value());
  EXPECT_EQ(name->ToString(), "HI.T");
  EXPECT_THAT(name->GetWords(),
              ElementsAre('H' | 'I' << 8, 0, 0, 0, 0, 0, 'T', 0));
}

TEST(DiskFilenameTest, FromWordsRefusesZeroBeforeCharacter) {
  // "A", a zero, then "B", in the name.
  EXPECT_FALSE(
      DiskFilename::FromWords(DiskFilename::Words{'A', 'B', 0, 0, 0, 0, 0, 0})
          .has_value());
  // A zero word in the middle of the name.
  EXPECT_FALSE(DiskFilename::FromWords(
                   DiskFilename::Words{'A' | 'B' << 8, 0, 'C', 0, 0, 0, 0, 0})
                   .has_value());
  // A zero first in the extension.
  EXPECT_FALSE(DiskFilename::FromWords(
                   DiskFilename::Words{'A', 0, 0, 0, 0, 0, 'T' << 8, 0})
                   .has_value());
  // The extension's padding is separate from the name's.
  EXPECT_TRUE(
      DiskFilename::FromWords(DiskFilename::Words{'A', 0, 0, 0, 0, 0, 'T', 0})
          .has_value());
}

TEST(DiskFilenameTest, FromWordsRefusesBadNames) {
  // Empty, as a free directory entry is.
  EXPECT_FALSE(DiskFilename::FromWords(DiskFilename::Words{}).has_value());
  // An extension with no name.
  EXPECT_FALSE(
      DiskFilename::FromWords(DiskFilename::Words{0, 0, 0, 0, 0, 0, 'T', 0})
          .has_value());
  // A dot, a pattern character, and a character of 80h or more.
  EXPECT_FALSE(DiskFilename::FromWords(
                   DiskFilename::Words{'A' | '.' << 8, 0, 0, 0, 0, 0, 0, 0})
                   .has_value());
  EXPECT_FALSE(DiskFilename::FromWords(
                   DiskFilename::Words{'A' | '?' << 8, 0, 0, 0, 0, 0, 0, 0})
                   .has_value());
  EXPECT_FALSE(DiskFilename::FromWords(
                   DiskFilename::Words{'A' | 0xC1 << 8, 0, 0, 0, 0, 0, 0, 0})
                   .has_value());
}

TEST(DiskFilenameTest, OrdersInReadingOrder) {
  // Each pair differs first in a word's low byte, which the high byte
  // ordering the other way would contradict.
  EXPECT_LT(Name("AZ"), Name("BA"));
  EXPECT_LT(Name("BA"), Name("BB"));
  EXPECT_LT(Name("A"), Name("AA"));
  EXPECT_LT(Name("ABCDEFGHIJKA"), Name("ABCDEFGHIJKB"));
  // By name first, then by extension.
  EXPECT_LT(Name("A.Z"), Name("B.A"));
  EXPECT_LT(Name("A"), Name("A.A"));
  EXPECT_LT(Name("A.AZ"), Name("A.BA"));
  EXPECT_LT(Name("A.AAAA"), Name("A.AAAB"));
  EXPECT_EQ(Name("A.B"), Name("a.b"));
}

TEST(DiskFilenameTest, NameMatchesOnlyItself) {
  EXPECT_TRUE(Name("A.TXT").Matches(Name("A.TXT")));
  EXPECT_FALSE(Name("A.TXT").Matches(Name("A.TX")));
  EXPECT_FALSE(Name("A.TXT").Matches(Name("B.TXT")));
  EXPECT_FALSE(Name("A").Matches(Name("A.TXT")));
}

//------------------------------------------------------------------------------
// Patterns
//------------------------------------------------------------------------------

TEST(DiskFilenameTest, StarStarMatchesEverything) {
  const DiskFilename pattern = Pattern("*.*");
  EXPECT_TRUE(pattern.IsPattern());
  EXPECT_EQ(pattern.ToString(), "*.*");
  EXPECT_TRUE(pattern.Matches(Name("A")));
  EXPECT_TRUE(pattern.Matches(Name("A.B")));
  EXPECT_TRUE(pattern.Matches(Name("ABCDEFGHIJKL.MNOP")));
}

TEST(DiskFilenameTest, StarDotExtensionMatchesTheExtension) {
  const DiskFilename pattern = Pattern("*.TXT");
  EXPECT_EQ(pattern.ToString(), "*.TXT");
  EXPECT_TRUE(pattern.Matches(Name("A.TXT")));
  EXPECT_TRUE(pattern.Matches(Name("ABCDEFGHIJKL.TXT")));
  EXPECT_FALSE(pattern.Matches(Name("A")));
  EXPECT_FALSE(pattern.Matches(Name("A.TX")));
  EXPECT_FALSE(pattern.Matches(Name("A.TXTS")));
}

TEST(DiskFilenameTest, PrefixStarMatchesThePrefix) {
  const DiskFilename pattern = Pattern("A*.*");
  EXPECT_TRUE(pattern.Matches(Name("A")));
  EXPECT_TRUE(pattern.Matches(Name("ABC.D")));
  EXPECT_FALSE(pattern.Matches(Name("BA.D")));
}

TEST(DiskFilenameTest, StarAloneMatchesNoExtension) {
  const DiskFilename pattern = Pattern("*");
  EXPECT_EQ(pattern.ToString(), "*");
  EXPECT_TRUE(pattern.Matches(Name("A")));
  EXPECT_TRUE(pattern.Matches(Name("ABCDEFGHIJKL")));
  EXPECT_FALSE(pattern.Matches(Name("A.B")));
}

TEST(DiskFilenameTest, QuestionMatchesPadding) {
  const DiskFilename pattern = Pattern("A?.T?");
  EXPECT_TRUE(pattern.Matches(Name("A.T")));
  EXPECT_TRUE(pattern.Matches(Name("AB.TX")));
  EXPECT_FALSE(pattern.Matches(Name("ABC.T")));
  EXPECT_FALSE(pattern.Matches(Name("AB.TXT")));
}

TEST(DiskFilenameTest, CharacterAfterStarIsRefused) {
  EXPECT_FALSE(DiskFilename::ParsePattern("*A.TXT").has_value());
  EXPECT_FALSE(DiskFilename::ParsePattern("A*?").has_value());
  EXPECT_FALSE(DiskFilename::ParsePattern("A**").has_value());
  EXPECT_FALSE(DiskFilename::ParsePattern("A.*T").has_value());
  EXPECT_TRUE(DiskFilename::ParsePattern("*.T*").has_value());
}

TEST(DiskFilenameTest, PatternsKeepTheNameRules) {
  EXPECT_FALSE(DiskFilename::ParsePattern("").has_value());
  EXPECT_FALSE(DiskFilename::ParsePattern(".*").has_value());
  EXPECT_FALSE(DiskFilename::ParsePattern("*.*.*").has_value());
  EXPECT_FALSE(DiskFilename::ParsePattern("A B").has_value());
  EXPECT_FALSE(DiskFilename::ParsePattern("ABCDEFGHIJKL*").has_value());
  EXPECT_EQ(Pattern("a?.txt").ToString(), "A?.TXT");
}

TEST(DiskFilenameTest, PatternShowsTrailingQuestionsAsStar) {
  // ?s that stop short of the end of their field stay as they are.
  EXPECT_EQ(Pattern("A?.T?").ToString(), "A?.T?");
  EXPECT_EQ(Pattern("?A").ToString(), "?A");
  // ?s written out to the end of a field are the same pattern as a *.
  EXPECT_EQ(Pattern("A???????????.????").ToString(), "A*.*");
  EXPECT_EQ(Pattern("A???????????.????"), Pattern("A*.*"));
  EXPECT_EQ(Pattern("A?C*.T??*").ToString(), "A?C*.T*");
  EXPECT_EQ(Pattern("ABCDEFGHIJK?").ToString(), "ABCDEFGHIJK*");
  // A full field ending in a character has no *.
  EXPECT_EQ(Pattern("???????????A.???B").ToString(), "???????????A.???B");
}

TEST(DiskFilenameTest, PatternTextParsesBackToItself) {
  for (std::string_view text : {"*.*", "*.TXT", "A*.*", "*", "A?.T?", "A?C*.T*",
                                "???????????A.???B", "ABCDEFGHIJK*.MNO*"}) {
    const DiskFilename pattern = Pattern(text);
    EXPECT_EQ(Pattern(pattern.ToString()), pattern) << text;
  }
}

TEST(DiskFilenameTest, PatternWithoutWildcardsIsAName) {
  const DiskFilename pattern = Pattern("A.TXT");
  EXPECT_FALSE(pattern.IsPattern());
  EXPECT_EQ(pattern, Name("A.TXT"));
}

TEST(DiskFilenameTest, PatternFromWordsExpandsStar) {
  const DiskFilename::Words words = {'A' | '*' << 8, 0, 0, 0, 0, 0, '*', 0};
  const std::optional<DiskFilename> pattern =
      DiskFilename::PatternFromWords(words);
  ASSERT_TRUE(pattern.has_value());
  EXPECT_EQ(*pattern, Pattern("A*.*"));
  EXPECT_FALSE(DiskFilename::FromWords(words).has_value());
}

TEST(DiskFilenameTest, PatternFromWordsRefusesCharacterAfterStar) {
  // A character in a later word than the *.
  EXPECT_FALSE(DiskFilename::PatternFromWords(
                   DiskFilename::Words{'*', 0, 'A', 0, 0, 0, 0, 0})
                   .has_value());
  // A zero before a character.
  EXPECT_FALSE(DiskFilename::PatternFromWords(
                   DiskFilename::Words{'?', '?', 0, 0, 0, 0, 0, 0})
                   .has_value());
}

}  // namespace
}  // namespace oz3
