// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_label.h"

#include <optional>
#include <string>
#include <string_view>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;

TEST(DiskLabelTest, DefaultIsEmpty) {
  const DiskLabel label;
  EXPECT_EQ(label.ToString(), "");
  EXPECT_THAT(label.GetWords(), ElementsAre(0, 0, 0, 0, 0, 0, 0, 0));
}

TEST(DiskLabelTest, ParsesLimits) {
  const std::optional<DiskLabel> empty = DiskLabel::Parse("");
  ASSERT_TRUE(empty.has_value());
  EXPECT_EQ(*empty, DiskLabel());

  const std::optional<DiskLabel> full = DiskLabel::Parse("abcdefghijklmnop");
  ASSERT_TRUE(full.has_value());
  EXPECT_EQ(full->ToString(), "ABCDEFGHIJKLMNOP");
  EXPECT_THAT(full->GetWords(),
              ElementsAre('A' | 'B' << 8, 'C' | 'D' << 8, 'E' | 'F' << 8,
                          'G' | 'H' << 8, 'I' | 'J' << 8, 'K' | 'L' << 8,
                          'M' | 'N' << 8, 'O' | 'P' << 8));

  EXPECT_FALSE(DiskLabel::Parse("ABCDEFGHIJKLMNOPQ").has_value());
}

TEST(DiskLabelTest, RefusesDotsAndPatterns) {
  EXPECT_FALSE(DiskLabel::Parse("WORK.DISK").has_value());
  EXPECT_FALSE(DiskLabel::Parse("WORK*").has_value());
  EXPECT_FALSE(DiskLabel::Parse("WORK?").has_value());
  EXPECT_FALSE(DiskLabel::Parse("MY WORK").has_value());
  EXPECT_FALSE(DiskLabel::Parse(std::string_view("A\0", 2)).has_value());
  EXPECT_TRUE(DiskLabel::Parse("WORK_DISK-1").has_value());
}

TEST(DiskLabelTest, FromWords) {
  const std::optional<DiskLabel> label =
      DiskLabel::FromWords(DiskLabel::Words{'w' | 'o' << 8, 'r' | 'k' << 8});
  ASSERT_TRUE(label.has_value());
  EXPECT_EQ(label->ToString(), "WORK");
  EXPECT_EQ(*label, *DiskLabel::Parse("WORK"));

  // A zero before a character.
  EXPECT_FALSE(DiskLabel::FromWords(DiskLabel::Words{'A', 'B'}).has_value());
}

TEST(DiskLabelTest, CharactersAreTheNameCharacters) {
  const std::string allowed =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&'()-@^_`{}~";
  for (int i = 1; i < 256; ++i) {
    const char c = static_cast<char>(i);
    const std::string text = "A" + std::string(1, c);
    const std::optional<DiskLabel> label = DiskLabel::Parse(text);
    if (allowed.find(c) != std::string::npos) {
      ASSERT_TRUE(label.has_value()) << "Character " << i;
      EXPECT_EQ(label->ToString(), text);
    } else if (c >= 'a' && c <= 'z') {
      ASSERT_TRUE(label.has_value()) << "Character " << i;
      EXPECT_EQ(label->ToString(), "A" + std::string(1, c - 'a' + 'A'));
    } else {
      EXPECT_FALSE(label.has_value()) << "Character " << i;
    }
  }
}

}  // namespace
}  // namespace oz3
