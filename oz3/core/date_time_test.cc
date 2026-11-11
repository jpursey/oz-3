// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/date_time.h"

#include <optional>

#include "absl/time/civil_time.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::Optional;

TEST(DateTimeTest, DefaultIsNoDate) {
  const DateTime date_time;
  EXPECT_FALSE(date_time.IsSet());
  EXPECT_EQ(date_time.ToCivil(), std::nullopt);
  EXPECT_THAT(date_time.GetWords(), ElementsAre(0, 0, 0, 0));
}

TEST(DateTimeTest, AllZerosIsNoDate) {
  const DateTime date_time = DateTime::FromWords({0, 0, 0, 0});
  EXPECT_FALSE(date_time.IsSet());
  EXPECT_EQ(date_time.ToCivil(), std::nullopt);
  EXPECT_EQ(date_time, DateTime());
}

TEST(DateTimeTest, MonthZeroIsNoDate) {
  const DateTime date_time =
      DateTime::FromWords({2026, 0x000B, 13, (45 << 8) | 30});
  EXPECT_FALSE(date_time.IsSet());
  EXPECT_EQ(date_time.ToCivil(), std::nullopt);
}

TEST(DateTimeTest, WordsFromCivil) {
  const DateTime date_time =
      DateTime::FromCivil(absl::CivilSecond(2026, 11, 12, 13, 45, 30));
  EXPECT_TRUE(date_time.IsSet());
  EXPECT_THAT(date_time.GetWords(),
              ElementsAre(2026, (11 << 8) | 12, 13, (45 << 8) | 30));
}

TEST(DateTimeTest, CivilRoundTrips) {
  const absl::CivilSecond time(2026, 12, 31, 23, 59, 58);
  EXPECT_THAT(DateTime::FromCivil(time).ToCivil(), Optional(time));
  EXPECT_THAT(
      DateTime::FromWords(DateTime::FromCivil(time).GetWords()).ToCivil(),
      Optional(time));
}

TEST(DateTimeTest, WordsRoundTrip) {
  const DateTime::Words words = {1999, (1 << 8) | 2, 3, (4 << 8) | 5};
  EXPECT_EQ(DateTime::FromWords(words).GetWords(), words);
  EXPECT_THAT(DateTime::FromWords(words).ToCivil(),
              Optional(absl::CivilSecond(1999, 1, 2, 3, 4, 5)));
}

TEST(DateTimeTest, FromWordsKeepsInvalidValues) {
  const DateTime::Words words = {2026, (13 << 8) | 40, 25, (61 << 8) | 61};
  const DateTime date_time = DateTime::FromWords(words);
  EXPECT_EQ(date_time.GetWords(), words);
  EXPECT_TRUE(date_time.IsSet());
}

TEST(DateTimeTest, YearKeepsItsLow16Bits) {
  const DateTime date_time =
      DateTime::FromCivil(absl::CivilSecond(65536 + 2026, 1, 1, 0, 0, 0));
  EXPECT_THAT(date_time.GetWords(), ElementsAre(2026, (1 << 8) | 1, 0, 0));
}

TEST(DateTimeTest, OrdersChronologicallyAcrossEachWord) {
  const DateTime base =
      DateTime::FromCivil(absl::CivilSecond(2026, 6, 15, 12, 30, 30));
  EXPECT_EQ(base, base);
  EXPECT_LT(DateTime(), base);
  EXPECT_LT(base, DateTime::FromCivil(absl::CivilSecond(2027, 1, 1, 0, 0, 0)));
  EXPECT_LT(base, DateTime::FromCivil(absl::CivilSecond(2026, 7, 1, 0, 0, 0)));
  EXPECT_LT(base, DateTime::FromCivil(absl::CivilSecond(2026, 6, 16, 0, 0, 0)));
  EXPECT_LT(base,
            DateTime::FromCivil(absl::CivilSecond(2026, 6, 15, 13, 0, 0)));
  EXPECT_LT(base,
            DateTime::FromCivil(absl::CivilSecond(2026, 6, 15, 12, 31, 0)));
  EXPECT_LT(base,
            DateTime::FromCivil(absl::CivilSecond(2026, 6, 15, 12, 30, 31)));
}

}  // namespace
}  // namespace oz3
