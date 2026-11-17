// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/disk_transfer.h"

#include <cstddef>
#include <cstdint>
#include <string_view>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

std::vector<std::byte> Bytes(std::vector<int> values) {
  std::vector<std::byte> bytes;
  for (const int value : values) {
    bytes.push_back(static_cast<std::byte>(value));
  }
  return bytes;
}

TEST(DiskTransferTest, WordsFromNoBytes) {
  EXPECT_THAT(WordsFromBytes({}), IsEmpty());
}

TEST(DiskTransferTest, WordsFromEvenBytesAreLowByteFirst) {
  EXPECT_THAT(WordsFromBytes(Bytes({0x34, 0x12, 0xCD, 0xAB})),
              ElementsAre(0x1234, 0xABCD));
}

TEST(DiskTransferTest, WordsFromOddBytesPadTheLastWord) {
  EXPECT_THAT(WordsFromBytes(Bytes({0x34, 0x12, 0xFF})),
              ElementsAre(0x1234, 0x00FF));
}

TEST(DiskTransferTest, BytesFromWordsAreLowByteFirst) {
  EXPECT_THAT(BytesFromWords({}), IsEmpty());
  EXPECT_EQ(BytesFromWords({0x1234, 0xABCD}), Bytes({0x34, 0x12, 0xCD, 0xAB}));
}

TEST(DiskTransferTest, BytesFromWordsKeepEveryByte) {
  EXPECT_EQ(BytesFromWords({0x00FF, 0x0000}), Bytes({0xFF, 0, 0, 0}));
}

TEST(DiskTransferTest, WordsFromTextPacksLowByteFirst) {
  EXPECT_THAT(WordsFromText("AB"), ElementsAre('A' | 'B' << 8));
  EXPECT_THAT(WordsFromText("ABC"), ElementsAre('A' | 'B' << 8, 'C'));
}

TEST(DiskTransferTest, WordsFromTextTurnsLineEndingsToLf) {
  const std::vector<uint16_t> lf = WordsFromText("a\nb\nc\n\n");
  EXPECT_EQ(WordsFromText("a\r\nb\rc\r\r\n"), lf);
  EXPECT_EQ(WordsFromText("a\r\nb\r\nc\r\n\r\n"), lf);
  EXPECT_EQ(WordsFromText("a\rb\rc\r\r"), lf);
}

TEST(DiskTransferTest, WordsFromTextKeepsEveryOtherByte) {
  EXPECT_THAT(WordsFromText(std::string_view("\0\xFF", 2)),
              ElementsAre(0xFF00));
}

TEST(DiskTransferTest, TextFromWordsDropsZeroBytes) {
  EXPECT_EQ(TextFromWords({}), "");
  EXPECT_EQ(TextFromWords({'A' | 'B' << 8, 'C'}), "ABC");
  EXPECT_EQ(TextFromWords({'A' << 8, 0, 'B'}), "AB");
}

TEST(DiskTransferTest, TextRoundTrips) {
  EXPECT_EQ(TextFromWords(WordsFromText("one\ntwo\n")), "one\ntwo\n");
  EXPECT_EQ(TextFromWords(WordsFromText("odd\n")), "odd\n");
  EXPECT_EQ(TextFromWords(WordsFromText("crlf\r\n")), "crlf\n");
}

}  // namespace
}  // namespace oz3
