// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_transfer.h"

#include <string>
#include <string_view>

#include "gtest/gtest.h"

namespace oz3 {
namespace {

TEST(DiskTransferTest, DiskTextFromHostTurnsLineEndingsToLf) {
  const std::string lf = "a\nb\nc\n\n";
  EXPECT_EQ(DiskTextFromHost(lf), lf);
  EXPECT_EQ(DiskTextFromHost("a\r\nb\rc\r\r\n"), lf);
  EXPECT_EQ(DiskTextFromHost("a\r\nb\r\nc\r\n\r\n"), lf);
  EXPECT_EQ(DiskTextFromHost("a\rb\rc\r\r"), lf);
}

TEST(DiskTransferTest, DiskTextFromHostKeepsEveryOtherByte) {
  EXPECT_EQ(DiskTextFromHost(""), "");
  EXPECT_EQ(DiskTextFromHost(std::string_view("\0\xFF\n", 3)),
            std::string("\0\xFF\n", 3));
}

TEST(DiskTransferTest, HostTextFromDiskDropsZeroBytes) {
  EXPECT_EQ(HostTextFromDisk(""), "");
  EXPECT_EQ(HostTextFromDisk("ABC"), "ABC");
  EXPECT_EQ(HostTextFromDisk(std::string_view("\0A\0\0B\0", 6)), "AB");
}

TEST(DiskTransferTest, HostTextFromDiskKeepsLineEndings) {
  EXPECT_EQ(HostTextFromDisk("one\ntwo\r\n"), "one\ntwo\r\n");
}

}  // namespace
}  // namespace oz3
