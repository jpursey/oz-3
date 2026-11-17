// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_status.h"

#include <string_view>

#include "absl/container/flat_hash_set.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

TEST(DiskStatusTest, TextOfEachStatus) {
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kOk), "ok");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kBadCommand), "bad command");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kBadHandle), "bad handle");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kBadName), "bad name");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kBadPattern), "bad pattern");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kBadLabel), "bad label");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kBadAttributes), "bad attributes");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kBadArgument), "bad argument");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kPastEnd), "past end");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kWrongHandleType),
            "wrong handle type");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kNotFound), "not found");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kExists), "exists");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kInUse), "in use");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kTooManyOpen), "too many open");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kReadOnly), "read-only");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kDiskChanged), "disk changed");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kNoDisk), "no disk");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kWriteProtected), "write protected");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kDiskFull), "disk full");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kUnformatted), "unformatted");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kCorrupt), "corrupt");
  EXPECT_EQ(GetDiskStatusText(DiskStatus::kIoError), "I/O error");
}

TEST(DiskStatusTest, EachStatusHasItsOwnText) {
  absl::flat_hash_set<std::string_view> texts;
  int status_count = 0;
  for (int code = 0; code <= 0xFF; ++code) {
    const std::string_view text =
        GetDiskStatusText(static_cast<DiskStatus>(code));
    if (text == "unknown status") {
      continue;
    }
    ++status_count;
    EXPECT_TRUE(texts.insert(text).second) << "Code " << code << ": " << text;
  }
  EXPECT_EQ(status_count, 22);
}

TEST(DiskStatusTest, TextOfAValueThatIsNotAStatus) {
  EXPECT_EQ(GetDiskStatusText(static_cast<DiskStatus>(0x0A)), "unknown status");
  EXPECT_EQ(GetDiskStatusText(static_cast<DiskStatus>(0xFFFF)),
            "unknown status");
}

}  // namespace
}  // namespace oz3
