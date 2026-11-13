// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_hardware.h"

#include <cstdint>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/faulty_file_protocol.h"

namespace oz3 {
namespace {

using WriteCaching = DiskHardware::WriteCaching;

constexpr char kPath[] = "mem:/disk.img";
constexpr char kOtherPath[] = "mem:/other.img";
constexpr int kSectorCount = 8;

// Timing that is easy to add up: a read takes 1 second, a write 2 seconds, a
// seek 100 milliseconds, and a read from the cache 10 milliseconds.
constexpr absl::Duration kRead = absl::Seconds(1);
constexpr absl::Duration kWrite = absl::Seconds(2);
constexpr absl::Duration kLatency = absl::Milliseconds(100);
constexpr absl::Duration kCached = absl::Milliseconds(10);

// A sector of words.
using Sector = std::vector<uint16_t>;

// Returns a sector whose words are `first`, `first` + 1, and so on.
Sector MakeSector(uint16_t first) {
  Sector sector(kDiskSectorWords);
  for (int i = 0; i < kDiskSectorWords; ++i) {
    sector[i] = static_cast<uint16_t>(first + i);
  }
  return sector;
}

// Returns settings with the timing above, and `read_cache_sectors`.
DiskHardwareSettings GetTimedSettings(int read_cache_sectors) {
  return {
      .read_cache_sectors = read_cache_sectors,
      .read_words_per_sec = kDiskSectorWords,
      .write_words_per_sec = kDiskSectorWords / 2,
      .cache_words_per_sec = kDiskSectorWords * 100,
      .latency = kLatency,
  };
}

class DiskHardwareTest : public ::testing::Test {
 protected:
  DiskHardwareTest() {
    std::unique_ptr<FaultyFileProtocol> protocol =
        std::make_unique<FaultyFileProtocol>();
    protocol_ = protocol.get();
    file_system_.Register(std::move(protocol));
  }

  // Returns a new image of kSectorCount sectors at `path`, whose sector n holds
  // MakeSector(n * 0x100 + `first`).
  std::unique_ptr<DiskImage> MakeImage(std::string_view path,
                                       uint16_t first = 0) {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&file_system_, path, kSectorCount);
    EXPECT_TRUE(image.ok()) << image.status();
    if (!image.ok()) {
      return nullptr;
    }
    for (int sector = 0; sector < kSectorCount; ++sector) {
      EXPECT_EQ((*image)->WriteSector(sector, MakeSector(static_cast<uint16_t>(
                                                  sector * 0x100 + first))),
                DiskStatus::kOk);
    }
    return std::move(*image);
  }

  // Makes hardware_ with `settings`, with MakeImage(kPath) attached.
  void MakeHardware(const DiskHardwareSettings& settings) {
    hardware_.reset();  // Closes any image already at kPath.
    hardware_ = std::make_unique<DiskHardware>(settings);
    std::unique_ptr<DiskImage> image = MakeImage(kPath);
    ASSERT_NE(image, nullptr);
    hardware_->AttachImage(std::move(image));
  }

  // Reads `sector`, expecting kOk, and returns its words.
  Sector Read(int sector) {
    Sector words(kDiskSectorWords);
    EXPECT_EQ(hardware_->ReadSector(sector, absl::MakeSpan(words)),
              DiskStatus::kOk);
    return words;
  }

  // Writes `words` to `sector` with `caching`, expecting kOk.
  void Write(int sector, const Sector& words, WriteCaching caching) {
    EXPECT_EQ(hardware_->WriteSector(sector, words, caching), DiskStatus::kOk);
  }

  gb::FileSystem file_system_;
  FaultyFileProtocol* protocol_ = nullptr;  // Owned by file_system_.
  std::unique_ptr<DiskHardware> hardware_;
};

TEST_F(DiskHardwareTest, ReadsAndWritesTheImage) {
  MakeHardware({});
  EXPECT_EQ(Read(3), MakeSector(0x300));
  Write(3, MakeSector(0x1234), WriteCaching::kCache);
  Write(4, MakeSector(0x5678), WriteCaching::kSkip);
  std::unique_ptr<DiskImage> image = hardware_->DetachImage();
  ASSERT_NE(image, nullptr);
  Sector words(kDiskSectorWords);
  ASSERT_EQ(image->ReadSector(3, absl::MakeSpan(words)), DiskStatus::kOk);
  EXPECT_EQ(words, MakeSector(0x1234));
  ASSERT_EQ(image->ReadSector(4, absl::MakeSpan(words)), DiskStatus::kOk);
  EXPECT_EQ(words, MakeSector(0x5678));
}

TEST_F(DiskHardwareTest, KeepsItsSettings) {
  MakeHardware(GetTimedSettings(3));
  EXPECT_EQ(hardware_->GetSettings(), GetTimedSettings(3));
}

TEST_F(DiskHardwareTest, StartsEmpty) {
  DiskHardware hardware(GetTimedSettings(2));
  EXPECT_FALSE(hardware.HasImage());
  EXPECT_EQ(hardware.GetMedia(), nullptr);
  EXPECT_EQ(hardware.DetachImage(), nullptr);
}

TEST_F(DiskHardwareTest, EmptyDriveHasNoDisk) {
  DiskHardware hardware(GetTimedSettings(2));
  Sector words(kDiskSectorWords);
  EXPECT_EQ(hardware.ReadSector(0, absl::MakeSpan(words)), DiskStatus::kNoDisk);
  EXPECT_EQ(hardware.WriteSector(0, words, WriteCaching::kCache),
            DiskStatus::kNoDisk);
  Sector short_words(kDiskSectorWords - 1);
  EXPECT_EQ(hardware.ReadSector(-1, absl::MakeSpan(short_words)),
            DiskStatus::kNoDisk);
  EXPECT_EQ(hardware.CollectBusyTime(), absl::ZeroDuration());
}

TEST_F(DiskHardwareTest, DetachImageReturnsTheImage) {
  MakeHardware(GetTimedSettings(2));
  const DiskMedia* media = hardware_->GetMedia();
  ASSERT_NE(media, nullptr);
  EXPECT_EQ(media->GetSectorCount(), kSectorCount);
  EXPECT_TRUE(hardware_->HasImage());
  EXPECT_EQ(hardware_->DetachImage().get(), media);
  EXPECT_FALSE(hardware_->HasImage());
  EXPECT_EQ(hardware_->GetMedia(), nullptr);
  Sector words(kDiskSectorWords);
  EXPECT_EQ(hardware_->ReadSector(0, absl::MakeSpan(words)),
            DiskStatus::kNoDisk);
}

TEST_F(DiskHardwareTest, AttachingReplacesTheImage) {
  MakeHardware(GetTimedSettings(2));
  hardware_->AttachImage(MakeImage(kOtherPath, 0x8000));
  EXPECT_EQ(Read(1), MakeSector(0x8100));

  // The first image was destroyed, closing its host file.
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(&file_system_, kPath);
  EXPECT_TRUE(image.ok()) << image.status();
}

TEST_F(DiskHardwareTest, AttachingNullEmptiesTheDrive) {
  MakeHardware(GetTimedSettings(2));
  hardware_->AttachImage(nullptr);
  EXPECT_FALSE(hardware_->HasImage());
}

TEST_F(DiskHardwareTest, ChangingTheImageEmptiesTheCache) {
  MakeHardware(GetTimedSettings(2));
  Read(0);
  std::unique_ptr<DiskImage> image = hardware_->DetachImage();
  hardware_->AttachImage(MakeImage(kOtherPath, 0x8000));
  hardware_->CollectBusyTime();
  EXPECT_EQ(Read(0), MakeSector(0x8000));
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + kRead);

  hardware_->AttachImage(std::move(image));
  hardware_->CollectBusyTime();
  EXPECT_EQ(Read(0), MakeSector(0));
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + kRead);
}

TEST_F(DiskHardwareTest, ChangingTheImageForgetsTheHead) {
  MakeHardware(GetTimedSettings(0));
  Read(0);
  hardware_->AttachImage(hardware_->DetachImage());
  Read(1);
  EXPECT_EQ(hardware_->CollectBusyTime(), 2 * (kLatency + kRead));
}

TEST_F(DiskHardwareTest, ChangingTheImageKeepsTheBusyTime) {
  MakeHardware(GetTimedSettings(0));
  Read(0);
  std::unique_ptr<DiskImage> image = hardware_->DetachImage();
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + kRead);
}

TEST_F(DiskHardwareTest, ZeroRatesTakeNoTime) {
  MakeHardware({});
  Read(0);
  Read(5);
  Write(2, MakeSector(0), WriteCaching::kCache);
  EXPECT_EQ(hardware_->CollectBusyTime(), absl::ZeroDuration());
}

TEST_F(DiskHardwareTest, FirstTransferCostsTheLatency) {
  MakeHardware(GetTimedSettings(0));
  Read(0);
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + kRead);

  MakeHardware(GetTimedSettings(0));
  Write(1, MakeSector(0), WriteCaching::kCache);
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + kWrite);
}

TEST_F(DiskHardwareTest, CollectBusyTimeResets) {
  MakeHardware(GetTimedSettings(0));
  Read(0);
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + kRead);
  EXPECT_EQ(hardware_->CollectBusyTime(), absl::ZeroDuration());
  Read(1);
  EXPECT_EQ(hardware_->CollectBusyTime(), kRead);
}

TEST_F(DiskHardwareTest, ContinuousReadsCostTheLatencyOnce) {
  MakeHardware(GetTimedSettings(0));
  Read(2);
  Read(3);
  Read(4);
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + 3 * kRead);
}

TEST_F(DiskHardwareTest, ContinuousWritesCostTheLatencyOnce) {
  MakeHardware(GetTimedSettings(0));
  Write(2, MakeSector(0), WriteCaching::kCache);
  Write(3, MakeSector(0), WriteCaching::kSkip);
  Write(4, MakeSector(0), WriteCaching::kCache);
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + 3 * kWrite);
}

TEST_F(DiskHardwareTest, NonContinuousTransfersCostTheLatency) {
  MakeHardware(GetTimedSettings(0));
  Read(2);
  Read(4);  // Skips a sector.
  Read(3);  // Goes back.
  Read(3);  // Repeats.
  EXPECT_EQ(hardware_->CollectBusyTime(), 4 * (kLatency + kRead));

  Write(5, MakeSector(0), WriteCaching::kCache);
  Write(7, MakeSector(0), WriteCaching::kCache);
  Write(6, MakeSector(0), WriteCaching::kCache);
  EXPECT_EQ(hardware_->CollectBusyTime(), 3 * (kLatency + kWrite));
}

TEST_F(DiskHardwareTest, SwitchingDirectionCostsTheLatency) {
  MakeHardware(GetTimedSettings(0));
  Read(0);
  Write(1, MakeSector(0), WriteCaching::kCache);
  Read(2);
  EXPECT_EQ(hardware_->CollectBusyTime(), 3 * kLatency + 2 * kRead + kWrite);
}

TEST_F(DiskHardwareTest, NoCacheReadsEveryTime) {
  MakeHardware(GetTimedSettings(0));
  Read(0);
  Read(0);
  EXPECT_EQ(hardware_->CollectBusyTime(), 2 * (kLatency + kRead));
}

TEST_F(DiskHardwareTest, CachedReadsCostTheCacheRateWithNoLatency) {
  MakeHardware(GetTimedSettings(4));
  Read(0);
  Read(5);
  hardware_->CollectBusyTime();
  EXPECT_EQ(Read(0), MakeSector(0));
  EXPECT_EQ(Read(5), MakeSector(0x500));
  EXPECT_EQ(hardware_->CollectBusyTime(), 2 * kCached);
}

TEST_F(DiskHardwareTest, CachedReadsAtACacheRateOfZeroAreFree) {
  DiskHardwareSettings settings = GetTimedSettings(4);
  settings.cache_words_per_sec = 0;
  MakeHardware(settings);
  Read(0);
  hardware_->CollectBusyTime();
  EXPECT_EQ(Read(0), MakeSector(0));
  EXPECT_EQ(hardware_->CollectBusyTime(), absl::ZeroDuration());
}

TEST_F(DiskHardwareTest, CachedReadsDontUseTheImage) {
  MakeHardware(GetTimedSettings(4));
  Read(3);
  protocol_->SetLimits({.reads = 0});
  EXPECT_EQ(Read(3), MakeSector(0x300));
  Sector words(kDiskSectorWords);
  EXPECT_EQ(hardware_->ReadSector(4, absl::MakeSpan(words)),
            DiskStatus::kIoError);
}

TEST_F(DiskHardwareTest, CachedReadsDontMoveTheHead) {
  MakeHardware(GetTimedSettings(4));
  Read(0);
  Read(1);
  Read(0);  // Cached.
  Read(2);  // Continuous with sector 1.
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + 3 * kRead + kCached);

  Write(3, MakeSector(0), WriteCaching::kCache);
  Read(1);                                        // Cached.
  Write(4, MakeSector(0), WriteCaching::kCache);  // Continuous with sector 3.
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + 2 * kWrite + kCached);
}

TEST_F(DiskHardwareTest, EvictsTheLeastRecentlyUsedSector) {
  MakeHardware(GetTimedSettings(2));
  Read(0);
  Read(1);
  Read(0);  // Cached, so sector 1 is now the least recently used.
  Read(2);  // Evicts sector 1.
  hardware_->CollectBusyTime();

  EXPECT_EQ(Read(0), MakeSector(0));
  EXPECT_EQ(Read(2), MakeSector(0x200));
  EXPECT_EQ(hardware_->CollectBusyTime(), 2 * kCached);
  EXPECT_EQ(Read(1), MakeSector(0x100));
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + kRead);
}

TEST_F(DiskHardwareTest, CacheWritesAddTheSector) {
  MakeHardware(GetTimedSettings(2));
  Write(5, MakeSector(0x1234), WriteCaching::kCache);
  hardware_->CollectBusyTime();
  EXPECT_EQ(Read(5), MakeSector(0x1234));
  EXPECT_EQ(hardware_->CollectBusyTime(), kCached);
}

TEST_F(DiskHardwareTest, CacheWritesUpdateACachedSector) {
  MakeHardware(GetTimedSettings(2));
  Read(5);
  Write(5, MakeSector(0x1234), WriteCaching::kCache);
  hardware_->CollectBusyTime();
  EXPECT_EQ(Read(5), MakeSector(0x1234));
  EXPECT_EQ(hardware_->CollectBusyTime(), kCached);
}

TEST_F(DiskHardwareTest, CacheWritesAreMostRecentlyUsed) {
  MakeHardware(GetTimedSettings(2));
  Read(0);
  Read(1);
  Write(0, MakeSector(0x1234), WriteCaching::kCache);
  Read(2);  // Evicts sector 1, not sector 0.
  hardware_->CollectBusyTime();
  EXPECT_EQ(Read(0), MakeSector(0x1234));
  EXPECT_EQ(hardware_->CollectBusyTime(), kCached);
}

TEST_F(DiskHardwareTest, SkipWritesDontAddTheSector) {
  MakeHardware(GetTimedSettings(2));
  Read(0);
  Write(5, MakeSector(0x1234), WriteCaching::kSkip);
  hardware_->CollectBusyTime();
  EXPECT_EQ(Read(5), MakeSector(0x1234));
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + kRead);
}

TEST_F(DiskHardwareTest, SkipWritesDontEvict) {
  MakeHardware(GetTimedSettings(1));
  Read(0);
  Write(5, MakeSector(0x1234), WriteCaching::kSkip);
  hardware_->CollectBusyTime();
  EXPECT_EQ(Read(0), MakeSector(0));
  EXPECT_EQ(hardware_->CollectBusyTime(), kCached);
}

TEST_F(DiskHardwareTest, SkipWritesUpdateACachedSector) {
  MakeHardware(GetTimedSettings(2));
  Read(5);
  Write(5, MakeSector(0x1234), WriteCaching::kSkip);
  hardware_->CollectBusyTime();
  EXPECT_EQ(Read(5), MakeSector(0x1234));
  EXPECT_EQ(hardware_->CollectBusyTime(), kCached);
}

TEST_F(DiskHardwareTest, FailedTransfersTakeNoTimeAndDontMoveTheHead) {
  MakeHardware(GetTimedSettings(2));
  Read(0);
  Sector words(kDiskSectorWords);
  EXPECT_EQ(hardware_->ReadSector(kSectorCount, absl::MakeSpan(words)),
            DiskStatus::kBadArgument);
  EXPECT_EQ(hardware_->WriteSector(-1, words, WriteCaching::kCache),
            DiskStatus::kBadArgument);
  Read(1);  // Continuous with sector 0.
  EXPECT_EQ(hardware_->CollectBusyTime(), kLatency + 2 * kRead);
}

TEST_F(DiskHardwareTest, WrongSizedBuffersAreBadArguments) {
  MakeHardware(GetTimedSettings(2));
  Read(0);
  Sector short_words(kDiskSectorWords - 1);
  Sector long_words(kDiskSectorWords + 1);
  EXPECT_EQ(hardware_->ReadSector(0, absl::MakeSpan(short_words)),
            DiskStatus::kBadArgument);
  EXPECT_EQ(hardware_->ReadSector(0, absl::MakeSpan(long_words)),
            DiskStatus::kBadArgument);
  EXPECT_EQ(hardware_->WriteSector(0, short_words, WriteCaching::kCache),
            DiskStatus::kBadArgument);
  EXPECT_EQ(Read(0), MakeSector(0));
}

TEST_F(DiskHardwareTest, WriteProtectedWritesLeaveTheCache) {
  MakeHardware(GetTimedSettings(2));
  Read(0);
  ASSERT_EQ(hardware_->GetMedia()->SetWriteProtected(true), DiskStatus::kOk);
  hardware_->CollectBusyTime();
  EXPECT_EQ(hardware_->WriteSector(0, MakeSector(0x1234), WriteCaching::kCache),
            DiskStatus::kWriteProtected);
  EXPECT_EQ(hardware_->WriteSector(0, MakeSector(0x1234), WriteCaching::kSkip),
            DiskStatus::kWriteProtected);
  EXPECT_EQ(Read(0), MakeSector(0));
  EXPECT_EQ(hardware_->CollectBusyTime(), kCached);
}

TEST_F(DiskHardwareTest, HostFailuresAreIoErrors) {
  MakeHardware(GetTimedSettings(2));
  protocol_->SetLimits({.writes = 0});
  EXPECT_EQ(hardware_->WriteSector(0, MakeSector(0x1234), WriteCaching::kCache),
            DiskStatus::kIoError);
  Sector words(kDiskSectorWords);
  EXPECT_EQ(hardware_->ReadSector(0, absl::MakeSpan(words)),
            DiskStatus::kIoError);
  EXPECT_EQ(hardware_->CollectBusyTime(), absl::ZeroDuration());
}

}  // namespace
}  // namespace oz3
