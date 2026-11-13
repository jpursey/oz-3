// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_DISK_IMAGE_H_
#define OZ3_DISK_DISK_IMAGE_H_

#include <cstdint>
#include <memory>
#include <string_view>

#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "gb/file/file.h"
#include "gb/file/file_system.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_media.h"
#include "oz3/disk/disk_status.h"

namespace oz3 {

// A disk image: a host file holding an 8-word header, and then a flat array of
// sectors. Words are stored two bytes each, low byte first, so sector n starts
// at byte 16 + 512 * n. The header is the medium's (see DiskMedia).
//
// The image owns its host file, open for reading and writing until the image
// is destroyed. It is write-through: each write is flushed to the host before
// it returns, so a host failure is the result of the write it failed. Once a
// host read, write, or flush fails, the host file is unusable, and every later
// operation fails with kIoError too.
class DiskImage final : public DiskMedia {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a blank image of `sector_count` sectors at `path`, replacing any
  // file there: every sector is zeros, an unformatted disk. `file_system` must
  // outlive the image. Fails if `sector_count` is not 1 to kMaxSectorCount, or
  // the host file can't be written.
  static absl::StatusOr<std::unique_ptr<DiskImage>> Create(
      gb::FileSystem* file_system, std::string_view path, int sector_count);

  // Opens the existing image at `path` for reading and writing. `file_system`
  // must outlive the image. Fails with a message saying what is wrong if the
  // file is missing, can't be opened for reading and writing, isn't an image,
  // has a version or sector size this doesn't support, has no sectors or more
  // than kMaxSectorCount, or is too short for the sectors its header counts.
  static absl::StatusOr<std::unique_ptr<DiskImage>> Open(
      gb::FileSystem* file_system, std::string_view path);

  DiskImage(const DiskImage&) = delete;
  DiskImage& operator=(const DiskImage&) = delete;
  ~DiskImage() = default;

  //----------------------------------------------------------------------------
  // Sectors
  //----------------------------------------------------------------------------

  // Reads sector `sector` into `words`. Returns kOk, kBadArgument if the
  // sector is out of range or `words` isn't kDiskSectorWords long, or kIoError
  // if the host read fails.
  DiskStatus ReadSector(int sector, absl::Span<uint16_t> words);

  // Writes `words` to sector `sector`. Returns kOk, kBadArgument as for
  // ReadSector, kWriteProtected if the image is write-protected, or kIoError
  // if the host write or flush fails.
  DiskStatus WriteSector(int sector, absl::Span<const uint16_t> words);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  DiskImage(std::unique_ptr<gb::File> file, int sector_count, uint16_t flags);

  // Returns whether `sector` is a sector, and `words` a sector's words.
  bool IsValidSector(int sector, absl::Span<const uint16_t> words) const;

  // Writes `flags` to the header, flushing it.
  DiskStatus WriteFlags(uint16_t flags) override;

  std::unique_ptr<gb::File> file_;
};

}  // namespace oz3

#endif  // OZ3_DISK_DISK_IMAGE_H_
