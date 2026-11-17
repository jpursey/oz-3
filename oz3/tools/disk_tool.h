// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_DISK_TOOL_H_
#define OZ3_TOOLS_DISK_TOOL_H_

#include <memory>
#include <string>
#include <string_view>

#include "absl/functional/function_ref.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "gb/file/file_system.h"
#include "oz3/disk/disk_label.h"
#include "oz3/tools/disk_file_protocol.h"

namespace oz3 {

// The commands of oz3dsk, each a call on the image at `image_path`, a path in
// the tool's file system. A command opens the image, and closes it before it
// returns, so the tool holds nothing between commands.
//
// A failure's message names the image, or the argument, that failed, and what
// went wrong, such as "work.ozd: write protected", with a DiskStatus as its
// GetDiskStatusText.
//
// It is called from one thread at a time.
class DiskTool final {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  using Clock = DiskFileProtocol::Clock;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Makes a tool on `file_system`, registering a DiskFileProtocol there as
  // `protocol_name`, which stamps files written from `clock`, or the host's
  // local time without one. Fails if the name can't be registered, as when it
  // is taken. `file_system` must outlive the tool, and the protocol stays
  // registered after it, with nothing mounted.
  static absl::StatusOr<std::unique_ptr<DiskTool>> Create(
      gb::FileSystem* file_system, Clock clock = nullptr,
      std::string_view protocol_name = DiskFileProtocol::kDefaultName);

  DiskTool(const DiskTool&) = delete;
  DiskTool& operator=(const DiskTool&) = delete;
  ~DiskTool() = default;

  //----------------------------------------------------------------------------
  // Arguments
  //----------------------------------------------------------------------------

  // Returns the sectors in `text`, a size in words with an optional K (1,024
  // words) or M (1,048,576 words) suffix, such as "720K". Fails if it isn't a
  // size, or isn't a whole number of sectors from 1 to the most an image has.
  static absl::StatusOr<int> ParseSectorCount(std::string_view text);

  //----------------------------------------------------------------------------
  // Image commands
  //----------------------------------------------------------------------------

  // Creates a blank image of `sector_count` sectors, an unformatted disk.
  // Fails if anything is at `image_path` already, so an image is never
  // replaced.
  absl::Status New(std::string_view image_path, int sector_count);

  // Formats the disk as the drive's FORMAT does, with `label`, and
  // `sectors_per_cluster`, or 0 for the drive's choice (see ChooseDiskLayout).
  absl::Status Format(std::string_view image_path, const DiskLabel& label,
                      int sectors_per_cluster);

  // Returns lines showing the medium (its size and write protection), and the
  // file system on it (its label, cluster size, clusters, and free clusters),
  // or that it is unformatted, or corrupt.
  absl::StatusOr<std::string> Info(std::string_view image_path);

  // Sets the disk's volume label.
  absl::Status Label(std::string_view image_path, const DiskLabel& label);

  // Sets or clears the image's write protection. Fails to clear it on a
  // read-only image file, which is always write protected.
  absl::Status Protect(std::string_view image_path, bool write_protected);

  // Checks the file system for problems, repairing them if `fix` is true (see
  // RepairDisk), and returns a line for each problem it found, or "no
  // problems". After repairing, a last line says so.
  absl::StatusOr<std::string> Check(std::string_view image_path, bool fix);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // `file_system` and `protocol`, which it owns, must outlive the tool.
  DiskTool(gb::FileSystem* file_system, DiskFileProtocol* protocol)
      : file_system_(file_system), protocol_(protocol) {}

  // Mounts the image at `image_path`, runs `command` on it, and unmounts it,
  // committing what `command` wrote. Returns the failure to open or mount the
  // image, `command`'s failure, named for the image, or the failure to
  // commit, in that order.
  absl::Status RunMounted(std::string_view image_path,
                          absl::FunctionRef<absl::Status()> command);

  // Formats the mounted disk, for Format.
  absl::Status DoFormat(const DiskLabel& label, int sectors_per_cluster);

  // Sets `text` to the mounted disk's info, for Info.
  absl::Status DoInfo(std::string* text);

  gb::FileSystem* const file_system_;
  DiskFileProtocol* const protocol_;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_DISK_TOOL_H_
