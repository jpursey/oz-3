// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_DISK_TOOL_H_
#define OZ3_TOOLS_DISK_TOOL_H_

#include <cstddef>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/functional/function_ref.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_label.h"
#include "oz3/tools/disk_file_protocol.h"

namespace oz3 {

// The commands of oz3dsk, each a call on the image at `image_path`, a path in
// the tool's file system, as host paths are. A command opens the image, and
// closes it before it returns, so the tool holds nothing between commands.
//
// A failure's message names the image, or the argument, that failed, and what
// went wrong, such as "work.ozd: write protected" or "work.ozd: HELLO.TXT:
// read-only", with a DiskStatus as its GetDiskStatusText. A command on one
// file fails with "bad name" if given a pattern.
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

  // Returns the attributes in `letters`: r for read-only and h for hidden, in
  // either case. Fails for any other character.
  static absl::StatusOr<gb::FileAttributes> ParseAttributes(
      std::string_view letters);

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

  //----------------------------------------------------------------------------
  // File commands
  //----------------------------------------------------------------------------

  // Returns a line for each file `pattern` matches, in directory order: its
  // name, its size in words, its attributes (r and h, or - for each not set),
  // and its modified time, blank with no date. Hidden files are left out
  // unless `all` is true. With no file to list, it returns "no files".
  absl::StatusOr<std::string> Dir(std::string_view image_path,
                                  const DiskFilename& pattern, bool all);

  // Copies the host file at `host_path` onto the disk as `name`, or the host
  // file's own name without one, replacing any file of that name. As `text`,
  // each CRLF and lone CR becomes LF (see WordsFromText). The copy keeps the
  // attributes both the host and the disk support, and fails onto a read-only
  // file. Fails if the host file's name isn't a disk name, such as one too
  // long, and a copy that fails leaves no file of that name.
  absl::Status Put(std::string_view image_path, std::string_view host_path,
                   std::optional<DiskFilename> name, bool text);

  // Copies the file `name` from the disk to the host file at `host_path`,
  // replacing any file there. As `text`, zero bytes are dropped (see
  // TextFromWords). The copy keeps attributes as Put's does, and fails onto a
  // read-only host file.
  absl::Status Get(std::string_view image_path, const DiskFilename& name,
                   std::string_view host_path, bool text);

  // Deletes every file `pattern` matches, hidden ones included. Fails if none
  // matches, or if any can't be deleted, such as a read-only file, naming each
  // that failed, after deleting the rest.
  absl::Status Delete(std::string_view image_path, const DiskFilename& pattern);

  // Renames the file `from` to `to`. Fails if another file is named `to`.
  absl::Status Rename(std::string_view image_path, const DiskFilename& from,
                      const DiskFilename& to);

  // Sets the attributes in `set` and clears those in `clear` on the file
  // `name`, then returns a line with its name and attributes, as Dir shows
  // them. With neither, it only shows them. Fails if an attribute is in both.
  absl::StatusOr<std::string> Attrib(std::string_view image_path,
                                     const DiskFilename& name,
                                     gb::FileAttributes set,
                                     gb::FileAttributes clear);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // `file_system` and `protocol`, which it owns, must outlive the tool.
  DiskTool(gb::FileSystem* file_system, DiskFileProtocol* protocol,
           std::string_view protocol_name)
      : file_system_(file_system),
        protocol_(protocol),
        protocol_name_(protocol_name) {}

  // What a command needs of the mounted disk.
  enum class Access {
    kImage,  // Only the image, to format or report on.
    kRead,   // Its files, to read.
    kWrite,  // Its files, to change.
  };

  // Mounts the image at `image_path`, runs `command` on it if the disk allows
  // `access`, and unmounts it, committing what `command` wrote. Returns the
  // failure to open or mount the image, the disk's failure to allow `access`
  // or `command`'s failure, named for the image, or the failure to commit, in
  // that order.
  absl::Status RunMounted(std::string_view image_path, Access access,
                          absl::FunctionRef<absl::Status()> command);

  // Formats the mounted disk, for Format.
  absl::Status DoFormat(const DiskLabel& label, int sectors_per_cluster);

  // Sets `text` to the mounted disk's info, for Info.
  absl::Status DoInfo(std::string* text);

  // Sets `text` to the lines Dir returns, from the mounted disk.
  absl::Status DoDir(const DiskFilename& pattern, bool all, std::string* text);

  // Writes `bytes` to the file `name` on the mounted disk, replacing it, and
  // sets its attributes in `mask` to `attributes`, for Put.
  absl::Status DoPut(const DiskFilename& name,
                     absl::Span<const std::byte> bytes,
                     gb::FileAttributes attributes, gb::FileAttributes mask);

  // Reads the file `name` on the mounted disk into `bytes`, and its attributes
  // into `attributes`, for Get.
  absl::Status DoGet(const DiskFilename& name, std::vector<std::byte>* bytes,
                     gb::FileAttributes* attributes);

  // Deletes the files `pattern` matches on the mounted disk, for Delete.
  absl::Status DoDelete(const DiskFilename& pattern);

  // Renames a file on the mounted disk, for Rename.
  absl::Status DoRename(const DiskFilename& from, const DiskFilename& to);

  // Changes and shows a file's attributes on the mounted disk, for Attrib.
  absl::Status DoAttrib(const DiskFilename& name, gb::FileAttributes set,
                        gb::FileAttributes clear, std::string* text);

  // Returns the protocol's last failure, for a call on it that just failed, or
  // `otherwise` if no disk operation failed, as when gb refused the call
  // itself (see DiskFileProtocol::TakeLastFailure).
  DiskStatus TakeFailure(DiskStatus otherwise);

  // Returns the path of the file `name` in the protocol.
  std::string GetDiskPath(const DiskFilename& name) const;

  // Returns the attributes that copies between `host_path` and the disk keep:
  // those both protocols support.
  gb::FileAttributes GetCopiedAttributes(std::string_view host_path) const;

  gb::FileSystem* const file_system_;
  DiskFileProtocol* const protocol_;
  const std::string protocol_name_;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_DISK_TOOL_H_
