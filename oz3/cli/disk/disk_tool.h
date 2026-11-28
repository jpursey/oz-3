// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CLI_DISK_DISK_TOOL_H_
#define OZ3_CLI_DISK_DISK_TOOL_H_

#include <memory>
#include <string>
#include <string_view>

#include "absl/functional/function_ref.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "gb/base/validated_context.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
#include "oz3/disk/disk_file_protocol.h"
#include "oz3/disk/disk_label.h"

namespace oz3 {

// The commands of oz3dsk, each a call on the image at `image_path`, a path in
// the tool's file system, as host paths are. A command opens the image, and
// closes it before it returns, so the tool holds nothing between commands.
//
// A failure's message names the image, or the argument, that failed, and what
// went wrong, such as "work.ozd: write protected", with a DiskStatus as its
// GetDiskStatusText. A file command goes through the file system, which says
// only whether a call failed, so a failure there says what couldn't be done to
// the file, as in "work.ozd: HELLO.TXT: can't be written", and is kUnknown.
//
// It is called from one thread at a time.
class DiskTool final {
 public:
  //----------------------------------------------------------------------------
  // Contract
  //----------------------------------------------------------------------------

  // The file system the tool works on, which must outlive it. The tool's
  // protocol stays registered there after it, with nothing mounted.
  static GB_CONTEXT_CONSTRAINT(kConstraintFileSystem, kInRequired,
                               gb::FileSystem);

  // The name the tool registers its DiskFileProtocol as.
  static constexpr const char* kKeyProtocolName = "disk_protocol_name";
  static GB_CONTEXT_CONSTRAINT_NAMED_DEFAULT(
      kConstraintProtocolName, kInOptional, std::string, kKeyProtocolName,
      std::string(DiskFileProtocol::kDefaultName));

  // What the tool takes from its context: its own needs, and its protocol's
  // (see DiskFileProtocol::Contract), such as the clock it stamps the files it
  // writes from.
  using Contract = gb::DerivedContextContract<DiskFileProtocol::Contract,
                                              kConstraintFileSystem,
                                              kConstraintProtocolName>;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Makes a tool on the context's file system, registering a DiskFileProtocol
  // there, made from the same context. Fails if the context doesn't meet the
  // contract, or if the protocol's name can't be registered, as when it is
  // taken.
  static absl::StatusOr<std::unique_ptr<DiskTool>> Create(Contract contract);

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

  // Returns the volume label in `text`, as DiskLabel::Parse parses one. Fails
  // if it breaks the label rules, such as one too long.
  static absl::StatusOr<DiskLabel> ParseLabel(std::string_view text);

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

  // Writes the bare program file at `program_path` (see BareProgram) into the
  // disk's boot area, as loading it at the boot area's first word would, with
  // every word no data block sets 0. Its stack isn't written. Fails if its
  // contents are more than the boot area's DiskSuperblock::kBootAreaWords
  // words.
  absl::Status Boot(std::string_view image_path, std::string_view program_path);

  //----------------------------------------------------------------------------
  // File commands
  //----------------------------------------------------------------------------

  // Returns a line for each file `pattern` matches, in directory order: its
  // name, its size in words, its attributes (r and h, or - for each not set),
  // and its modified time to the minute in the host's local time, such as
  // 2026-11-17 10:30, if it has one. Hidden files are left out unless `all` is
  // true. The pattern is matched as gb::FileSystem::List matches one, ignoring
  // case: * matches any characters, and an empty pattern matches every file.
  // With no file to list, it returns "no files". Fails if the disk has no file
  // system to read.
  absl::StatusOr<std::string> Dir(std::string_view image_path,
                                  std::string_view pattern, bool all);

  // Copies the host file at `host_path` onto the disk as `name`, or the host
  // file's own name if `name` is empty, replacing any file of that name. As
  // `text`, each CRLF and lone CR becomes LF (see DiskTextFromHost). The copy
  // keeps the attributes both the host and the disk support, and fails if they
  // can't be set, leaving the file it wrote. Fails if the name isn't a valid
  // disk name, such as one too long. A copy that fails to write can leave part
  // of the file, such as on a full disk.
  absl::Status Put(std::string_view image_path, std::string_view host_path,
                   std::string_view name, bool text);

  // Copies the file `name` from the disk to the host file at `host_path`,
  // replacing any file there that can be replaced, which a read-only one
  // can't. If `host_path` is a folder, the host file is in it, with the file's
  // name as the disk has it (HELLO.TXT), and if `host_path` is empty, it is
  // that name in the file system's current folder. As `text`, zero bytes are
  // dropped (see HostTextFromDisk). The copy keeps attributes as Put's does,
  // and fails if they can't be set, leaving the file it wrote.
  absl::Status Get(std::string_view image_path, std::string_view name,
                   std::string_view host_path, bool text);

  // Deletes every file `pattern` matches, as Dir matches it, hidden ones
  // included. Fails for an empty pattern, rather than deleting every file, as
  // * does. Fails if none matches, or if any can't be deleted, such as a
  // read-only file, naming each that failed, after deleting the rest.
  absl::Status Delete(std::string_view image_path, std::string_view pattern);

  // Renames the file `from` to `to`. Fails if `to` isn't a disk name, or
  // another file has it.
  absl::Status Rename(std::string_view image_path, std::string_view from,
                      std::string_view to);

  // Sets the attributes in `set` and clears those in `clear` on the file
  // `name`, then returns a line with its name and attributes, as Dir shows
  // them. With neither, it only shows them. Fails if an attribute is in both.
  absl::StatusOr<std::string> Attrib(std::string_view image_path,
                                     std::string_view name,
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
    kImage,  // The image, and its volume's own words: to format or report
             // on it, or to set its label or boot area.
    kFiles,  // Its file system, to read or change files.
  };

  // Mounts the image at `image_path`, runs `command` on it, and unmounts it,
  // committing what `command` wrote. For kFiles, a disk with no file system to
  // reach, unformatted or corrupt, fails before `command` runs. Returns the
  // failure to open or mount the image or `command`'s failure, named for the
  // image, or the failure to commit, in that order.
  absl::Status RunMounted(std::string_view image_path, Access access,
                          absl::FunctionRef<absl::Status()> command);

  // Formats the mounted disk, for Format.
  absl::Status DoFormat(const DiskLabel& label, int sectors_per_cluster);

  // Sets `text` to the mounted disk's info, for Info.
  absl::Status DoInfo(std::string* text);

  // Sets `text` to the lines Dir returns, from the mounted disk.
  absl::Status DoDir(std::string_view pattern, bool all, std::string* text);

  // Writes `bytes` to the file `name` on the mounted disk, replacing it, and
  // sets its attributes in `mask` to `attributes`, for Put.
  absl::Status DoPut(std::string_view name, std::string_view bytes,
                     gb::FileAttributes attributes, gb::FileAttributes mask);

  // Reads the file `name` on the mounted disk into `bytes`, and its attributes
  // into `attributes`, for Get.
  absl::Status DoGet(std::string_view name, std::string* bytes,
                     gb::FileAttributes* attributes);

  // Deletes the files `pattern` matches on the mounted disk, for Delete.
  absl::Status DoDelete(std::string_view pattern);

  // Renames a file on the mounted disk, for Rename.
  absl::Status DoRename(std::string_view from, std::string_view to);

  // Changes and shows a file's attributes on the mounted disk, for Attrib.
  absl::Status DoAttrib(std::string_view name, gb::FileAttributes set,
                        gb::FileAttributes clear, std::string* text);

  // Returns the path of the file `name` in the protocol.
  std::string GetDiskPath(std::string_view name) const;

  // Returns the attributes that copies between `host_path` and the disk keep:
  // those both protocols support.
  gb::FileAttributes GetCopiedAttributes(std::string_view host_path) const;

  gb::FileSystem* const file_system_;
  DiskFileProtocol* const protocol_;
  const std::string protocol_name_;
};

}  // namespace oz3

#endif  // OZ3_CLI_DISK_DISK_TOOL_H_
