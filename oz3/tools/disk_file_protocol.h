// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_DISK_FILE_PROTOCOL_H_
#define OZ3_TOOLS_DISK_FILE_PROTOCOL_H_

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/functional/any_invocable.h"
#include "absl/time/civil_time.h"
#include "gb/file/file_protocol.h"
#include "gb/file/file_types.h"
#include "gb/file/raw_file.h"
#include "oz3/core/date_time.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_media.h"
#include "oz3/disk/disk_status.h"

namespace oz3 {

// A file protocol over a mounted disk image, so host code reaches the files on
// a disk as it reaches any other files. It registers as "ozdisk" by default, so
// its paths look like "ozdisk:/HELLO.OZ3".
//
// Paths are "/", the root folder, and "/<name>" for each file in it, a dotted
// name that DiskFilename::Parse accepts, matched without regard to case. List
// patterns are gb's (see gb::PathMatchesPattern), also without regard to
// case, so "*.*" matches only names with an extension.
// There are no other folders. With nothing mounted, every path is invalid,
// and an unformatted disk's root is an empty folder.
//
// A file's bytes are its words, two bytes each, low byte first, so its size is
// its words times 2. Reading from an odd byte reads the high byte of its word
// first, and writing there changes only that byte. Writing an odd number of
// bytes at the end pads the last word with a zero high byte. A file's size, as
// GetPathInfo gives it, is as its directory entry was last committed, while
// an open file's size, which SeekEnd moves to, includes what it has written.
//
// Files have the disk's read-only and hidden attributes. A read-only file
// can't be opened for writing or deleted, and an open file can't be
// deleted, renamed, or have its attributes changed. Files written are stamped
// from the clock as they are committed: flushed, or closed as the RawFile is
// destroyed.
//
// While a formatted disk is mounted, its root is open in a slot of its own,
// which the protocol finds and lists files through, so those never need a
// free slot. The root's slot counts against the files open.
//
// It is called from one thread at a time.
class DiskFileProtocol final : public gb::FileProtocol {
 public:
  //----------------------------------------------------------------------------
  // Types and Constants
  //----------------------------------------------------------------------------

  // Returns the host's date and time, as the drive's clock does.
  using Clock = absl::AnyInvocable<absl::CivilSecond()>;

  static constexpr std::string_view kDefaultName = "ozdisk";
  static constexpr int kDefaultMaxOpenFiles = 16;

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Makes a protocol with nothing mounted, stamping files from `clock`, or the
  // host's local time without one. Up to `max_open_files` files may be open at
  // once, which must be at least 2, the root's slot included.
  explicit DiskFileProtocol(Clock clock = nullptr,
                            int max_open_files = kDefaultMaxOpenFiles);
  DiskFileProtocol(const DiskFileProtocol&) = delete;
  DiskFileProtocol& operator=(const DiskFileProtocol&) = delete;

  // Unmounts as Unmount does. A RawFile must not outlive the protocol.
  ~DiskFileProtocol() override;

  //----------------------------------------------------------------------------
  // Mounting
  //----------------------------------------------------------------------------

  // Unmounts any image mounted, discarding the image, then mounts `image`,
  // which must not be null, opening its root. Returns kOk, kUnformatted (it is
  // mounted with no root, to format), or kIoError, or opening the root's
  // failure (such as kCorrupt), which leaves it mounted with no root. Unmount
  // frees the root's slot, so files left stale never keep the next root from
  // opening.
  DiskStatus Mount(std::unique_ptr<DiskImage> image);

  // Commits the open files as DiskFileSystem::Flush does, stopping at the
  // first that fails, and closes the root, then detaches the image, setting
  // `image` to it if not null. Files still open are stale (see
  // DiskFileSystem): reading and writing them fails. Returns kNoDisk with
  // nothing mounted, kIoError for a failed image, the failure committing, or
  // kOk. The image is detached whatever it returns.
  DiskStatus Unmount(std::unique_ptr<DiskImage>* image = nullptr);

  // Returns whether an image is mounted.
  bool IsMounted() const { return file_system_.HasImage(); }

  // Formats the mounted disk as DiskFileSystem::Format does, stamped from the
  // clock, closing the root first and opening it again after. Returns kNoDisk
  // with nothing mounted, and kInUse with any file open.
  DiskStatus Format(const DiskLabel& label, int sectors_per_cluster);

  // Returns the mounted image, or null with nothing mounted.
  const DiskMedia* GetMedia() const { return file_system_.GetMedia(); }

  // Sets `info` to the mounted disk's file system, as
  // DiskFileSystem::GetVolumeInfo does. Returns kNoDisk with nothing mounted.
  DiskStatus GetVolumeInfo(DiskVolumeInfo* info) {
    return file_system_.GetVolumeInfo(info);
  }

  // Sets the mounted disk's volume label, as DiskFileSystem::SetLabel does.
  // Returns kNoDisk with nothing mounted.
  DiskStatus SetLabel(const DiskLabel& label) {
    return file_system_.SetLabel(label);
  }

  //----------------------------------------------------------------------------
  // gb::FileProtocol
  //----------------------------------------------------------------------------

  gb::FileProtocolFlags GetFlags() const override;
  std::vector<std::string> GetDefaultNames() const override;
  std::string GetCanonicalPath(std::string_view protocol_name,
                               std::string_view path) const override;
  gb::FileAttributes GetAttributeFlags() const override;

 protected:
  gb::PathInfo DoGetPathInfo(std::string_view protocol_name,
                             std::string_view path) override;
  std::vector<std::string> DoList(std::string_view protocol_name,
                                  std::string_view path,
                                  std::string_view pattern, gb::FolderMode mode,
                                  gb::PathTypes types) override;
  bool BasicSetAttributes(std::string_view protocol_name, std::string_view path,
                          gb::FileAttributes attributes) override;
  bool BasicDeleteFile(std::string_view protocol_name,
                       std::string_view path) override;
  bool BasicMoveFile(std::string_view from_protocol_name,
                     std::string_view from_path,
                     std::string_view to_protocol_name,
                     std::string_view to_path) override;
  std::unique_ptr<gb::RawFile> BasicOpenFile(std::string_view protocol_name,
                                             std::string_view path,
                                             gb::FileFlags flags) override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  class DiskRawFile;

  // Returns the clock's date and time.
  DateTime GetNow();

  // Opens the root in root_ if the disk is formatted, returning the failure
  // to open it, or kOk.
  DiskStatus OpenRoot();

  // Closes root_ if it is open.
  void CloseRoot();

  // Sets `entry` to the entry of the file `name`, returning whether it is
  // found.
  bool FindEntry(const DiskFilename& name, DiskDirEntry* entry);

  // Returns the name in a file's path ("/HELLO.TXT"), or nullopt if `path`
  // isn't one.
  static std::optional<DiskFilename> GetName(std::string_view path);

  Clock clock_;
  DiskFileSystem file_system_;
  DiskFile root_ = -1;
};

}  // namespace oz3

#endif  // OZ3_TOOLS_DISK_FILE_PROTOCOL_H_
