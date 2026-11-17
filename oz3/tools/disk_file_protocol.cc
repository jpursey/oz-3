// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/disk_file_protocol.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "absl/strings/ascii.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/file/file_protocol.h"
#include "gb/file/file_types.h"
#include "gb/file/path.h"
#include "gb/file/raw_file.h"
#include "oz3/core/date_time.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_hardware.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_status.h"

namespace oz3 {

namespace {

// Returns the gb attributes of a directory entry's attribute word.
gb::FileAttributes AttributesFromDisk(uint16_t disk_attributes) {
  gb::FileAttributes attributes;
  if ((disk_attributes & kDiskAttribReadOnly) != 0) {
    attributes += gb::FileAttribute::kReadOnly;
  }
  if ((disk_attributes & kDiskAttribHidden) != 0) {
    attributes += gb::FileAttribute::kHidden;
  }
  return attributes;
}

// Returns the attribute word of `attributes`.
uint16_t AttributesToDisk(gb::FileAttributes attributes) {
  uint16_t disk_attributes = 0;
  if (attributes.IsSet(gb::FileAttribute::kReadOnly)) {
    disk_attributes |= kDiskAttribReadOnly;
  }
  if (attributes.IsSet(gb::FileAttribute::kHidden)) {
    disk_attributes |= kDiskAttribHidden;
  }
  return disk_attributes;
}

}  // namespace

//==============================================================================
// DiskRawFile
//==============================================================================

// A file open through the protocol: its slot, and a position in bytes. The
// file system's position is set from it before each read and write, as the
// odd byte at either end of a transfer reads or writes its word on its own.
class DiskFileProtocol::DiskRawFile final : public gb::RawFile {
 public:
  // `protocol` must outlive the file.
  DiskRawFile(DiskFileProtocol* protocol, DiskFile file)
      : protocol_(protocol), file_(file) {}
  ~DiskRawFile() override {
    protocol_->file_system_.Close(file_, protocol_->GetNow());
  }

  int64_t SeekEnd() override;
  int64_t SeekTo(int64_t position) override;
  int64_t Write(const void* buffer, int64_t size) override;
  bool Flush() override;
  int64_t Read(void* buffer, int64_t size) override;

 private:
  // Sets `size` to the file's size, in words.
  DiskStatus GetSize(int* size);

  // Reads the word at `index`, which must be in the file, into `word`.
  DiskStatus ReadWord(int index, uint16_t* word);

  // Writes `word` at `index`, which may be the file's end.
  DiskStatus WriteWord(int index, uint16_t word);

  DiskFileProtocol* const protocol_;
  const DiskFile file_;
  int64_t position_ = 0;
};

DiskStatus DiskFileProtocol::DiskRawFile::GetSize(int* size) {
  DiskFileInfo info;
  const DiskStatus status = protocol_->file_system_.GetFileInfo(file_, &info);
  *size = info.size;
  return status;
}

DiskStatus DiskFileProtocol::DiskRawFile::ReadWord(int index, uint16_t* word) {
  DiskFileSystem& file_system = protocol_->file_system_;
  if (DiskStatus status = file_system.Seek(file_, index);
      status != DiskStatus::kOk) {
    return status;
  }
  int count = 0;
  if (DiskStatus status =
          file_system.Read(file_, absl::MakeSpan(word, 1), &count);
      status != DiskStatus::kOk) {
    return status;
  }
  return count == 1 ? DiskStatus::kOk : DiskStatus::kCorrupt;
}

DiskStatus DiskFileProtocol::DiskRawFile::WriteWord(int index, uint16_t word) {
  DiskFileSystem& file_system = protocol_->file_system_;
  if (DiskStatus status = file_system.Seek(file_, index);
      status != DiskStatus::kOk) {
    return status;
  }
  int count = 0;
  return file_system.Write(file_, absl::MakeConstSpan(&word, 1), &count);
}

int64_t DiskFileProtocol::DiskRawFile::SeekEnd() {
  int size = 0;
  if (GetSize(&size) != DiskStatus::kOk) {
    return -1;
  }
  position_ = int64_t{size} * 2;
  return position_;
}

int64_t DiskFileProtocol::DiskRawFile::SeekTo(int64_t position) {
  int size = 0;
  if (position < 0 || GetSize(&size) != DiskStatus::kOk) {
    return -1;
  }
  position_ = std::min(position, int64_t{size} * 2);
  return position_;
}

int64_t DiskFileProtocol::DiskRawFile::Read(void* buffer, int64_t size) {
  DiskFileSystem& file_system = protocol_->file_system_;
  uint8_t* const bytes = static_cast<uint8_t*>(buffer);
  std::array<uint16_t, kDiskSectorWords> words;
  int64_t done = 0;
  while (done < size) {
    const int64_t index = position_ / 2;
    if (file_system.Seek(file_, static_cast<int>(index)) != DiskStatus::kOk) {
      return -1;
    }

    // The words that hold the rest of the bytes asked for, including the
    // word an odd position starts in.
    const int64_t word_count = std::min<int64_t>(
        kDiskSectorWords, (position_ + (size - done) + 1) / 2 - index);
    int count = 0;
    if (file_system.Read(
            file_, absl::MakeSpan(words).first(static_cast<int>(word_count)),
            &count) != DiskStatus::kOk) {
      return -1;
    }
    if (count == 0) {
      break;
    }
    for (int i = 0; i < count && done < size; ++i) {
      for (int byte = static_cast<int>(position_ % 2); byte < 2 && done < size;
           ++byte) {
        bytes[done++] = static_cast<uint8_t>(words[i] >> (byte * 8));
        ++position_;
      }
    }
  }
  return done;
}

int64_t DiskFileProtocol::DiskRawFile::Write(const void* buffer, int64_t size) {
  DiskFileSystem& file_system = protocol_->file_system_;
  const uint8_t* const bytes = static_cast<const uint8_t*>(buffer);
  std::array<uint16_t, kDiskSectorWords> words;
  int64_t done = 0;
  while (done < size) {
    const int index = static_cast<int>(position_ / 2);
    const int64_t left = size - done;

    // A byte alone in its word changes only that byte, keeping the other as
    // it is in the file, or zero past its end.
    if (position_ % 2 != 0 || left == 1) {
      int file_size = 0;
      if (GetSize(&file_size) != DiskStatus::kOk) {
        return done;
      }
      uint16_t word = 0;
      if (index < file_size && ReadWord(index, &word) != DiskStatus::kOk) {
        return done;
      }
      if (position_ % 2 != 0) {
        word = static_cast<uint16_t>((word & 0x00FF) | (bytes[done] << 8));
      } else {
        word = static_cast<uint16_t>((word & 0xFF00) | bytes[done]);
      }
      if (WriteWord(index, word) != DiskStatus::kOk) {
        return done;
      }
      ++done;
      ++position_;
      continue;
    }

    const int word_count =
        static_cast<int>(std::min<int64_t>(kDiskSectorWords, left / 2));
    for (int i = 0; i < word_count; ++i) {
      words[i] = static_cast<uint16_t>(bytes[done + i * 2] |
                                       (bytes[done + i * 2 + 1] << 8));
    }
    if (file_system.Seek(file_, index) != DiskStatus::kOk) {
      return done;
    }
    int count = 0;
    const DiskStatus status = file_system.Write(
        file_, absl::MakeConstSpan(words).first(word_count), &count);
    done += int64_t{count} * 2;
    position_ += int64_t{count} * 2;
    if (status != DiskStatus::kOk || count == 0) {
      return done;
    }
  }
  return done;
}

bool DiskFileProtocol::DiskRawFile::Flush() {
  return protocol_->file_system_.Flush(file_, protocol_->GetNow()) ==
         DiskStatus::kOk;
}

//==============================================================================
// DiskFileProtocol
//==============================================================================

DiskFileProtocol::DiskFileProtocol(Clock clock, int max_open_files)
    : clock_(std::move(clock)),
      file_system_(DiskHardwareSettings{}, max_open_files) {
  CHECK_GE(max_open_files, 2);
}

DiskFileProtocol::~DiskFileProtocol() { Unmount(); }

DiskStatus DiskFileProtocol::Mount(std::unique_ptr<DiskImage> image) {
  CHECK(image != nullptr);
  Unmount();
  if (DiskStatus status = file_system_.AttachImage(std::move(image));
      status != DiskStatus::kOk) {
    return status;
  }
  return OpenRoot();
}

DiskStatus DiskFileProtocol::Unmount(std::unique_ptr<DiskImage>* image) {
  if (!file_system_.HasImage()) {
    return DiskStatus::kNoDisk;
  }
  DiskStatus status = file_system_.GetVolumeStatus();
  if (status == DiskStatus::kOk) {
    status = file_system_.Flush(GetNow());
  } else if (status == DiskStatus::kUnformatted) {
    status = DiskStatus::kOk;
  }
  CloseRoot();
  std::unique_ptr<DiskImage> detached = file_system_.DetachImage();
  if (image != nullptr) {
    *image = std::move(detached);
  }
  return status;
}

DiskStatus DiskFileProtocol::Format(const DiskLabel& label,
                                    int sectors_per_cluster) {
  if (!file_system_.HasImage()) {
    return DiskStatus::kNoDisk;
  }
  CloseRoot();
  const DiskStatus status =
      file_system_.Format(label, sectors_per_cluster, GetNow());
  const DiskStatus root_status = OpenRoot();
  return status != DiskStatus::kOk ? status : root_status;
}

DiskFileSystem* DiskFileProtocol::GetFileSystem() {
  return file_system_.HasImage() ? &file_system_ : nullptr;
}

gb::FileProtocolFlags DiskFileProtocol::GetFlags() const {
  return {gb::FileProtocolFlag::kInfo,       gb::FileProtocolFlag::kList,
          gb::FileProtocolFlag::kFileCreate, gb::FileProtocolFlag::kFileRead,
          gb::FileProtocolFlag::kFileWrite,  gb::FileProtocolFlag::kMove};
}

std::vector<std::string> DiskFileProtocol::GetDefaultNames() const {
  return {std::string(kDefaultName)};
}

std::string DiskFileProtocol::GetCanonicalPath(std::string_view protocol_name,
                                               std::string_view path) const {
  const std::optional<DiskFilename> name = GetName(path);
  if (!name.has_value()) {
    return std::string(path);
  }
  return absl::StrCat("/", name->ToString());
}

gb::FileAttributes DiskFileProtocol::GetAttributeFlags() const {
  return gb::kAllFileAttributes;
}

gb::PathInfo DiskFileProtocol::DoGetPathInfo(std::string_view protocol_name,
                                             std::string_view path) {
  if (!file_system_.HasImage()) {
    return {};
  }
  if (path == "/") {
    return gb::PathInfo(gb::PathType::kFolder);
  }
  const std::optional<DiskFilename> name = GetName(path);
  DiskDirEntry entry;
  if (!name.has_value() || !FindEntry(*name, &entry)) {
    return {};
  }
  return gb::PathInfo(gb::PathType::kFile,
                      int64_t{entry.GetSize().value_or(0)} * 2,
                      AttributesFromDisk(entry.GetAttributes()));
}

std::vector<std::string> DiskFileProtocol::DoList(
    std::string_view protocol_name, std::string_view path,
    std::string_view pattern, gb::FolderMode mode, gb::PathTypes types) {
  std::vector<std::string> paths;
  if (path != "/" || root_ < 0 || !types.IsSet(gb::PathType::kFile) ||
      file_system_.Seek(root_, 0) != DiskStatus::kOk) {
    return paths;
  }

  // Names are upper case, and matched without regard to case.
  const std::string upper_pattern = absl::AsciiStrToUpper(pattern);
  const std::optional<DiskFilename> all = DiskFilename::ParsePattern("*.*");
  DiskDirEntry entry;
  while (file_system_.Find(root_, *all, &entry) == DiskStatus::kOk) {
    const std::string name = entry.GetName()->ToString();
    if (upper_pattern.empty() || gb::PathMatchesPattern(name, upper_pattern)) {
      paths.push_back(absl::StrCat(protocol_name, ":/", name));
    }
  }
  return paths;
}

bool DiskFileProtocol::BasicSetAttributes(std::string_view protocol_name,
                                          std::string_view path,
                                          gb::FileAttributes attributes) {
  const std::optional<DiskFilename> name = GetName(path);
  return name.has_value() &&
         file_system_.SetAttributes(*name, AttributesToDisk(attributes)) ==
             DiskStatus::kOk;
}

bool DiskFileProtocol::BasicDeleteFile(std::string_view protocol_name,
                                       std::string_view path) {
  const std::optional<DiskFilename> name = GetName(path);
  return name.has_value() && file_system_.Delete(*name) == DiskStatus::kOk;
}

bool DiskFileProtocol::BasicMoveFile(std::string_view from_protocol_name,
                                     std::string_view from_path,
                                     std::string_view to_protocol_name,
                                     std::string_view to_path) {
  const std::optional<DiskFilename> from_name = GetName(from_path);
  const std::optional<DiskFilename> to_name = GetName(to_path);
  if (!from_name.has_value() || !to_name.has_value()) {
    return false;
  }

  // Names are stored in upper case, so a move that changes only case is
  // already done.
  if (*from_name == *to_name) {
    return true;
  }
  return file_system_.Rename(*from_name, *to_name) == DiskStatus::kOk;
}

std::unique_ptr<gb::RawFile> DiskFileProtocol::BasicOpenFile(
    std::string_view protocol_name, std::string_view path,
    gb::FileFlags flags) {
  const std::optional<DiskFilename> name = GetName(path);
  if (!name.has_value()) {
    return nullptr;
  }
  const bool write = flags.IsSet(gb::FileFlag::kWrite);
  if (write && file_system_.GetWritableStatus() != DiskStatus::kOk) {
    return nullptr;
  }

  DiskFile file = -1;
  DiskDirEntry entry;
  if (FindEntry(*name, &entry)) {
    if (write && (entry.GetAttributes() & kDiskAttribReadOnly) != 0) {
      return nullptr;
    }
    if (file_system_.Open(*name, &file) != DiskStatus::kOk) {
      return nullptr;
    }
  } else if (!flags.IsSet(gb::FileFlag::kCreate) ||
             file_system_.Create(*name, GetNow(), &file) != DiskStatus::kOk) {
    return nullptr;
  }

  // The file closes with the RawFile, even if it fails to truncate.
  auto raw_file = std::make_unique<DiskRawFile>(this, file);
  if (flags.IsSet(gb::FileFlag::kReset) &&
      file_system_.Truncate(file, GetNow()) != DiskStatus::kOk) {
    return nullptr;
  }
  return raw_file;
}

DateTime DiskFileProtocol::GetNow() {
  return clock_ ? DateTime::FromCivil(clock_()) : DateTime();
}

DiskStatus DiskFileProtocol::OpenRoot() {
  if (file_system_.GetVolumeStatus() != DiskStatus::kOk) {
    return DiskStatus::kOk;
  }
  return file_system_.OpenRoot(&root_);
}

void DiskFileProtocol::CloseRoot() {
  if (root_ < 0) {
    return;
  }
  file_system_.Close(root_, GetNow());
  root_ = -1;
}

bool DiskFileProtocol::FindEntry(const DiskFilename& name,
                                 DiskDirEntry* entry) {
  return root_ >= 0 && file_system_.Seek(root_, 0) == DiskStatus::kOk &&
         file_system_.Find(root_, name, entry) == DiskStatus::kOk;
}

std::optional<DiskFilename> DiskFileProtocol::GetName(std::string_view path) {
  if (path.size() < 2 || path[0] != '/' ||
      path.find('/', 1) != std::string_view::npos) {
    return std::nullopt;
  }
  return DiskFilename::Parse(path.substr(1));
}

}  // namespace oz3
