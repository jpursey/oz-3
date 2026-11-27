// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/disk_tool.h"

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/functional/function_ref.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_join.h"
#include "absl/time/time.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
#include "gb/file/path.h"
#include "oz3/disk/disk_check.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_file_protocol.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_media.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/disk_superblock.h"
#include "oz3/disk/disk_transfer.h"
#include "oz3/program/bare_program.h"
#include "oz3/program/program.h"
#include "oz3/program/program_file.h"

namespace oz3 {

namespace {

constexpr int64_t kKiloWords = 1024;
constexpr int64_t kMegaWords = 1024 * 1024;

// Returns `status` as an absl::Status, with its text as the message.
absl::Status ToStatus(DiskStatus status) {
  const std::string_view text = GetDiskStatusText(status);
  switch (status) {
    case DiskStatus::kOk:
      return absl::OkStatus();
    case DiskStatus::kBadCommand:
    case DiskStatus::kBadHandle:
    case DiskStatus::kBadName:
    case DiskStatus::kBadPattern:
    case DiskStatus::kBadLabel:
    case DiskStatus::kBadAttributes:
    case DiskStatus::kBadArgument:
    case DiskStatus::kPastEnd:
    case DiskStatus::kWrongHandleType:
      return absl::InvalidArgumentError(text);
    case DiskStatus::kNotFound:
      return absl::NotFoundError(text);
    case DiskStatus::kExists:
      return absl::AlreadyExistsError(text);
    case DiskStatus::kTooManyOpen:
    case DiskStatus::kDiskFull:
      return absl::ResourceExhaustedError(text);
    case DiskStatus::kInUse:
    case DiskStatus::kReadOnly:
    case DiskStatus::kDiskChanged:
    case DiskStatus::kNoDisk:
    case DiskStatus::kWriteProtected:
    case DiskStatus::kUnformatted:
      return absl::FailedPreconditionError(text);
    case DiskStatus::kCorrupt:
    case DiskStatus::kIoError:
      return absl::DataLossError(text);
  }
  return absl::UnknownError(text);
}

// Returns `status` with `name` before its message. DiskImage's own errors
// name the image already, so they aren't passed through this.
absl::Status Named(std::string_view name, const absl::Status& status) {
  if (status.ok()) {
    return status;
  }
  return absl::Status(status.code(),
                      absl::StrCat(name, ": ", status.message()));
}

// Returns `words` with a K or M suffix if it is a whole number of them.
std::string FormatWords(int64_t words) {
  if (words != 0 && words % kMegaWords == 0) {
    return absl::StrCat(words / kMegaWords, "M words");
  }
  if (words != 0 && words % kKiloWords == 0) {
    return absl::StrCat(words / kKiloWords, "K words");
  }
  return absl::StrCat(words, " words");
}

// Returns "1 <singular>" or "<count> <plural>".
std::string Count(int count, std::string_view singular,
                  std::string_view plural) {
  return absl::StrCat(count, " ", count == 1 ? singular : plural);
}

// Returns the attribute letters Dir and Attrib show: r and h, or - for each not
// set.
std::string FormatAttributes(gb::FileAttributes attributes) {
  return absl::StrCat(
      attributes.IsSet(gb::FileAttribute::kReadOnly) ? "r" : "-",
      attributes.IsSet(gb::FileAttribute::kHidden) ? "h" : "-");
}

// Returns the name of `entry` as a person reads it.
std::string EntryName(const DiskDirEntry& entry) {
  const std::optional<DiskFilename> name = entry.GetName();
  return name.has_value() ? name->ToString() : "(bad name)";
}

// Returns what is wrong with a file's chain, as a person reads it.
std::string_view ChainProblemText(DiskCheckReport::ChainProblem problem) {
  switch (problem) {
    case DiskCheckReport::ChainProblem::kLong:
      return "chain longer than its size";
    case DiskCheckReport::ChainProblem::kShort:
      return "chain shorter than its size";
    case DiskCheckReport::ChainProblem::kBroken:
      return "broken chain";
    case DiskCheckReport::ChainProblem::kCrossLinked:
      return "cross-linked";
  }
  return "unknown problem";
}

// Returns a line for each problem in `report`, or "no problems", and after
// repairing, a last line saying so.
std::string FormatReport(const DiskCheckReport& report, bool repaired) {
  std::string text;
  if (report.broken_root) {
    absl::StrAppend(&text, "root directory: broken chain\n");
  }
  if (report.broken_free_list) {
    absl::StrAppend(&text, "free list: broken\n");
  }
  if (report.orphaned_clusters > 0) {
    absl::StrAppend(&text,
                    Count(report.orphaned_clusters, "cluster", "clusters"),
                    " on no chain\n");
  }
  for (const DiskCheckReport::FileProblem& problem : report.file_problems) {
    absl::StrAppend(&text, EntryName(problem.entry), ": ",
                    ChainProblemText(problem.problem), "\n");
  }
  if (!report.bad_names.empty()) {
    absl::StrAppend(
        &text,
        Count(static_cast<int>(report.bad_names.size()), "entry", "entries"),
        " with a bad name\n");
  }
  for (const DiskFilename& name : report.duplicate_names) {
    absl::StrAppend(&text, name.ToString(), ": more than one entry\n");
  }
  if (text.empty()) {
    return "no problems\n";
  }

  // RepairDisk fixes everything but duplicate names, as only renaming or
  // deleting one of them fixes it.
  const bool repairs = report.broken_root || report.broken_free_list ||
                       report.orphaned_clusters > 0 ||
                       !report.file_problems.empty() ||
                       !report.bad_names.empty();
  if (repaired && repairs) {
    absl::StrAppend(&text, "repaired\n");
  }
  return text;
}

}  // namespace

//==============================================================================
// Construction / Destruction
//==============================================================================

absl::StatusOr<std::unique_ptr<DiskTool>> DiskTool::Create(Contract contract) {
  gb::ValidatedContext context(std::move(contract));
  if (!context.IsValid()) {
    return absl::InvalidArgumentError(
        "The context doesn't meet the disk tool's contract");
  }
  gb::FileSystem* const file_system = context.GetPtr<gb::FileSystem>();
  const std::string protocol_name =
      context.GetValue<std::string>(kKeyProtocolName);
  auto protocol = std::make_unique<DiskFileProtocol>(context);
  DiskFileProtocol* const protocol_ptr = protocol.get();
  if (!file_system->Register(std::move(protocol), std::string(protocol_name))) {
    return absl::InvalidArgumentError(absl::StrCat(
        "The disk protocol can't be registered as \"", protocol_name, "\""));
  }
  return std::unique_ptr<DiskTool>(
      new DiskTool(file_system, protocol_ptr, protocol_name));
}

//==============================================================================
// Arguments
//==============================================================================

absl::StatusOr<int> DiskTool::ParseSectorCount(std::string_view text) {
  std::string_view digits = text;
  int64_t multiplier = 1;
  if (!digits.empty()) {
    const char suffix = absl::ascii_toupper(digits.back());
    if (suffix == 'K') {
      multiplier = kKiloWords;
      digits.remove_suffix(1);
    } else if (suffix == 'M') {
      multiplier = kMegaWords;
      digits.remove_suffix(1);
    }
  }
  int64_t size = 0;
  if (!absl::c_all_of(digits, absl::ascii_isdigit) ||
      !absl::SimpleAtoi(digits, &size)) {
    return absl::InvalidArgumentError(
        absl::StrCat("Size \"", text,
                     "\" isn't a number of words, with an optional K or M"));
  }

  constexpr int64_t kMaxWords =
      int64_t{DiskMedia::kMaxSectorCount} * kDiskSectorWords;
  if (size == 0 || size > kMaxWords / multiplier) {
    return absl::InvalidArgumentError(
        absl::StrCat("Size ", text, " is out of range: an image has 1 to ",
                     DiskMedia::kMaxSectorCount, " sectors of ",
                     kDiskSectorWords, " words"));
  }
  const int64_t words = size * multiplier;
  if (words % kDiskSectorWords != 0) {
    return absl::InvalidArgumentError(
        absl::StrCat("Size ", text, " isn't a whole number of sectors of ",
                     kDiskSectorWords, " words"));
  }
  return static_cast<int>(words / kDiskSectorWords);
}

absl::StatusOr<gb::FileAttributes> DiskTool::ParseAttributes(
    std::string_view letters) {
  gb::FileAttributes attributes;
  for (const char letter : letters) {
    switch (absl::ascii_tolower(letter)) {
      case 'r':
        attributes += gb::FileAttribute::kReadOnly;
        break;
      case 'h':
        attributes += gb::FileAttribute::kHidden;
        break;
      default:
        return absl::InvalidArgumentError(
            absl::StrCat("Attributes \"", letters,
                         "\" can only hold r (read-only) and h (hidden)"));
    }
  }
  return attributes;
}

absl::StatusOr<DiskLabel> DiskTool::ParseLabel(std::string_view text) {
  std::optional<DiskLabel> label = DiskLabel::Parse(text);
  if (!label.has_value()) {
    return absl::InvalidArgumentError(
        absl::StrCat("Label \"", text, "\" isn't up to ", DiskLabel::kMaxLength,
                     " letters, digits, and ! # $ % & ' ( ) - @ ^ _ ` { } ~"));
  }
  return *label;
}

//==============================================================================
// Image commands
//==============================================================================

absl::Status DiskTool::New(std::string_view image_path, int sector_count) {
  if (file_system_->GetPathInfo(image_path).type != gb::PathType::kInvalid) {
    return absl::AlreadyExistsError(
        absl::StrCat(image_path, ": already exists"));
  }
  return DiskImage::Create(file_system_, image_path, sector_count).status();
}

absl::Status DiskTool::Format(std::string_view image_path,
                              const DiskLabel& label, int sectors_per_cluster) {
  return RunMounted(image_path, Access::kImage,
                    [&] { return DoFormat(label, sectors_per_cluster); });
}

absl::StatusOr<std::string> DiskTool::Info(std::string_view image_path) {
  std::string text;
  if (absl::Status status =
          RunMounted(image_path, Access::kImage, [&] { return DoInfo(&text); });
      !status.ok()) {
    return status;
  }
  return text;
}

absl::Status DiskTool::Label(std::string_view image_path,
                             const DiskLabel& label) {
  return RunMounted(image_path, Access::kImage,
                    [&] { return ToStatus(protocol_->SetLabel(label)); });
}

absl::Status DiskTool::Protect(std::string_view image_path,
                               bool write_protected) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(file_system_, image_path);
  if (!image.ok()) {
    return image.status();
  }
  const DiskStatus status = (*image)->SetWriteProtected(write_protected);
  if (status == DiskStatus::kWriteProtected) {
    return absl::FailedPreconditionError(absl::StrCat(
        image_path,
        ": the image file is read-only, so it is always write protected"));
  }
  return Named(image_path, ToStatus(status));
}

absl::StatusOr<std::string> DiskTool::Check(std::string_view image_path,
                                            bool fix) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(file_system_, image_path);
  if (!image.ok()) {
    return image.status();
  }
  DiskCheckReport report;
  const DiskStatus status =
      fix ? RepairDisk(**image, &report) : CheckDisk(**image, &report);
  if (status != DiskStatus::kOk) {
    return Named(image_path, ToStatus(status));
  }
  return FormatReport(report, fix);
}

absl::Status DiskTool::Boot(std::string_view image_path,
                            std::string_view program_path) {
  const gb::PathType path_type = file_system_->GetPathInfo(program_path).type;
  if (path_type == gb::PathType::kInvalid) {
    return absl::NotFoundError(absl::StrCat(program_path, ": not found"));
  }
  if (path_type != gb::PathType::kFile) {
    return absl::InvalidArgumentError(
        absl::StrCat(program_path, ": not a file"));
  }
  absl::StatusOr<BareProgram> program =
      ReadBareProgramFile(*file_system_, program_path);
  if (!program.ok()) {
    return Named(program_path, program.status());
  }
  if (program->contents_words > DiskSuperblock::kBootAreaWords) {
    return absl::InvalidArgumentError(absl::StrCat(
        program_path, ": its contents are ", program->contents_words,
        " words, but the boot area holds ", DiskSuperblock::kBootAreaWords));
  }

  // A valid bare program's blocks are all within its contents.
  DiskSuperblock::BootArea words(DiskSuperblock::kBootAreaWords, 0);
  for (const Program::Segment::Data& block : program->data) {
    absl::c_copy(block.data, words.begin() + block.offset);
  }
  return RunMounted(image_path, Access::kImage,
                    [&] { return ToStatus(protocol_->SetBootArea(words)); });
}

//==============================================================================
// File commands
//==============================================================================

absl::StatusOr<std::string> DiskTool::Dir(std::string_view image_path,
                                          std::string_view pattern, bool all) {
  std::string text;
  if (absl::Status status =
          RunMounted(image_path, Access::kFiles,
                     [&] { return DoDir(pattern, all, &text); });
      !status.ok()) {
    return status;
  }
  return text;
}

absl::Status DiskTool::Put(std::string_view image_path,
                           std::string_view host_path, std::string_view name,
                           bool text) {
  const gb::PathInfo host_info = file_system_->GetPathInfo(host_path);
  if (host_info.type == gb::PathType::kInvalid) {
    return absl::NotFoundError(absl::StrCat(host_path, ": not found"));
  }
  if (host_info.type != gb::PathType::kFile) {
    return absl::InvalidArgumentError(absl::StrCat(host_path, ": not a file"));
  }
  std::string default_name;
  if (name.empty()) {
    default_name = gb::RemoveFolder(gb::NormalizePath(host_path));
    if (!DiskFilename::Parse(default_name).has_value()) {
      return absl::InvalidArgumentError(
          absl::StrCat(host_path, ": \"", default_name,
                       "\" isn't a valid disk name, so it needs one given"));
    }
    name = default_name;
  } else if (!DiskFilename::Parse(name).has_value()) {
    return Named(name, ToStatus(DiskStatus::kBadName));
  }

  std::string bytes;
  if (!file_system_->ReadFile(host_path, &bytes)) {
    return Named(host_path, absl::UnknownError("can't be read"));
  }
  if (text) {
    bytes = DiskTextFromHost(bytes);
  }

  return RunMounted(image_path, Access::kFiles, [&] {
    return DoPut(name, bytes, host_info.attributes,
                 GetCopiedAttributes(host_path));
  });
}

absl::Status DiskTool::Get(std::string_view image_path, std::string_view name,
                           std::string_view host_path, bool text) {
  // Without a host path, or with a folder, the host file is named as the disk
  // names it. A name that doesn't parse fails to be found before anything is
  // written, so it needs no host file.
  std::string named_path;
  if (host_path.empty() ||
      file_system_->GetPathInfo(host_path).type == gb::PathType::kFolder) {
    if (const std::optional<DiskFilename> filename = DiskFilename::Parse(name);
        filename.has_value()) {
      named_path =
          gb::JoinPath(gb::NormalizePath(host_path), filename->ToString());
      host_path = named_path;
    }
  }

  std::string bytes;
  gb::FileAttributes attributes;
  if (absl::Status status =
          RunMounted(image_path, Access::kFiles,
                     [&] { return DoGet(name, &bytes, &attributes); });
      !status.ok()) {
    return status;
  }

  if (text) {
    bytes = HostTextFromDisk(bytes);
  }
  if (!file_system_->WriteFile(host_path, bytes)) {
    return Named(host_path, absl::UnknownError("can't be written"));
  }
  const gb::FileAttributes mask = GetCopiedAttributes(host_path);
  if (!mask.IsEmpty() &&
      !file_system_->SetAttributes(host_path, attributes, mask)) {
    return Named(host_path, absl::UnknownError("its attributes can't be set"));
  }
  return absl::OkStatus();
}

absl::Status DiskTool::Delete(std::string_view image_path,
                              std::string_view pattern) {
  if (pattern.empty()) {
    return absl::InvalidArgumentError(
        "A pattern is needed, such as * for every file");
  }
  return RunMounted(image_path, Access::kFiles,
                    [&] { return DoDelete(pattern); });
}

absl::Status DiskTool::Rename(std::string_view image_path,
                              std::string_view from, std::string_view to) {
  return RunMounted(image_path, Access::kFiles,
                    [&] { return DoRename(from, to); });
}

absl::StatusOr<std::string> DiskTool::Attrib(std::string_view image_path,
                                             std::string_view name,
                                             gb::FileAttributes set,
                                             gb::FileAttributes clear) {
  if (set.Intersects(clear)) {
    return absl::InvalidArgumentError(
        "An attribute can't be both set and cleared");
  }
  std::string text;
  if (absl::Status status =
          RunMounted(image_path, Access::kFiles,
                     [&] { return DoAttrib(name, set, clear, &text); });
      !status.ok()) {
    return status;
  }
  return text;
}

//==============================================================================
// Implementation
//==============================================================================

absl::Status DiskTool::RunMounted(std::string_view image_path, Access access,
                                  absl::FunctionRef<absl::Status()> command) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(file_system_, image_path);
  if (!image.ok()) {
    return image.status();
  }

  // An unformatted or corrupt disk is still mounted, for the command to
  // format, or to report on.
  DiskStatus disk_status = protocol_->Mount(*std::move(image));
  if (access == Access::kImage && disk_status != DiskStatus::kIoError) {
    disk_status = DiskStatus::kOk;
  }
  const absl::Status status =
      disk_status == DiskStatus::kOk ? command() : ToStatus(disk_status);
  const absl::Status unmount_status = ToStatus(protocol_->Unmount());
  return Named(image_path, status.ok() ? unmount_status : status);
}

absl::Status DiskTool::DoFormat(const DiskLabel& label,
                                int sectors_per_cluster) {
  const int sector_count = protocol_->GetMedia()->GetSectorCount();
  if (!ChooseDiskLayout(sector_count, sectors_per_cluster).has_value()) {
    return absl::InvalidArgumentError(
        absl::StrCat("a disk of ", Count(sector_count, "sector", "sectors"),
                     " can't have clusters of ",
                     Count(sectors_per_cluster, "sector", "sectors")));
  }
  return ToStatus(protocol_->Format(label, sectors_per_cluster));
}

absl::Status DiskTool::DoInfo(std::string* text) {
  const DiskMedia* const media = protocol_->GetMedia();
  const int sector_count = media->GetSectorCount();
  *text = absl::StrCat(
      "Size:     ", FormatWords(int64_t{sector_count} * kDiskSectorWords), " (",
      Count(sector_count, "sector", "sectors"), ")\n",
      "Protect:  ", media->IsWriteProtected() ? "on" : "off", "\n");

  DiskVolumeInfo info;
  const DiskStatus status = protocol_->GetVolumeInfo(&info);
  if (status == DiskStatus::kUnformatted) {
    absl::StrAppend(text, "Volume:   unformatted\n");
    return absl::OkStatus();
  }
  if (status == DiskStatus::kCorrupt) {
    absl::StrAppend(text, "Volume:   corrupt\n");
    return absl::OkStatus();
  }
  if (status != DiskStatus::kOk) {
    return ToStatus(status);
  }
  const std::string label = info.label.ToString();
  const int64_t cluster_words =
      int64_t{info.sectors_per_cluster} * kDiskSectorWords;
  absl::StrAppend(text, "Volume:   ", label.empty() ? "(no label)" : label,
                  "\n", "Cluster:  ", FormatWords(cluster_words), " (",
                  Count(info.sectors_per_cluster, "sector", "sectors"), ")\n",
                  "Clusters: ", info.cluster_count, "\n",
                  "Free:     ", info.free_clusters, " (",
                  FormatWords(info.free_clusters * cluster_words), ")\n");
  return absl::OkStatus();
}

absl::Status DiskTool::DoDir(std::string_view pattern, bool all,
                             std::string* text) {
  const absl::TimeZone time_zone = protocol_->GetTimeZone();
  for (const std::string& path :
       file_system_->ListFiles(GetDiskPath(""), pattern)) {
    const gb::PathInfo info = file_system_->GetPathInfo(path);
    if (info.attributes.IsSet(gb::FileAttribute::kHidden) && !all) {
      continue;
    }
    absl::StrAppend(text, absl::StrFormat("%-17s %8d  %s",
                                          gb::RemoveFolder(path), info.size / 2,
                                          FormatAttributes(info.attributes)));
    if (info.modified_time.has_value()) {
      absl::StrAppend(
          text, "  ",
          absl::FormatTime("%Y-%m-%d %H:%M", *info.modified_time, time_zone));
    }
    absl::StrAppend(text, "\n");
  }
  if (text->empty()) {
    *text = "no files\n";
  }
  return absl::OkStatus();
}

absl::Status DiskTool::DoPut(std::string_view name, std::string_view bytes,
                             gb::FileAttributes attributes,
                             gb::FileAttributes mask) {
  const std::string path = GetDiskPath(name);
  if (!file_system_->WriteFile(path, bytes)) {
    return Named(name, absl::UnknownError("can't be written"));
  }
  if (!mask.IsEmpty() && !file_system_->SetAttributes(path, attributes, mask)) {
    return Named(name, absl::UnknownError("its attributes can't be set"));
  }
  return absl::OkStatus();
}

absl::Status DiskTool::DoGet(std::string_view name, std::string* bytes,
                             gb::FileAttributes* attributes) {
  // No file has a name that isn't a disk name, though the file system would
  // find one for a path such as ./HELLO.TXT.
  const std::string path = GetDiskPath(name);
  const gb::PathInfo info = file_system_->GetPathInfo(path);
  if (!DiskFilename::Parse(name).has_value() ||
      info.type != gb::PathType::kFile) {
    return Named(name, ToStatus(DiskStatus::kNotFound));
  }
  if (!file_system_->ReadFile(path, bytes)) {
    return Named(name, absl::UnknownError("can't be read"));
  }
  *attributes = info.attributes;
  return absl::OkStatus();
}

absl::Status DiskTool::DoDelete(std::string_view pattern) {
  const std::vector<std::string> paths =
      file_system_->ListFiles(GetDiskPath(""), pattern);
  if (paths.empty()) {
    return Named(pattern, ToStatus(DiskStatus::kNotFound));
  }

  // Each failure is named, and the rest are still deleted.
  std::vector<std::string> failures;
  for (const std::string& path : paths) {
    if (!file_system_->DeleteFile(path)) {
      failures.push_back(
          absl::StrCat(gb::RemoveFolder(path), ": can't be deleted"));
    }
  }
  if (failures.empty()) {
    return absl::OkStatus();
  }
  return absl::UnknownError(absl::StrJoin(failures, "; "));
}

absl::Status DiskTool::DoRename(std::string_view from, std::string_view to) {
  // No file has a name that isn't a disk name.
  const std::optional<DiskFilename> from_name = DiskFilename::Parse(from);
  const std::string from_path = GetDiskPath(from);
  if (!from_name.has_value() || !file_system_->IsValidFile(from_path)) {
    return Named(from, ToStatus(DiskStatus::kNotFound));
  }
  const std::optional<DiskFilename> to_name = DiskFilename::Parse(to);
  if (!to_name.has_value()) {
    return Named(to, ToStatus(DiskStatus::kBadName));
  }

  // Renaming a file to its own name, in another case, isn't a move onto
  // another file.
  const std::string to_path = GetDiskPath(to);
  if (*from_name != *to_name && file_system_->IsValidPath(to_path)) {
    return Named(to, ToStatus(DiskStatus::kExists));
  }
  if (!file_system_->MoveFile(from_path, to_path)) {
    return Named(from, absl::UnknownError("can't be renamed"));
  }
  return absl::OkStatus();
}

absl::Status DiskTool::DoAttrib(std::string_view name, gb::FileAttributes set,
                                gb::FileAttributes clear, std::string* text) {
  // No file has a name that isn't a disk name.
  const std::optional<DiskFilename> disk_name = DiskFilename::Parse(name);
  const std::string path = GetDiskPath(name);
  if (!disk_name.has_value() || !file_system_->IsValidFile(path)) {
    return Named(name, ToStatus(DiskStatus::kNotFound));
  }
  const gb::FileAttributes mask = gb::Union(set, clear);
  if (!mask.IsEmpty() && !file_system_->SetAttributes(path, set, mask)) {
    return Named(name, absl::UnknownError("its attributes can't be set"));
  }
  *text = absl::StrCat(
      disk_name->ToString(), " ",
      FormatAttributes(file_system_->GetPathInfo(path).attributes), "\n");
  return absl::OkStatus();
}

std::string DiskTool::GetDiskPath(std::string_view name) const {
  return absl::StrCat(protocol_name_, ":/", name);
}

gb::FileAttributes DiskTool::GetCopiedAttributes(
    std::string_view host_path) const {
  const std::string normalized_path = gb::NormalizePath(host_path);
  std::string_view protocol_name;
  gb::RemoveProtocol(normalized_path, &protocol_name);
  if (protocol_name.empty()) {
    protocol_name = file_system_->GetDefaultProtocolName();
  }
  return gb::Intersect(file_system_->GetAttributeFlags(protocol_name),
                       protocol_->GetAttributeFlags());
}

}  // namespace oz3
