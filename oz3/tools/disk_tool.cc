// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/tools/disk_tool.h"

#include <cstddef>
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
#include "absl/strings/strip.h"
#include "absl/time/civil_time.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
#include "gb/file/path.h"
#include "oz3/core/date_time.h"
#include "oz3/disk/disk_check.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_media.h"
#include "oz3/disk/disk_status.h"
#include "oz3/tools/disk_file_protocol.h"
#include "oz3/tools/disk_transfer.h"

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
// name the image already, so they are returned as they are.
absl::Status Named(std::string_view name, const absl::Status& status) {
  if (status.ok()) {
    return status;
  }
  return absl::Status(status.code(),
                      absl::StrCat(name, ": ", status.message()));
}

// Returns `status` as an absl::Status with the file `name` before its text.
absl::Status Named(const DiskFilename& name, DiskStatus status) {
  return Named(name.ToString(), ToStatus(status));
}

// Returns a failure if `name` is a pattern, which names no one file.
absl::Status ValidateName(const DiskFilename& name) {
  return name.IsPattern() ? Named(name, DiskStatus::kBadName)
                          : absl::OkStatus();
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

// Returns `time` as "2026-11-17 10:30:15", or nothing with no date.
std::string FormatTime(const DateTime& time) {
  const std::optional<absl::CivilSecond> civil = time.ToCivil();
  if (!civil.has_value()) {
    return "";
  }
  return absl::StrFormat("%04d-%02d-%02d %02d:%02d:%02d", civil->year(),
                         civil->month(), civil->day(), civil->hour(),
                         civil->minute(), civil->second());
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

absl::StatusOr<std::unique_ptr<DiskTool>> DiskTool::Create(
    gb::FileSystem* file_system, Clock clock, std::string_view protocol_name) {
  auto protocol = std::make_unique<DiskFileProtocol>(std::move(clock));
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

//==============================================================================
// File commands
//==============================================================================

absl::StatusOr<std::string> DiskTool::Dir(std::string_view image_path,
                                          const DiskFilename& pattern,
                                          bool all) {
  std::string text;
  if (absl::Status status =
          RunMounted(image_path, Access::kRead,
                     [&] { return DoDir(pattern, all, &text); });
      !status.ok()) {
    return status;
  }
  return text;
}

absl::Status DiskTool::Put(std::string_view image_path,
                           std::string_view host_path,
                           std::optional<DiskFilename> name, bool text) {
  if (!name.has_value()) {
    const std::string normalized_path = gb::NormalizePath(host_path);
    const std::string filename = gb::RemoveFolder(normalized_path);
    name = DiskFilename::Parse(filename);
    if (!name.has_value()) {
      return absl::InvalidArgumentError(
          absl::StrCat(host_path, ": \"", filename,
                       "\" isn't a disk name, so it needs one given"));
    }
  }
  if (absl::Status status = ValidateName(*name); !status.ok()) {
    return status;
  }
  const gb::PathInfo host_info = file_system_->GetPathInfo(host_path);
  if (host_info.type != gb::PathType::kFile) {
    return absl::NotFoundError(absl::StrCat(host_path, ": not a file"));
  }

  std::vector<std::byte> bytes;
  bool read = false;
  if (text) {
    std::string host_text;
    read = file_system_->ReadFile(host_path, &host_text);
    bytes = BytesFromWords(WordsFromText(host_text));
  } else {
    read = file_system_->ReadFile(host_path, &bytes);
  }
  if (!read) {
    return absl::DataLossError(absl::StrCat(host_path, ": can't be read"));
  }

  return RunMounted(image_path, Access::kWrite, [&] {
    return DoPut(*name, bytes, host_info.attributes,
                 GetCopiedAttributes(host_path));
  });
}

absl::Status DiskTool::Get(std::string_view image_path,
                           const DiskFilename& name, std::string_view host_path,
                           bool text) {
  if (absl::Status status = ValidateName(name); !status.ok()) {
    return status;
  }
  const gb::PathInfo host_info = file_system_->GetPathInfo(host_path);
  if (host_info.type == gb::PathType::kFolder) {
    return absl::FailedPreconditionError(
        absl::StrCat(host_path, ": is a folder"));
  }
  if (host_info.type == gb::PathType::kFile &&
      host_info.attributes.IsSet(gb::FileAttribute::kReadOnly)) {
    return absl::FailedPreconditionError(
        absl::StrCat(host_path, ": read-only"));
  }

  std::vector<std::byte> bytes;
  gb::FileAttributes attributes;
  if (absl::Status status =
          RunMounted(image_path, Access::kRead,
                     [&] { return DoGet(name, &bytes, &attributes); });
      !status.ok()) {
    return status;
  }

  const bool written =
      text ? file_system_->WriteFile(host_path,
                                     TextFromWords(WordsFromBytes(bytes)))
           : file_system_->WriteFile(host_path, bytes);
  if (!written) {
    return absl::DataLossError(absl::StrCat(host_path, ": can't be written"));
  }
  const gb::FileAttributes mask = GetCopiedAttributes(host_path);
  if (!mask.IsEmpty() &&
      !file_system_->SetAttributes(host_path, attributes, mask)) {
    return absl::DataLossError(
        absl::StrCat(host_path, ": its attributes can't be set"));
  }
  return absl::OkStatus();
}

absl::Status DiskTool::Delete(std::string_view image_path,
                              const DiskFilename& pattern) {
  return RunMounted(image_path, Access::kWrite,
                    [&] { return DoDelete(pattern); });
}

absl::Status DiskTool::Rename(std::string_view image_path,
                              const DiskFilename& from,
                              const DiskFilename& to) {
  for (const DiskFilename* name : {&from, &to}) {
    if (absl::Status status = ValidateName(*name); !status.ok()) {
      return status;
    }
  }
  return RunMounted(image_path, Access::kWrite,
                    [&] { return DoRename(from, to); });
}

absl::StatusOr<std::string> DiskTool::Attrib(std::string_view image_path,
                                             const DiskFilename& name,
                                             gb::FileAttributes set,
                                             gb::FileAttributes clear) {
  if (absl::Status status = ValidateName(name); !status.ok()) {
    return status;
  }
  if (set.Intersects(clear)) {
    return absl::InvalidArgumentError(
        "An attribute can't be both set and cleared");
  }
  const Access access =
      set.IsEmpty() && clear.IsEmpty() ? Access::kRead : Access::kWrite;
  std::string text;
  if (absl::Status status =
          RunMounted(image_path, access,
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
  if (disk_status != DiskStatus::kIoError) {
    switch (access) {
      case Access::kImage:
        disk_status = DiskStatus::kOk;
        break;
      case Access::kRead:
        disk_status = protocol_->GetVolumeStatus();
        break;
      case Access::kWrite:
        disk_status = protocol_->GetWritableStatus();
        break;
    }
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

absl::Status DiskTool::DoDir(const DiskFilename& pattern, bool all,
                             std::string* text) {
  std::vector<DiskDirEntry> entries;
  if (DiskStatus status = protocol_->FindEntries(pattern, &entries);
      status != DiskStatus::kOk) {
    return ToStatus(status);
  }
  for (const DiskDirEntry& entry : entries) {
    const gb::FileAttributes attributes =
        AttributesFromDisk(entry.GetAttributes());
    if (attributes.IsSet(gb::FileAttribute::kHidden) && !all) {
      continue;
    }
    const std::string line = absl::StrFormat(
        "%-17s %8d  %s  %s", EntryName(entry), entry.GetSize().value_or(0),
        FormatAttributes(attributes), FormatTime(entry.GetModified()));
    absl::StrAppend(text, absl::StripTrailingAsciiWhitespace(line), "\n");
  }
  if (text->empty()) {
    *text = "no files\n";
  }
  return absl::OkStatus();
}

absl::Status DiskTool::DoPut(const DiskFilename& name,
                             absl::Span<const std::byte> bytes,
                             gb::FileAttributes attributes,
                             gb::FileAttributes mask) {
  const std::string path = GetDiskPath(name);
  const bool written =
      file_system_->WriteFile(path, bytes.data(),
                              static_cast<int64_t>(bytes.size())) &&
      (mask.IsEmpty() || file_system_->SetAttributes(path, attributes, mask));
  if (!written) {
    // A read-only file refuses the delete as it refused the write, so it is
    // kept.
    const DiskStatus status = TakeFailure(DiskStatus::kIoError);
    file_system_->DeleteFile(path);
    return Named(name, status);
  }
  return absl::OkStatus();
}

absl::Status DiskTool::DoGet(const DiskFilename& name,
                             std::vector<std::byte>* bytes,
                             gb::FileAttributes* attributes) {
  const std::string path = GetDiskPath(name);
  const gb::PathInfo info = file_system_->GetPathInfo(path);
  if (info.type != gb::PathType::kFile) {
    return Named(name, TakeFailure(DiskStatus::kNotFound));
  }
  if (!file_system_->ReadFile(path, bytes)) {
    return Named(name, TakeFailure(DiskStatus::kIoError));
  }
  *attributes = info.attributes;
  return absl::OkStatus();
}

absl::Status DiskTool::DoDelete(const DiskFilename& pattern) {
  std::vector<DiskDirEntry> entries;
  if (DiskStatus status = protocol_->FindEntries(pattern, &entries);
      status != DiskStatus::kOk) {
    return ToStatus(status);
  }
  if (entries.empty()) {
    return Named(pattern, DiskStatus::kNotFound);
  }

  // Each failure is named, and the rest are still deleted. The first failure
  // gives the code.
  std::vector<std::string> failures;
  absl::StatusCode code = absl::StatusCode::kOk;
  for (const DiskDirEntry& entry : entries) {
    const DiskFilename name = *entry.GetName();
    if (file_system_->DeleteFile(GetDiskPath(name))) {
      continue;
    }
    const DiskStatus status = TakeFailure(DiskStatus::kIoError);
    if (failures.empty()) {
      code = ToStatus(status).code();
    }
    failures.push_back(
        absl::StrCat(name.ToString(), ": ", GetDiskStatusText(status)));
  }
  return absl::Status(code, absl::StrJoin(failures, "; "));
}

absl::Status DiskTool::DoRename(const DiskFilename& from,
                                const DiskFilename& to) {
  const std::string from_path = GetDiskPath(from);
  const std::string to_path = GetDiskPath(to);
  if (!file_system_->IsValidFile(from_path)) {
    return Named(from, TakeFailure(DiskStatus::kNotFound));
  }

  // gb refuses a move onto a file that exists before it reaches the disk, so
  // the protocol has no failure to give.
  if (from != to && file_system_->IsValidPath(to_path)) {
    return Named(to, DiskStatus::kExists);
  }
  if (!file_system_->MoveFile(from_path, to_path)) {
    return Named(from, TakeFailure(DiskStatus::kIoError));
  }
  return absl::OkStatus();
}

absl::Status DiskTool::DoAttrib(const DiskFilename& name,
                                gb::FileAttributes set,
                                gb::FileAttributes clear, std::string* text) {
  const gb::FileAttributes mask = gb::Union(set, clear);
  const std::string path = GetDiskPath(name);
  if (!file_system_->IsValidFile(path)) {
    return Named(name, TakeFailure(DiskStatus::kNotFound));
  }
  if (!mask.IsEmpty() && !file_system_->SetAttributes(path, set, mask)) {
    return Named(name, TakeFailure(DiskStatus::kIoError));
  }
  *text = absl::StrCat(
      name.ToString(), " ",
      FormatAttributes(file_system_->GetPathInfo(path).attributes), "\n");
  return absl::OkStatus();
}

DiskStatus DiskTool::TakeFailure(DiskStatus otherwise) {
  const DiskStatus status = protocol_->TakeLastFailure();
  return status != DiskStatus::kOk ? status : otherwise;
}

std::string DiskTool::GetDiskPath(const DiskFilename& name) const {
  return absl::StrCat(protocol_name_, ":/", name.ToString());
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
