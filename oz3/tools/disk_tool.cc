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

#include "absl/algorithm/container.h"
#include "absl/functional/function_ref.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/ascii.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_cat.h"
#include "gb/file/file_system.h"
#include "gb/file/file_types.h"
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
    case DiskStatus::kIoError:
      return absl::DataLossError(text);
    default:
      return absl::FailedPreconditionError(text);
  }
}

// Returns `status` with `name` before its message.
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

  // Repairing leaves duplicate names, as only renaming or deleting one of
  // them fixes it.
  const bool repairs = !text.empty();
  for (const DiskFilename& name : report.duplicate_names) {
    absl::StrAppend(&text, name.ToString(), ": more than one entry\n");
  }
  if (text.empty()) {
    return "no problems\n";
  }
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
  return std::unique_ptr<DiskTool>(new DiskTool(file_system, protocol_ptr));
}

//==============================================================================
// Arguments
//==============================================================================

absl::StatusOr<int> DiskTool::ParseSize(std::string_view text) {
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
  int64_t count = 0;
  if (digits.empty() || !absl::c_all_of(digits, absl::ascii_isdigit) ||
      !absl::SimpleAtoi(digits, &count)) {
    return absl::InvalidArgumentError(
        absl::StrCat("Size \"", text,
                     "\" isn't a number of words, with an optional K or M"));
  }

  constexpr int64_t kMaxWords =
      int64_t{DiskMedia::kMaxSectorCount} * kDiskSectorWords;
  if (count == 0 || count > kMaxWords / multiplier) {
    return absl::InvalidArgumentError(
        absl::StrCat("Size ", text, " is out of range: an image has 1 to ",
                     DiskMedia::kMaxSectorCount, " sectors of ",
                     kDiskSectorWords, " words"));
  }
  const int64_t words = count * multiplier;
  if (words % kDiskSectorWords != 0) {
    return absl::InvalidArgumentError(
        absl::StrCat("Size ", text, " isn't a whole number of sectors of ",
                     kDiskSectorWords, " words"));
  }
  return static_cast<int>(words / kDiskSectorWords);
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
  return RunMounted(image_path,
                    [&] { return DoFormat(label, sectors_per_cluster); });
}

absl::StatusOr<std::string> DiskTool::Info(std::string_view image_path) {
  std::string text;
  if (absl::Status status =
          RunMounted(image_path, [&] { return DoInfo(&text); });
      !status.ok()) {
    return status;
  }
  return text;
}

absl::Status DiskTool::Label(std::string_view image_path,
                             const DiskLabel& label) {
  return RunMounted(image_path, [&] {
    return ToStatus(protocol_->GetFileSystem()->SetLabel(label));
  });
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
// Implementation
//==============================================================================

absl::Status DiskTool::RunMounted(std::string_view image_path,
                                  absl::FunctionRef<absl::Status()> command) {
  absl::StatusOr<std::unique_ptr<DiskImage>> image =
      DiskImage::Open(file_system_, image_path);
  if (!image.ok()) {
    return image.status();
  }

  // An unformatted or corrupt disk is still mounted, for the command to
  // format, or to report on.
  absl::Status status;
  if (protocol_->Mount(*std::move(image)) == DiskStatus::kIoError) {
    status = ToStatus(DiskStatus::kIoError);
  } else {
    status = command();
  }
  const absl::Status unmount_status = ToStatus(protocol_->Unmount());
  return Named(image_path, status.ok() ? unmount_status : status);
}

absl::Status DiskTool::DoFormat(const DiskLabel& label,
                                int sectors_per_cluster) {
  const int sector_count =
      protocol_->GetFileSystem()->GetMedia()->GetSectorCount();
  if (!ChooseDiskLayout(sector_count, sectors_per_cluster).has_value()) {
    return absl::InvalidArgumentError(
        absl::StrCat("a disk of ", Count(sector_count, "sector", "sectors"),
                     " can't have clusters of ",
                     Count(sectors_per_cluster, "sector", "sectors")));
  }
  return ToStatus(protocol_->Format(label, sectors_per_cluster));
}

absl::Status DiskTool::DoInfo(std::string* text) {
  DiskFileSystem* const file_system = protocol_->GetFileSystem();
  const DiskMedia* const media = file_system->GetMedia();
  const int sector_count = media->GetSectorCount();
  *text = absl::StrCat(
      "Size:     ", FormatWords(int64_t{sector_count} * kDiskSectorWords), " (",
      Count(sector_count, "sector", "sectors"), ")\n",
      "Protect:  ", media->IsWriteProtected() ? "on" : "off", "\n");

  DiskVolumeInfo info;
  const DiskStatus status = file_system->GetVolumeInfo(&info);
  if (status == DiskStatus::kUnformatted) {
    absl::StrAppend(text, "Volume:   unformatted\n");
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

}  // namespace oz3
