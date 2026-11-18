// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <bit>
#include <cstdint>
#include <cstdio>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "gb/base/context.h"
#include "gb/base/validated_context.h"
#include "gb/cli/cli.h"
#include "gb/cli/cli_command_line.h"
#include "gb/cli/cli_path.h"
#include "gb/cli/cli_validators.h"
#include "gb/cli/cli_writer.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/tools/disk_tool.h"

namespace oz3 {
namespace {

//==============================================================================
// Validators
//
// Sizes, cluster sizes, labels, and attributes are checked as the command line
// is parsed, so a bad one is a usage error before the image is touched. The run
// functions parse them again, knowing they parse.
//==============================================================================

absl::Status ValidateSize(const gb::ValidatedContext& context,
                          const std::string_view& value) {
  return DiskTool::ParseSectorCount(value).status();
}

absl::Status ValidateCluster(const gb::ValidatedContext& context,
                             const int64_t& value) {
  if (value < 1 || value > DiskLayout::kMaxSectorsPerCluster ||
      !std::has_single_bit(static_cast<uint64_t>(value))) {
    return absl::InvalidArgumentError(
        absl::StrCat(value, " isn't a power of two from 1 to ",
                     DiskLayout::kMaxSectorsPerCluster));
  }
  return absl::OkStatus();
}

absl::Status ValidateLabel(const gb::ValidatedContext& context,
                           const std::string_view& value) {
  return DiskTool::ParseLabel(value).status();
}

absl::Status ValidateAttributes(const gb::ValidatedContext& context,
                                const std::string_view& value) {
  return DiskTool::ParseAttributes(value).status();
}

// get's host file is a folder to copy into, or a file that can be written.
absl::Status ValidateGetHostFile(const gb::ValidatedContext& context,
                                 const std::string_view& value) {
  if (gb::kCliFolderExists.validate(context, value).ok()) {
    return absl::OkStatus();
  }
  return gb::kCliFileWritable.validate(context, value);
}

const gb::CliValidator<std::string_view> kSizeValidator = {
    .validate = ValidateSize,
};
const gb::CliValidator<int64_t> kClusterValidator = {
    .validate = ValidateCluster,
};
const gb::CliValidator<std::string_view> kLabelValidator = {
    .validate = ValidateLabel,
};
const gb::CliValidator<std::string_view> kAttributesValidator = {
    .validate = ValidateAttributes,
};

// Its constraints are those of gb::kCliFolderExists and gb::kCliFileWritable,
// which it calls.
const gb::CliValidator<std::string_view> kGetHostFileValidator = {
    .validate = ValidateGetHostFile,
    .constraints = {&gb::kConstraintCliFileSystem,
                    &gb::kConstraintCliFileProtocol},
};

//==============================================================================
// Arguments and flags
//==============================================================================

const gb::CliArg<std::string_view> kImage = {
    .name = "image",
    .help = "The disk image (.ozd).",
};

const gb::CliArg<std::string_view> kSize = {
    .name = "size",
    .help =
        "The disk's size in words, with an optional K (1,024 words) or M "
        "(1,048,576 words) suffix, such as 720K. It must be a whole number "
        "of 256 word sectors.",
    .validators = {&kSizeValidator},
};
const gb::CliFlag<int64_t> kCluster = {
    .name = "cluster",
    .short_name = 'c',
    .value_name = "sectors",
    .help =
        "The sectors in each cluster: 1, 2, 4, and so on up to 64. Defaults "
        "to the drive's choice, the fewest that cover the disk.",
    .validators = {&kClusterValidator},
};
const gb::CliFlag<std::string_view> kFormatLabel = {
    .name = "label",
    .short_name = 'l',
    .help = "The volume label. Defaults to none.",
    .validators = {&kLabelValidator},
};
const gb::CliArg<std::string_view> kLabel = {
    .name = "label",
    .help = "The volume label.",
    .validators = {&kLabelValidator},
};
const gb::CliArg<std::string_view> kPattern = {
    .name = "pattern",
    .help =
        "Which files, where * matches any characters, in any case, such as "
        "*.OZ3.",
};
const gb::CliArg<std::string_view> kDirPattern = {
    .name = "pattern",
    .help =
        "Which files, where * matches any characters, in any case, such as "
        "*.OZ3. Defaults to every file.",
    .optional = true,
};
const gb::CliFlag<bool> kAll = {
    .name = "all",
    .short_name = 'a',
    .help = "Lists hidden files too.",
};
const gb::CliFlag<bool> kText = {
    .name = "text",
    .short_name = 't',
    .help =
        "Copies the file as text. Going in, each CRLF and CR becomes LF. "
        "Coming out, zero bytes are dropped.",
};
const gb::CliArg<std::string_view> kPutHostFile = {
    .name = "host file",
    .help = "The host file to copy onto the disk.",
    .validators = {&gb::kCliFileExists},
};
const gb::CliArg<std::string_view> kPutName = {
    .name = "name",
    .help =
        "The file's name on the disk, such as HELLO.OZ3. Defaults to the "
        "host file's name.",
    .optional = true,
};
const gb::CliArg<std::string_view> kName = {
    .name = "name",
    .help = "The file's name on the disk, such as HELLO.OZ3.",
};
const gb::CliArg<std::string_view> kGetHostFile = {
    .name = "host file",
    .help =
        "Where to copy the file, or a folder to copy it into under its name "
        "on the disk. Defaults to the current directory.",
    .optional = true,
    .validators = {&kGetHostFileValidator},
};
const gb::CliArg<std::string_view> kNewName = {
    .name = "new name",
    .help = "The file's new name.",
};
const gb::CliFlag<std::string_view> kSet = {
    .name = "set",
    .short_name = 's',
    .value_name = "letters",
    .help = "Sets the attributes: r for read-only and h for hidden.",
    .validators = {&kAttributesValidator},
};
const gb::CliFlag<std::string_view> kClear = {
    .name = "clear",
    .short_name = 'c',
    .value_name = "letters",
    .help = "Clears the attributes: r for read-only and h for hidden.",
    .validators = {&kAttributesValidator},
};
const gb::CliArg<std::string_view> kProtection = {
    .name = "protection",
    .help = "Whether the image is write protected.",
    .choices = {{.name = "on", .help = "Write protects it."},
                {.name = "off", .help = "Lets it be written."}},
};
const gb::CliFlag<bool> kFix = {
    .name = "fix",
    .short_name = 'f',
    .help = "Repairs what it finds.",
};

//==============================================================================
// Running
//==============================================================================

// Returns whether `status` is OK, writing its message as the error if not.
bool Report(gb::ValidatedContext& context, const absl::Status& status) {
  if (!status.ok()) {
    (*context.GetPtr<gb::CliWriter>())(gb::CliText::kError, status.message());
    return false;
  }
  return true;
}

// Returns whether `text` is OK, writing it to stdout if so, and otherwise its
// message as the error.
bool Report(gb::ValidatedContext& context,
            const absl::StatusOr<std::string>& text) {
  if (!Report(context, text.status())) {
    return false;
  }
  std::fwrite(text->data(), 1, text->size(), stdout);
  return true;
}

// Returns a tool on the context's file system, or null after writing the error
// if it can't be made.
std::unique_ptr<DiskTool> CreateTool(gb::ValidatedContext& context) {
  absl::StatusOr<std::unique_ptr<DiskTool>> tool =
      DiskTool::Create(context.GetPtr<gb::FileSystem>());
  if (!Report(context, tool.status())) {
    return nullptr;
  }
  return *std::move(tool);
}

// Returns the image the command line names.
std::string_view GetImage(const gb::CliCommandLine& command_line) {
  return *command_line.Get(kImage);
}

bool RunNew(gb::ValidatedContext& context,
            const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr &&
         Report(
             context,
             tool->New(GetImage(command_line),
                       *DiskTool::ParseSectorCount(*command_line.Get(kSize))));
}

bool RunFormat(gb::ValidatedContext& context,
               const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  const std::optional<std::string_view> label = command_line.Get(kFormatLabel);
  return tool != nullptr &&
         Report(context,
                tool->Format(
                    GetImage(command_line),
                    label.has_value() ? *DiskTool::ParseLabel(*label)
                                      : DiskLabel(),
                    static_cast<int>(command_line.Get(kCluster).value_or(0))));
}

bool RunInfo(gb::ValidatedContext& context,
             const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr && Report(context, tool->Info(GetImage(command_line)));
}

bool RunDir(gb::ValidatedContext& context,
            const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr &&
         Report(context, tool->Dir(GetImage(command_line),
                                   command_line.Get(kDirPattern).value_or(""),
                                   command_line.Get(kAll)));
}

bool RunPut(gb::ValidatedContext& context,
            const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr &&
         Report(context, tool->Put(GetImage(command_line),
                                   *command_line.Get(kPutHostFile),
                                   command_line.Get(kPutName).value_or(""),
                                   command_line.Get(kText)));
}

bool RunGet(gb::ValidatedContext& context,
            const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr &&
         Report(context,
                tool->Get(GetImage(command_line), *command_line.Get(kName),
                          command_line.Get(kGetHostFile).value_or(""),
                          command_line.Get(kText)));
}

bool RunDel(gb::ValidatedContext& context,
            const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr &&
         Report(context, tool->Delete(GetImage(command_line),
                                      *command_line.Get(kPattern)));
}

bool RunRen(gb::ValidatedContext& context,
            const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr &&
         Report(context,
                tool->Rename(GetImage(command_line), *command_line.Get(kName),
                             *command_line.Get(kNewName)));
}

bool RunAttrib(gb::ValidatedContext& context,
               const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr &&
         Report(context,
                tool->Attrib(GetImage(command_line), *command_line.Get(kName),
                             *DiskTool::ParseAttributes(
                                 command_line.Get(kSet).value_or("")),
                             *DiskTool::ParseAttributes(
                                 command_line.Get(kClear).value_or(""))));
}

bool RunLabel(gb::ValidatedContext& context,
              const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr &&
         Report(context,
                tool->Label(GetImage(command_line),
                            *DiskTool::ParseLabel(*command_line.Get(kLabel))));
}

bool RunProtect(gb::ValidatedContext& context,
                const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr &&
         Report(context, tool->Protect(GetImage(command_line),
                                       *command_line.Get(kProtection) == "on"));
}

bool RunCheck(gb::ValidatedContext& context,
              const gb::CliCommandLine& command_line) {
  std::unique_ptr<DiskTool> tool = CreateTool(context);
  return tool != nullptr &&
         Report(context,
                tool->Check(GetImage(command_line), command_line.Get(kFix)));
}

//==============================================================================
// Commands
//==============================================================================

const gb::CliCommand kNew = {
    .name = "new",
    .help =
        "Creates a blank image of a size, an unformatted disk. Fails if "
        "the image exists.",
    .args = {&kSize},
    .run = RunNew,
};
const gb::CliCommand kFormat = {
    .name = "format",
    .help = "Formats the disk, as the drive's FORMAT does, erasing it.",
    .flags = {&kCluster, &kFormatLabel},
    .run = RunFormat,
};
const gb::CliCommand kInfo = {
    .name = "info",
    .help =
        "Shows the medium (its size and write protection) and the file "
        "system (its label, cluster size, clusters, and free clusters).",
    .run = RunInfo,
};
const gb::CliCommand kDir = {
    .name = "dir",
    .help = "Lists files: each one's name, size in words, and attributes.",
    .flags = {&kAll},
    .args = {&kDirPattern},
    .run = RunDir,
};
const gb::CliCommand kPut = {
    .name = "put",
    .help =
        "Copies a host file onto the disk, replacing any file of that name. "
        "The copy keeps read-only and hidden, where the host has them.",
    .flags = {&kText},
    .args = {&kPutHostFile, &kPutName},
    .run = RunPut,
};
const gb::CliCommand kGet = {
    .name = "get",
    .help =
        "Copies a file from the disk to the host. The copy keeps read-only "
        "and hidden, where the host has them.",
    .flags = {&kText},
    .args = {&kName, &kGetHostFile},
    .run = RunGet,
};
const gb::CliCommand kDel = {
    .name = "del",
    .help =
        "Deletes every file it can that matches, hidden ones included, "
        "naming each it can't. Fails if any failed or none matched.",
    .args = {&kPattern},
    .run = RunDel,
};
const gb::CliCommand kRen = {
    .name = "ren",
    .help = "Renames a file.",
    .args = {&kName, &kNewName},
    .run = RunRen,
};
const gb::CliCommand kAttrib = {
    .name = "attrib",
    .help =
        "Sets or clears a file's attributes, then shows them. With neither "
        "flag, only shows them.",
    .flags = {&kSet, &kClear},
    .args = {&kName},
    .run = RunAttrib,
};
const gb::CliCommand kLabelCommand = {
    .name = "label",
    .help = "Sets the volume label.",
    .args = {&kLabel},
    .run = RunLabel,
};
const gb::CliCommand kProtect = {
    .name = "protect",
    .help =
        "Sets the image's write protection. A read-only image file is "
        "always write protected.",
    .args = {&kProtection},
    .run = RunProtect,
};
const gb::CliCommand kCheck = {
    .name = "check",
    .help = "Checks the file system for problems, naming each it finds.",
    .flags = {&kFix},
    .run = RunCheck,
};

const gb::CliCommand kProgram = {
    .name = "oz3dsk",
    .help =
        "Manages OZ-3 disk images: makes and formats them, and copies files "
        "on and off them. Paths are written the operating system's way, "
        "absolute or relative to the current directory.",
    .args = {&kImage},
    .commands = {&kNew, &kFormat, &kInfo, &kDir, &kPut, &kGet, &kDel, &kRen,
                 &kAttrib, &kLabelCommand, &kProtect, &kCheck},
    .constraints = {&gb::kConstraintCliFileSystem},
};

}  // namespace
}  // namespace oz3

int main(int argc, char* argv[]) {
  gb::Context context;
  context.SetOwned(gb::CreateCliFileSystem());
  return gb::RunCommandLine(&context, &oz3::kProgram,
                            gb::GetMainArgs(argc, argv));
}
