// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/cli/disk/oz3dsk.h"

#include <array>
#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/time/civil_time.h"
#include "absl/types/span.h"
#include "gb/base/context_builder.h"
#include "gb/cli/cli_writer.h"
#include "gb/cli/fake_cli_writer.h"
#include "gb/cli/run_command_line.h"
#include "gb/file/file_system.h"
#include "gb/file/memory_file_protocol.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/cli/disk/disk_tool.h"
#include "oz3/disk/disk_file_protocol.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_status.h"
#include "oz3/program/bare_program.h"
#include "oz3/program/program_file.h"

namespace oz3 {
namespace {

using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::StartsWith;

constexpr char kImage[] = "mem:/work.ozd";
const absl::CivilSecond kNow(2026, 11, 17, 10, 30, 15);

// main()'s exit codes, as gb::RunCommandLine returns them.
constexpr int kExitSuccess = 0;
constexpr int kExitFailure = 1;
constexpr int kExitInvalidCommandLine = 2;

class Oz3dskTest : public ::testing::Test {
 protected:
  void SetUp() override {
    ASSERT_TRUE(file_system_.Register(
        std::make_unique<gb::MemoryFileProtocol>(), "mem"));
    ASSERT_TRUE(file_system_.SetDefaultProtocol("mem"));
  }

  // Runs oz3dsk with `args`, as main() would, and returns its exit code. What
  // it writes is in writer_.
  int Run(std::vector<std::string_view> args) {
    // Each run makes a DiskTool, which registers its protocol in the file
    // system for good, as a process runs once, so each takes a name of its own.
    ++run_count_;
    return Run(args, absl::StrCat("ozdisk", run_count_));
  }

  // As above, with the DiskTool's protocol registered as `protocol_name`.
  int Run(std::vector<std::string_view> args, std::string_view protocol_name) {
    return gb::RunCommandLine(
        gb::ContextBuilder()
            .SetPtr<gb::FileSystem>(&file_system_)
            .SetValue<gb::CliWriter>(writer_.GetWriter())
            .SetValue<HostClock>([] { return kNow; })
            .SetValue<std::string>(DiskTool::kKeyProtocolName,
                                   std::string(protocol_name))
            .Build(),
        &GetOz3dskCommand(), args);
  }

  // Runs oz3dsk on kImage with `args`, expecting it to succeed, and returns
  // what it reports.
  std::string RunOk(std::vector<std::string_view> args) {
    args.insert(args.begin(), kImage);
    EXPECT_EQ(Run(args), kExitSuccess) << writer_.TakeError();
    EXPECT_THAT(writer_.TakeError(), IsEmpty());
    return writer_.TakeHelp();
  }

  // Runs oz3dsk on kImage with `args`, expecting it to exit with `exit_code`
  // and report nothing, and returns the error it writes.
  std::string RunError(int exit_code, std::vector<std::string_view> args) {
    args.insert(args.begin(), kImage);
    EXPECT_EQ(Run(args), exit_code);
    EXPECT_THAT(writer_.TakeHelp(), IsEmpty());
    return writer_.TakeError();
  }

  // Makes a new image at kImage, formatted as WORK.
  void NewFormatted() {
    RunOk({"new", "180K"});
    RunOk({"format", "--label", "WORK"});
  }

  // Empties the image's free list, so every free cluster is on no chain.
  void BreakFreeList() {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&file_system_, kImage);
    ASSERT_TRUE(image.ok()) << image.status();
    std::array<uint16_t, kDiskSectorWords> words;
    const int sector = DiskLayout::GetFatSector(0);
    ASSERT_EQ((*image)->ReadSector(sector, absl::MakeSpan(words)),
              DiskStatus::kOk);
    words[0] = DiskLayout::kFatEnd;
    ASSERT_EQ((*image)->WriteSector(sector, words), DiskStatus::kOk);
  }

  // Returns the bytes of the host file at `path`.
  std::string ReadHost(std::string_view path) {
    std::string bytes;
    EXPECT_TRUE(file_system_.ReadFile(path, &bytes)) << path;
    return bytes;
  }

  // Writes `bytes` to the host file at `path`.
  void WriteHost(std::string_view path, std::string_view bytes) {
    ASSERT_TRUE(file_system_.WriteFile(path, bytes)) << path;
  }

  gb::FileSystem file_system_;
  gb::FakeCliWriter writer_;
  int run_count_ = 0;
};

//==============================================================================
// Help and usage
//==============================================================================

TEST_F(Oz3dskTest, HelpNamesEveryCommand) {
  EXPECT_EQ(Run({"--help"}), kExitSuccess);
  const std::string help = writer_.TakeHelp();
  EXPECT_THAT(help, HasSubstr("Manages OZ-3 disk images"));
  for (std::string_view command :
       {"new", "format", "info", "dir", "put", "get", "del", "ren", "attrib",
        "label", "boot", "protect", "check"}) {
    EXPECT_THAT(help, HasSubstr(command));
  }
  EXPECT_THAT(writer_.TakeError(), IsEmpty());
}

TEST_F(Oz3dskTest, NoCommandWritesHelp) {
  EXPECT_THAT(RunOk({}), StartsWith("Usage: oz3dsk"));
}

TEST_F(Oz3dskTest, UnknownCommandIsAUsageError) {
  EXPECT_EQ(RunError(kExitInvalidCommandLine, {"erase"}),
            "oz3dsk: unknown command \"erase\". Run \"oz3dsk --help\" for "
            "help.");
}

//==============================================================================
// new and format
//==============================================================================

TEST_F(Oz3dskTest, NewMakesAnUnformattedImage) {
  EXPECT_THAT(RunOk({"new", "180K"}), IsEmpty());
  EXPECT_EQ(RunOk({"info"}),
            "Size:     180K words (720 sectors)\n"
            "Protect:  off\n"
            "Volume:   unformatted\n");
}

TEST_F(Oz3dskTest, NewTakesASizeInWords) {
  RunOk({"new", "512"});
  EXPECT_THAT(RunOk({"info"}), HasSubstr("(2 sectors)"));
}

TEST_F(Oz3dskTest, NewFailsForAnImageThatExists) {
  RunOk({"new", "180K"});
  EXPECT_EQ(RunError(kExitFailure, {"new", "180K"}),
            "mem:/work.ozd: already exists");
}

TEST_F(Oz3dskTest, NewRejectsABadSize) {
  EXPECT_EQ(RunError(kExitInvalidCommandLine, {"new", "100"}),
            "oz3dsk new: <size>: Size 100 isn't a whole number of sectors of "
            "256 words. Run \"oz3dsk <image> new --help\" for help.");
  for (std::string_view size : {"100", "720X", "0", ""}) {
    EXPECT_THAT(RunError(kExitInvalidCommandLine, {"new", size}),
                HasSubstr("size"))
        << size;
  }
  EXPECT_FALSE(file_system_.IsValidPath(kImage));
}

TEST_F(Oz3dskTest, FormatWritesTheLabelAndCluster) {
  RunOk({"new", "180K"});
  EXPECT_THAT(RunOk({"format", "-l", "WORK", "-c", "4"}), IsEmpty());
  const std::string info = RunOk({"info"});
  EXPECT_THAT(info, HasSubstr("Volume:   WORK\n"));
  EXPECT_THAT(info, HasSubstr("(4 sectors)"));
}

TEST_F(Oz3dskTest, FormatRejectsABadCluster) {
  RunOk({"new", "180K"});
  EXPECT_EQ(RunError(kExitInvalidCommandLine, {"format", "-c", "3"}),
            "oz3dsk format: \"-c\": 3 isn't a power of two from 1 to 64. Run "
            "\"oz3dsk <image> format --help\" for help.");
  for (std::string_view cluster : {"0", "3", "128"}) {
    EXPECT_THAT(
        RunError(kExitInvalidCommandLine, {"format", "--cluster", cluster}),
        HasSubstr("isn't a power of two from 1 to 64"))
        << cluster;
  }
  EXPECT_THAT(RunOk({"info"}), HasSubstr("unformatted"));
}

TEST_F(Oz3dskTest, FormatRejectsABadLabel) {
  RunOk({"new", "180K"});
  EXPECT_THAT(RunError(kExitInvalidCommandLine,
                       {"format", "--label", "A_LABEL_FAR_TOO_LONG"}),
              StartsWith("oz3dsk format: \"--label\": Label "
                         "\"A_LABEL_FAR_TOO_LONG\" isn't up to 16 letters"));
  EXPECT_THAT(RunOk({"info"}), HasSubstr("unformatted"));
}

TEST_F(Oz3dskTest, FormatFailsForAMissingImage) {
  EXPECT_THAT(RunError(kExitFailure, {"format"}), HasSubstr("mem:/work.ozd"));
}

//==============================================================================
// Files
//==============================================================================

TEST_F(Oz3dskTest, DirOfAnEmptyDisk) {
  NewFormatted();
  EXPECT_EQ(RunOk({"dir"}), "no files\n");
}

TEST_F(Oz3dskTest, DirNeedsAFileSystem) {
  RunOk({"new", "180K"});
  EXPECT_THAT(RunError(kExitFailure, {"dir"}), HasSubstr("mem:/work.ozd"));
}

TEST_F(Oz3dskTest, PutAndGetCopyAFile) {
  NewFormatted();
  WriteHost("mem:/hello.txt", "hello!");
  EXPECT_THAT(RunOk({"put", "mem:/hello.txt"}), IsEmpty());
  EXPECT_EQ(RunOk({"dir"}),
            "HELLO.TXT                3  --  2026-11-17 10:30\n");
  EXPECT_THAT(RunOk({"get", "HELLO.TXT", "mem:/copy.txt"}), IsEmpty());
  EXPECT_EQ(ReadHost("mem:/copy.txt"), "hello!");
}

TEST_F(Oz3dskTest, PutTakesAName) {
  NewFormatted();
  WriteHost("mem:/hello.txt", "hello!");
  RunOk({"put", "mem:/hello.txt", "OTHER.TXT"});
  EXPECT_THAT(RunOk({"dir"}), StartsWith("OTHER.TXT"));
}

TEST_F(Oz3dskTest, PutAndGetText) {
  NewFormatted();
  WriteHost("mem:/hello.txt", "a\r\nb");
  RunOk({"put", "--text", "mem:/hello.txt"});
  RunOk({"get", "HELLO.TXT", "mem:/binary.txt"});
  EXPECT_EQ(ReadHost("mem:/binary.txt"), std::string("a\nb\0", 4));
  RunOk({"get", "-t", "HELLO.TXT", "mem:/text.txt"});
  EXPECT_EQ(ReadHost("mem:/text.txt"), "a\nb");
}

TEST_F(Oz3dskTest, PutRejectsAMissingHostFile) {
  NewFormatted();
  RunError(kExitInvalidCommandLine, {"put", "mem:/missing.txt"});
  EXPECT_EQ(RunOk({"dir"}), "no files\n");
}

TEST_F(Oz3dskTest, PutFailsForABadName) {
  NewFormatted();
  WriteHost("mem:/hello.txt", "hello!");
  EXPECT_EQ(RunError(kExitFailure, {"put", "mem:/hello.txt", "bad name"}),
            "bad name: bad name");
}

TEST_F(Oz3dskTest, GetIntoAFolder) {
  NewFormatted();
  WriteHost("mem:/hello.txt", "hello!");
  RunOk({"put", "mem:/hello.txt"});
  ASSERT_TRUE(file_system_.CreateFolder("mem:/out"));
  RunOk({"get", "HELLO.TXT", "mem:/out"});
  EXPECT_EQ(ReadHost("mem:/out/HELLO.TXT"), "hello!");
}

TEST_F(Oz3dskTest, GetRejectsAMissingFolder) {
  NewFormatted();
  RunError(kExitInvalidCommandLine, {"get", "HELLO.TXT", "mem:/none/a.txt"});
}

TEST_F(Oz3dskTest, GetFailsForAMissingFile) {
  NewFormatted();
  EXPECT_THAT(RunError(kExitFailure, {"get", "HELLO.TXT", "mem:/copy.txt"}),
              HasSubstr("HELLO.TXT"));
}

TEST_F(Oz3dskTest, DelDeletesWhatMatches) {
  NewFormatted();
  WriteHost("mem:/a.txt", "a");
  RunOk({"put", "mem:/a.txt"});
  RunOk({"put", "mem:/a.txt", "B.TXT"});
  RunOk({"put", "mem:/a.txt", "C.OZ3"});
  EXPECT_THAT(RunOk({"del", "*.TXT"}), IsEmpty());
  EXPECT_THAT(RunOk({"dir"}), StartsWith("C.OZ3"));
}

TEST_F(Oz3dskTest, DelFailsWhenNoneMatches) {
  NewFormatted();
  EXPECT_EQ(RunError(kExitFailure, {"del", "*.TXT"}),
            "mem:/work.ozd: *.TXT: not found");
}

TEST_F(Oz3dskTest, RenRenamesAFile) {
  NewFormatted();
  WriteHost("mem:/a.txt", "a");
  RunOk({"put", "mem:/a.txt"});
  EXPECT_THAT(RunOk({"ren", "A.TXT", "B.TXT"}), IsEmpty());
  EXPECT_THAT(RunOk({"dir"}), StartsWith("B.TXT"));
}

TEST_F(Oz3dskTest, RenFailsForAMissingFile) {
  NewFormatted();
  EXPECT_THAT(RunError(kExitFailure, {"ren", "A.TXT", "B.TXT"}),
              HasSubstr("A.TXT"));
}

TEST_F(Oz3dskTest, AttribSetsClearsAndShows) {
  NewFormatted();
  WriteHost("mem:/a.txt", "a");
  RunOk({"put", "mem:/a.txt"});
  EXPECT_EQ(RunOk({"attrib", "A.TXT"}), "A.TXT --\n");
  EXPECT_EQ(RunOk({"attrib", "--set", "rh", "A.TXT"}), "A.TXT rh\n");
  EXPECT_EQ(RunOk({"dir"}), "no files\n");
  EXPECT_THAT(RunOk({"dir", "--all"}), StartsWith("A.TXT"));
  EXPECT_EQ(RunOk({"attrib", "-c", "H", "A.TXT"}), "A.TXT r-\n");
}

TEST_F(Oz3dskTest, AttribRejectsBadLetters) {
  NewFormatted();
  WriteHost("mem:/a.txt", "a");
  RunOk({"put", "mem:/a.txt"});
  EXPECT_EQ(
      RunError(kExitInvalidCommandLine, {"attrib", "--set", "x", "A.TXT"}),
      "oz3dsk attrib: \"--set\": Attributes \"x\" can only hold r "
      "(read-only) and h (hidden). Run \"oz3dsk <image> attrib --help\" for "
      "help.");
  EXPECT_THAT(
      RunError(kExitInvalidCommandLine, {"attrib", "--clear", "rx", "A.TXT"}),
      StartsWith("oz3dsk attrib: \"--clear\": Attributes \"rx\""));
}

TEST_F(Oz3dskTest, AttribFailsToSetAndClearOne) {
  NewFormatted();
  WriteHost("mem:/a.txt", "a");
  RunOk({"put", "mem:/a.txt"});
  RunError(kExitFailure, {"attrib", "-s", "r", "-c", "r", "A.TXT"});
  EXPECT_EQ(RunOk({"attrib", "A.TXT"}), "A.TXT --\n");
}

//==============================================================================
// The volume and the medium
//==============================================================================

TEST_F(Oz3dskTest, LabelSetsTheLabel) {
  NewFormatted();
  EXPECT_THAT(RunOk({"label", "GAMES"}), IsEmpty());
  EXPECT_THAT(RunOk({"info"}), HasSubstr("Volume:   GAMES\n"));
}

TEST_F(Oz3dskTest, LabelRejectsABadLabel) {
  NewFormatted();
  RunError(kExitInvalidCommandLine, {"label", "A LABEL FAR TOO LONG"});
  EXPECT_THAT(RunOk({"info"}), HasSubstr("Volume:   WORK\n"));
}

TEST_F(Oz3dskTest, BootWritesABareProgram) {
  NewFormatted();
  ASSERT_TRUE(
      WriteBareProgramFile(file_system_, "mem:/boot.oz3",
                           {.contents_words = 2, .data = {{.data = {1, 2}}}})
          .ok());
  EXPECT_THAT(RunOk({"boot", "mem:/boot.oz3"}), IsEmpty());
}

TEST_F(Oz3dskTest, BootRejectsAMissingProgram) {
  NewFormatted();
  RunError(kExitInvalidCommandLine, {"boot", "mem:/boot.oz3"});
}

TEST_F(Oz3dskTest, BootFailsForABadProgram) {
  NewFormatted();
  WriteHost("mem:/boot.oz3", "odd");
  EXPECT_THAT(RunError(kExitFailure, {"boot", "mem:/boot.oz3"}),
              HasSubstr("mem:/boot.oz3"));
}

TEST_F(Oz3dskTest, ProtectSetsWriteProtection) {
  NewFormatted();
  EXPECT_THAT(RunOk({"protect", "on"}), IsEmpty());
  EXPECT_THAT(RunOk({"info"}), HasSubstr("Protect:  on\n"));
  EXPECT_EQ(RunError(kExitFailure, {"label", "GAMES"}),
            "mem:/work.ozd: write protected");
  RunOk({"protect", "off"});
  EXPECT_THAT(RunOk({"info"}), HasSubstr("Protect:  off\n"));
}

TEST_F(Oz3dskTest, ProtectRejectsAnythingButOnOrOff) {
  NewFormatted();
  EXPECT_EQ(RunError(kExitInvalidCommandLine, {"protect", "yes"}),
            "oz3dsk protect: <protection>: \"yes\" is not one of: on, off. Run "
            "\"oz3dsk <image> protect --help\" for help.");
}

TEST_F(Oz3dskTest, CheckFindsNoProblems) {
  NewFormatted();
  EXPECT_EQ(RunOk({"check"}), "no problems\n");
  EXPECT_EQ(RunOk({"check", "--fix"}), "no problems\n");
}

TEST_F(Oz3dskTest, CheckNamesProblemsAndRepairsThem) {
  NewFormatted();
  BreakFreeList();
  EXPECT_EQ(RunOk({"check"}), "715 clusters on no chain\n");
  EXPECT_EQ(RunOk({"check", "--fix"}), "715 clusters on no chain\nrepaired\n");
  EXPECT_EQ(RunOk({"check"}), "no problems\n");
}

TEST_F(Oz3dskTest, CheckNeedsAnImage) {
  EXPECT_THAT(RunError(kExitFailure, {"check"}), HasSubstr("mem:/work.ozd"));
}

//==============================================================================
// Context
//==============================================================================

TEST_F(Oz3dskTest, FailsWhenItsProtocolCantBeRegistered) {
  RunOk({"new", "180K"});

  // A second tool on the same name can't register its protocol.
  EXPECT_EQ(Run({kImage, "info"}, "ozdisk1"), kExitFailure);
  EXPECT_THAT(writer_.TakeError(), HasSubstr("ozdisk1"));
}

}  // namespace
}  // namespace oz3
