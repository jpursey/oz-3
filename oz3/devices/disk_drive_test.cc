// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/disk_drive.h"

#include <algorithm>
#include <cstdint>
#include <iterator>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/functional/function_ref.h"
#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_replace.h"
#include "absl/time/civil_time.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/date_time.h"
#include "oz3/core/lockable.h"
#include "oz3/core/port.h"
#include "oz3/devices/device_program_test.h"
#include "oz3/devices/disk_drive_command.h"
#include "oz3/disk/disk_dir_entry.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_filename.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_layout.h"
#include "oz3/disk/disk_media.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/disk_superblock.h"
#include "oz3/disk/faulty_file_protocol.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Le;
using ::testing::Not;
using ::testing::Pair;

// The processor ports of the drive's command, data, and control ports, and its
// interrupt.
constexpr int kPort = 16;
constexpr int kCommandPort = kPort + DiskDrive::kCommandPort;
constexpr int kDataPort = kPort + DiskDrive::kDataPort;
constexpr int kControlPort = kPort + DiskDrive::kControlPort;
constexpr int kInterrupt = 2;

// A clock rate of a cycle a millisecond, and hardware where a sector read
// takes a second (1000 cycles), a write half that, and the latency 10 cycles.
// The test disk's clusters are one sector.
constexpr int64_t kClockHz = 1000;
constexpr DiskHardwareSettings kHardware = {.read_words_per_sec = 256,
                                            .write_words_per_sec = 512,
                                            .latency = absl::Milliseconds(10)};
constexpr Cycles kReadCycles = 1000;
constexpr Cycles kWriteCycles = 500;
constexpr Cycles kLatencyCycles = 10;

// The superblock read when an image is attached: a seek, then a sector.
constexpr Cycles kAttachCycles = kLatencyCycles + kReadCycles;

constexpr char kPath[] = "mem:/disk.ozd";
constexpr char kOtherPath[] = "mem:/other.ozd";
constexpr char kThirdPath[] = "mem:/third.ozd";
constexpr int kSectors = 720;

// The dwords of the boot area, which READ BOOT and WRITE BOOT move.
constexpr int kBootDwords = DiskSuperblock::kBootAreaWords / 2;

constexpr DiskStatus kOk = DiskStatus::kOk;

// The times the drive's clock gives, where a test sets it.
constexpr absl::CivilSecond kNow(2026, 11, 16, 12, 30, 15);
constexpr absl::CivilSecond kLater(2026, 11, 16, 12, 31, 0);

// Returns `status` as the field the drive sends.
PortWords Field(DiskStatus status) {
  return PortWords::FromWord(static_cast<uint16_t>(status));
}

// Returns a command word: `command`'s code in the high byte, and `handle`.
constexpr uint16_t CommandWord(DiskDriveCommand command, int handle = 0) {
  return static_cast<uint16_t>(
      static_cast<int>(command) << DiskDrive::kCommandShift | handle);
}

// Returns `words`, an even number of them, as fields of two: a name's,
// pattern's, or label's 8 words are the 4 fields of its argument.
std::vector<PortWords> NameFields(absl::Span<const uint16_t> words) {
  std::vector<PortWords> fields;
  for (int i = 0; i < static_cast<int>(words.size()); i += 2) {
    fields.push_back(PortWords::FromWords(words[i], words[i + 1]));
  }
  return fields;
}

// Returns the words of a name, pattern, or label that has a zero before a
// character, which none may.
DiskFilename::Words GapWords() {
  DiskFilename::Words words = DiskFilename::Parse("ABC")->GetWords();
  words[0] = 0;
  return words;
}

// Returns the name in `text` as the fields of a name argument.
std::vector<PortWords> NameFields(std::string_view text) {
  std::optional<DiskFilename> name = DiskFilename::Parse(text);
  CHECK(name.has_value()) << text;
  return NameFields(name->GetWords());
}

// Returns the pattern in `text` as the fields of a pattern argument.
std::vector<PortWords> PatternFields(std::string_view text) {
  std::optional<DiskFilename> pattern = DiskFilename::ParsePattern(text);
  CHECK(pattern.has_value()) << text;
  return NameFields(pattern->GetWords());
}

// Returns the label in `text` as the fields of a label argument.
std::vector<PortWords> LabelFields(std::string_view text) {
  std::optional<DiskLabel> label = DiskLabel::Parse(text);
  CHECK(label.has_value()) << text;
  return NameFields(label->GetWords());
}

// Returns `fields`, then `word` as a field.
std::vector<PortWords> AndWord(std::vector<PortWords> fields, uint16_t word) {
  fields.push_back(PortWords::FromWord(word));
  return fields;
}

// Returns `first`, then `second`.
std::vector<PortWords> AndFields(std::vector<PortWords> first,
                                 absl::Span<const PortWords> second) {
  first.insert(first.end(), second.begin(), second.end());
  return first;
}

// Returns FIND's completion for `entry`: an OK status, then its words as 8
// fields.
std::vector<PortWords> FoundFields(const DiskDirEntry& entry) {
  return AndFields({Field(DiskStatus::kOk)}, NameFields(entry.GetWords()));
}

// Returns VOLUME's completion for `info`: an OK status, its label as 4 fields,
// and its three counts.
std::vector<PortWords> VolumeFields(const DiskVolumeInfo& info) {
  std::vector<PortWords> fields =
      AndFields({Field(DiskStatus::kOk)}, NameFields(info.label.GetWords()));
  for (const int count :
       {info.sectors_per_cluster, info.cluster_count, info.free_clusters}) {
    fields.push_back(PortWords::FromWord(static_cast<uint16_t>(count)));
  }
  return fields;
}

// The host file system that images are made in. It is a base of
// DiskDriveTest, before the processor's, so it outlives the processor, whose
// drive holds an image until it is destroyed.
class DiskHost {
 protected:
  DiskHost() {
    std::unique_ptr<FaultyFileProtocol> protocol =
        std::make_unique<FaultyFileProtocol>();
    protocol_ = protocol.get();
    host_.Register(std::move(protocol));
  }

  gb::FileSystem host_;
  FaultyFileProtocol* protocol_ = nullptr;
};

class DiskDriveTest : public DiskHost, public DeviceProgramTest {
 protected:
  // The most cycles RunUntil waits.
  static constexpr Cycles kMaxRunCycles = 100'000;

  // Initializes a processor with a drive named "Drive" with `settings`, and
  // sets drive_ to it, with the host's clock from GetHostContext(). Returns
  // whether it did.
  bool InitDrive(DiskDriveSettings settings = GetSettings()) {
    absl::StatusOr<std::unique_ptr<DiskDriveFactory>> factory =
        DiskDriveFactory::Create("Drive", settings);
    EXPECT_TRUE(factory.ok()) << factory.status();
    if (!factory.ok() || !Init({.num_ports = settings.port + DiskDrive::kPorts,
                                .clock_hz = kClockHz,
                                .devices = {*std::move(factory)},
                                .context = GetHostContext()})) {
      return false;
    }
    drive_ = static_cast<DiskDrive*>(GetProcessor().GetDevice("Drive"));
    return drive_ != nullptr;
  }

  // The settings InitDrive uses by default.
  static DiskDriveSettings GetSettings() {
    return {.port = kPort,
            .interrupt = {.interrupt = kInterrupt},
            .hardware = kHardware};
  }

  // Returns a new image of kSectors sectors at `path`, formatted unless
  // `formatted` is false.
  std::unique_ptr<DiskImage> MakeImage(bool formatted = true,
                                       std::string_view path = kPath) {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Create(&host_, path, kSectors);
    EXPECT_TRUE(image.ok()) << image.status();
    if (!image.ok() || !formatted) {
      return image.ok() ? *std::move(image) : nullptr;
    }
    DiskFileSystem file_system({}, 1);
    file_system.AttachImage(*std::move(image));
    EXPECT_EQ(file_system.Format(*DiskLabel::Parse("WORK"), 0, DateTime()),
              DiskStatus::kOk);
    return file_system.DetachImage();
  }

  // Returns a new formatted image at `path` holding the file `name`, of `size`
  // words, each FileWord(i).
  std::unique_ptr<DiskImage> MakeImageWithFile(std::string_view name, int size,
                                               std::string_view path = kPath) {
    DiskFileSystem file_system({}, 1);
    file_system.AttachImage(MakeImage(/*formatted=*/true, path));
    AddFile(file_system, name, size);
    return file_system.DetachImage();
  }

  // Returns a boot area whose words count up from `first`.
  static DiskSuperblock::BootArea BootWords(uint16_t first) {
    DiskSuperblock::BootArea words(DiskSuperblock::kBootAreaWords);
    for (int i = 0; i < DiskSuperblock::kBootAreaWords; ++i) {
      words[i] = static_cast<uint16_t>(first + i);
    }
    return words;
  }

  // Returns word `i` of a file AddFile makes.
  static uint16_t FileWord(int i) { return static_cast<uint16_t>(i + 1); }

  // Returns FileWord(first) on, `count` of them.
  static std::vector<uint16_t> FileWords(int first, int count) {
    std::vector<uint16_t> words;
    for (int i = first; i < first + count; ++i) {
      words.push_back(FileWord(i));
    }
    return words;
  }

  // Creates the file `name` on `file_system`'s disk, of `size` words, each
  // FileWord(i), and closes it.
  static void AddFile(DiskFileSystem& file_system, std::string_view name,
                      int size) {
    DiskFile file = 0;
    ASSERT_EQ(file_system.Create(*DiskFilename::Parse(name), DateTime(), &file),
              kOk);
    const std::vector<uint16_t> words = FileWords(0, size);
    for (int written = 0; written < size;) {
      int count = 0;
      if (file_system.Write(file, absl::MakeConstSpan(words).subspan(written),
                            &count) != kOk ||
          count == 0) {
        ADD_FAILURE() << "Write failed at " << written;
        break;
      }
      written += count;
    }
    EXPECT_EQ(file_system.Close(file, DateTime()), kOk);
  }

  // Returns `image` with the chain of its root's file `name` ended after its
  // first cluster, whatever its size.
  std::unique_ptr<DiskImage> BreakChain(std::unique_ptr<DiskImage> image,
                                        std::string_view name) {
    DiskFileSystem file_system({}, 1);
    file_system.AttachImage(std::move(image));
    DiskFile root = 0;
    EXPECT_EQ(file_system.OpenRoot(&root), kOk);
    DiskDirEntry entry;
    EXPECT_EQ(file_system.Find(root, *DiskFilename::Parse(name), &entry), kOk);
    image = file_system.DetachImage();
    const int cluster = entry.GetFirstCluster();
    std::vector<uint16_t> fat(kDiskSectorWords);
    EXPECT_EQ(image->ReadSector(DiskLayout::GetFatSector(cluster),
                                absl::MakeSpan(fat)),
              kOk);
    fat[cluster % kDiskSectorWords] = DiskLayout::kFatEnd;
    EXPECT_EQ(image->WriteSector(DiskLayout::GetFatSector(cluster), fat), kOk);
    return image;
  }

  // Returns the words of the root's file `name` on `image`, or its first
  // `count` if there are more, or nothing if they can't be read.
  std::vector<uint16_t> ReadFile(std::unique_ptr<DiskImage> image,
                                 std::string_view name,
                                 int count = DiskDirEntry::kMaxSize) {
    DiskFileSystem file_system({}, 1);
    file_system.AttachImage(std::move(image));
    DiskFile file = 0;
    std::vector<uint16_t> words;
    if (file_system.Open(*DiskFilename::Parse(name), &file) != kOk) {
      ADD_FAILURE() << "Can't open " << name;
      return words;
    }
    DiskFileInfo info;
    EXPECT_EQ(file_system.GetFileInfo(file, &info), kOk);
    words.resize(std::min(info.size, count));
    for (int read = 0; read < static_cast<int>(words.size());) {
      int read_count = 0;
      if (file_system.Read(file, absl::MakeSpan(words).subspan(read),
                           &read_count) != kOk ||
          read_count == 0) {
        ADD_FAILURE() << "Read failed at " << read;
        return {};
      }
      read += read_count;
    }
    return words;
  }

  // Detaches the drive's image, and returns ReadFile of it.
  std::vector<uint16_t> DetachAndReadFile(std::string_view name) {
    return ReadFile(drive_->DetachImage(), name);
  }

  // Opens the image at kPath again, as the host would, to see what the drive
  // left on it. Nothing else may have it open.
  std::unique_ptr<DiskImage> ReopenImage() {
    absl::StatusOr<std::unique_ptr<DiskImage>> image =
        DiskImage::Open(&host_, kPath);
    EXPECT_TRUE(image.ok()) << image.status();
    return image.ok() ? *std::move(image) : nullptr;
  }

  // Returns a new formatted image holding the file "A", of `size` words, then
  // "FILL", taking every free cluster but `free`.
  std::unique_ptr<DiskImage> MakeFullImage(int size, int free) {
    DiskFileSystem file_system({}, 1);
    file_system.AttachImage(MakeImage());
    AddFile(file_system, "A", size);
    DiskVolumeInfo volume;
    EXPECT_EQ(file_system.GetVolumeInfo(&volume), kOk);
    AddFile(file_system, "FILL",
            (volume.free_clusters - free) * kDiskSectorWords);
    return file_system.DetachImage();
  }

  // Detaches the drive's image, and returns FindEntry of it.
  std::optional<DiskDirEntry> DetachAndFindEntry(std::string_view name) {
    return FindEntry(drive_->DetachImage(), name);
  }

  // Expects the image at kPath, which nothing else may have open, to hold
  // `name` committed with `words`, modified at `modified`.
  void ExpectCommitted(std::string_view name, absl::Span<const uint16_t> words,
                       absl::CivilSecond modified) {
    std::optional<DiskDirEntry> entry = FindEntry(ReopenImage(), name);
    ASSERT_TRUE(entry.has_value());
    EXPECT_EQ(entry->GetSize(), static_cast<int>(words.size()));
    EXPECT_EQ(entry->GetModified(), DateTime::FromCivil(modified));
    EXPECT_EQ(ReadFile(ReopenImage(), name),
              std::vector<uint16_t>(words.begin(), words.end()));
  }

  // Returns the root directory's entry for `name` on `image`, or nullopt if
  // there is none.
  static std::optional<DiskDirEntry> FindEntry(std::unique_ptr<DiskImage> image,
                                               std::string_view name) {
    DiskFileSystem file_system({}, 1);
    file_system.AttachImage(std::move(image));
    DiskFile root = 0;
    EXPECT_EQ(file_system.OpenRoot(&root), kOk);
    DiskDirEntry entry;
    if (file_system.Find(root, *DiskFilename::Parse(name), &entry) != kOk) {
      return std::nullopt;
    }
    return entry;
  }

  // Attaches a new formatted image, and runs until the drive has read its
  // superblock.
  void AttachFormatted() {
    drive_->AttachImage(MakeImage());
    Execute(kAttachCycles + 2);
  }

  // Attaches a new formatted image whose boot area is `words`, and runs until
  // the drive has read its superblock.
  void AttachWithBootArea(const DiskSuperblock::BootArea& words,
                          std::string_view path = kPath) {
    DiskFileSystem file_system({}, 1);
    file_system.AttachImage(MakeImage(/*formatted=*/true, path));
    EXPECT_EQ(file_system.SetBootArea(words), kOk);
    drive_->AttachImage(file_system.DetachImage());
    Execute(kAttachCycles + 2);
  }

  // Executes until `condition` is true, checking it before each cycle. Adds a
  // test failure and returns false if it isn't within kMaxRunCycles.
  bool RunUntil(absl::FunctionRef<bool()> condition) {
    for (Cycles cycles = 0; cycles < kMaxRunCycles; ++cycles) {
      if (condition()) {
        return true;
      }
      Execute(1);
    }
    ADD_FAILURE() << "RunUntil ran " << kMaxRunCycles << " cycles";
    return false;
  }

  // Returns whether `port`'s flag is set.
  bool IsSet(int port) { return GetPort(port).GetStatus() != 0; }

  // Returns the control port's control word, as IN reads it, and its phase.
  uint16_t GetControlWord() { return GetPort(kControlPort).GetValue(0); }
  int GetPhase() { return GetControlWord() & DiskDrive::kPhaseMask; }

  // Writes both of `port`'s words as OUTS.D does, only if its flag is clear,
  // setting it. Returns whether it wrote. No core may be using the port.
  bool Out(int port, PortWords words) {
    std::unique_ptr<Lock> lock = LockPort(port);
    EXPECT_TRUE(lock->IsLocked());
    return words.Store(GetPort(port), *lock, Port::T | Port::S) == 0;
  }

  // Reads both of `port`'s words as INS.D does, only if its flag is set,
  // clearing it. Returns nullopt if the flag is clear. No core may be using
  // the port.
  std::optional<PortWords> In(int port) {
    std::unique_ptr<Lock> lock = LockPort(port);
    EXPECT_TRUE(lock->IsLocked());
    PortWords words;
    if (words.Load(GetPort(port), *lock, Port::T | Port::S) == 0) {
      return std::nullopt;
    }
    return words;
  }

  // Writes a request to the control port with OUTS, and runs until the drive
  // clears its flag, as TSTP sees it.
  void Request(uint16_t request) {
    ASSERT_TRUE(Out(kControlPort, PortWords::FromWord(request)));
    RunUntil([this] { return !IsSet(kControlPort); });
  }

  // Waits for the command port to be clear, then writes a field to it.
  void SendField(PortWords field) {
    RunUntil([this] { return !IsSet(kCommandPort); });
    EXPECT_TRUE(Out(kCommandPort, field));
  }

  // Waits for the drive to put a field on the command port, then reads it, and
  // executes the cycle the drive notices it in, as it would after INS.
  PortWords ReceiveField() {
    RunUntil([this] { return IsSet(kCommandPort); });
    std::optional<PortWords> field = In(kCommandPort);
    EXPECT_TRUE(field.has_value());
    Execute(1);
    return field.value_or(PortWords::FromWords(0xFFFF, 0xFFFF));
  }

  // Reads a status, and if it is OK, `results` fields after it.
  std::vector<PortWords> ReceiveStep(int results) {
    std::vector<PortWords> fields = {ReceiveField()};
    if (fields[0] != Field(kOk)) {
      return fields;
    }
    for (int i = 0; i < results; ++i) {
      fields.push_back(ReceiveField());
    }
    return fields;
  }

  // Waits for the drive to be idle, as the OS does by reading the control port
  // until the phase is 0, then writes `command_word` alone, with `word1` in the
  // field's word 1, recording its definition for ReceiveAccept and Complete.
  void Start(uint16_t command_word, uint16_t word1 = 0) {
    RunUntil([this] { return GetPhase() == 0; });
    def_ = GetDiskCommandDef(
        static_cast<uint8_t>(command_word >> DiskDrive::kCommandShift));
    SendField(PortWords::FromWords(command_word, word1));
  }

  // Writes `command_word` and its `arguments`, which must be as many fields as
  // the command table gives it, and returns ReceiveAccept().
  std::vector<PortWords> Command(uint16_t command_word,
                                 absl::Span<const PortWords> arguments = {}) {
    Start(command_word);
    EXPECT_EQ(static_cast<int>(arguments.size()),
              def_ != nullptr ? def_->arguments : 0);
    for (const PortWords& argument : arguments) {
      SendField(argument);
    }
    return ReceiveAccept();
  }

  // Waits for the drive to take the last command's words, as the OS does by
  // reading the control port until the phase is 2 or higher, and returns the
  // accept status and its results.
  std::vector<PortWords> ReceiveAccept() {
    RunUntil([this] { return GetPhase() >= 2; });
    return ReceiveStep(def_ != nullptr ? def_->accept_results : 0);
  }

  // Waits for the last command's completion, and returns its status and
  // results.
  std::vector<PortWords> Complete() {
    CHECK(def_ != nullptr);
    return ReceiveStep(def_->completion_results);
  }

  // Runs a latent command with no accept results, expecting an OK accept, and
  // returns its completion.
  std::vector<PortWords> Run(uint16_t command_word,
                             absl::Span<const PortWords> arguments = {}) {
    EXPECT_THAT(Command(command_word, arguments), ElementsAre(Field(kOk)));
    return Complete();
  }

  // Returns SEEK's status for `handle` and `position`.
  std::vector<PortWords> Seek(int handle, uint32_t position) {
    return Command(CommandWord(DiskDriveCommand::kSeek, handle),
                   {PortWords::FromDword(position)});
  }

  // Returns the INFO results for `handle`: its position and size.
  std::vector<PortWords> Info(int handle) {
    return Command(CommandWord(DiskDriveCommand::kInfo, handle));
  }

  // Returns the accept of `command`, a READ, WRITE, or their packed forms, for
  // `handle` and `count`.
  std::vector<PortWords> Transfer(DiskDriveCommand command, int handle,
                                  int count) {
    return Command(CommandWord(command, handle),
                   {PortWords::FromWord(static_cast<uint16_t>(count))});
  }

  // The transfers that waited for the drive, as {index, cycles waited}.
  using Stalls = std::vector<std::pair<int, Cycles>>;

  // What a read moved through the data port: the transfers, and its stalls.
  struct DataPhase {
    std::vector<PortWords> transfers;
    Stalls stalls;
  };

  // Reads `count` transfers from the data port, as INR does, each as soon as
  // the drive puts it there. A transfer waits for the disk work that reads it.
  DataPhase ReadData(int count) {
    DataPhase data;
    for (int i = 0; i < count; ++i) {
      const Cycles start = GetCycles();
      if (!RunUntil([this] { return IsSet(kDataPort); })) {
        break;
      }
      if (GetCycles() > start) {
        data.stalls.emplace_back(i, GetCycles() - start);
      }
      std::optional<PortWords> words = In(kDataPort);
      EXPECT_TRUE(words.has_value());
      data.transfers.push_back(words.value_or(PortWords()));
      Execute(1);
    }
    return data;
  }

  // Writes `transfers` to the data port, as OUTR does, each as soon as the port
  // is clear. The drive takes a transfer the cycle after it arrives, and does
  // its disk work then, so the port holds the next while that work goes on, and
  // the one after waits. Returns the stalls.
  Stalls WriteData(absl::Span<const PortWords> transfers) {
    Stalls stalls;
    for (int i = 0; i < static_cast<int>(transfers.size()); ++i) {
      const Cycles start = GetCycles();
      if (!RunUntil([this] { return !IsSet(kDataPort); })) {
        break;
      }
      if (GetCycles() > start) {
        stalls.emplace_back(i, GetCycles() - start);
      }
      EXPECT_TRUE(Out(kDataPort, transfers[i]));
      Execute(1);
    }
    return stalls;
  }

  // Expects a reset to have left the drive idle, with nothing on the data
  // port, and handle 0, a 600-word file, at `position`.
  void ExpectResetAt(int position) {
    EXPECT_EQ(GetPhase(), 0);
    EXPECT_FALSE(IsSet(kDataPort));
    EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(position),
                                     PortWords::FromDword(600)));
  }

  // WRITEs `words` to `handle`, which they all fit in, and completes it.
  void WriteWords(int handle, absl::Span<const uint16_t> words) {
    const uint16_t count = static_cast<uint16_t>(words.size());
    EXPECT_THAT(Transfer(DiskDriveCommand::kWrite, handle, count),
                ElementsAre(Field(kOk), PortWords::FromWord(count)));
    WriteData(DataWords(words));
    EXPECT_THAT(Complete(),
                ElementsAre(Field(kOk), PortWords::FromWord(count)));
  }

  // Returns `words` as transfers on the data port: one word each, or two for
  // `packed`, with a zero high word if they run out.
  static std::vector<PortWords> DataWords(absl::Span<const uint16_t> words,
                                          bool packed = false) {
    std::vector<PortWords> transfers;
    const int step = packed ? 2 : 1;
    for (int i = 0; i < static_cast<int>(words.size()); i += step) {
      transfers.push_back(PortWords::FromWords(
          words[i], packed && i + 1 < static_cast<int>(words.size())
                        ? words[i + 1]
                        : uint16_t{0}));
    }
    return transfers;
  }

  // Opens the root's file `name`, expecting it in slot `handle`.
  void OpenFile(std::string_view name, int handle = 0) {
    EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpen), NameFields(name)),
                ElementsAre(Field(kOk), PortWords::FromWord(handle)));
  }

  // Creates the file `name`, expecting it in slot `handle`.
  void CreateFile(std::string_view name, int handle = 0) {
    EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kCreate), NameFields(name)),
                ElementsAre(Field(kOk), PortWords::FromWord(handle)));
  }

  // Expects every command but CLOSE that takes a handle to fail at accept with
  // `status` for `handle`. SEEK is given a position past the end, which it
  // checks after the handle, and the transfers a count of 0.
  void ExpectHandleCommandsFail(int handle, DiskStatus status) {
    SCOPED_TRACE(handle);
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kFlush, handle)),
                ElementsAre(Field(status)));
    for (const DiskDriveCommand command :
         {DiskDriveCommand::kRead, DiskDriveCommand::kWrite,
          DiskDriveCommand::kReadPacked, DiskDriveCommand::kWritePacked}) {
      EXPECT_THAT(Transfer(command, handle, 0), ElementsAre(Field(status)));
    }
    EXPECT_THAT(Seek(handle, 0xFFFFFFFF), ElementsAre(Field(status)));
    EXPECT_THAT(Info(handle), ElementsAre(Field(status)));
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kTruncate, handle)),
                ElementsAre(Field(status)));
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kFind, handle),
                        PatternFields("*")),
                ElementsAre(Field(status)));
  }

  // Expects the commands that write to fail at accept with `status` for
  // `handle`, and READ to be accepted.
  void ExpectWritesFail(int handle, DiskStatus status) {
    SCOPED_TRACE(handle);
    for (const DiskDriveCommand command :
         {DiskDriveCommand::kWrite, DiskDriveCommand::kWritePacked}) {
      EXPECT_THAT(Transfer(command, handle, 1), ElementsAre(Field(status)));
    }
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kTruncate, handle)),
                ElementsAre(Field(status)));
    EXPECT_THAT(Transfer(DiskDriveCommand::kRead, handle, 0),
                ElementsAre(Field(kOk), PortWords::FromWord(0)));
    EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
  }

  // Returns the completion of FIND for `handle` and the pattern `text`.
  std::vector<PortWords> Find(int handle, std::string_view pattern) {
    return Run(CommandWord(DiskDriveCommand::kFind, handle),
               PatternFields(pattern));
  }

  // Returns the completion of VOLUME.
  std::vector<PortWords> Volume() {
    return Run(CommandWord(DiskDriveCommand::kVolume));
  }

  // Returns the volume a new formatted image has with `used` clusters taken
  // beyond the root's, with `sectors_per_cluster`.
  static DiskVolumeInfo GetVolume(std::string_view label,
                                  int sectors_per_cluster, int used) {
    const std::optional<DiskLayout> layout =
        ChooseDiskLayout(kSectors, sectors_per_cluster);
    CHECK(layout.has_value());
    return {.label = *DiskLabel::Parse(label),
            .sectors_per_cluster = layout->sectors_per_cluster,
            .cluster_count = layout->cluster_count,
            .free_clusters = layout->cluster_count - 1 - used};
  }

  // Detaches the drive's image, and returns its superblock.
  DiskSuperblock DetachAndReadSuperblock() {
    std::unique_ptr<DiskImage> image = drive_->DetachImage();
    DiskSuperblock::Words words = {};
    EXPECT_EQ(
        image->ReadSector(DiskLayout::kSuperblockSector, absl::MakeSpan(words)),
        kOk);
    return DiskSuperblock::FromWords(words);
  }

  // Expects DELETE, RENAME, ATTRIB, and LABEL, each with valid arguments, to
  // fail at accept with `status`.
  void ExpectChangesFail(DiskStatus status) {
    EXPECT_THAT(
        Command(CommandWord(DiskDriveCommand::kDelete), NameFields("A")),
        ElementsAre(Field(status)));
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kRename),
                        AndFields(NameFields("A"), NameFields("B"))),
                ElementsAre(Field(status)));
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kAttrib),
                        AndWord(NameFields("A"), kDiskAttribReadOnly)),
                ElementsAre(Field(status)));
    EXPECT_THAT(
        Command(CommandWord(DiskDriveCommand::kLabel), LabelFields("WORK")),
        ElementsAre(Field(status)));
  }

  // Returns whether kInterrupt is raised on core 0.
  bool IsInterruptRaised() {
    return (GetState().core.GetInterrupts() & (1u << kInterrupt)) != 0;
  }

  DiskDrive* drive_ = nullptr;
  const DiskCommandDef* def_ = nullptr;
};

//==============================================================================
// The command table
//==============================================================================

TEST(DiskCommandDefTest, EveryCommandIsAsDesigned) {
  using enum DiskDriveCommand;
  constexpr DiskCommandKind kSynchronous = DiskCommandKind::kSynchronous;
  constexpr DiskCommandKind kLatent = DiskCommandKind::kLatent;
  constexpr DiskCommandKind kReads = DiskCommandKind::kRead;
  constexpr DiskCommandKind kWrites = DiskCommandKind::kWrite;
  constexpr int kName = 4;  // 8 words, in dword fields.
  const DiskCommandDef expected[] = {
      {kOpen, false, kLatent, false, kName, 0, 1},
      {kOpenRoot, false, kLatent, false, 0, 0, 1},
      {kCreate, false, kLatent, false, kName, 0, 1},
      {kClose, true, kLatent, false, 0, 0, 0},
      {kFlush, true, kLatent, false, 0, 0, 0},
      {kRead, true, kReads, false, 1, 1, 1},
      {kWrite, true, kWrites, false, 1, 1, 1},
      {kReadPacked, true, kReads, true, 1, 1, 1},
      {kWritePacked, true, kWrites, true, 1, 1, 1},
      {kSeek, true, kSynchronous, false, 1, 0, 0},
      {kInfo, true, kSynchronous, false, 0, 2, 0},
      {kTruncate, true, kLatent, false, 0, 0, 0},
      {kFind, true, kLatent, false, kName, 0, 8},
      {kDelete, false, kLatent, false, kName, 0, 0},
      {kRename, false, kLatent, false, 2 * kName, 0, 0},
      {kAttrib, false, kLatent, false, kName + 1, 0, 0},
      {kMedia, false, kSynchronous, false, 0, 2, 0},
      {kVolume, false, kLatent, false, 0, 0, kName + 3},
      {kFormat, false, kLatent, false, kName + 1, 0, 0},
      {kLabel, false, kLatent, false, kName, 0, 0},
      {kReadBoot, false, DiskCommandKind::kReadBoot, true, 0, 0, 0},
      {kWriteBoot, false, DiskCommandKind::kWriteBoot, true, 0, 0, 0},
  };
  for (const DiskCommandDef& command : expected) {
    SCOPED_TRACE(static_cast<int>(command.command));
    const DiskCommandDef* def =
        GetDiskCommandDef(static_cast<uint8_t>(command.command));
    ASSERT_NE(def, nullptr);
    EXPECT_EQ(def->command, command.command);
    EXPECT_EQ(def->takes_handle, command.takes_handle);
    EXPECT_EQ(def->kind, command.kind);
    EXPECT_EQ(def->packed, command.packed);
    EXPECT_EQ(def->arguments, command.arguments);
    EXPECT_EQ(def->accept_results, command.accept_results);
    EXPECT_EQ(def->completion_results, command.completion_results);
  }

  // Every other code is no command.
  int defined = 0;
  for (int code = 0; code < 256; ++code) {
    if (GetDiskCommandDef(static_cast<uint8_t>(code)) != nullptr) {
      ++defined;
    }
  }
  EXPECT_EQ(defined, static_cast<int>(std::size(expected)));
}

//==============================================================================
// Settings
//==============================================================================

TEST(DiskDriveFactoryTest, WiresThreePortsAndAnInterrupt) {
  absl::StatusOr<std::unique_ptr<DiskDriveFactory>> factory =
      DiskDriveFactory::Create(
          "Drive", {.port = 16, .interrupt = {.interrupt = 3, .cores = 1}});
  ASSERT_TRUE(factory.ok()) << factory.status();
  const DeviceConfig& config = (*factory)->GetConfig();
  EXPECT_EQ(config.name, "Drive");
  EXPECT_THAT(config.ports, ElementsAre(16, 17, 18));
  EXPECT_THAT(config.interrupts,
              ElementsAre(DeviceInterrupt{.interrupt = 3, .cores = 1}));
}

TEST(DiskDriveFactoryTest, RefusesBadSettings) {
  for (const int slots : {0, DiskDriveSettings::kMaxSlots + 1}) {
    SCOPED_TRACE(slots);
    absl::StatusOr<std::unique_ptr<DiskDriveFactory>> factory =
        DiskDriveFactory::Create("Drive", {.slots = slots});
    EXPECT_EQ(factory.status().code(), absl::StatusCode::kInvalidArgument);
    EXPECT_THAT(factory.status().message(), HasSubstr("slots"));
  }
  EXPECT_TRUE(DiskDriveFactory::Create("Drive", {.slots = 1}).ok());
  EXPECT_TRUE(
      DiskDriveFactory::Create("Drive", {.slots = DiskDriveSettings::kMaxSlots})
          .ok());

  const DiskHardwareSettings refused[] = {
      {.read_cache_sectors = -1},
      {.read_words_per_sec = -1},
      {.write_words_per_sec = -1},
      {.cache_words_per_sec = -1},
      {.latency = -absl::Milliseconds(1)},
      {.latency = DiskDriveSettings::kMaxLatency + absl::Nanoseconds(1)},
      {.latency = absl::InfiniteDuration()},
  };
  for (const DiskHardwareSettings& hardware : refused) {
    SCOPED_TRACE(hardware.latency);
    absl::StatusOr<std::unique_ptr<DiskDriveFactory>> factory =
        DiskDriveFactory::Create("Drive", {.hardware = hardware});
    EXPECT_EQ(factory.status().code(), absl::StatusCode::kInvalidArgument);
  }
  EXPECT_TRUE(DiskDriveFactory::Create("Drive", {.hardware = {}}).ok());
  EXPECT_TRUE(
      DiskDriveFactory::Create(
          "Drive", {.hardware = {.latency = DiskDriveSettings::kMaxLatency}})
          .ok());
  EXPECT_EQ(DiskDriveSettings::kMaxLatency, absl::Minutes(1));
}

TEST(DiskDrivePresetTest, PresetsAreAsDesigned) {
  struct Preset {
    DiskDrivePreset preset;
    int64_t read_kiw_per_sec;
    int64_t write_kiw_per_sec;
    int latency_ms;
  };
  const Preset presets[] = {
      {DiskDrivePreset::kFloppy360k, 8, 6, 150},
      {DiskDrivePreset::kFloppy720k, 10, 8, 120},
      {DiskDrivePreset::kFloppy1440k, 20, 16, 100},
      {DiskDrivePreset::kHardDisk10m, 40, 40, 85},
      {DiskDrivePreset::kHardDisk20m, 64, 64, 65},
      {DiskDrivePreset::kHardDisk40m, 128, 128, 40},
  };
  for (const Preset& preset : presets) {
    SCOPED_TRACE(static_cast<int>(preset.preset));
    EXPECT_EQ(GetDiskDrivePreset(preset.preset),
              (DiskHardwareSettings{
                  .read_cache_sectors = 4,
                  .read_words_per_sec = preset.read_kiw_per_sec * 1024,
                  .write_words_per_sec = preset.write_kiw_per_sec * 1024,
                  .cache_words_per_sec = 1'000'000,
                  .latency = absl::Milliseconds(preset.latency_ms)}));
  }
  EXPECT_EQ(kDiskDriveReadCacheSectors, 4);
  EXPECT_EQ(kDiskDriveCacheWordsPerSec, 1'000'000);
  EXPECT_EQ(DiskDriveSettings().hardware,
            GetDiskDrivePreset(DiskDrivePreset::kFloppy1440k));
  EXPECT_EQ(DiskDriveSettings().slots, 4);
}

//==============================================================================
// The control port
//==============================================================================

TEST_F(DiskDriveTest, ControlWordShowsTheDisk) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  EXPECT_EQ(GetControlWord(), 0);
  EXPECT_FALSE(IsSet(kControlPort));

  // The drive shows the disk once it has read its superblock.
  drive_->AttachImage(MakeImage());
  Execute(kAttachCycles);
  EXPECT_EQ(GetControlWord(), 0);
  Execute(2);
  EXPECT_EQ(GetControlWord(), DiskDrive::kDiskAttached);
  EXPECT_FALSE(IsSet(kControlPort));

  ASSERT_NE(drive_->DetachImage(), nullptr);
  Execute(1);
  EXPECT_EQ(GetControlWord(), 0);
}

TEST_F(DiskDriveTest, ControlWordShowsEachPhaseAndTheCommand) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  const uint16_t disk = DiskDrive::kDiskAttached;

  // Sending, then working, then sending the completion, then idle.
  Start(CommandWord(DiskDriveCommand::kOpenRoot));
  Execute(1);
  EXPECT_EQ(GetControlWord(), 0x1100 | disk | 3);
  ASSERT_EQ(ReceiveField(), Field(kOk));
  EXPECT_EQ(GetControlWord(), 0x1100 | disk | 2);
  RunUntil([this] { return IsSet(kCommandPort); });
  EXPECT_EQ(GetControlWord(), 0x1100 | disk | 3);
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_EQ(GetControlWord(), disk);
}

TEST_F(DiskDriveTest, ResetAbortsEveryPhase) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // Arguments, as OPEN takes a name, with an argument field waiting.
  Start(CommandWord(DiskDriveCommand::kOpen));
  Execute(1);
  EXPECT_EQ(GetControlWord(), 0x1000 | DiskDrive::kDiskAttached | 1);
  ASSERT_TRUE(Out(kCommandPort, PortWords::FromWords(1, 2)));
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetControlWord(), DiskDrive::kDiskAttached);
  EXPECT_FALSE(IsSet(kCommandPort));

  // Sending, with the accept status waiting, and a word in the data port.
  Start(CommandWord(DiskDriveCommand::kMedia));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  ASSERT_TRUE(Out(kDataPort, PortWords::FromWord(5)));
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetPhase(), 0);
  EXPECT_FALSE(IsSet(kCommandPort));
  EXPECT_FALSE(IsSet(kDataPort));

  // Working: the reset waits for OPEN ROOT's read of the FAT, which still
  // opens the root, but its completion is dropped, and no interrupt is raised.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  ASSERT_EQ(GetPhase(), 2);
  const Cycles requested = GetCycles();
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetCycles(), requested + kReadCycles);
  EXPECT_EQ(GetPhase(), 0);
  Execute(10);
  EXPECT_FALSE(IsSet(kCommandPort));
  EXPECT_FALSE(IsInterruptRaised());

  // Sending the completion, which is in use, as the root is open: the
  // completion is dropped, though its interrupt has been raised.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  ASSERT_TRUE(RunUntil([this] { return GetPhase() == 3; }));
  EXPECT_TRUE(IsSet(kCommandPort));
  EXPECT_TRUE(IsInterruptRaised());
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetPhase(), 0);
  EXPECT_FALSE(IsSet(kCommandPort));

  // The next commands work as usual, and the root is open in slot 0.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(Field(kOk), PortWords::FromWord(kSectors),
                          PortWords::FromWord(0)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));
}

TEST_F(DiskDriveTest, OtherRequestsDoNothing) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  Start(CommandWord(DiskDriveCommand::kMedia));
  Execute(1);
  for (const uint16_t request : {uint16_t{0}, uint16_t{0xFFFE}}) {
    SCOPED_TRACE(request);
    Request(request);
    EXPECT_EQ(GetPhase(), 3);
  }
  EXPECT_THAT(ReceiveAccept(),
              ElementsAre(Field(kOk), PortWords::FromWord(kSectors),
                          PortWords::FromWord(0)));
}

TEST_F(DiskDriveTest, OnlyAResetWaitsForTheOtherPorts) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  std::unique_ptr<Lock> lock = LockPort(kDataPort);
  ASSERT_TRUE(lock->IsLocked());
  ASSERT_TRUE(Out(kControlPort, PortWords::FromWord(0)));
  Execute(2);
  EXPECT_FALSE(IsSet(kControlPort));

  ASSERT_TRUE(Out(kControlPort, PortWords::FromWord(DiskDrive::kResetRequest)));
  Execute(5);
  EXPECT_TRUE(IsSet(kControlPort));
  lock.reset();
  Execute(1);
  EXPECT_FALSE(IsSet(kControlPort));
}

TEST_F(DiskDriveTest, ControlWordWaitsForTheControlPort) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  std::unique_ptr<Lock> lock = LockPort(kControlPort);
  ASSERT_TRUE(lock->IsLocked());
  drive_->AttachImage(MakeImage());
  Execute(kAttachCycles + 5);
  EXPECT_EQ(GetControlWord(), 0);
  lock.reset();
  Execute(1);
  EXPECT_EQ(GetControlWord(), DiskDrive::kDiskAttached);
}

TEST_F(DiskDriveTest, AnIdleDriveDoesNotWaitForTheCommandPort) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  std::unique_ptr<Lock> lock = LockPort(kCommandPort);
  ASSERT_TRUE(lock->IsLocked());
  drive_->AttachImage(MakeImage());
  Execute(kAttachCycles + 1);
  EXPECT_EQ(GetControlWord(), DiskDrive::kDiskAttached);
}

//==============================================================================
// The command protocol
//==============================================================================

TEST_F(DiskDriveTest, TheDriveTakesAWordTheCycleAfter) {
  ASSERT_TRUE(InitDrive());
  Execute(1);

  // Until the drive takes the command word, the flag is the OS's, and the
  // phase is still 0, so the OS doesn't read its own word as the status.
  ASSERT_TRUE(Out(kCommandPort,
                  PortWords::FromWord(CommandWord(DiskDriveCommand::kMedia))));
  EXPECT_EQ(GetPhase(), 0);
  Execute(1);
  EXPECT_EQ(GetPhase(), 3);
  EXPECT_EQ(ReceiveField(), Field(DiskStatus::kNoDisk));
  EXPECT_EQ(GetPhase(), 0);
}

TEST_F(DiskDriveTest, AWordFieldIgnoresWord1) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  Start(CommandWord(DiskDriveCommand::kMedia), 0x1234);
  EXPECT_THAT(ReceiveAccept(),
              ElementsAre(Field(kOk), PortWords::FromWord(kSectors),
                          PortWords::FromWord(0)));
}

TEST_F(DiskDriveTest, AnUnknownCommandIsBadCommandAtOnce) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  for (const int code : {0x00, 0x15, 0x99, 0xFF}) {
    SCOPED_TRACE(code);
    SendField(PortWords::FromWord(static_cast<uint16_t>(code << 8 | 3)));
    Execute(1);
    EXPECT_EQ(GetControlWord(), (code << 8) | DiskDrive::kDiskAttached | 3);
    EXPECT_EQ(ReceiveField(), Field(DiskStatus::kBadCommand));
    EXPECT_EQ(GetPhase(), 0);
  }
}

TEST_F(DiskDriveTest, MediaDescribesTheMedium) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(Field(DiskStatus::kNoDisk)));

  // Unformatted is fine, as the medium is the image's header.
  drive_->AttachImage(MakeImage(/*formatted=*/false));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(Field(kOk), PortWords::FromWord(kSectors),
                          PortWords::FromWord(0)));

  std::unique_ptr<DiskImage> image = drive_->DetachImage();
  ASSERT_EQ(image->SetWriteProtected(true), DiskStatus::kOk);
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(Field(kOk), PortWords::FromWord(kSectors),
                          PortWords::FromWord(DiskMedia::kWriteProtectedFlag)));
}

TEST_F(DiskDriveTest, AFailedImageFailsCommandsWithNoHandle) {
  ASSERT_TRUE(InitDrive());
  std::unique_ptr<DiskImage> image = MakeImage();
  protocol_->SetLimits({.reads = 0});
  drive_->AttachImage(std::move(image));
  for (const DiskDriveCommand command :
       {DiskDriveCommand::kMedia, DiskDriveCommand::kOpenRoot,
        DiskDriveCommand::kVolume, DiskDriveCommand::kReadBoot,
        DiskDriveCommand::kWriteBoot}) {
    SCOPED_TRACE(static_cast<int>(command));
    EXPECT_THAT(Command(CommandWord(command)),
                ElementsAre(Field(DiskStatus::kIoError)));
  }
  for (const DiskDriveCommand command :
       {DiskDriveCommand::kOpen, DiskDriveCommand::kCreate}) {
    SCOPED_TRACE(static_cast<int>(command));
    EXPECT_THAT(Command(CommandWord(command), NameFields("HELLO")),
                ElementsAre(Field(DiskStatus::kIoError)));
  }
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kFormat),
                      AndWord(LabelFields("A"), 0)),
              ElementsAre(Field(DiskStatus::kIoError)));
  ExpectChangesFail(DiskStatus::kIoError);
}

TEST_F(DiskDriveTest, AWordInAnArgumentPhaseIsAnArgument) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  Start(CommandWord(DiskDriveCommand::kOpen));
  SendField(PortWords::FromWord(CommandWord(DiskDriveCommand::kMedia)));
  Execute(1);
  EXPECT_FALSE(IsSet(kCommandPort));
  EXPECT_EQ(GetPhase(), 1);
}

TEST_F(DiskDriveTest, OutsIsRefusedWhileSending) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  Start(CommandWord(DiskDriveCommand::kMedia));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  EXPECT_FALSE(Out(kCommandPort,
                   PortWords::FromWord(CommandWord(DiskDriveCommand::kMedia))));
  EXPECT_THAT(ReceiveAccept(),
              ElementsAre(Field(kOk), PortWords::FromWord(kSectors),
                          PortWords::FromWord(0)));
}

TEST_F(DiskDriveTest, AWordWhileWorkingIsLostUnderTheCompletion) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  ASSERT_EQ(GetPhase(), 2);
  ASSERT_TRUE(Out(kCommandPort,
                  PortWords::FromWord(CommandWord(DiskDriveCommand::kMedia))));
  Execute(10);
  EXPECT_EQ(GetPhase(), 2);
  RunUntil([this] { return GetPhase() == 3; });
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
}

//==============================================================================
// Timing and completions
//==============================================================================

TEST_F(DiskDriveTest, CompletionsWaitForTheDisk) {
  ASSERT_TRUE(InitDrive());
  Execute(1);

  // The drive is busy reading the superblock from the cycle after the attach,
  // so it takes the command word the cycle after that.
  drive_->AttachImage(MakeImage());
  const Cycles attached = GetCycles();
  Start(CommandWord(DiskDriveCommand::kOpenRoot));
  ASSERT_TRUE(RunUntil([this] { return GetPhase() >= 2; }));
  EXPECT_EQ(GetCycles(), attached + kAttachCycles + 1);

  // OPEN ROOT's work, a read of the FAT's first sector for the root's chain,
  // starts once the OS has read its accept status. It is the next sector, so
  // it has no latency, and the completion follows it.
  EXPECT_THAT(ReceiveStep(0), ElementsAre(Field(kOk)));
  const Cycles accepted = GetCycles();
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(GetCycles(), accepted + kReadCycles);
  EXPECT_EQ(GetPhase(), 3);
  EXPECT_TRUE(IsInterruptRaised());
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));

  // Closing the root writes nothing, so it completes in the run that sees its
  // accept status read.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(GetPhase(), 3);
  EXPECT_TRUE(IsSet(kCommandPort));
  EXPECT_TRUE(IsInterruptRaised());
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));
}

TEST_F(DiskDriveTest, BusyTimeRoundsUpToWholeCycles) {
  // A sector read takes 256/300ths of a second, or 853 1/3 cycles, and the
  // superblock read after the latency 863 1/3, so the drive is busy for 864.
  DiskDriveSettings settings = GetSettings();
  settings.hardware.read_words_per_sec = 300;
  ASSERT_TRUE(InitDrive(settings));
  Execute(1);
  drive_->AttachImage(MakeImage());
  const Cycles attached = GetCycles();
  Start(CommandWord(DiskDriveCommand::kMedia));
  ASSERT_TRUE(RunUntil([this] { return GetPhase() >= 2; }));
  EXPECT_EQ(GetCycles(), attached + 864 + 1);
}

TEST_F(DiskDriveTest, TheDriveDoesNothingWhileBusy) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // A field read while the drive reads a new image's superblock: the next
  // field waits for the read.
  Start(CommandWord(DiskDriveCommand::kMedia));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  drive_->AttachImage(MakeImage(/*formatted=*/true, kOtherPath));
  Execute(1);
  const Cycles attached = GetCycles();
  EXPECT_EQ(In(kCommandPort), Field(kOk));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(GetCycles(), attached + kAttachCycles);
  EXPECT_EQ(ReceiveField(), PortWords::FromWord(kSectors));
  EXPECT_EQ(ReceiveField(), PortWords::FromWord(0));

  // A command word, and a request, while the drive reads its superblock.
  drive_->AttachImage(MakeImage(/*formatted=*/true, kThirdPath));
  Execute(2);
  ASSERT_TRUE(Out(kCommandPort,
                  PortWords::FromWord(CommandWord(DiskDriveCommand::kMedia))));
  ASSERT_TRUE(Out(kControlPort, PortWords::FromWord(0)));
  Execute(kAttachCycles - 2);
  EXPECT_TRUE(IsSet(kCommandPort));
  EXPECT_TRUE(IsSet(kControlPort));
  EXPECT_EQ(GetPhase(), 0);
  Execute(1);
  EXPECT_FALSE(IsSet(kControlPort));
  EXPECT_EQ(GetPhase(), 3);
}

TEST_F(DiskDriveTest, AResponseTakenAsTheHostAttachesEndsOnceItIsRead) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // The OS takes MEDIA's last field, and before the drive runs again, the
  // host attaches an image. The drive sees the field taken only once it has
  // read the superblock, so the phase shows sending until then, and the OS
  // waits for idle before its next command word.
  Start(CommandWord(DiskDriveCommand::kMedia));
  RunUntil([this] { return GetPhase() >= 2; });
  EXPECT_EQ(ReceiveField(), Field(kOk));
  EXPECT_EQ(ReceiveField(), PortWords::FromWord(kSectors));
  RunUntil([this] { return IsSet(kCommandPort); });
  EXPECT_EQ(In(kCommandPort), PortWords::FromWord(0));
  drive_->AttachImage(MakeImage(/*formatted=*/true, kOtherPath));
  Execute(1);
  const Cycles attached = GetCycles();
  ASSERT_TRUE(RunUntil([this] { return GetPhase() != 3; }));
  EXPECT_EQ(GetCycles(), attached + kAttachCycles);
  EXPECT_EQ(GetPhase(), 0);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(Field(kOk), PortWords::FromWord(kSectors),
                          PortWords::FromWord(0)));
}

TEST_F(DiskDriveTest, TheDriveSleepsWhileIdle) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  EXPECT_EQ(drive_->GetDueCycle(), kSleepForever);
  AttachFormatted();
  EXPECT_EQ(drive_->GetDueCycle(), kSleepForever);

  // While working, it runs again once the disk is done.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(drive_->GetDueCycle(), GetCycles() - 1 + kReadCycles);
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_EQ(drive_->GetDueCycle(), kSleepForever);
}

TEST_F(DiskDriveTest, AHeldCommandPortDelaysTheDrive) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  ASSERT_TRUE(Out(kCommandPort,
                  PortWords::FromWord(CommandWord(DiskDriveCommand::kMedia))));
  {
    std::unique_ptr<Lock> lock = LockPort(kCommandPort);
    ASSERT_TRUE(lock->IsLocked());
    Execute(5);
    EXPECT_EQ(GetPhase(), 0);
  }
  Execute(1);
  EXPECT_EQ(GetPhase(), 3);

  // The data port isn't the drive's to lock while it has no data phase.
  std::unique_ptr<Lock> data_lock = LockPort(kDataPort);
  ASSERT_TRUE(data_lock->IsLocked());
  EXPECT_THAT(ReceiveStep(0), ElementsAre(Field(DiskStatus::kNoDisk)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(Field(DiskStatus::kNoDisk)));
}

TEST_F(DiskDriveTest, ACompletionWaitsForTheCommandPort) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));

  // The phase stays at working until the completion's status is on the port.
  {
    std::unique_ptr<Lock> lock = LockPort(kCommandPort);
    ASSERT_TRUE(lock->IsLocked());
    Execute(kReadCycles + 5);
    EXPECT_EQ(GetPhase(), 2);
    EXPECT_FALSE(IsSet(kCommandPort));
  }
  Execute(1);
  EXPECT_EQ(GetPhase(), 3);
  EXPECT_TRUE(IsSet(kCommandPort));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
}

TEST_F(DiskDriveTest, ACompletionWaitsForTheControlPort) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));

  // The status, phase 3, and the interrupt go out together.
  {
    std::unique_ptr<Lock> lock = LockPort(kControlPort);
    ASSERT_TRUE(lock->IsLocked());
    Execute(kReadCycles + 5);
    EXPECT_EQ(GetPhase(), 2);
    EXPECT_FALSE(IsSet(kCommandPort));
    EXPECT_FALSE(IsInterruptRaised());
  }
  Execute(1);
  EXPECT_EQ(GetPhase(), 3);
  EXPECT_TRUE(IsSet(kCommandPort));
  EXPECT_TRUE(IsInterruptRaised());
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
}

//==============================================================================
// Commands
//==============================================================================

TEST_F(DiskDriveTest, OpenRootChecksAtAccept) {
  DiskDriveSettings settings = GetSettings();
  settings.slots = 1;
  ASSERT_TRUE(InitDrive(settings));
  Execute(1);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(DiskStatus::kNoDisk)));

  // With the one slot taken, a second is too many open at accept, rather than
  // in use at completion.
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(DiskStatus::kTooManyOpen)));

  // An unformatted disk comes before too many open, with the slot stale.
  drive_->AttachImage(MakeImage(/*formatted=*/false, kOtherPath));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(DiskStatus::kUnformatted)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(DiskStatus::kUnformatted)));
}

TEST_F(DiskDriveTest, CloseNeedsAnOpenSlot) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  for (const int handle : {0, 3, 4, 0xFE}) {
    SCOPED_TRACE(handle);
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, handle)),
                ElementsAre(Field(DiskStatus::kBadHandle)));
  }
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(DiskStatus::kBadHandle)));
}

TEST_F(DiskDriveTest, CloseFreesAStaleSlot) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));

  // Attaching over the image leaves slot 0 stale, which closes without error,
  // as the root had nothing to write.
  drive_->AttachImage(MakeImage(/*formatted=*/true, kOtherPath));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));

  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(DiskStatus::kBadHandle)));
}

TEST_F(DiskDriveTest, OpenOpensAFileByName) {
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("HELLO.TXT", 300));

  // Names match without regard to case.
  EXPECT_THAT(
      Run(CommandWord(DiskDriveCommand::kOpen), NameFields("hello.txt")),
      ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(0),
                                   PortWords::FromDword(300)));

  // A file open in one slot can't be opened in another, and the next free
  // slot is the lowest.
  EXPECT_THAT(
      Run(CommandWord(DiskDriveCommand::kOpen), NameFields("HELLO.TXT")),
      ElementsAre(Field(DiskStatus::kInUse)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpen), NameFields("HELLO")),
              ElementsAre(Field(DiskStatus::kNotFound)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(1)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(
      Run(CommandWord(DiskDriveCommand::kOpen), NameFields("HELLO.TXT")),
      ElementsAre(Field(kOk), PortWords::FromWord(0)));
}

TEST_F(DiskDriveTest, OpenChecksAtAccept) {
  DiskDriveSettings settings = GetSettings();
  settings.slots = 1;
  ASSERT_TRUE(InitDrive(settings));
  Execute(1);

  // The name comes before the disk.
  for (const DiskFilename::Words& words :
       {DiskFilename::Words(), GapWords(),
        DiskFilename::ParsePattern("*.TXT")->GetWords()}) {
    EXPECT_THAT(
        Command(CommandWord(DiskDriveCommand::kOpen), NameFields(words)),
        ElementsAre(Field(DiskStatus::kBadName)));
  }
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(DiskStatus::kNoDisk)));

  // With the one slot taken, by the root or by a stale slot, the disk comes
  // before too many open.
  drive_->AttachImage(MakeImageWithFile("A", 1));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(DiskStatus::kTooManyOpen)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpen),
                      NameFields(DiskFilename::Words())),
              ElementsAre(Field(DiskStatus::kBadName)));
  ASSERT_NE(drive_->DetachImage(), nullptr);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(DiskStatus::kNoDisk)));
  drive_->AttachImage(MakeImage(/*formatted=*/false, kOtherPath));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(DiskStatus::kUnformatted)));
  std::unique_ptr<DiskImage> image = MakeImage(/*formatted=*/true, kThirdPath);
  protocol_->SetLimits({.reads = 0});
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(DiskStatus::kIoError)));
}

TEST_F(DiskDriveTest, CreateMakesAnEmptyFileStampedWithTheClock) {
  ASSERT_TRUE(InitDrive());
  SetNow(kNow);
  AttachFormatted();
  EXPECT_THAT(
      Run(CommandWord(DiskDriveCommand::kCreate), NameFields("new.txt")),
      ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(0),
                                   PortWords::FromDword(0)));

  // A name that is taken exists, even open.
  EXPECT_THAT(
      Run(CommandWord(DiskDriveCommand::kCreate), NameFields("NEW.TXT")),
      ElementsAre(Field(DiskStatus::kExists)));

  // The file is on the disk while it is still open.
  std::optional<DiskDirEntry> entry = DetachAndFindEntry("NEW.TXT");
  ASSERT_TRUE(entry.has_value());
  EXPECT_EQ(entry->GetSize(), 0);
  EXPECT_EQ(entry->GetFirstCluster(), 0);
  EXPECT_EQ(entry->GetModified(), DateTime::FromCivil(kNow));
}

TEST_F(DiskDriveTest, WithAnEmptyClockFilesAreStampedNoDate) {
  with_clock_ = false;
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(
      Run(CommandWord(DiskDriveCommand::kCreate), NameFields("new.txt")),
      ElementsAre(Field(kOk), PortWords::FromWord(0)));
  std::optional<DiskDirEntry> entry = DetachAndFindEntry("NEW.TXT");
  ASSERT_TRUE(entry.has_value());
  EXPECT_EQ(entry->GetModified(), DateTime());
}

TEST_F(DiskDriveTest, CreateChecksAtAccept) {
  DiskDriveSettings settings = GetSettings();
  settings.slots = 1;
  ASSERT_TRUE(InitDrive(settings));
  Execute(1);

  // The name comes before the disk.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kCreate),
                      NameFields(DiskFilename::Words())),
              ElementsAre(Field(DiskStatus::kBadName)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kCreate), NameFields("A")),
              ElementsAre(Field(DiskStatus::kNoDisk)));

  // With the one slot taken, write protected comes before too many open.
  AttachFormatted();
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kCreate), NameFields("A")),
              ElementsAre(Field(DiskStatus::kTooManyOpen)));
  std::unique_ptr<DiskImage> image = drive_->DetachImage();
  ASSERT_EQ(image->SetWriteProtected(true), kOk);
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kCreate), NameFields("A")),
              ElementsAre(Field(DiskStatus::kWriteProtected)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kCreate),
                      NameFields(DiskFilename::Words())),
              ElementsAre(Field(DiskStatus::kBadName)));

  // OPEN only reads, so a protected disk doesn't stop it.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(DiskStatus::kTooManyOpen)));

  // An unformatted disk comes after write protected, and before too many open.
  image = MakeImage(/*formatted=*/false, kOtherPath);
  ASSERT_EQ(image->SetWriteProtected(true), kOk);
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kCreate), NameFields("A")),
              ElementsAre(Field(DiskStatus::kWriteProtected)));
  image = drive_->DetachImage();
  ASSERT_EQ(image->SetWriteProtected(false), kOk);
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kCreate), NameFields("A")),
              ElementsAre(Field(DiskStatus::kUnformatted)));
}

TEST_F(DiskDriveTest, CloseOfEverySlotClosesThemAll) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // With nothing open, it does nothing.
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, DiskDrive::kAllSlots)),
              ElementsAre(Field(kOk)));

  // Slot 1 is free, and is skipped. Slot 0 is stale, and had nothing to write.
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  drive_->AttachImage(MakeImage(/*formatted=*/true, kOtherPath));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(1)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kCreate), NameFields("A")),
              ElementsAre(Field(kOk), PortWords::FromWord(2)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 1)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kCreate), NameFields("B")),
              ElementsAre(Field(kOk), PortWords::FromWord(1)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 1)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, DiskDrive::kAllSlots)),
              ElementsAre(Field(kOk)));
  for (int handle = 0; handle < 4; ++handle) {
    SCOPED_TRACE(handle);
    EXPECT_THAT(Info(handle), ElementsAre(Field(DiskStatus::kBadHandle)));
  }
}

TEST_F(DiskDriveTest, FlushWritesNothingUnwritten) {
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 10));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(kOk), PortWords::FromWord(1)));
  protocol_->CollectWriteOffsets();

  // A directory's handle flushes too.
  for (const int handle : {0, 1}) {
    SCOPED_TRACE(handle);
    EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kFlush, handle)),
                ElementsAre(Field(kOk)));
  }
  EXPECT_THAT(protocol_->CollectWriteOffsets(), IsEmpty());
}

TEST_F(DiskDriveTest, SeekMovesThePosition) {
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 300));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));

  // The position may be anywhere up to the size.
  EXPECT_THAT(Seek(0, 300), ElementsAre(Field(kOk)));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(300),
                                   PortWords::FromDword(300)));
  EXPECT_THAT(Seek(0, 100), ElementsAre(Field(kOk)));

  // Past the end, or past the largest size, leaves it as it was.
  for (const uint32_t position :
       {301u, static_cast<uint32_t>(DiskDirEntry::kMaxSize) + 1, 0x80000000u,
        0xFFFFFFFFu}) {
    SCOPED_TRACE(position);
    EXPECT_THAT(Seek(0, position), ElementsAre(Field(DiskStatus::kPastEnd)));
  }
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(100),
                                   PortWords::FromDword(300)));
  EXPECT_THAT(Seek(0, 0), ElementsAre(Field(kOk)));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(0),
                                   PortWords::FromDword(300)));
}

TEST_F(DiskDriveTest, SeekAndInfoWorkOnTheRoot) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  const std::vector<PortWords> info = Info(0);
  ASSERT_EQ(info.size(), 3);
  const uint32_t size = info[2].GetDword();
  EXPECT_GT(size, 0);
  EXPECT_THAT(Seek(0, size), ElementsAre(Field(kOk)));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(size),
                                   PortWords::FromDword(size)));
  EXPECT_THAT(Seek(0, size + 1), ElementsAre(Field(DiskStatus::kPastEnd)));
}

TEST_F(DiskDriveTest, HandleCommandsCheckTheHandleFirst) {
  DiskDriveSettings settings = GetSettings();
  settings.slots = 2;
  ASSERT_TRUE(InitDrive(settings));
  drive_->AttachImage(MakeImageWithFile("A", 10));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));

  ExpectHandleCommandsFail(1, DiskStatus::kBadHandle);
  ExpectHandleCommandsFail(2, DiskStatus::kBadHandle);
  ExpectHandleCommandsFail(0xFE, DiskStatus::kBadHandle);

  // Slot 0 is stale on another disk.
  drive_->AttachImage(MakeImageWithFile("A", 10, kOtherPath));
  ExpectHandleCommandsFail(0, DiskStatus::kDiskChanged);
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));

  // Once the image fails, every open handle fails, until it closes.
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  protocol_->SetLimits({.reads = 0});
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(DiskStatus::kIoError)));
  ExpectHandleCommandsFail(0, DiskStatus::kIoError);
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  ExpectHandleCommandsFail(0, DiskStatus::kBadHandle);
}

TEST_F(DiskDriveTest, DetachingLeavesWorkDoneAsItIs) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // OPEN ROOT's work is done once the OS has read its accept status, so it
  // completes as it did, after its time on the disk.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  const Cycles accepted = GetCycles();
  ASSERT_NE(drive_->DetachImage(), nullptr);
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(GetCycles(), accepted + kReadCycles);
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_EQ(GetControlWord(), 0);
}

TEST_F(DiskDriveTest, DetachingFailsWorkNotStarted) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // The accept status is still to be read, so the work finds no disk, and
  // completes in the run that sees the accept status read.
  Start(CommandWord(DiskDriveCommand::kOpenRoot));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  ASSERT_NE(drive_->DetachImage(), nullptr);
  EXPECT_EQ(ReceiveField(), Field(kOk));
  EXPECT_TRUE(IsSet(kCommandPort));
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kNoDisk)));
  EXPECT_EQ(GetControlWord(), 0);
}

//==============================================================================
// Reading and writing
//==============================================================================

TEST_F(DiskDriveTest, ReadStreamsTheFile) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());

  // disk_design.md's example: a 300-word file, read with a count of 500. Its
  // sectors follow the root's, which OPEN read last, so neither load seeks.
  drive_->AttachImage(MakeImageWithFile("A", 300));
  OpenFile("A");
  EXPECT_THAT(Transfer(kRead, 0, 500),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));
  // The second sector loads from when the OS takes the word two before it,
  // which here is a cycle before it takes the next.
  const DataPhase data = ReadData(300);
  EXPECT_EQ(data.transfers, DataWords(FileWords(0, 300)));
  EXPECT_THAT(data.stalls,
              ElementsAre(Pair(0, kReadCycles),
                          Pair(kDiskSectorWords, kReadCycles - 1)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(300)));
  EXPECT_EQ(GetControlWord(), DiskDrive::kDiskAttached);
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(300),
                                   PortWords::FromDword(300)));

  // Going back to the start seeks.
  EXPECT_THAT(Seek(0, 10), ElementsAre(Field(kOk)));
  EXPECT_THAT(Transfer(kRead, 0, 5),
              ElementsAre(Field(kOk), PortWords::FromWord(5)));
  const DataPhase again = ReadData(5);
  EXPECT_EQ(again.transfers, DataWords(FileWords(10, 5)));
  EXPECT_THAT(again.stalls, ElementsAre(Pair(0, kLatencyCycles + kReadCycles)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(5)));
}

TEST_F(DiskDriveTest, ReadLoadsTheNextTransferWhileTheOsTakesTheLast) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 300));
  OpenFile("A");
  EXPECT_THAT(Transfer(kRead, 0, 300),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));

  // Taking the sector's second last word starts the next sector's load, so
  // the OS's time spent before taking the last is time it doesn't wait.
  constexpr Cycles kOsCycles = 400;
  EXPECT_EQ(ReadData(kDiskSectorWords - 1).transfers,
            DataWords(FileWords(0, kDiskSectorWords - 1)));
  Execute(kOsCycles);
  const DataPhase data = ReadData(2);
  EXPECT_EQ(data.transfers, DataWords(FileWords(kDiskSectorWords - 1, 2)));
  EXPECT_THAT(data.stalls, ElementsAre(Pair(1, Le(kReadCycles - kOsCycles))));
}

TEST_F(DiskDriveTest, ReadAcceptsWhatIsLeft) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 300));
  OpenFile("A");

  // A count of 0 completes at once, with no data phase.
  EXPECT_THAT(Transfer(kRead, 0, 0),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  Cycles start = GetCycles();
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_LT(GetCycles() - start, kLatencyCycles);

  // n is the count, or the words left, or for packed reads, the dwords they
  // need. Each is cut short with a reset, which leaves the position as it was.
  struct Case {
    DiskDriveCommand command;
    int position;
    int count;
    int n;
  };
  for (const Case& c :
       {Case{kRead, 0, 100, 100}, Case{kRead, 250, 100, 50},
        Case{kReadPacked, 0, 100, 100}, Case{kReadPacked, 250, 100, 25},
        Case{kReadPacked, 251, 100, 25}, Case{kReadPacked, 251, 10, 10},
        Case{kReadPacked, 299, 10, 1}}) {
    SCOPED_TRACE(absl::StrCat(static_cast<int>(c.command), " at ", c.position,
                              " of ", c.count));
    EXPECT_THAT(Seek(0, c.position), ElementsAre(Field(kOk)));
    EXPECT_THAT(Transfer(c.command, 0, c.count),
                ElementsAre(Field(kOk), PortWords::FromWord(c.n)));
    Request(DiskDrive::kResetRequest);
    EXPECT_THAT(Info(0),
                ElementsAre(Field(kOk), PortWords::FromDword(c.position),
                            PortWords::FromDword(300)));
  }

  // At the end, n is 0, completing at once.
  EXPECT_THAT(Seek(0, 300), ElementsAre(Field(kOk)));
  for (const DiskDriveCommand command : {kRead, kReadPacked}) {
    EXPECT_THAT(Transfer(command, 0, 5),
                ElementsAre(Field(kOk), PortWords::FromWord(0)));
    start = GetCycles();
    EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
    EXPECT_LT(GetCycles() - start, kLatencyCycles);
  }

  // A directory's handle reads its entries, which start with their names.
  EXPECT_THAT(Run(CommandWord(kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(1)));
  EXPECT_THAT(Transfer(kReadPacked, 1, 4),
              ElementsAre(Field(kOk), PortWords::FromWord(4)));
  EXPECT_EQ(ReadData(4).transfers, NameFields("A"));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(4)));
}

TEST_F(DiskDriveTest, WritesNeedAWritableFile) {
  DiskFileSystem file_system({}, 1);
  file_system.AttachImage(MakeImage());
  AddFile(file_system, "A", 10);
  AddFile(file_system, "RO", 10);
  ASSERT_EQ(file_system.SetAttributes(*DiskFilename::Parse("RO"),
                                      kDiskAttribReadOnly),
            kOk);
  std::unique_ptr<DiskImage> image = file_system.DetachImage();

  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  OpenFile("RO", 1);
  ExpectWritesFail(0, DiskStatus::kWrongHandleType);
  ExpectWritesFail(1, DiskStatus::kReadOnly);

  // On a protected disk, the handle's type and the file's attribute come
  // first.
  image = drive_->DetachImage();
  ASSERT_EQ(image->SetWriteProtected(true), kOk);
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, DiskDrive::kAllSlots)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  OpenFile("RO", 1);
  OpenFile("A", 2);
  ExpectWritesFail(0, DiskStatus::kWrongHandleType);
  ExpectWritesFail(1, DiskStatus::kReadOnly);
  ExpectWritesFail(2, DiskStatus::kWriteProtected);
}

TEST_F(DiskDriveTest, WriteTakesEachWord) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  SetNow(kNow);

  // disk_design.md's example: 100 words written at the end of a 500-word
  // file. The OS writes a transfer while the drive works on the one before, so
  // it waits at the one after: for word 0, which loads its sector, seeking
  // from the root's (less the cycle the drive takes the word in), and word 12,
  // which writes it back, seeking from reading, and starts a new cluster.
  drive_->AttachImage(MakeImageWithFile("A", 500));
  OpenFile("A");
  EXPECT_THAT(Seek(0, 500), ElementsAre(Field(kOk)));
  EXPECT_THAT(Transfer(kWrite, 0, 100),
              ElementsAre(Field(kOk), PortWords::FromWord(100)));
  EXPECT_THAT(WriteData(DataWords(FileWords(500, 100))),
              ElementsAre(Pair(2, kLatencyCycles + kReadCycles - 1),
                          Pair(14, kLatencyCycles + kWriteCycles - 1)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(100)));
  EXPECT_EQ(GetControlWord(), DiskDrive::kDiskAttached);
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(600),
                                   PortWords::FromDword(600)));

  // A count of 0 completes at once.
  EXPECT_THAT(Transfer(kWrite, 0, 0),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  const Cycles start = GetCycles();
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_LT(GetCycles() - start, kLatencyCycles);

  // CLOSE commits the file, stamped with the clock.
  SetNow(kLater);
  EXPECT_THAT(Run(CommandWord(kClose, 0)), ElementsAre(Field(kOk)));
  drive_->DetachImage();
  ExpectCommitted("A", FileWords(0, 600), kLater);
}

TEST_F(DiskDriveTest, WriteAcceptsWhatFits) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());

  // "A" is a word into its cluster, with one cluster free, so 512 words fit
  // from its start, and 511 from its end.
  drive_->AttachImage(MakeFullImage(1, 1));
  OpenFile("A");
  struct Case {
    DiskDriveCommand command;
    int position;
    int count;
    int n;
  };
  for (const Case& c :
       {Case{kWrite, 0, 1000, 512}, Case{kWrite, 0, 512, 512},
        Case{kWrite, 0, 10, 10}, Case{kWritePacked, 0, 1000, 256},
        Case{kWrite, 1, 1000, 511}, Case{kWritePacked, 1, 1000, 255},
        Case{kWritePacked, 1, 100, 100}}) {
    SCOPED_TRACE(absl::StrCat(static_cast<int>(c.command), " at ", c.position,
                              " of ", c.count));
    EXPECT_THAT(Seek(0, c.position), ElementsAre(Field(kOk)));
    EXPECT_THAT(Transfer(c.command, 0, c.count),
                ElementsAre(Field(kOk), PortWords::FromWord(c.n)));
    Request(DiskDrive::kResetRequest);
    EXPECT_THAT(Info(0),
                ElementsAre(Field(kOk), PortWords::FromDword(c.position),
                            PortWords::FromDword(1)));
  }

  // When only some of the count fits, the OS writes what does, and the
  // completion is disk full.
  EXPECT_THAT(Seek(0, 1), ElementsAre(Field(kOk)));
  EXPECT_THAT(Transfer(kWrite, 0, 600),
              ElementsAre(Field(kOk), PortWords::FromWord(511)));
  WriteData(DataWords(FileWords(1, 511)));
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kDiskFull)));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(512),
                                   PortWords::FromDword(512)));

  // With nothing left to fit, a count is disk full at accept, but a count of
  // 0 isn't. A dword doesn't fit in the last word.
  for (const DiskDriveCommand command : {kWrite, kWritePacked}) {
    EXPECT_THAT(Transfer(command, 0, 1),
                ElementsAre(Field(DiskStatus::kDiskFull)));
    EXPECT_THAT(Transfer(command, 0, 0),
                ElementsAre(Field(kOk), PortWords::FromWord(0)));
    EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(0)));
  }
  EXPECT_THAT(Seek(0, 511), ElementsAre(Field(kOk)));
  EXPECT_THAT(Transfer(kWritePacked, 0, 1),
              ElementsAre(Field(DiskStatus::kDiskFull)));
  EXPECT_THAT(Transfer(kWrite, 0, 1),
              ElementsAre(Field(kOk), PortWords::FromWord(1)));
  WriteData(DataWords(FileWords(511, 1)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(1)));
  EXPECT_THAT(Run(CommandWord(kClose, 0)), ElementsAre(Field(kOk)));
  EXPECT_EQ(DetachAndReadFile("A"), FileWords(0, 512));
}

TEST_F(DiskDriveTest, PackedReadEndsAnOddFileWithZero) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 5));
  OpenFile("A");
  EXPECT_THAT(Transfer(kReadPacked, 0, 10),
              ElementsAre(Field(kOk), PortWords::FromWord(3)));
  EXPECT_EQ(ReadData(3).transfers, DataWords(FileWords(0, 5), /*packed=*/true));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(3)));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(5),
                                   PortWords::FromDword(5)));
}

TEST_F(DiskDriveTest, PackedTransfersCrossSectors) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 255));
  OpenFile("A");

  // A dword written across a sector's end fills it and starts a new cluster.
  EXPECT_THAT(Seek(0, 255), ElementsAre(Field(kOk)));
  EXPECT_THAT(Transfer(kWritePacked, 0, 2),
              ElementsAre(Field(kOk), PortWords::FromWord(2)));
  WriteData(DataWords(FileWords(255, 4), /*packed=*/true));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(2)));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(259),
                                   PortWords::FromDword(259)));

  // A dword read across it waits for both sectors, seeking from CLOSE's
  // writes.
  EXPECT_THAT(Run(CommandWord(kClose, 0)), ElementsAre(Field(kOk)));
  OpenFile("A");
  EXPECT_THAT(Seek(0, 255), ElementsAre(Field(kOk)));
  EXPECT_THAT(Transfer(kReadPacked, 0, 2),
              ElementsAre(Field(kOk), PortWords::FromWord(2)));
  const DataPhase data = ReadData(2);
  EXPECT_EQ(data.transfers, DataWords(FileWords(255, 4), /*packed=*/true));
  EXPECT_THAT(data.stalls,
              ElementsAre(Pair(0, kLatencyCycles + 2 * kReadCycles)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords::FromWord(2)));
  EXPECT_THAT(Run(CommandWord(kClose, 0)), ElementsAre(Field(kOk)));
  EXPECT_EQ(DetachAndReadFile("A"), FileWords(0, 259));
}

TEST_F(DiskDriveTest, TruncateShrinksTheFile) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  SetNow(kNow);
  drive_->AttachImage(MakeImageWithFile("A", 600));
  OpenFile("A");
  EXPECT_THAT(Seek(0, 300), ElementsAre(Field(kOk)));
  protocol_->CollectWriteOffsets();

  // It commits before completing.
  EXPECT_THAT(Run(CommandWord(kTruncate, 0)), ElementsAre(Field(kOk)));
  EXPECT_THAT(protocol_->CollectWriteOffsets(), Not(IsEmpty()));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(300),
                                   PortWords::FromDword(300)));
  drive_->DetachImage();
  ExpectCommitted("A", FileWords(0, 300), kNow);
}

TEST_F(DiskDriveTest, FlushCommitsAWrittenFile) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  SetNow(kNow);
  AttachFormatted();
  CreateFile("A");
  SetNow(kLater);
  WriteWords(0, FileWords(0, 300));

  protocol_->CollectWriteOffsets();
  EXPECT_THAT(Run(CommandWord(kFlush, 0)), ElementsAre(Field(kOk)));
  EXPECT_THAT(protocol_->CollectWriteOffsets(), Not(IsEmpty()));
  drive_->DetachImage();
  ExpectCommitted("A", FileWords(0, 300), kLater);
}

TEST_F(DiskDriveTest, TheDriveCommitsWhenDestroyed) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  SetNow(kNow);
  AttachFormatted();
  CreateFile("A");
  SetNow(kLater);
  WriteWords(0, FileWords(0, 300));
  drive_ = nullptr;
  DestroyProcessor();
  ExpectCommitted("A", FileWords(0, 300), kLater);
}

TEST_F(DiskDriveTest, CloseOfEverySlotClosesTheRestAfterAFailure) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());

  // Slot 0 is stale, with a write not committed, which is lost.
  AttachFormatted();
  CreateFile("A", 0);
  WriteWords(0, FileWords(0, 10));
  drive_->AttachImage(MakeImage(/*formatted=*/true, kOtherPath));
  EXPECT_THAT(Run(CommandWord(kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(1)));
  CreateFile("B", 2);
  WriteWords(2, FileWords(0, 10));

  EXPECT_THAT(Run(CommandWord(kClose, DiskDrive::kAllSlots)),
              ElementsAre(Field(DiskStatus::kDiskChanged)));
  for (int handle = 0; handle < 3; ++handle) {
    SCOPED_TRACE(handle);
    EXPECT_THAT(Info(handle), ElementsAre(Field(DiskStatus::kBadHandle)));
  }
  EXPECT_EQ(DetachAndReadFile("B"), FileWords(0, 10));
}

//==============================================================================
// Early completion
//==============================================================================

TEST_F(DiskDriveTest, ABrokenChainEndsAReadEarly) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(BreakChain(MakeImageWithFile("A", 600), "A"));
  OpenFile("A");
  EXPECT_THAT(Transfer(kRead, 0, 600),
              ElementsAre(Field(kOk), PortWords::FromWord(600)));
  EXPECT_EQ(ReadData(256).transfers, DataWords(FileWords(0, 256)));

  // The completion comes at once, alone, and the OS reads it partway.
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(GetPhase(), 3);
  EXPECT_EQ(ReceiveField(), Field(DiskStatus::kCorrupt));
  EXPECT_EQ(GetPhase(), 4);

  // The rest of the read is zeros, at port speed, and ends the command.
  const DataPhase data = ReadData(344);
  EXPECT_EQ(data.transfers, std::vector<PortWords>(344));
  EXPECT_THAT(data.stalls, IsEmpty());
  EXPECT_EQ(GetControlWord(), DiskDrive::kDiskAttached);
  Execute(10);
  EXPECT_FALSE(IsSet(kCommandPort));
  EXPECT_FALSE(IsSet(kDataPort));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(256),
                                   PortWords::FromDword(600)));
}

TEST_F(DiskDriveTest, AnEarlyCompletionHoldsUpTheDataPhaseForItsLocks) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(BreakChain(MakeImageWithFile("A", 600), "A"));
  OpenFile("A");
  EXPECT_THAT(Transfer(kRead, 0, 600),
              ElementsAre(Field(kOk), PortWords::FromWord(600)));
  EXPECT_EQ(ReadData(254).transfers, DataWords(FileWords(0, 254)));

  // The read past the cluster, ahead of the OS, fails while the command port
  // is held, so its status waits, and the data with it. The word on the port
  // and then the zeros follow the status.
  std::unique_ptr<Lock> lock = LockPort(kCommandPort);
  ASSERT_TRUE(lock->IsLocked());
  EXPECT_EQ(ReadData(1).transfers, DataWords(FileWords(254, 1)));
  Execute(10);
  EXPECT_FALSE(IsSet(kCommandPort));
  EXPECT_EQ(GetPhase(), 4);
  lock.reset();
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(GetPhase(), 3);
  EXPECT_EQ(ReceiveField(), Field(DiskStatus::kCorrupt));
  EXPECT_EQ(ReadData(1).transfers, DataWords(FileWords(255, 1)));
  EXPECT_EQ(ReadData(344).transfers, std::vector<PortWords>(344));
}

TEST_F(DiskDriveTest, AFailedImageEndsAReadEarly) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 300));
  OpenFile("A");
  protocol_->SetLimits({.reads = 0});
  EXPECT_THAT(Transfer(kRead, 0, 300),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));

  // The OS runs its loop out, with the completion waiting, and reads it last.
  const DataPhase data = ReadData(300);
  EXPECT_EQ(data.transfers, std::vector<PortWords>(300));
  EXPECT_EQ(GetPhase(), 3);
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kIoError)));
  EXPECT_EQ(GetPhase(), 0);
  EXPECT_THAT(Info(0), ElementsAre(Field(DiskStatus::kIoError)));
}

TEST_F(DiskDriveTest, RemovingTheDiskEndsAReadEarly) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 300));
  OpenFile("A");
  EXPECT_THAT(Transfer(kRead, 0, 300),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));
  EXPECT_EQ(ReadData(100).transfers, DataWords(FileWords(0, 100)));

  // The word in the port was read before, but the slot is stale for the next.
  ASSERT_NE(drive_->DetachImage(), nullptr);
  EXPECT_EQ(ReadData(1).transfers, DataWords(FileWords(100, 1)));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(ReceiveField(), Field(DiskStatus::kDiskChanged));

  // A reset cuts the rest short.
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  Request(DiskDrive::kResetRequest);
  EXPECT_FALSE(IsSet(kDataPort));
  EXPECT_FALSE(IsSet(kCommandPort));
  EXPECT_EQ(GetPhase(), 0);
}

TEST_F(DiskDriveTest, RemovingTheDiskBeforeAReadStartsFailsItAtOnce) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 300));
  OpenFile("A");

  // The slot goes stale while the OS reads the accept, so the read fails
  // before its first word, and the OS runs its loop out over zeros.
  Start(CommandWord(kRead, 0));
  SendField(PortWords::FromWord(300));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  ASSERT_NE(drive_->DetachImage(), nullptr);
  EXPECT_EQ(ReceiveField(), Field(kOk));
  EXPECT_EQ(ReceiveField(), PortWords::FromWord(300));
  EXPECT_EQ(ReadData(300).transfers, std::vector<PortWords>(300));
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kDiskChanged)));
  EXPECT_EQ(GetPhase(), 0);
}

TEST_F(DiskDriveTest, AFailedImageEndsAWriteEarly) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  CreateFile("A");
  protocol_->SetLimits({.writes = 0});
  EXPECT_THAT(Transfer(kWrite, 0, 300),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));

  // Word 256 writes back the first sector, which fails.
  EXPECT_THAT(WriteData(DataWords(FileWords(0, 257))), IsEmpty());
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(GetPhase(), 3);
  EXPECT_EQ(ReceiveField(), Field(DiskStatus::kIoError));
  EXPECT_EQ(GetPhase(), 5);

  // The rest is taken and thrown away, at port speed.
  EXPECT_THAT(WriteData(DataWords(FileWords(257, 43))), IsEmpty());
  Execute(10);
  EXPECT_EQ(GetPhase(), 0);
  EXPECT_FALSE(IsSet(kCommandPort));
}

TEST_F(DiskDriveTest, ABrokenChainEndsAWriteEarlyAfterAFlush) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  SetNow(kNow);
  drive_->AttachImage(BreakChain(MakeImageWithFile("A", 600), "A"));
  OpenFile("A");
  EXPECT_THAT(Transfer(kWrite, 0, 300),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));
  WriteData(DataWords(FileWords(1000, 257)));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(ReceiveField(), Field(DiskStatus::kCorrupt));

  // A reset cuts the rest short. The words before the failure were flushed.
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetPhase(), 0);
  EXPECT_FALSE(IsSet(kDataPort));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(256),
                                   PortWords::FromDword(600)));
  std::optional<DiskDirEntry> entry = DetachAndFindEntry("A");
  ASSERT_TRUE(entry.has_value());
  EXPECT_EQ(entry->GetModified(), DateTime::FromCivil(kNow));
  EXPECT_EQ(ReadFile(ReopenImage(), "A", 256), FileWords(1000, 256));
}

TEST_F(DiskDriveTest, RemovingTheDiskEndsAWriteEarly) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  CreateFile("A");
  EXPECT_THAT(Transfer(kWrite, 0, 20),
              ElementsAre(Field(kOk), PortWords::FromWord(20)));
  WriteData(DataWords(FileWords(0, 10)));
  ASSERT_NE(drive_->DetachImage(), nullptr);
  WriteData(DataWords(FileWords(10, 10)));
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kDiskChanged)));
  EXPECT_EQ(GetPhase(), 0);
}

//==============================================================================
// Resets while reading and writing
//==============================================================================

TEST_F(DiskDriveTest, AResetLeavesAReadPastTheWordsTaken) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 600));
  OpenFile("A");
  // During the first sector's load, nothing has moved.
  EXPECT_THAT(Transfer(kRead, 0, 300),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));
  Request(DiskDrive::kResetRequest);
  ExpectResetAt(0);

  // A word in the port goes back to the file.
  EXPECT_THAT(Transfer(kRead, 0, 300),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));
  ReadData(10);
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  Request(DiskDrive::kResetRequest);
  ExpectResetAt(10);

  // A word the OS took, before the drive sees it go, is the OS's.
  EXPECT_THAT(Transfer(kRead, 0, 300),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  EXPECT_EQ(In(kDataPort), PortWords::FromWord(FileWord(10)));
  Request(DiskDrive::kResetRequest);
  ExpectResetAt(11);

  // A request waits out a sector's load, which began a cycle before the OS
  // took the last word, and the word it read goes back.
  EXPECT_THAT(Transfer(kRead, 0, 300),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));
  ReadData(256 - 11);
  const Cycles start = GetCycles();
  Request(DiskDrive::kResetRequest);
  EXPECT_GE(GetCycles() - start, kReadCycles - 1);
  ExpectResetAt(256);

  // A packed read's dword goes back whole.
  EXPECT_THAT(Transfer(kReadPacked, 0, 100),
              ElementsAre(Field(kOk), PortWords::FromWord(100)));
  ReadData(2);
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  Request(DiskDrive::kResetRequest);
  ExpectResetAt(260);
}

TEST_F(DiskDriveTest, AResetLeavesAPackedReadOfAnOddFileInsideIt) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 5));
  OpenFile("A");

  // The last dword holds the file's last word, which goes back if the OS
  // hasn't taken it.
  EXPECT_THAT(Transfer(kReadPacked, 0, 3),
              ElementsAre(Field(kOk), PortWords::FromWord(3)));
  ReadData(2);
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  Request(DiskDrive::kResetRequest);
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(4),
                                   PortWords::FromDword(5)));

  // Once the OS has taken it, the position stays at the end.
  EXPECT_THAT(Transfer(kReadPacked, 0, 1),
              ElementsAre(Field(kOk), PortWords::FromWord(1)));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  EXPECT_EQ(In(kDataPort), PortWords::FromWord(FileWord(4)));
  Request(DiskDrive::kResetRequest);
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(5),
                                   PortWords::FromDword(5)));
}

TEST_F(DiskDriveTest, APackedReadThatFailsInsideADwordSendsTheWordItRead) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(BreakChain(MakeImageWithFile("A", 600), "A"));
  OpenFile("A");

  // The first dword's high word is past the broken link, so the read fails
  // after its low word. A reset before the OS takes that word puts it back.
  EXPECT_THAT(Seek(0, 255), ElementsAre(Field(kOk)));
  EXPECT_THAT(Transfer(kReadPacked, 0, 10),
              ElementsAre(Field(kOk), PortWords::FromWord(10)));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(ReceiveField(), Field(DiskStatus::kCorrupt));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  Request(DiskDrive::kResetRequest);
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(255),
                                   PortWords::FromDword(600)));

  // The word comes through padded with a zero, and zeros follow it. The
  // position stays where the failure left it.
  EXPECT_THAT(Transfer(kReadPacked, 0, 10),
              ElementsAre(Field(kOk), PortWords::FromWord(10)));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(ReceiveField(), Field(DiskStatus::kCorrupt));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  EXPECT_EQ(In(kDataPort), PortWords::FromWords(FileWord(255), 0));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  EXPECT_EQ(In(kDataPort), PortWords::FromWords(0, 0));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  Request(DiskDrive::kResetRequest);
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(256),
                                   PortWords::FromDword(600)));
}

TEST_F(DiskDriveTest, AResetKeepsTheWordsAWriteTook) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  CreateFile("A");
  EXPECT_THAT(Transfer(kWrite, 0, 300),
              ElementsAre(Field(kOk), PortWords::FromWord(300)));
  WriteData(DataWords(FileWords(0, 100)));

  // The word the drive hasn't taken is thrown away.
  ASSERT_TRUE(RunUntil([this] { return !IsSet(kDataPort); }));
  ASSERT_TRUE(Out(kDataPort, PortWords::FromWord(0xFFFF)));
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetPhase(), 0);
  EXPECT_FALSE(IsSet(kDataPort));
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(100),
                                   PortWords::FromDword(100)));
  EXPECT_THAT(Run(CommandWord(kClose, 0)), ElementsAre(Field(kOk)));
  EXPECT_EQ(DetachAndReadFile("A"), FileWords(0, 100));
}

//==============================================================================
// Directory and disk commands
//==============================================================================

TEST_F(DiskDriveTest, FindListsTheDirectory) {
  ASSERT_TRUE(InitDrive());

  // The entries, as the file system finds them, in the order they are in.
  DiskFileSystem file_system({}, 1);
  file_system.AttachImage(MakeImageWithFile("A", 10));
  AddFile(file_system, "B.TXT", 300);
  AddFile(file_system, "C.TXT", 0);
  DiskFile root = 0;
  ASSERT_EQ(file_system.OpenRoot(&root), kOk);
  std::vector<DiskDirEntry> entries(3);
  for (DiskDirEntry& entry : entries) {
    ASSERT_EQ(
        file_system.Find(root, *DiskFilename::ParsePattern("*.*"), &entry),
        kOk);
  }
  ASSERT_EQ(file_system.Close(root, DateTime()), kOk);
  drive_->AttachImage(file_system.DetachImage());

  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_EQ(Find(0, "*.txt"), FoundFields(entries[1]));
  EXPECT_EQ(Find(0, "*.TXT"), FoundFields(entries[2]));
  EXPECT_THAT(Find(0, "*.TXT"), ElementsAre(Field(DiskStatus::kNotFound)));

  // The listing stays at its end until a SEEK restarts it, and each FIND
  // leaves the position just past its entry.
  EXPECT_THAT(Find(0, "*.*"), ElementsAre(Field(DiskStatus::kNotFound)));
  EXPECT_THAT(Seek(0, 0), ElementsAre(Field(kOk)));
  EXPECT_EQ(Find(0, "*.*"), FoundFields(entries[0]));
  std::vector<PortWords> info = Info(0);
  ASSERT_EQ(info.size(), 3);
  EXPECT_EQ(info[1], PortWords::FromDword(DiskDirEntry::kWordCount));
  EXPECT_EQ(Find(0, "*.*"), FoundFields(entries[1]));
}

TEST_F(DiskDriveTest, FindChecksAtAccept) {
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 1));
  OpenFile("A", 0);
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(1)));

  // A file's handle is the wrong type, which comes before the pattern.
  EXPECT_THAT(
      Command(CommandWord(DiskDriveCommand::kFind, 0), PatternFields("*")),
      ElementsAre(Field(DiskStatus::kWrongHandleType)));
  EXPECT_THAT(
      Command(CommandWord(DiskDriveCommand::kFind, 0), NameFields(GapWords())),
      ElementsAre(Field(DiskStatus::kWrongHandleType)));
  for (const DiskFilename::Words& words : {DiskFilename::Words(), GapWords()}) {
    EXPECT_THAT(
        Command(CommandWord(DiskDriveCommand::kFind, 1), NameFields(words)),
        ElementsAre(Field(DiskStatus::kBadPattern)));
  }

  // A name is a pattern that matches only itself.
  std::vector<PortWords> found =
      Run(CommandWord(DiskDriveCommand::kFind, 1), NameFields("a"));
  ASSERT_FALSE(found.empty());
  EXPECT_EQ(found[0], Field(kOk));
}

TEST_F(DiskDriveTest, DeleteFreesTheFile) {
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 600));

  // A's 600 words take three clusters, which DELETE frees.
  EXPECT_EQ(Volume(), VolumeFields(GetVolume("WORK", 0, 3)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kDelete), NameFields("a")),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(Volume(), VolumeFields(GetVolume("WORK", 0, 0)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kDelete), NameFields("A")),
              ElementsAre(Field(DiskStatus::kNotFound)));
  EXPECT_FALSE(DetachAndFindEntry("A").has_value());
}

TEST_F(DiskDriveTest, RenameKeepsTheRestOfTheEntry) {
  ASSERT_TRUE(InitDrive());
  SetNow(kNow);
  DiskFileSystem file_system({}, 1);
  file_system.AttachImage(MakeImageWithFile("A", 10));
  AddFile(file_system, "B", 1);
  drive_->AttachImage(file_system.DetachImage());

  // A new name that is taken exists, even by the file itself.
  for (const std::string_view new_name : {"B", "a"}) {
    EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kRename),
                    AndFields(NameFields("A"), NameFields(new_name))),
                ElementsAre(Field(DiskStatus::kExists)));
  }
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kRename),
                  AndFields(NameFields("C"), NameFields("D"))),
              ElementsAre(Field(DiskStatus::kNotFound)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kRename),
                  AndFields(NameFields("a"), NameFields("c.txt"))),
              ElementsAre(Field(kOk)));

  std::optional<DiskDirEntry> entry = DetachAndFindEntry("C.TXT");
  ASSERT_TRUE(entry.has_value());
  EXPECT_EQ(entry->GetSize(), 10);
  EXPECT_EQ(entry->GetModified(), DateTime());
  EXPECT_EQ(ReadFile(ReopenImage(), "C.TXT"), FileWords(0, 10));
  EXPECT_FALSE(FindEntry(ReopenImage(), "A").has_value());
}

TEST_F(DiskDriveTest, AttribSetsReadOnlyAndHidden) {
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 1));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kAttrib),
                  AndWord(NameFields("A"), kDiskAttribSettable)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(
      Run(CommandWord(DiskDriveCommand::kAttrib), AndWord(NameFields("B"), 0)),
      ElementsAre(Field(DiskStatus::kNotFound)));

  // A read-only file opens, but can't be written or deleted.
  OpenFile("A");
  ExpectWritesFail(0, DiskStatus::kReadOnly);
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kDelete), NameFields("A")),
              ElementsAre(Field(DiskStatus::kReadOnly)));

  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kAttrib),
                  AndWord(NameFields("A"), kDiskAttribHidden)),
              ElementsAre(Field(kOk)));
  std::optional<DiskDirEntry> entry = DetachAndFindEntry("A");
  ASSERT_TRUE(entry.has_value());
  EXPECT_EQ(entry->GetAttributes(), kDiskAttribHidden);
}

TEST_F(DiskDriveTest, AnOpenFileIsInUse) {
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 1));
  OpenFile("A");
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kDelete), NameFields("A")),
              ElementsAre(Field(DiskStatus::kInUse)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kRename),
                  AndFields(NameFields("A"), NameFields("B"))),
              ElementsAre(Field(DiskStatus::kInUse)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kAttrib),
                  AndWord(NameFields("A"), kDiskAttribReadOnly)),
              ElementsAre(Field(DiskStatus::kInUse)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kFormat),
                      AndWord(LabelFields("WORK"), 0)),
              ElementsAre(Field(DiskStatus::kInUse)));
  EXPECT_TRUE(DetachAndFindEntry("A").has_value());
}

TEST_F(DiskDriveTest, ChangesCheckAtAccept) {
  ASSERT_TRUE(InitDrive());
  Execute(1);

  // Names, labels, and attributes come before the disk, and a name before
  // its attributes.
  for (const std::vector<PortWords>& name :
       {NameFields(DiskFilename::Words()), PatternFields("*")}) {
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kDelete), name),
                ElementsAre(Field(DiskStatus::kBadName)));
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kRename),
                        AndFields(name, NameFields("B"))),
                ElementsAre(Field(DiskStatus::kBadName)));
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kRename),
                        AndFields(NameFields("A"), name)),
                ElementsAre(Field(DiskStatus::kBadName)));
    EXPECT_THAT(
        Command(CommandWord(DiskDriveCommand::kAttrib), AndWord(name, 0x8000)),
        ElementsAre(Field(DiskStatus::kBadName)));
  }
  for (const uint16_t attributes : {kDiskAttribDirectory, uint16_t{0x8000}}) {
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kAttrib),
                        AndWord(NameFields("A"), attributes)),
                ElementsAre(Field(DiskStatus::kBadAttributes)));
  }
  for (const DiskLabel::Words& label :
       {GapWords(), DiskFilename::ParsePattern("?")->GetWords()}) {
    EXPECT_THAT(
        Command(CommandWord(DiskDriveCommand::kLabel), NameFields(label)),
        ElementsAre(Field(DiskStatus::kBadLabel)));
  }
  ExpectChangesFail(DiskStatus::kNoDisk);

  // Write protected comes before unformatted.
  std::unique_ptr<DiskImage> image = MakeImage(/*formatted=*/false);
  ASSERT_EQ(image->SetWriteProtected(true), kOk);
  drive_->AttachImage(std::move(image));
  ExpectChangesFail(DiskStatus::kWriteProtected);
  image = drive_->DetachImage();
  ASSERT_EQ(image->SetWriteProtected(false), kOk);
  drive_->AttachImage(std::move(image));
  ExpectChangesFail(DiskStatus::kUnformatted);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kVolume)),
              ElementsAre(Field(DiskStatus::kUnformatted)));

  image = MakeImage(/*formatted=*/true, kOtherPath);
  protocol_->SetLimits({.reads = 0});
  drive_->AttachImage(std::move(image));
  ExpectChangesFail(DiskStatus::kIoError);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kVolume)),
              ElementsAre(Field(DiskStatus::kIoError)));
}

TEST_F(DiskDriveTest, VolumeKeepsTheFreeCount) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kVolume)),
              ElementsAre(Field(DiskStatus::kNoDisk)));
  AttachFormatted();
  EXPECT_EQ(Volume(), VolumeFields(GetVolume("WORK", 0, 0)));

  // 300 words take two clusters.
  CreateFile("A");
  WriteWords(0, FileWords(0, 300));
  EXPECT_EQ(Volume(), VolumeFields(GetVolume("WORK", 0, 2)));
  EXPECT_THAT(Seek(0, 0), ElementsAre(Field(kOk)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kTruncate, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(Volume(), VolumeFields(GetVolume("WORK", 0, 0)));
}

TEST_F(DiskDriveTest, FormatMakesAnEmptyFileSystem) {
  ASSERT_TRUE(InitDrive());
  SetNow(kNow);
  drive_->AttachImage(MakeImage(/*formatted=*/false));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kFormat),
                  AndWord(LabelFields("games"), 2)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(Volume(), VolumeFields(GetVolume("GAMES", 2, 0)));

  // Formatting again leaves no files.
  CreateFile("A");
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kFormat),
                  AndWord(LabelFields("WORK"), 0)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpen), NameFields("A")),
              ElementsAre(Field(DiskStatus::kNotFound)));
  EXPECT_EQ(Volume(), VolumeFields(GetVolume("WORK", 0, 0)));

  // The superblock is stamped with the clock.
  const DiskSuperblock superblock = DetachAndReadSuperblock();
  const std::optional<DiskLayout> layout = ChooseDiskLayout(kSectors, 0);
  ASSERT_TRUE(layout.has_value());
  EXPECT_EQ(superblock, DiskSuperblock(*layout, superblock.GetRootCluster(),
                                       *DiskLabel::Parse("WORK"),
                                       DateTime::FromCivil(kNow)));
}

TEST_F(DiskDriveTest, FormatChecksAtAccept) {
  DiskDriveSettings settings = GetSettings();
  settings.slots = 1;
  ASSERT_TRUE(InitDrive(settings));
  Execute(1);

  // The label comes before the disk.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kFormat),
                      AndWord(NameFields(GapWords()), 0)),
              ElementsAre(Field(DiskStatus::kBadLabel)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kFormat),
                      AndWord(LabelFields("A"), 0)),
              ElementsAre(Field(DiskStatus::kNoDisk)));

  // A protected disk can't be formatted, formatted or not.
  std::unique_ptr<DiskImage> image = MakeImage(/*formatted=*/false);
  ASSERT_EQ(image->SetWriteProtected(true), kOk);
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kFormat),
                      AndWord(LabelFields("A"), 0)),
              ElementsAre(Field(DiskStatus::kWriteProtected)));

  // An open slot comes before the sectors per cluster, which must be a power
  // of two up to 64 that covers the image in 65,534 clusters.
  drive_->AttachImage(MakeImage(/*formatted=*/true, kOtherPath));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kFormat),
                      AndWord(LabelFields("A"), 3)),
              ElementsAre(Field(DiskStatus::kInUse)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  for (const uint16_t sectors_per_cluster :
       {uint16_t{3}, uint16_t{128}, uint16_t{0x8000}}) {
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kFormat),
                        AndWord(LabelFields("A"), sectors_per_cluster)),
                ElementsAre(Field(DiskStatus::kBadArgument)));
  }

  // A stale slot isn't in use.
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  drive_->AttachImage(MakeImage(/*formatted=*/false, kThirdPath));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kFormat),
                  AndWord(LabelFields("A"), 64)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(Volume(), VolumeFields(GetVolume("A", 64, 0)));
}

TEST_F(DiskDriveTest, LabelRewritesTheLabel) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kLabel), LabelFields("games")),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(Volume(), VolumeFields(GetVolume("GAMES", 0, 0)));
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kLabel),
                  NameFields(DiskLabel::Words())),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(DetachAndReadSuperblock().GetLabel(), DiskLabel());
}

TEST_F(DiskDriveTest, CreateFailsWhenTheRootCannotGrow) {
  ASSERT_TRUE(InitDrive());

  // The root's one cluster holds 16 entries, A, FILL, and 14 more, and FILL
  // takes every free cluster.
  DiskFileSystem file_system({}, 1);
  file_system.AttachImage(MakeFullImage(0, 0));
  for (int i = 2; i < DiskDirEntry::kEntriesPerSector; ++i) {
    AddFile(file_system, absl::StrCat("E", i), 0);
  }
  drive_->AttachImage(file_system.DetachImage());

  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kCreate), NameFields("NEW")),
              ElementsAre(Field(DiskStatus::kDiskFull)));
  EXPECT_FALSE(DetachAndFindEntry("NEW").has_value());
}

//==============================================================================
// Boot commands
//==============================================================================

TEST_F(DiskDriveTest, ReadBootSendsTheBootAreaWithNoDiskTime) {
  ASSERT_TRUE(InitDrive());
  const DiskSuperblock::BootArea boot = BootWords(0x1000);
  AttachWithBootArea(boot);

  // The drive holds the superblock, so no dword waits for the disk.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kReadBoot)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(GetPhase(), static_cast<int>(DiskDrivePhase::kReading));
  const DataPhase data = ReadData(kBootDwords);
  EXPECT_EQ(data.transfers, DataWords(boot, /*packed=*/true));
  EXPECT_THAT(data.stalls, IsEmpty());
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_TRUE(IsInterruptRaised());
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));
  EXPECT_EQ(GetControlWord(), DiskDrive::kDiskAttached);
}

TEST_F(DiskDriveTest, WriteBootWritesTheBootAreaOnceItIsAllIn) {
  ASSERT_TRUE(InitDrive());
  SetNow(kNow);
  AttachFormatted();
  const DiskSuperblock before = DetachAndReadSuperblock();
  drive_->AttachImage(ReopenImage());
  Execute(kAttachCycles + 2);

  // The OS never waits for the drive while it writes. The drive takes the
  // last dword the cycle after it arrives, and then writes the superblock's
  // sector, seeking back to it, before it completes.
  const DiskSuperblock::BootArea boot = BootWords(0x2000);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kWriteBoot)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(GetPhase(), static_cast<int>(DiskDrivePhase::kWriting));
  EXPECT_THAT(WriteData(DataWords(boot, /*packed=*/true)), IsEmpty());
  const Cycles written = GetCycles();
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(GetCycles(), written + kLatencyCycles + kWriteCycles);
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));

  // It is read back as written, and the rest of the superblock is kept.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kReadBoot)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(ReadData(kBootDwords).transfers, DataWords(boot, /*packed=*/true));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));
  DiskSuperblock expected = before;
  expected.SetBootArea(boot);
  EXPECT_EQ(DetachAndReadSuperblock(), expected);
}

TEST_F(DiskDriveTest, BootCommandsCheckTheDiskAtAccept) {
  using enum DiskDriveCommand;
  ASSERT_TRUE(InitDrive());
  Execute(1);
  for (const DiskDriveCommand command : {kReadBoot, kWriteBoot}) {
    EXPECT_THAT(Command(CommandWord(command)),
                ElementsAre(Field(DiskStatus::kNoDisk)));
  }

  // Write protected comes before unformatted, and only for WRITE BOOT.
  std::unique_ptr<DiskImage> image = MakeImage(/*formatted=*/false);
  ASSERT_EQ(image->SetWriteProtected(true), kOk);
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Command(CommandWord(kWriteBoot)),
              ElementsAre(Field(DiskStatus::kWriteProtected)));
  EXPECT_THAT(Command(CommandWord(kReadBoot)),
              ElementsAre(Field(DiskStatus::kUnformatted)));
  image = drive_->DetachImage();
  ASSERT_EQ(image->SetWriteProtected(false), kOk);
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Command(CommandWord(kWriteBoot)),
              ElementsAre(Field(DiskStatus::kUnformatted)));

  // A protected disk can still be read, and open files don't matter.
  image = MakeImage(/*formatted=*/true, kOtherPath);
  ASSERT_EQ(image->SetWriteProtected(true), kOk);
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Run(CommandWord(kOpenRoot)),
              ElementsAre(Field(kOk), PortWords::FromWord(0)));
  EXPECT_THAT(Command(CommandWord(kReadBoot)), ElementsAre(Field(kOk)));
  EXPECT_EQ(ReadData(kBootDwords).transfers,
            DataWords(DiskSuperblock::BootArea(DiskSuperblock::kBootAreaWords),
                      /*packed=*/true));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));
}

TEST_F(DiskDriveTest, AResetBeforeTheLastDwordWritesNoBootArea) {
  ASSERT_TRUE(InitDrive());
  const DiskSuperblock::BootArea old_boot = BootWords(0x1000);
  AttachWithBootArea(old_boot);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kWriteBoot)),
              ElementsAre(Field(kOk)));
  const std::vector<PortWords> transfers =
      DataWords(BootWords(0x2000), /*packed=*/true);
  WriteData(absl::MakeConstSpan(transfers).first(kBootDwords - 1));
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetPhase(), 0);
  EXPECT_FALSE(IsSet(kDataPort));
  EXPECT_EQ(DetachAndReadSuperblock().GetBootArea(), old_boot);
}

TEST_F(DiskDriveTest, AResetEndsReadBootWithItsDwordWithdrawn) {
  ASSERT_TRUE(InitDrive());
  AttachWithBootArea(BootWords(0x1000));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kReadBoot)),
              ElementsAre(Field(kOk)));
  ReadData(10);
  ASSERT_TRUE(RunUntil([this] { return IsSet(kDataPort); }));
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetPhase(), 0);
  EXPECT_FALSE(IsSet(kDataPort));
  EXPECT_FALSE(IsSet(kCommandPort));

  // The drive takes the next command at once.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kReadBoot)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(ReadData(kBootDwords).transfers,
            DataWords(BootWords(0x1000), /*packed=*/true));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));
}

TEST_F(DiskDriveTest, WriteBootFailsAtItsSectorWrite) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  protocol_->SetLimits({.writes = 0});
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kWriteBoot)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(WriteData(DataWords(BootWords(0x2000), /*packed=*/true)),
              IsEmpty());
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kIoError)));
  EXPECT_EQ(GetControlWord(), DiskDrive::kDiskAttached);
}

TEST_F(DiskDriveTest, WriteBootFindsNoDiskIfItIsRemoved) {
  // The words are taken as before, and the write finds no disk.
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kWriteBoot)),
              ElementsAre(Field(kOk)));
  const std::vector<PortWords> transfers =
      DataWords(BootWords(0x2000), /*packed=*/true);
  EXPECT_THAT(WriteData(absl::MakeConstSpan(transfers).first(50)), IsEmpty());
  ASSERT_NE(drive_->DetachImage(), nullptr);
  EXPECT_THAT(WriteData(absl::MakeConstSpan(transfers).subspan(50)), IsEmpty());
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kNoDisk)));
  EXPECT_EQ(GetControlWord(), 0);
}

TEST_F(DiskDriveTest, ReadBootTakesTheBootAreaOnceItsAcceptIsRead) {
  // The accept status is still to be read, so the disk swapped in is the one
  // whose boot area is sent.
  ASSERT_TRUE(InitDrive());
  AttachWithBootArea(BootWords(0x1000));
  Start(CommandWord(DiskDriveCommand::kReadBoot));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  const DiskSuperblock::BootArea boot = BootWords(0x2000);
  AttachWithBootArea(boot, kOtherPath);
  EXPECT_EQ(ReceiveField(), Field(kOk));
  EXPECT_EQ(ReadData(kBootDwords).transfers, DataWords(boot, /*packed=*/true));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));
}

TEST_F(DiskDriveTest, ReadBootSendsZerosIfTheDiskFailsOnceItsAcceptIsRead) {
  // The accept promised every dword, so they all come, and the completion says
  // there is no disk.
  ASSERT_TRUE(InitDrive());
  AttachWithBootArea(BootWords(0x1000));
  Start(CommandWord(DiskDriveCommand::kReadBoot));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  ASSERT_NE(drive_->DetachImage(), nullptr);
  EXPECT_EQ(ReceiveField(), Field(kOk));
  const std::vector<PortWords> zeros =
      DataWords(DiskSuperblock::BootArea(DiskSuperblock::kBootAreaWords),
                /*packed=*/true);
  EXPECT_EQ(ReadData(kBootDwords).transfers, zeros);
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kNoDisk)));
  EXPECT_EQ(GetControlWord(), 0);

  // The same goes for a disk swapped in that the accept would have failed.
  AttachWithBootArea(BootWords(0x1000));
  Start(CommandWord(DiskDriveCommand::kReadBoot));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  drive_->AttachImage(MakeImage(/*formatted=*/false, kOtherPath));
  EXPECT_EQ(ReceiveField(), Field(kOk));
  EXPECT_EQ(ReadData(kBootDwords).transfers, zeros);
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kUnformatted)));
}

TEST_F(DiskDriveTest, ReadBootCompletesThoughTheDiskIsRemoved) {
  // The boot area was taken as the data phase started, so the rest is sent
  // from it.
  ASSERT_TRUE(InitDrive());
  const DiskSuperblock::BootArea boot = BootWords(0x1000);
  AttachWithBootArea(boot);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kReadBoot)),
              ElementsAre(Field(kOk)));
  const std::vector<PortWords> expected = DataWords(boot, /*packed=*/true);
  EXPECT_EQ(ReadData(50).transfers, absl::MakeConstSpan(expected).first(50));
  ASSERT_NE(drive_->DetachImage(), nullptr);
  EXPECT_EQ(ReadData(kBootDwords - 50).transfers,
            absl::MakeConstSpan(expected).subspan(50));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));
}

//==============================================================================
// Programs
//==============================================================================

TEST_F(DiskDriveTest, AProgramOpensAndClosesTheRoot) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  ASSERT_TRUE(Load(R"---(
    .STACK 16

    .SEGMENT main CODE
        SETI 2, handler
        MVQ R6, 0               ; Completions handled
        EI

    idle_open:
        IN 18, R0
        AND R0, 15              ; Phase 0
        JC NZ, idle_open
        MOV R2, open_completion ; Where the handler reads to, and how much
        MVQ R3, 2
        MOV R1, open_root
        MVQ R7, 1
        OUTR.IW 16, (R1)
    wait_open:
        IN 18, R0
        AND R0, 14              ; Phase 2 or higher
        JC Z, wait_open
        MOV R1, open_accept
    read_open:
        INS.IW 16, (R1)
        JC NS, read_open
    wait_open_completion:
        CMP R6, 1
        JC NZ, wait_open_completion

    idle_close:
        IN 18, R0
        AND R0, 15
        JC NZ, idle_close
        MOV R2, close_completion
        MVQ R3, 1
        MOV R1, close
        MVQ R7, 1
        OUTR.IW 16, (R1)
    wait_close:
        IN 18, R0
        AND R0, 14
        JC Z, wait_close
        MOV R1, close_accept
    read_close:
        INS.IW 16, (R1)
        JC NS, read_close
    wait_close_completion:
        CMP R6, 2
        JC NZ, wait_close_completion
        HALT

    handler:                    ; Reads a completion
        MOV R7, R3
    read_completion:
        INR.IW 16, (R2)
        JC NS, read_completion
        ADQ R6, 1
        IRT

    .SEGMENT vars DATA
    open_root:
        .DW 1100h
    close:
        .DW 1300h
    open_accept:
        .DW 0FFFFh
    open_completion:
        .DW 0FFFFh, 0FFFFh
    close_accept:
        .DW 0FFFFh
    close_completion:
        .DW 0FFFFh

    .SEGMENT stack STACK
  )---"));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(GetState().r6, 2);
  const uint16_t ok = static_cast<uint16_t>(kOk);
  EXPECT_THAT(GetData("OPEN_ACCEPT", 1), ElementsAre(ok));
  EXPECT_THAT(GetData("OPEN_COMPLETION", 2), ElementsAre(ok, 0));
  EXPECT_THAT(GetData("CLOSE_ACCEPT", 1), ElementsAre(ok));
  EXPECT_THAT(GetData("CLOSE_COMPLETION", 1), ElementsAre(ok));
  Execute(1);
  EXPECT_EQ(GetControlWord(), DiskDrive::kDiskAttached);
}

// Returns the words of `fields`, as a program's data holds them, two for each.
std::vector<uint16_t> FieldWords(absl::Span<const PortWords> fields) {
  std::vector<uint16_t> words;
  for (const PortWords& field : fields) {
    words.push_back(field.GetWord(0));
    words.push_back(field.GetWord(1));
  }
  return words;
}

// Returns the words a program sends for `command_word` and its `arguments`.
std::vector<uint16_t> CommandData(uint16_t command_word,
                                  absl::Span<const PortWords> arguments = {}) {
  return FieldWords(AndFields({PortWords::FromWord(command_word)}, arguments));
}

TEST_F(DiskDriveTest, AProgramFormatsAndListsTheDisk) {
  ASSERT_TRUE(InitDrive());
  SetNow(kNow);
  drive_->AttachImage(MakeImage(/*formatted=*/false));
  ASSERT_TRUE(Load(R"---(
    .STACK 16

    .SEGMENT main CODE
        MOV R1, format
        MVQ R7, 6
        MOV R2, format_response
        MVQ R3, 0
        CALL run
        MOV R1, create_a
        MVQ R7, 5
        MOV R2, create_a_response
        MVQ R3, 1
        CALL run
        MOV R1, create_b
        MVQ R7, 5
        MOV R2, create_b_response
        MVQ R3, 1
        CALL run
        MOV R1, close_all
        MVQ R7, 1
        MOV R2, close_response
        MVQ R3, 0
        CALL run
        MOV R1, open_root
        MVQ R7, 1
        MOV R2, open_response
        MVQ R3, 1
        CALL run
        MOV R2, found           ; Each FIND's response, 20 words
    list:
        MOV R1, find
        MVQ R7, 5
        MVQ R3, 8
        CALL run
        CMP R0, 0               ; The completion's status
        JC Z, list
        HALT

    run:                        ; Sends R7 fields from R1, then reads the accept,
                                ; and the completion with R3 results, to R2,
                                ; leaving R2 past them and R0 the status
        OUTR.ID 16, [R1]
        JC NS, run
    wait_accept:
        IN 18, R0
        AND R0, 14              ; Phase 2 or higher
        JC Z, wait_accept
        MVQ R7, 1
    read_accept:
        INR.ID 16, [R2]
        JC NS, read_accept
        MOV R1, R2
        MVQ R7, 1
    read_status:
        INR.ID 16, [R2]
        JC NS, read_status
        MOV R0, (R1)
        CMP R0, 0
        RETC NZ
        CMP R3, 0
        RETC Z
        MOV R7, R3
    read_results:
        INR.ID 16, [R2]
        JC NS, read_results
        RET

    .SEGMENT vars DATA
    format:
        .DS 12
    create_a:
        .DS 10
    create_b:
        .DS 10
    close_all:
        .DS 2
    open_root:
        .DS 2
    find:
        .DS 10
    format_response:
        .DS 4
    create_a_response:
        .DS 6
    create_b_response:
        .DS 6
    close_response:
        .DS 4
    open_response:
        .DS 6
    found:
        .DS 60

    .SEGMENT stack STACK
  )---"));
  SetData("FORMAT", CommandData(CommandWord(DiskDriveCommand::kFormat),
                                AndWord(LabelFields("WORK"), 0)));
  SetData("CREATE_A", CommandData(CommandWord(DiskDriveCommand::kCreate),
                                  NameFields("A.TXT")));
  SetData("CREATE_B",
          CommandData(CommandWord(DiskDriveCommand::kCreate), NameFields("B")));
  SetData("CLOSE_ALL", CommandData(CommandWord(DiskDriveCommand::kClose,
                                               DiskDrive::kAllSlots)));
  SetData("OPEN_ROOT", CommandData(CommandWord(DiskDriveCommand::kOpenRoot)));
  SetData("FIND", CommandData(CommandWord(DiskDriveCommand::kFind),
                              PatternFields("*.*")));
  ASSERT_TRUE(ExecuteUntilHalt());

  const std::vector<uint16_t> ok_twice = FieldWords({Field(kOk), Field(kOk)});
  EXPECT_EQ(GetData("FORMAT_RESPONSE", 4), ok_twice);
  EXPECT_EQ(GetData("CREATE_A_RESPONSE", 6),
            FieldWords({Field(kOk), Field(kOk), PortWords::FromWord(0)}));
  EXPECT_EQ(GetData("CREATE_B_RESPONSE", 6),
            FieldWords({Field(kOk), Field(kOk), PortWords::FromWord(1)}));
  EXPECT_EQ(GetData("CLOSE_RESPONSE", 4), ok_twice);
  EXPECT_EQ(GetData("OPEN_RESPONSE", 6),
            FieldWords({Field(kOk), Field(kOk), PortWords::FromWord(0)}));

  // FIND gives each entry as it is on the disk, then not found.
  std::optional<DiskDirEntry> a = DetachAndFindEntry("A.TXT");
  std::optional<DiskDirEntry> b = FindEntry(ReopenImage(), "B");
  ASSERT_TRUE(a.has_value());
  ASSERT_TRUE(b.has_value());
  EXPECT_EQ(a->GetModified(), DateTime::FromCivil(kNow));
  std::vector<PortWords> found = {Field(kOk)};
  for (const DiskDirEntry& entry : {*a, *b}) {
    found = AndFields(std::move(found), FoundFields(entry));
    found.push_back(Field(kOk));
  }
  found.push_back(Field(DiskStatus::kNotFound));
  const std::vector<uint16_t> found_words = FieldWords(found);
  EXPECT_EQ(GetData("FOUND", static_cast<int>(found_words.size())),
            found_words);
}

// What a transfer program runs: `command` with `count` on handle 0, moving
// its transfers with `instruction` (such as "INR.IW 17, (R1)").
struct TransferProgram {
  DiskDriveCommand command;
  int count;
  std::string_view instruction;

  // Whether the loop starts the buffer again after each stop, rather than
  // logging stalls, so a transfer of any size fits in it. An INR or OUTR moves
  // a sector's words at most between stops.
  bool reuse_buffer = false;
};

// Returns a program that runs `program`'s transfer, as disk_design.md's OS
// does: it writes the command, reads the accept (ACCEPT, then the
// interrupts handled so far in ACCEPT_INTERRUPTS), moves the transfers between
// BUFFER and the data port, and reads the completion (COMPLETION) once its
// interrupt is handled, then halts. R6 counts the interrupts handled, where
// one is already raised for the command before. Each time the loop stops with
// transfers left and the control port shows the data phase, it logs R7 in
// STALLS, once for each R7.
// Returns the phase the drive shows during `program`'s data phase.
DiskDrivePhase GetTransferPhase(const TransferProgram& program) {
  const DiskCommandDef* def =
      GetDiskCommandDef(static_cast<uint8_t>(program.command));
  return def->kind == DiskCommandKind::kRead ? DiskDrivePhase::kReading
                                             : DiskDrivePhase::kWriting;
}

std::string MakeTransferProgram(const TransferProgram& program) {
  return absl::StrReplaceAll(
      R"---(
    .STACK 16

    .SEGMENT main CODE
        SETI 2, handler
        MVQ R6, 0               ; Interrupts handled
        EI
    wait_earlier:
        CMP R6, 1
        JC NZ, wait_earlier
        MOV R1, command         ; The command word and count
        MVQ R7, 2
    send:
        OUTR.IW 16, (R1)
        JC NS, send
    wait_accept:
        IN 18, R0
        AND R0, 14              ; Phase 2 or higher
        JC Z, wait_accept
        MOV R1, accept
        MVQ R7, 2
    read_accept:
        INR.IW 16, (R1)
        JC NS, read_accept
        MOV R1, accept_interrupts
        MOV (R1), R6
        MOV R1, accept_n
        MOV R7, (R1)
        MOV R1, buffer
        MOV R2, stalls
        MOV R3, 0FFFFh          ; R7 when last logged
    data:
        $INSTRUCTION
        JC S, data_done
        IN 18, R0
        AND R0, 15
        CMP R0, 3               ; A completion waiting: the transfer failed
        JC Z, data_done
        CMP R0, $PHASE
        JC NZ, data
        $AFTER_STOP
        CMP R7, R3
        JC Z, data
        MOV R3, R7
        MOV (R2), R7
        ADQ R2, 1
        JP data
    data_done:
        CMP R6, 2
        JC NZ, data_done
        MOV R1, completion
        MVQ R7, 2
    read_completion:
        INR.IW 16, (R1)
        JC NS, read_completion
        HALT

    handler:
        ADQ R6, 1
        IRT

    .SEGMENT vars DATA
    command:
        .DW $COMMAND, $COUNT
    accept:
        .DW 0FFFFh
    accept_n:
        .DW 0FFFFh
    accept_interrupts:
        .DW 0FFFFh
    completion:
        .DW 0FFFFh, 0FFFFh
    stalls:
        .DW 0FFFFh, 0FFFFh, 0FFFFh, 0FFFFh
    buffer:
        .DS 512

    .SEGMENT stack STACK
  )---",
      {{"$INSTRUCTION", program.instruction},
       {"$PHASE", absl::StrCat(static_cast<int>(GetTransferPhase(program)))},
       {"$AFTER_STOP",
        program.reuse_buffer ? "MOV R1, buffer\n        JP data" : ""},
       {"$COMMAND", absl::StrCat(CommandWord(program.command))},
       {"$COUNT", absl::StrCat(program.count)}});
}

class DiskDriveProgramTest : public DiskDriveTest {
 protected:
  // Expects the program's accept and completion to be OK with `n`, with no
  // interrupt for the accept and one for the completion, and `stalls` logged.
  void ExpectTransfer(int n, absl::Span<const uint16_t> stalls) {
    EXPECT_EQ(GetState().r6, 2);
    EXPECT_THAT(GetData("ACCEPT", 3),
                ElementsAre(static_cast<uint16_t>(kOk), n, 1));
    EXPECT_THAT(GetData("COMPLETION", 2),
                ElementsAre(static_cast<uint16_t>(kOk), n));
    std::vector<uint16_t> expected_stalls(stalls.begin(), stalls.end());
    expected_stalls.resize(4, 0xFFFF);
    EXPECT_EQ(GetData("STALLS", 4), expected_stalls);
  }
};

TEST_F(DiskDriveProgramTest, AnInrLoopReads) {
  // The loop stops only for the drive's sector loads.
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 300));
  OpenFile("A");
  ASSERT_TRUE(Load(MakeTransferProgram({.command = DiskDriveCommand::kRead,
                                        .count = 500,
                                        .instruction = "INR.IW 17, (R1)"})));
  ASSERT_TRUE(ExecuteUntilHalt());
  ExpectTransfer(300, {300, 300 - kDiskSectorWords});
  EXPECT_EQ(GetData("BUFFER", 300), FileWords(0, 300));
}

TEST_F(DiskDriveProgramTest, AnInrDwordLoopReadsPacked) {
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 299));
  OpenFile("A");
  ASSERT_TRUE(
      Load(MakeTransferProgram({.command = DiskDriveCommand::kReadPacked,
                                .count = 500,
                                .instruction = "INR.ID 17, [R1]"})));
  ASSERT_TRUE(ExecuteUntilHalt());
  ExpectTransfer(150, {150, 150 - kDiskSectorWords / 2});
  std::vector<uint16_t> expected = FileWords(0, 299);
  expected.push_back(0);
  EXPECT_EQ(GetData("BUFFER", 300), expected);
}

TEST_F(DiskDriveProgramTest, AnOutrLoopWrites) {
  // As in WriteTakesEachWord, the loop stops two transfers after the drive's
  // sector load and its new cluster.
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 500));
  OpenFile("A");
  EXPECT_THAT(Seek(0, 500), ElementsAre(Field(kOk)));
  ASSERT_TRUE(Load(MakeTransferProgram({.command = DiskDriveCommand::kWrite,
                                        .count = 100,
                                        .instruction = "OUTR.IW 17, (R1)"})));
  SetData("BUFFER", FileWords(500, 100));
  ASSERT_TRUE(ExecuteUntilHalt());
  ExpectTransfer(100, {100 - 2, 100 - 14});
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(DetachAndReadFile("A"), FileWords(0, 600));
}

TEST_F(DiskDriveProgramTest, AnOutrDwordLoopWritesPacked) {
  // The new cluster starts at dword 6.
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", 500));
  OpenFile("A");
  EXPECT_THAT(Seek(0, 500), ElementsAre(Field(kOk)));
  ASSERT_TRUE(
      Load(MakeTransferProgram({.command = DiskDriveCommand::kWritePacked,
                                .count = 50,
                                .instruction = "OUTR.ID 17, [R1]"})));
  SetData("BUFFER", FileWords(500, 100));
  ASSERT_TRUE(ExecuteUntilHalt());
  ExpectTransfer(50, {50 - 2, 50 - 8});
  EXPECT_THAT(Run(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(DetachAndReadFile("A"), FileWords(0, 600));
}

TEST_F(DiskDriveProgramTest, AProgramReads64KiWords) {
  // The largest READ, run in chunks, as it takes more cycles than
  // ExecuteUntilHalt runs: a sector's load for each 256 words, and the INR's
  // cycles for each word.
  constexpr int kWords = 0xFFFF;
  ASSERT_TRUE(InitDrive());
  drive_->AttachImage(MakeImageWithFile("A", kWords));
  OpenFile("A");
  ASSERT_TRUE(Load(MakeTransferProgram({.command = DiskDriveCommand::kRead,
                                        .count = kWords,
                                        .instruction = "INR.IW 17, (R1)",
                                        .reuse_buffer = true})));
  const Cycles start = GetCycles();
  for (int chunk = 0;
       chunk < 20 && GetState().core.GetState() != CpuCore::State::kIdle;
       ++chunk) {
    Execute(100'000);
  }
  ASSERT_EQ(GetState().core.GetState(), CpuCore::State::kIdle);
  EXPECT_GT(GetCycles() - start, 256 * kReadCycles);
  ExpectTransfer(kWords, {});
  EXPECT_THAT(Info(0), ElementsAre(Field(kOk), PortWords::FromDword(kWords),
                                   PortWords::FromDword(kWords)));
}

// Returns a program that runs `command`, READ BOOT or WRITE BOOT, as
// disk_design.md's OS does: it writes the command word, reads the accept
// (ACCEPT), moves the boot area between BUFFER and the data port with
// `instruction` (INR.ID or OUTR.ID), and reads the completion (COMPLETION) once
// its interrupt is handled, then halts. R6 counts the interrupts handled.
std::string MakeBootProgram(DiskDriveCommand command,
                            std::string_view instruction) {
  return absl::StrReplaceAll(
      R"---(
    .STACK 16

    .SEGMENT main CODE
        SETI 2, handler
        MVQ R6, 0               ; Interrupts handled
        EI
        MOV R1, command
        MVQ R7, 1
    send:
        OUTR.IW 16, (R1)
        JC NS, send
    wait_accept:
        IN 18, R0
        AND R0, 14              ; Phase 2 or higher
        JC Z, wait_accept
        MOV R1, accept
        MVQ R7, 1
    read_accept:
        INR.IW 16, (R1)
        JC NS, read_accept
        MOV R1, buffer
        MOV R7, $DWORDS
    data:
        $INSTRUCTION 17, [R1]
        JC NS, data
    wait_completion:
        CMP R6, 1
        JC NZ, wait_completion
        MOV R1, completion
        MVQ R7, 1
    read_completion:
        INR.IW 16, (R1)
        JC NS, read_completion
        HALT

    handler:
        ADQ R6, 1
        IRT

    .SEGMENT vars DATA
    command:
        .DW $COMMAND
    accept:
        .DW 0FFFFh
    completion:
        .DW 0FFFFh
    buffer:
        .DS $WORDS

    .SEGMENT stack STACK
  )---",
      {{"$INSTRUCTION", instruction},
       {"$DWORDS", absl::StrCat(kBootDwords)},
       {"$WORDS", absl::StrCat(DiskSuperblock::kBootAreaWords)},
       {"$COMMAND", absl::StrCat(CommandWord(command))}});
}

TEST_F(DiskDriveProgramTest, AnInrDwordLoopReadsTheBootArea) {
  ASSERT_TRUE(InitDrive());
  const DiskSuperblock::BootArea boot = BootWords(0x1000);
  AttachWithBootArea(boot);
  ASSERT_TRUE(Load(MakeBootProgram(DiskDriveCommand::kReadBoot, "INR.ID")));
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(GetState().r6, 1);
  EXPECT_THAT(GetData("ACCEPT", 1), ElementsAre(static_cast<uint16_t>(kOk)));
  EXPECT_THAT(GetData("COMPLETION", 1),
              ElementsAre(static_cast<uint16_t>(kOk)));
  EXPECT_THAT(GetData("BUFFER", DiskSuperblock::kBootAreaWords),
              ElementsAreArray(boot));
}

TEST_F(DiskDriveProgramTest, AnOutrDwordLoopWritesTheBootArea) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  const DiskSuperblock::BootArea boot = BootWords(0x2000);
  ASSERT_TRUE(Load(MakeBootProgram(DiskDriveCommand::kWriteBoot, "OUTR.ID")));
  SetData("BUFFER", boot);
  ASSERT_TRUE(ExecuteUntilHalt());
  EXPECT_EQ(GetState().r6, 1);
  EXPECT_THAT(GetData("ACCEPT", 1), ElementsAre(static_cast<uint16_t>(kOk)));
  EXPECT_THAT(GetData("COMPLETION", 1),
              ElementsAre(static_cast<uint16_t>(kOk)));
  EXPECT_EQ(DetachAndReadSuperblock().GetBootArea(), boot);
}

}  // namespace
}  // namespace oz3
