// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/disk_drive.h"

#include <array>
#include <cstdint>
#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/functional/function_ref.h"
#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "gb/file/file_system.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/core/core_types.h"
#include "oz3/core/date_time.h"
#include "oz3/core/lockable.h"
#include "oz3/core/port.h"
#include "oz3/devices/device_program_test.h"
#include "oz3/devices/disk_drive_command.h"
#include "oz3/disk/disk_file_system.h"
#include "oz3/disk/disk_image.h"
#include "oz3/disk/disk_label.h"
#include "oz3/disk/disk_status.h"
#include "oz3/disk/faulty_file_protocol.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::HasSubstr;

// The processor ports of the drive's command, data, and control ports, and its
// interrupt.
constexpr int kPort = 16;
constexpr int kCommandPort = kPort;
constexpr int kDataPort = kPort + 1;
constexpr int kControlPort = kPort + 2;
constexpr int kInterrupt = 2;

// A clock rate of a cycle a millisecond, and hardware where a sector read
// takes a second (1000 cycles), a write half that, and the latency 10 cycles.
constexpr int64_t kClockHz = 1000;
constexpr DiskHardwareSettings kHardware = {.read_words_per_sec = 256,
                                            .write_words_per_sec = 512,
                                            .latency = absl::Milliseconds(10)};
constexpr Cycles kReadCycles = 1000;
constexpr Cycles kLatencyCycles = 10;

constexpr char kPath[] = "mem:/disk.ozd";
constexpr char kOtherPath[] = "mem:/other.ozd";
constexpr int kSectors = 720;

constexpr DiskPortWidth kWord = DiskPortWidth::kWord;
constexpr DiskPortWidth kDword = DiskPortWidth::kDword;

constexpr uint16_t kOk = static_cast<uint16_t>(DiskStatus::kOk);

// Returns `status` as the word the drive sends.
constexpr uint16_t Word(DiskStatus status) {
  return static_cast<uint16_t>(status);
}

// Returns a command word: `command`'s code in the high byte, and `handle`.
constexpr uint16_t CommandWord(DiskDriveCommand command, int handle = 0) {
  return static_cast<uint16_t>(static_cast<int>(command) << 8 | handle);
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
  // sets drive_ to it. Returns whether it did.
  bool InitDrive(DiskDriveSettings settings = GetSettings()) {
    absl::StatusOr<std::unique_ptr<DiskDriveFactory>> factory =
        DiskDriveFactory::Create("Drive", settings);
    EXPECT_TRUE(factory.ok()) << factory.status();
    if (!factory.ok() || !Init({.num_ports = settings.port + DiskDrive::kPorts,
                                .clock_hz = kClockHz,
                                .devices = {*std::move(factory)}})) {
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

  // Attaches a new formatted image, and runs until the drive has read its
  // superblock.
  void AttachFormatted() {
    ASSERT_EQ(drive_->AttachImage(MakeImage()), DiskStatus::kOk);
    Execute(kLatencyCycles + kReadCycles + 1);
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

  // Returns the control port's state word, as IN reads it, and its phase.
  uint16_t GetStateWord() { return GetPort(kControlPort).GetValue(0); }
  int GetPhase() { return GetStateWord() & DiskDrive::kPhaseMask; }

  // Writes a field to `port` as OUTS does, only if its flag is clear, setting
  // it. Returns whether it wrote. No core may be using the port.
  bool Out(int port, DiskPortWidth width, std::array<uint16_t, 2> words) {
    std::unique_ptr<Lock> lock = LockPort(port);
    EXPECT_TRUE(lock->IsLocked());
    Port& p = GetPort(port);
    if (p.GetStatus() != 0) {
      return false;
    }
    if (width == kDword) {
      p.StoreWord(*lock, Port::A, words[0]);
      p.StoreWord(*lock, Port::S, words[1]);
    } else {
      p.StoreWord(*lock, Port::S, words[0]);
    }
    return true;
  }

  // Reads a field from `port` as INS does, only if its flag is set, clearing
  // it. Returns nullopt if the flag is clear. No core may be using the port.
  std::optional<std::array<uint16_t, 2>> In(int port, DiskPortWidth width) {
    std::unique_ptr<Lock> lock = LockPort(port);
    EXPECT_TRUE(lock->IsLocked());
    Port& p = GetPort(port);
    if (p.GetStatus() == 0) {
      return std::nullopt;
    }
    std::array<uint16_t, 2> words = {0, 0};
    if (width == kDword) {
      p.LoadWord(*lock, Port::A, words[0]);
      p.LoadWord(*lock, Port::S, words[1]);
    } else {
      p.LoadWord(*lock, Port::S, words[0]);
    }
    return words;
  }

  // Writes a request to the control port with OUTS, and runs until the drive
  // clears its flag, as TSTP sees it.
  void Request(uint16_t request) {
    ASSERT_TRUE(Out(kControlPort, kWord, {request, 0}));
    RunUntil([this] { return !IsSet(kControlPort); });
  }

  // Waits for the command port to be clear, then writes a field to it.
  void SendField(DiskPortWidth width, std::array<uint16_t, 2> words) {
    RunUntil([this] { return !IsSet(kCommandPort); });
    EXPECT_TRUE(Out(kCommandPort, width, words));
  }

  // Waits for the drive to put a field on the command port, then reads it, and
  // executes the cycle the drive notices it in, as it would after INS.
  std::array<uint16_t, 2> ReceiveField(DiskPortWidth width) {
    RunUntil([this] { return IsSet(kCommandPort); });
    std::optional<std::array<uint16_t, 2>> words = In(kCommandPort, width);
    EXPECT_TRUE(words.has_value());
    Execute(1);
    return words.value_or(std::array<uint16_t, 2>{0xFFFF, 0xFFFF});
  }

  // Reads a status, and if it is OK, the fields `widths` gives, returning
  // their words.
  std::vector<uint16_t> ReceiveStep(absl::Span<const DiskPortWidth> widths) {
    std::vector<uint16_t> words = {ReceiveField(kWord)[0]};
    if (words[0] != kOk) {
      return words;
    }
    for (const DiskPortWidth width : widths) {
      const std::array<uint16_t, 2> field = ReceiveField(width);
      words.push_back(field[0]);
      if (width == kDword) {
        words.push_back(field[1]);
      }
    }
    return words;
  }

  // Writes `command_word` alone, recording its definition for Complete.
  void Start(uint16_t command_word) {
    def_ = GetDiskCommandDef(static_cast<uint8_t>(command_word >> 8));
    SendField(kWord, {command_word, 0});
  }

  // Writes `command_word` and the words of its `arguments`, as the command
  // table moves them, waits for the drive to take them, as the OS does by
  // reading the control port until the phase is 2 or higher, and returns the
  // accept status and its results.
  std::vector<uint16_t> Command(uint16_t command_word,
                                absl::Span<const uint16_t> arguments = {}) {
    Start(command_word);
    int index = 0;
    if (def_ != nullptr) {
      for (const DiskPortWidth width : def_->arguments) {
        std::array<uint16_t, 2> words = {arguments[index++], 0};
        if (width == kDword) {
          words[1] = arguments[index++];
        }
        SendField(width, words);
      }
    }
    EXPECT_EQ(index, static_cast<int>(arguments.size()));
    RunUntil([this] { return GetPhase() >= 2; });
    return ReceiveStep(def_ != nullptr ? def_->accept_results
                                       : absl::Span<const DiskPortWidth>());
  }

  // Waits for the last command's completion, and returns its status and
  // results.
  std::vector<uint16_t> Complete() {
    CHECK(def_ != nullptr);
    return ReceiveStep(def_->completion_results);
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

// A command as disk_design.md's tables give it.
struct ExpectedCommand {
  DiskDriveCommand command;
  bool takes_handle;
  DiskCommandKind kind;
  bool packed;
  std::vector<DiskPortWidth> arguments;
  std::vector<DiskPortWidth> accept_results;
  std::vector<DiskPortWidth> completion_results;
};

std::vector<DiskPortWidth> Repeat(DiskPortWidth width, int count) {
  return std::vector<DiskPortWidth>(count, width);
}

std::vector<DiskPortWidth> Join(std::vector<DiskPortWidth> a,
                                const std::vector<DiskPortWidth>& b) {
  a.insert(a.end(), b.begin(), b.end());
  return a;
}

TEST(DiskCommandDefTest, EveryCommandIsAsDesigned) {
  using enum DiskDriveCommand;
  constexpr DiskCommandKind kSynchronous = DiskCommandKind::kSynchronous;
  constexpr DiskCommandKind kLatent = DiskCommandKind::kLatent;
  const std::vector<DiskPortWidth> name = Repeat(kDword, 4);
  const std::vector<ExpectedCommand> expected = {
      {kOpen, false, kLatent, false, name, {}, {kWord}},
      {kOpenRoot, false, kLatent, false, {}, {}, {kWord}},
      {kCreate, false, kLatent, false, name, {}, {kWord}},
      {kClose, true, kLatent, false, {}, {}, {}},
      {kFlush, true, kLatent, false, {}, {}, {}},
      {kRead, true, DiskCommandKind::kRead, false, {kWord}, {kWord}, {kWord}},
      {kWrite, true, DiskCommandKind::kWrite, false, {kWord}, {kWord}, {kWord}},
      {kReadPacked,
       true,
       DiskCommandKind::kRead,
       true,
       {kWord},
       {kWord},
       {kWord}},
      {kWritePacked,
       true,
       DiskCommandKind::kWrite,
       true,
       {kWord},
       {kWord},
       {kWord}},
      {kSeek, true, kSynchronous, false, {kDword}, {}, {}},
      {kInfo, true, kSynchronous, false, {}, {kDword, kDword}, {}},
      {kTruncate, true, kLatent, false, {}, {}, {}},
      {kFind, true, kLatent, false, name, {}, Repeat(kDword, 8)},
      {kDelete, false, kLatent, false, name, {}, {}},
      {kRename, false, kLatent, false, Repeat(kDword, 8), {}, {}},
      {kAttrib, false, kLatent, false, Join(name, {kWord}), {}, {}},
      {kMedia, false, kSynchronous, false, {}, {kDword, kWord}, {}},
      {kVolume,
       false,
       kLatent,
       false,
       {},
       {},
       Join(name, {kWord, kWord, kWord})},
      {kFormat, false, kLatent, false, Join(name, {kWord}), {}, {}},
      {kLabel, false, kLatent, false, name, {}, {}},
  };
  for (const ExpectedCommand& command : expected) {
    SCOPED_TRACE(static_cast<int>(command.command));
    const DiskCommandDef* def =
        GetDiskCommandDef(static_cast<uint8_t>(command.command));
    ASSERT_NE(def, nullptr);
    EXPECT_EQ(def->command, command.command);
    EXPECT_EQ(def->takes_handle, command.takes_handle);
    EXPECT_EQ(def->kind, command.kind);
    EXPECT_EQ(def->packed, command.packed);
    EXPECT_EQ(std::vector<DiskPortWidth>(def->arguments.begin(),
                                         def->arguments.end()),
              command.arguments);
    EXPECT_EQ(std::vector<DiskPortWidth>(def->accept_results.begin(),
                                         def->accept_results.end()),
              command.accept_results);
    EXPECT_EQ(std::vector<DiskPortWidth>(def->completion_results.begin(),
                                         def->completion_results.end()),
              command.completion_results);
  }

  // Every other code is no command.
  int defined = 0;
  for (int code = 0; code < 256; ++code) {
    if (GetDiskCommandDef(static_cast<uint8_t>(code)) != nullptr) {
      ++defined;
    }
  }
  EXPECT_EQ(defined, static_cast<int>(expected.size()));
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

  const std::vector<DiskHardwareSettings> negatives = {
      {.read_cache_sectors = -1},
      {.read_words_per_sec = -1},
      {.write_words_per_sec = -1},
      {.cache_words_per_sec = -1},
      {.latency = -absl::Milliseconds(1)},
      {.latency = absl::InfiniteDuration()},
  };
  for (const DiskHardwareSettings& hardware : negatives) {
    absl::StatusOr<std::unique_ptr<DiskDriveFactory>> factory =
        DiskDriveFactory::Create("Drive", {.hardware = hardware});
    EXPECT_EQ(factory.status().code(), absl::StatusCode::kInvalidArgument);
  }
  EXPECT_TRUE(DiskDriveFactory::Create("Drive", {.hardware = {}}).ok());
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
  EXPECT_EQ(DiskDriveSettings().hardware,
            GetDiskDrivePreset(DiskDrivePreset::kFloppy1440k));
  EXPECT_EQ(DiskDriveSettings().slots, 4);
}

//==============================================================================
// The control port
//==============================================================================

TEST_F(DiskDriveTest, StateWordShowsTheDisk) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  EXPECT_EQ(GetStateWord(), 0);
  EXPECT_FALSE(IsSet(kControlPort));

  ASSERT_EQ(drive_->AttachImage(MakeImage()), DiskStatus::kOk);
  Execute(1);
  EXPECT_EQ(GetStateWord(), DiskDrive::kDiskAttached);
  EXPECT_FALSE(IsSet(kControlPort));

  ASSERT_NE(drive_->DetachImage(), nullptr);
  Execute(1);
  EXPECT_EQ(GetStateWord(), 0);
}

TEST_F(DiskDriveTest, StateWordShowsEachPhaseAndTheCommand) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  const uint16_t disk = DiskDrive::kDiskAttached;

  // Sending, then working, then sending the completion, then idle.
  Start(CommandWord(DiskDriveCommand::kOpenRoot));
  Execute(1);
  EXPECT_EQ(GetStateWord(), 0x1100 | disk | 3);
  ASSERT_EQ(ReceiveField(kWord)[0], kOk);
  Execute(1);
  EXPECT_EQ(GetStateWord(), 0x1100 | disk | 2);
  RunUntil([this] { return IsSet(kCommandPort); });
  EXPECT_EQ(GetStateWord(), 0x1100 | disk | 3);
  EXPECT_EQ(Complete(), (std::vector<uint16_t>{kOk, 0}));
  Execute(1);
  EXPECT_EQ(GetStateWord(), disk);
}

TEST_F(DiskDriveTest, ResetAbortsEveryPhase) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // Arguments, as OPEN takes a name, with an argument word waiting.
  Start(CommandWord(DiskDriveCommand::kOpen));
  Execute(1);
  EXPECT_EQ(GetStateWord(), 0x1000 | DiskDrive::kDiskAttached | 1);
  ASSERT_TRUE(Out(kCommandPort, kDword, {1, 2}));
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetStateWord(), DiskDrive::kDiskAttached);
  EXPECT_FALSE(IsSet(kCommandPort));

  // Sending, with the accept status waiting, and a word in the data port.
  Start(CommandWord(DiskDriveCommand::kMedia));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  ASSERT_TRUE(Out(kDataPort, kWord, {5, 0}));
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetPhase(), 0);
  EXPECT_FALSE(IsSet(kCommandPort));
  EXPECT_FALSE(IsSet(kDataPort));

  // Working: the completion is dropped, and no interrupt is raised.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(kOk));
  ASSERT_EQ(GetPhase(), 2);
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetPhase(), 0);
  Execute(2 * kReadCycles);
  EXPECT_FALSE(IsSet(kCommandPort));
  EXPECT_FALSE(IsInterruptRaised());

  // The next command works as usual.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(kOk, kSectors, 0, 0));
}

TEST_F(DiskDriveTest, OtherRequestsDoNothing) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(kOk));
  for (const uint16_t request : {uint16_t{0}, uint16_t{0xFFFE}}) {
    Request(request);
    EXPECT_EQ(GetPhase(), 2);
  }
  EXPECT_THAT(Complete(), ElementsAre(kOk, 0));
}

TEST_F(DiskDriveTest, OnlyAResetWaitsForTheOtherPorts) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  std::unique_ptr<Lock> lock = LockPort(kDataPort);
  ASSERT_TRUE(lock->IsLocked());
  ASSERT_TRUE(Out(kControlPort, kWord, {0, 0}));
  Execute(2);
  EXPECT_FALSE(IsSet(kControlPort));

  ASSERT_TRUE(Out(kControlPort, kWord, {DiskDrive::kResetRequest, 0}));
  Execute(5);
  EXPECT_TRUE(IsSet(kControlPort));
  lock.reset();
  Execute(1);
  EXPECT_FALSE(IsSet(kControlPort));
}

TEST_F(DiskDriveTest, StateWaitsForTheControlPort) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  std::unique_ptr<Lock> lock = LockPort(kControlPort);
  ASSERT_TRUE(lock->IsLocked());
  ASSERT_EQ(drive_->AttachImage(MakeImage()), DiskStatus::kOk);
  Execute(5);
  EXPECT_EQ(GetStateWord(), 0);
  lock.reset();
  Execute(1);
  EXPECT_EQ(GetStateWord(), DiskDrive::kDiskAttached);
}

//==============================================================================
// The command protocol
//==============================================================================

TEST_F(DiskDriveTest, TheDriveTakesAWordTheCycleAfter) {
  ASSERT_TRUE(InitDrive());
  Execute(1);

  // Until the drive takes the command word, the flag is the OS's, and the
  // phase is still 0, so the OS doesn't read its own word as the status.
  ASSERT_TRUE(
      Out(kCommandPort, kWord, {CommandWord(DiskDriveCommand::kMedia), 0}));
  EXPECT_EQ(GetPhase(), 0);
  Execute(1);
  EXPECT_EQ(GetPhase(), 3);
  EXPECT_EQ(ReceiveField(kWord)[0], Word(DiskStatus::kNoDisk));
  Execute(1);
  EXPECT_EQ(GetPhase(), 0);
}

TEST_F(DiskDriveTest, AnUnknownCommandIsBadCommandAtOnce) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  for (const int code : {0x00, 0x15, 0x99, 0xFF}) {
    SCOPED_TRACE(code);
    SendField(kWord, {static_cast<uint16_t>(code << 8 | 3), 0});
    Execute(1);
    EXPECT_EQ(GetStateWord(), (code << 8) | DiskDrive::kDiskAttached | 3);
    EXPECT_EQ(ReceiveField(kWord)[0], Word(DiskStatus::kBadCommand));
    Execute(1);
    EXPECT_EQ(GetPhase(), 0);
  }
}

TEST_F(DiskDriveTest, MediaDescribesTheMedium) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(Word(DiskStatus::kNoDisk)));

  // Unformatted is fine, as the medium is the image's header.
  ASSERT_EQ(drive_->AttachImage(MakeImage(/*formatted=*/false)),
            DiskStatus::kUnformatted);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(kOk, kSectors, 0, 0));

  std::unique_ptr<DiskImage> image = drive_->DetachImage();
  ASSERT_EQ(image->SetWriteProtected(true), DiskStatus::kOk);
  ASSERT_EQ(drive_->AttachImage(std::move(image)), DiskStatus::kUnformatted);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(kOk, kSectors, 0, DiskMedia::kWriteProtectedFlag));
}

TEST_F(DiskDriveTest, AFailedImageFailsCommandsWithNoHandle) {
  ASSERT_TRUE(InitDrive());
  std::unique_ptr<DiskImage> image = MakeImage();
  protocol_->SetLimits({.reads = 0});
  ASSERT_EQ(drive_->AttachImage(std::move(image)), DiskStatus::kIoError);
  for (const DiskDriveCommand command :
       {DiskDriveCommand::kMedia, DiskDriveCommand::kOpenRoot}) {
    SCOPED_TRACE(static_cast<int>(command));
    EXPECT_THAT(Command(CommandWord(command)),
                ElementsAre(Word(DiskStatus::kIoError)));
  }
}

TEST_F(DiskDriveTest, AWordInAnArgumentPhaseIsAnArgument) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  Start(CommandWord(DiskDriveCommand::kOpen));
  Start(CommandWord(DiskDriveCommand::kMedia));
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
  EXPECT_FALSE(
      Out(kCommandPort, kWord, {CommandWord(DiskDriveCommand::kMedia), 0}));
  EXPECT_THAT(ReceiveStep(GetDiskCommandDef(0x40)->accept_results),
              ElementsAre(kOk, kSectors, 0, 0));
}

TEST_F(DiskDriveTest, AWordWhileWorkingIsLostUnderTheCompletion) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(kOk));
  ASSERT_EQ(GetPhase(), 2);
  ASSERT_TRUE(
      Out(kCommandPort, kWord, {CommandWord(DiskDriveCommand::kMedia), 0}));
  Execute(10);
  EXPECT_EQ(GetPhase(), 2);
  RunUntil([this] { return GetPhase() == 3; });
  EXPECT_THAT(Complete(), ElementsAre(kOk, 0));
}

//==============================================================================
// Timing and completions
//==============================================================================

TEST_F(DiskDriveTest, CompletionsWaitForTheDisk) {
  ASSERT_TRUE(InitDrive());
  Execute(1);

  // The superblock read starts the cycle after the attach, and OPEN ROOT's
  // read of the FAT's first sector, for the root's chain, waits for it. It is
  // the next sector, so it has no latency.
  ASSERT_EQ(drive_->AttachImage(MakeImage()), DiskStatus::kOk);
  const Cycles attached = GetCycles();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(kOk));
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(GetCycles(), attached + kLatencyCycles + 2 * kReadCycles + 1);
  EXPECT_TRUE(IsInterruptRaised());
  EXPECT_THAT(Complete(), ElementsAre(kOk, 0));

  // Closing the root writes nothing, so it completes at once, in the cycle the
  // drive sees its accept status read.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(kOk));
  EXPECT_TRUE(IsSet(kCommandPort));
  EXPECT_THAT(Complete(), ElementsAre(kOk));
}

TEST_F(DiskDriveTest, AResetCommandsWorkIsStillDone) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // OPEN ROOT's work is done at once, so its slot is open after a reset, and
  // the next command's completion waits for the disk to finish it.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(kOk));
  const Cycles accepted = GetCycles();
  Request(DiskDrive::kResetRequest);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(kOk));
  // The work started as the drive accepted OPEN ROOT, the cycle before the OS
  // read its status.
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_EQ(GetCycles(), accepted - 1 + kReadCycles);
  EXPECT_THAT(Complete(), ElementsAre(kOk));
}

TEST_F(DiskDriveTest, TheDriveSleepsWhileIdle) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  EXPECT_EQ(drive_->GetDueCycle(), kSleepForever);
  AttachFormatted();
  EXPECT_EQ(drive_->GetDueCycle(), kSleepForever);

  // While working, it sleeps until its completion is due.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(kOk));
  Execute(1);
  EXPECT_GT(drive_->GetDueCycle(), GetCycles() + kReadCycles / 2);
  EXPECT_NE(drive_->GetDueCycle(), kSleepForever);
}

TEST_F(DiskDriveTest, AHeldCommandPortDelaysTheDrive) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  ASSERT_TRUE(
      Out(kCommandPort, kWord, {CommandWord(DiskDriveCommand::kMedia), 0}));
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
  EXPECT_THAT(ReceiveStep({}), ElementsAre(Word(DiskStatus::kNoDisk)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(Word(DiskStatus::kNoDisk)));
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
              ElementsAre(Word(DiskStatus::kNoDisk)));

  // With the one slot taken, a second is too many open at accept, rather than
  // in use at completion.
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(kOk));
  EXPECT_THAT(Complete(), ElementsAre(kOk, 0));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Word(DiskStatus::kTooManyOpen)));

  // An unformatted disk is found at completion.
  ASSERT_EQ(drive_->AttachImage(MakeImage(/*formatted=*/false, kOtherPath)),
            DiskStatus::kUnformatted);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(kOk));
  EXPECT_THAT(Complete(), ElementsAre(kOk));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(kOk));
  EXPECT_THAT(Complete(), ElementsAre(Word(DiskStatus::kUnformatted)));
}

TEST_F(DiskDriveTest, CloseNeedsAnOpenSlot) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  for (const int handle : {0, 3, 4, 0xFF}) {
    SCOPED_TRACE(handle);
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, handle)),
                ElementsAre(Word(DiskStatus::kBadHandle)));
  }
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(kOk));
  EXPECT_THAT(Complete(), ElementsAre(kOk, 0));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(kOk));
  EXPECT_THAT(Complete(), ElementsAre(kOk));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Word(DiskStatus::kBadHandle)));
}

TEST_F(DiskDriveTest, CloseFreesAStaleSlot) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(kOk));
  EXPECT_THAT(Complete(), ElementsAre(kOk, 0));

  // Attaching over the image leaves slot 0 stale, which closes without error,
  // as the root had nothing to write.
  ASSERT_EQ(drive_->AttachImage(MakeImage(/*formatted=*/true, kOtherPath)),
            DiskStatus::kOk);
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(kOk));
  EXPECT_THAT(Complete(), ElementsAre(kOk));

  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Word(DiskStatus::kBadHandle)));
}

TEST_F(DiskDriveTest, DetachingCompletesTheCommandWithNoDisk) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // The accept status is still to be read, and the completion follows it.
  Start(CommandWord(DiskDriveCommand::kOpenRoot));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  ASSERT_NE(drive_->DetachImage(), nullptr);
  EXPECT_EQ(ReceiveField(kWord)[0], kOk);
  ASSERT_TRUE(RunUntil([this] { return IsSet(kCommandPort); }));
  EXPECT_LT(GetCycles(), 10 + kLatencyCycles + kReadCycles);
  EXPECT_THAT(Complete(), ElementsAre(Word(DiskStatus::kNoDisk)));
  Execute(1);
  EXPECT_EQ(GetStateWord(), 0);
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
  const CoreState& state = GetState();
  EXPECT_EQ(state.r6, 2);
  MemAccessor data = GetState().data;
  const uint16_t base = state.bd;
  EXPECT_EQ(data.SetAddress(base + GetLabel("OPEN_ACCEPT")).GetValue(), kOk);
  EXPECT_EQ(data.SetAddress(base + GetLabel("OPEN_COMPLETION")).GetValue(),
            kOk);
  EXPECT_EQ(data.GetValue(), 0);
  EXPECT_EQ(data.SetAddress(base + GetLabel("CLOSE_ACCEPT")).GetValue(), kOk);
  EXPECT_EQ(data.SetAddress(base + GetLabel("CLOSE_COMPLETION")).GetValue(),
            kOk);
  Execute(1);
  EXPECT_EQ(GetStateWord(), DiskDrive::kDiskAttached);
}

}  // namespace
}  // namespace oz3
