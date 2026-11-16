// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/devices/disk_drive.h"

#include <cstdint>
#include <iterator>
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
#include "oz3/disk/disk_media.h"
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

// The superblock read when an image is attached: a seek, then a sector.
constexpr Cycles kAttachCycles = kLatencyCycles + kReadCycles;

constexpr char kPath[] = "mem:/disk.ozd";
constexpr char kOtherPath[] = "mem:/other.ozd";
constexpr char kThirdPath[] = "mem:/third.ozd";
constexpr int kSectors = 720;

constexpr DiskStatus kOk = DiskStatus::kOk;

// Returns `status` as the field the drive sends.
PortWords Field(DiskStatus status) {
  return PortWords(static_cast<uint16_t>(status));
}

// Returns a command word: `command`'s code in the high byte, and `handle`.
constexpr uint16_t CommandWord(DiskDriveCommand command, int handle = 0) {
  return static_cast<uint16_t>(
      static_cast<int>(command) << DiskDrive::kCommandShift | handle);
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
    drive_->AttachImage(MakeImage());
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

  // Returns the control port's state word, as IN reads it, and its phase.
  uint16_t GetStateWord() { return GetPort(kControlPort).GetValue(0); }
  int GetPhase() { return GetStateWord() & DiskDrive::kPhaseMask; }

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
    ASSERT_TRUE(Out(kControlPort, PortWords(request)));
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
    return field.value_or(PortWords(0xFFFF, 0xFFFF));
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

  // Writes `command_word` alone, with `word1` in the field's word 1, recording
  // its definition for ReceiveAccept and Complete.
  void Start(uint16_t command_word, uint16_t word1 = 0) {
    def_ = GetDiskCommandDef(
        static_cast<uint8_t>(command_word >> DiskDrive::kCommandShift));
    SendField(PortWords(command_word, word1));
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

TEST_F(DiskDriveTest, StateWordShowsTheDisk) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  EXPECT_EQ(GetStateWord(), 0);
  EXPECT_FALSE(IsSet(kControlPort));

  // The drive shows the disk once it has read its superblock.
  drive_->AttachImage(MakeImage());
  Execute(kAttachCycles);
  EXPECT_EQ(GetStateWord(), 0);
  Execute(2);
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
  ASSERT_EQ(ReceiveField(), Field(kOk));
  EXPECT_EQ(GetStateWord(), 0x1100 | disk | 2);
  RunUntil([this] { return IsSet(kCommandPort); });
  EXPECT_EQ(GetStateWord(), 0x1100 | disk | 3);
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords(0)));
  EXPECT_EQ(GetStateWord(), disk);
}

TEST_F(DiskDriveTest, ResetAbortsEveryPhase) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // Arguments, as OPEN takes a name, with an argument field waiting.
  Start(CommandWord(DiskDriveCommand::kOpen));
  Execute(1);
  EXPECT_EQ(GetStateWord(), 0x1000 | DiskDrive::kDiskAttached | 1);
  ASSERT_TRUE(Out(kCommandPort, PortWords(1, 2)));
  Request(DiskDrive::kResetRequest);
  EXPECT_EQ(GetStateWord(), DiskDrive::kDiskAttached);
  EXPECT_FALSE(IsSet(kCommandPort));

  // Sending, with the accept status waiting, and a word in the data port.
  Start(CommandWord(DiskDriveCommand::kMedia));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  ASSERT_TRUE(Out(kDataPort, PortWords(5)));
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
              ElementsAre(Field(kOk), PortWords(kSectors), PortWords(0)));
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
              ElementsAre(Field(kOk), PortWords(kSectors), PortWords(0)));
}

TEST_F(DiskDriveTest, OnlyAResetWaitsForTheOtherPorts) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  std::unique_ptr<Lock> lock = LockPort(kDataPort);
  ASSERT_TRUE(lock->IsLocked());
  ASSERT_TRUE(Out(kControlPort, PortWords(0)));
  Execute(2);
  EXPECT_FALSE(IsSet(kControlPort));

  ASSERT_TRUE(Out(kControlPort, PortWords(DiskDrive::kResetRequest)));
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
  drive_->AttachImage(MakeImage());
  Execute(kAttachCycles + 5);
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
      Out(kCommandPort, PortWords(CommandWord(DiskDriveCommand::kMedia))));
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
              ElementsAre(Field(kOk), PortWords(kSectors), PortWords(0)));
}

TEST_F(DiskDriveTest, AnUnknownCommandIsBadCommandAtOnce) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  for (const int code : {0x00, 0x15, 0x99, 0xFF}) {
    SCOPED_TRACE(code);
    SendField(PortWords(static_cast<uint16_t>(code << 8 | 3)));
    Execute(1);
    EXPECT_EQ(GetStateWord(), (code << 8) | DiskDrive::kDiskAttached | 3);
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
              ElementsAre(Field(kOk), PortWords(kSectors), PortWords(0)));

  std::unique_ptr<DiskImage> image = drive_->DetachImage();
  ASSERT_EQ(image->SetWriteProtected(true), DiskStatus::kOk);
  drive_->AttachImage(std::move(image));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kMedia)),
              ElementsAre(Field(kOk), PortWords(kSectors),
                          PortWords(DiskMedia::kWriteProtectedFlag)));
}

TEST_F(DiskDriveTest, AFailedImageFailsCommandsWithNoHandle) {
  ASSERT_TRUE(InitDrive());
  std::unique_ptr<DiskImage> image = MakeImage();
  protocol_->SetLimits({.reads = 0});
  drive_->AttachImage(std::move(image));
  for (const DiskDriveCommand command :
       {DiskDriveCommand::kMedia, DiskDriveCommand::kOpenRoot}) {
    SCOPED_TRACE(static_cast<int>(command));
    EXPECT_THAT(Command(CommandWord(command)),
                ElementsAre(Field(DiskStatus::kIoError)));
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
      Out(kCommandPort, PortWords(CommandWord(DiskDriveCommand::kMedia))));
  EXPECT_THAT(ReceiveAccept(),
              ElementsAre(Field(kOk), PortWords(kSectors), PortWords(0)));
}

TEST_F(DiskDriveTest, AWordWhileWorkingIsLostUnderTheCompletion) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  ASSERT_EQ(GetPhase(), 2);
  ASSERT_TRUE(
      Out(kCommandPort, PortWords(CommandWord(DiskDriveCommand::kMedia))));
  Execute(10);
  EXPECT_EQ(GetPhase(), 2);
  RunUntil([this] { return GetPhase() == 3; });
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords(0)));
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
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords(0)));

  // Closing the root writes nothing, so it completes the cycle after the drive
  // sees its accept status read.
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_EQ(GetPhase(), 2);
  Execute(1);
  EXPECT_TRUE(IsSet(kCommandPort));
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
  EXPECT_EQ(ReceiveField(), PortWords(kSectors));
  EXPECT_EQ(ReceiveField(), PortWords(0));

  // A command word, and a request, while the drive reads its superblock.
  drive_->AttachImage(MakeImage(/*formatted=*/true, kThirdPath));
  Execute(2);
  ASSERT_TRUE(
      Out(kCommandPort, PortWords(CommandWord(DiskDriveCommand::kMedia))));
  ASSERT_TRUE(Out(kControlPort, PortWords(0)));
  Execute(kAttachCycles - 2);
  EXPECT_TRUE(IsSet(kCommandPort));
  EXPECT_TRUE(IsSet(kControlPort));
  EXPECT_EQ(GetPhase(), 0);
  Execute(1);
  EXPECT_FALSE(IsSet(kControlPort));
  EXPECT_EQ(GetPhase(), 3);
}

TEST_F(DiskDriveTest, AStepTakenAsTheHostAttachesStillEnds) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // The OS takes MEDIA's last field, and before the drive runs again, the
  // host attaches an image. The OS writes its next command word as soon as the
  // phase shows the drive has seen it.
  Start(CommandWord(DiskDriveCommand::kMedia));
  RunUntil([this] { return GetPhase() >= 2; });
  EXPECT_EQ(ReceiveField(), Field(kOk));
  EXPECT_EQ(ReceiveField(), PortWords(kSectors));
  RunUntil([this] { return IsSet(kCommandPort); });
  EXPECT_EQ(In(kCommandPort), PortWords(0));
  drive_->AttachImage(MakeImage(/*formatted=*/true, kOtherPath));
  Execute(1);
  EXPECT_EQ(GetPhase(), 0);
  Start(CommandWord(DiskDriveCommand::kMedia));

  // The drive takes it once the superblock is read.
  EXPECT_THAT(ReceiveAccept(),
              ElementsAre(Field(kOk), PortWords(kSectors), PortWords(0)));
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
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords(0)));
  EXPECT_EQ(drive_->GetDueCycle(), kSleepForever);
}

TEST_F(DiskDriveTest, AHeldCommandPortDelaysTheDrive) {
  ASSERT_TRUE(InitDrive());
  Execute(1);
  ASSERT_TRUE(
      Out(kCommandPort, PortWords(CommandWord(DiskDriveCommand::kMedia))));
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
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords(0)));
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
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords(0)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(DiskStatus::kTooManyOpen)));

  // An unformatted disk is found at completion.
  drive_->AttachImage(MakeImage(/*formatted=*/false, kOtherPath));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kUnformatted)));
}

TEST_F(DiskDriveTest, CloseNeedsAnOpenSlot) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();
  for (const int handle : {0, 3, 4, 0xFF}) {
    SCOPED_TRACE(handle);
    EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, handle)),
                ElementsAre(Field(DiskStatus::kBadHandle)));
  }
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kOpenRoot)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords(0)));
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
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords(0)));

  // Attaching over the image leaves slot 0 stale, which closes without error,
  // as the root had nothing to write.
  drive_->AttachImage(MakeImage(/*formatted=*/true, kOtherPath));
  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(kOk)));
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk)));

  EXPECT_THAT(Command(CommandWord(DiskDriveCommand::kClose, 0)),
              ElementsAre(Field(DiskStatus::kBadHandle)));
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
  EXPECT_THAT(Complete(), ElementsAre(Field(kOk), PortWords(0)));
  EXPECT_EQ(GetStateWord(), 0);
}

TEST_F(DiskDriveTest, DetachingFailsWorkNotStarted) {
  ASSERT_TRUE(InitDrive());
  AttachFormatted();

  // The accept status is still to be read, so the work finds no disk, and
  // completes at once.
  Start(CommandWord(DiskDriveCommand::kOpenRoot));
  Execute(1);
  ASSERT_EQ(GetPhase(), 3);
  ASSERT_NE(drive_->DetachImage(), nullptr);
  EXPECT_EQ(ReceiveField(), Field(kOk));
  Execute(1);
  EXPECT_TRUE(IsSet(kCommandPort));
  EXPECT_THAT(Complete(), ElementsAre(Field(DiskStatus::kNoDisk)));
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
  EXPECT_EQ(data.SetAddress(base + GetLabel("OPEN_ACCEPT")).GetValue(),
            static_cast<uint16_t>(kOk));
  EXPECT_EQ(data.SetAddress(base + GetLabel("OPEN_COMPLETION")).GetValue(),
            static_cast<uint16_t>(kOk));
  EXPECT_EQ(data.GetValue(), 0);
  EXPECT_EQ(data.SetAddress(base + GetLabel("CLOSE_ACCEPT")).GetValue(),
            static_cast<uint16_t>(kOk));
  EXPECT_EQ(data.SetAddress(base + GetLabel("CLOSE_COMPLETION")).GetValue(),
            static_cast<uint16_t>(kOk));
  Execute(1);
  EXPECT_EQ(GetStateWord(), DiskDrive::kDiskAttached);
}

}  // namespace
}  // namespace oz3
