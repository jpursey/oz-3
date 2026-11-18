// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/ozzy_settings.h"

#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/log/check.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/time/time.h"
#include "gb/config/config.h"
#include "gb/config/config_reader.h"
#include "gb/config/text_config.h"
#include "gb/file/path.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"
#include "oz3/devices/disk_drive.h"
#include "oz3/disk/disk_hardware.h"

namespace oz3 {

namespace {

//==============================================================================
// Device wiring
//==============================================================================

// The wiring fits every Ozzy, so no settings can make it invalid.
static_assert((GetBootRomPages(kOzzyScreenBank) & (1 << kOzzyScreenPage)) == 0,
              "The screen's page must not be the boot ROM's");
static_assert(kOzzyScreenPage < kMemoryBankPageCount,
              "The screen's page must be in its bank");
static_assert(kOzzyKeyboardPort < OzzySettings::kMinPorts &&
                  kOzzyKeyboardKeyStatePort + Keyboard::kKeyStatePorts <=
                      OzzySettings::kMinPorts &&
                  kOzzyTeletypePort < OzzySettings::kMinPorts &&
                  kOzzyTimerPort + Timer::kPorts <= OzzySettings::kMinPorts,
              "Device ports must be ports every Ozzy has");
static_assert(kOzzyKeyboardPort < kOzzyTimerPort &&
                  kOzzyTeletypePort < kOzzyTimerPort &&
                  kOzzyTimerPort + Timer::kPorts <= kOzzyKeyboardKeyStatePort,
              "The timer's ports must be between the other devices' ports");
static_assert(kOzzyKeyboardKeyStatePort + Keyboard::kKeyStatePorts <=
                      kOzzyDrivePort &&
                  kOzzyDrivePort + kOzzyDriveCount * DiskDrive::kPorts <=
                      OzzySettings::kMinPorts,
              "The drives' ports must be after the keyboard's, and ports "
              "every Ozzy has");
static_assert(kOzzyScreenInterrupt < CpuCore::kInterruptCount &&
                  kOzzyKeyboardInterrupt < CpuCore::kInterruptCount &&
                  kOzzyTimerInterrupt < CpuCore::kInterruptCount,
              "Device interrupts must be interrupts every core has");
static_assert(kOzzyTimerInterrupt != kOzzyScreenInterrupt &&
                  kOzzyTimerInterrupt != kOzzyKeyboardInterrupt &&
                  kOzzyScreenInterrupt != kOzzyKeyboardInterrupt,
              "Device interrupts must be different");
static_assert(kOzzyKeyboardInterrupt < kOzzyDriveInterrupt &&
                  kOzzyTimerInterrupt < kOzzyDriveInterrupt &&
                  kOzzyDriveInterrupt + kOzzyDriveCount <= kOzzyScreenInterrupt,
              "The drives' interrupts must be between the other devices'");

//==============================================================================
// Keys
//==============================================================================

constexpr std::string_view kMachineKeys[] = {"clock_hz", "cores",   "ports",
                                             "banks",    "devices", "drives"};
constexpr std::string_view kBankKeys[] = {"ram"};
constexpr std::string_view kScreenKeys[] = {
    "type",          "columns",       "rows",      "refresh_hz",
    "slow_blink_hz", "fast_blink_hz", "characters"};
constexpr std::string_view kKeyboardKeys[] = {"type"};
constexpr std::string_view kTeletypeKeys[] = {"type", "baud", "buffer_size"};
constexpr std::string_view kTimerKeys[] = {"type", "tick_hz"};
constexpr std::string_view kDriveKeys[] = {"preset",
                                           "read_cache_sectors",
                                           "read_words_per_sec",
                                           "write_words_per_sec",
                                           "cache_words_per_sec",
                                           "latency_ms",
                                           "slots",
                                           "image"};

//==============================================================================
// Drive presets
//==============================================================================

constexpr gb::ConfigEnumName<DiskDrivePreset> kDrivePresets[] = {
    {"floppy-360k", DiskDrivePreset::kFloppy360k},
    {"floppy-720k", DiskDrivePreset::kFloppy720k},
    {"floppy-1440k", DiskDrivePreset::kFloppy1440k},
    {"hd-10m", DiskDrivePreset::kHardDisk10m},
    {"hd-20m", DiskDrivePreset::kHardDisk20m},
    {"hd-40m", DiskDrivePreset::kHardDisk40m},
};

//==============================================================================
// Device types
//==============================================================================

enum class DeviceType {
  kNone,
  kScreen,
  kKeyboard,
  kTeletype,
  kTimer,
};

constexpr gb::ConfigEnumName<DeviceType> kDeviceTypes[] = {
    {"screen", DeviceType::kScreen},
    {"keyboard", DeviceType::kKeyboard},
    {"teletype", DeviceType::kTeletype},
    {"timer", DeviceType::kTimer},
};

//==============================================================================
// Validation errors
//==============================================================================

absl::Status PathError(std::string_view path, std::string_view message) {
  return absl::InvalidArgumentError(absl::StrCat(path, ": ", message));
}

std::string GetIndexPath(std::string_view path, int index) {
  return absl::StrCat(path, "[", index, "]");
}

//==============================================================================
// OzzySettingsReader
//==============================================================================

// Reads OzzySettings from a config. Each value need only fit in its setting's
// type, as ValidateOzzySettings, the processor, and the devices check the rest.
// Errors are left in the reader's status.
class OzzySettingsReader final {
 public:
  // Relative image paths are from `folder`, which is normalized, or kept as
  // they are if it is empty.
  explicit OzzySettingsReader(std::string_view folder) : folder_(folder) {}

  OzzySettings Read(const gb::ConfigReader& reader);

 private:
  void ReadBanks(const gb::ConfigReader& banks);
  void ReadDevices(const gb::ConfigReader& devices);
  void ReadDevice(const gb::ConfigReader& device);
  void ReadScreen(const gb::ConfigReader& device);
  void ReadKeyboard(const gb::ConfigReader& device);
  void ReadTeletype(const gb::ConfigReader& device);
  void ReadTimer(const gb::ConfigReader& device);
  void ReadDrives(const gb::ConfigReader& drives);
  void ReadDrive(const gb::ConfigReader& reader, OzzyDriveSettings* drive);
  void ReadImage(const gb::ConfigReader& image, std::string* path);

  const std::string folder_;
  OzzySettings settings_;
};

OzzySettings OzzySettingsReader::Read(const gb::ConfigReader& reader) {
  if (reader.ValidateMap(kMachineKeys)) {
    reader.Get("clock_hz").ReadInt(&settings_.clock_hz);
    reader.Get("cores").ReadInt(&settings_.cores);
    reader.Get("ports").ReadInt(&settings_.ports);
    ReadBanks(reader.Get("banks"));
    ReadDevices(reader.Get("devices"));
    ReadDrives(reader.Get("drives"));
  }
  return std::move(settings_);
}

void OzzySettingsReader::ReadBanks(const gb::ConfigReader& banks) {
  if (!banks.ValidateArray()) {
    return;
  }
  const int bank_count = banks.GetArraySize();
  settings_.banks.assign(bank_count, 0);
  for (int i = 0; i < bank_count; ++i) {
    const gb::ConfigReader bank = banks.Get(i);
    if (bank.ValidateMap(kBankKeys)) {
      bank.Get("ram").ReadInt(&settings_.banks[i]);
    }
  }
}

void OzzySettingsReader::ReadDevices(const gb::ConfigReader& devices) {
  if (!devices.ValidateArray()) {
    return;
  }
  settings_.screen.reset();
  settings_.keyboard = false;
  settings_.teletype.reset();
  settings_.timer.reset();
  const int device_count = devices.GetArraySize();
  for (int i = 0; i < device_count; ++i) {
    ReadDevice(devices.Get(i));
  }
}

void OzzySettingsReader::ReadDevice(const gb::ConfigReader& device) {
  // A device that isn't a map is rejected by Get(), which makes the type
  // missing too, but only the first error is kept.
  const gb::ConfigReader type = device.Get("type");
  if (type.IsMissing()) {
    device.Reject("missing type");
    return;
  }
  DeviceType device_type = DeviceType::kNone;
  type.ReadEnum<DeviceType>(kDeviceTypes, &device_type);
  switch (device_type) {
    case DeviceType::kNone:
      // The type was rejected.
      break;
    case DeviceType::kScreen:
      ReadScreen(device);
      break;
    case DeviceType::kKeyboard:
      ReadKeyboard(device);
      break;
    case DeviceType::kTeletype:
      ReadTeletype(device);
      break;
    case DeviceType::kTimer:
      ReadTimer(device);
      break;
  }
}

void OzzySettingsReader::ReadScreen(const gb::ConfigReader& device) {
  if (settings_.screen.has_value()) {
    device.Reject("an Ozzy has only one screen");
    return;
  }
  OzzyScreenSettings& screen = settings_.screen.emplace();
  if (!device.ValidateMap(kScreenKeys)) {
    return;
  }
  device.Get("columns").ReadInt(&screen.columns);
  device.Get("rows").ReadInt(&screen.rows);
  device.Get("refresh_hz").ReadInt(&screen.refresh_hz);
  device.Get("slow_blink_hz").ReadInt(&screen.slow_blink_hz);
  device.Get("fast_blink_hz").ReadInt(&screen.fast_blink_hz);
  const gb::ConfigReader characters = device.Get("characters");
  if (characters.ValidateArray()) {
    const int count = characters.GetArraySize();
    std::vector<uint16_t>& words = screen.characters.emplace(count, 0);
    for (int i = 0; i < count; ++i) {
      characters.Get(i).ReadInt(&words[i]);
    }
  }
}

void OzzySettingsReader::ReadKeyboard(const gb::ConfigReader& device) {
  if (settings_.keyboard) {
    device.Reject("an Ozzy has only one keyboard");
    return;
  }
  settings_.keyboard = true;
  device.ValidateMap(kKeyboardKeys);
}

void OzzySettingsReader::ReadTeletype(const gb::ConfigReader& device) {
  if (settings_.teletype.has_value()) {
    device.Reject("an Ozzy has only one teletype");
    return;
  }
  OzzyTeletypeSettings& teletype = settings_.teletype.emplace();
  if (!device.ValidateMap(kTeletypeKeys)) {
    return;
  }
  device.Get("baud").ReadInt(&teletype.baud);
  device.Get("buffer_size").ReadInt(&teletype.buffer_size);
}

void OzzySettingsReader::ReadTimer(const gb::ConfigReader& device) {
  if (settings_.timer.has_value()) {
    device.Reject("an Ozzy has only one timer");
    return;
  }
  OzzyTimerSettings& timer = settings_.timer.emplace();
  if (!device.ValidateMap(kTimerKeys)) {
    return;
  }
  device.Get("tick_hz").ReadInt(&timer.tick_hz);
}

void OzzySettingsReader::ReadDrives(const gb::ConfigReader& drives) {
  if (!drives.ValidateArray(0, kOzzyDriveCount)) {
    return;
  }
  const int drive_count = drives.GetArraySize();
  settings_.drives.assign(drive_count, OzzyDriveSettings());
  for (int i = 0; i < drive_count; ++i) {
    ReadDrive(drives.Get(i), &settings_.drives[i]);
  }
}

void OzzySettingsReader::ReadDrive(const gb::ConfigReader& reader,
                                   OzzyDriveSettings* drive) {
  if (!reader.ValidateMap(kDriveKeys)) {
    return;
  }

  // The preset comes first, as the other settings change what it gives.
  const gb::ConfigReader preset = reader.Get("preset");
  if (preset.IsMissing()) {
    reader.Reject("missing preset");
    return;
  }
  DiskDrivePreset drive_preset = DiskDrivePreset::kFloppy1440k;
  preset.ReadEnum<DiskDrivePreset>(kDrivePresets, &drive_preset);
  DiskHardwareSettings& hardware = drive->hardware;
  hardware = GetDiskDrivePreset(drive_preset);
  reader.Get("read_cache_sectors").ReadInt(&hardware.read_cache_sectors);
  reader.Get("read_words_per_sec").ReadInt(&hardware.read_words_per_sec);
  reader.Get("write_words_per_sec").ReadInt(&hardware.write_words_per_sec);
  reader.Get("cache_words_per_sec").ReadInt(&hardware.cache_words_per_sec);
  int latency_ms =
      static_cast<int>(absl::ToInt64Milliseconds(hardware.latency));
  reader.Get("latency_ms").ReadInt(&latency_ms);
  hardware.latency = absl::Milliseconds(latency_ms);
  reader.Get("slots").ReadInt(&drive->slots);
  ReadImage(reader.Get("image"), &drive->image);
}

void OzzySettingsReader::ReadImage(const gb::ConfigReader& image,
                                   std::string* path) {
  if (image.IsMissing()) {
    return;
  }
  std::string text;
  image.ReadString(&text);
  if (text.empty()) {
    image.Reject("must be a path");
    return;
  }
  *path = gb::NormalizePath(text);
  if (!path->empty() && !folder_.empty() && !gb::IsPathAbsolute(*path)) {
    *path = gb::NormalizePath(gb::JoinPath(folder_, *path));
  }
  if (path->empty()) {
    image.Reject(absl::StrCat("isn't a path in the config's folder: ", text));
  }
}

}  // namespace

//==============================================================================
// Device settings
//==============================================================================

ScreenSettings GetOzzyScreenSettings(const OzzyScreenSettings& screen) {
  return {.bank = kOzzyScreenBank,
          .page = kOzzyScreenPage,
          .interrupt = {.interrupt = kOzzyScreenInterrupt,
                        .cores = kOzzyScreenInterruptCores},
          .columns = screen.columns,
          .rows = screen.rows,
          .refresh_hz = screen.refresh_hz,
          .slow_blink_hz = screen.slow_blink_hz,
          .fast_blink_hz = screen.fast_blink_hz,
          .characters = screen.characters,
          .draw_pixels = screen.draw_pixels};
}

KeyboardSettings GetOzzyKeyboardSettings() {
  return {.state_port = kOzzyKeyboardKeyStatePort,
          .typed_port = kOzzyKeyboardPort,
          .typed_interrupt = {.interrupt = kOzzyKeyboardInterrupt,
                              .cores = kOzzyKeyboardInterruptCores,
                              .shared = true}};
}

TeletypeSettings GetOzzyTeletypeSettings(const OzzyTeletypeSettings& teletype) {
  return {.port = kOzzyTeletypePort,
          .baud = teletype.baud,
          .buffer_size = teletype.buffer_size};
}

TimerSettings GetOzzyTimerSettings(const OzzyTimerSettings& timer) {
  return {.port = kOzzyTimerPort,
          .interrupt = {.interrupt = kOzzyTimerInterrupt,
                        .cores = kOzzyTimerInterruptCores},
          .tick_hz = timer.tick_hz};
}

DiskDriveSettings GetOzzyDriveSettings(int drive,
                                       const OzzyDriveSettings& settings) {
  DCHECK(drive >= 0 && drive < kOzzyDriveCount);
  return {.port = kOzzyDrivePort + drive * DiskDrive::kPorts,
          .interrupt = {.interrupt = kOzzyDriveInterrupt + drive,
                        .cores = kOzzyDriveInterruptCores},
          .slots = settings.slots,
          .hardware = settings.hardware};
}

//==============================================================================
// Validating settings
//==============================================================================

absl::Status ValidateOzzySettings(const OzzySettings& settings) {
  if (settings.clock_hz < 1 || settings.clock_hz > OzzySettings::kMaxClockHz) {
    return PathError("clock_hz",
                     absl::StrCat("must be 1 to ", OzzySettings::kMaxClockHz));
  }
  if (settings.cores < 1 || settings.cores > kMaxCores) {
    return PathError("cores", absl::StrCat("must be 1 to ", kMaxCores));
  }
  if (settings.ports < OzzySettings::kMinPorts || settings.ports > kMaxPorts) {
    return PathError("ports", absl::StrCat("must be ", OzzySettings::kMinPorts,
                                           " to ", kMaxPorts));
  }
  const int bank_count = static_cast<int>(settings.banks.size());
  if (bank_count < 1 || bank_count > kMaxMemoryBanks) {
    return PathError(
        "banks", absl::StrCat("must have 1 to ", kMaxMemoryBanks, " banks"));
  }
  for (int i = 0; i < bank_count; ++i) {
    if ((settings.banks[i] & GetBootRomPages(i)) != 0) {
      return PathError(GetIndexPath("banks", i),
                       "RAM can't be on the boot ROM's page");
    }
  }
  if (absl::c_count(settings.banks, 0) == bank_count) {
    return PathError(
        "banks", "the Ozzy has no RAM, which the boot ROM needs for its stack");
  }
  if (settings.screen.has_value() &&
      (settings.banks[kOzzyScreenBank] & (1 << kOzzyScreenPage)) != 0) {
    return PathError("screen",
                     absl::StrCat("page ", kOzzyScreenPage, " of bank ",
                                  kOzzyScreenBank, " is RAM"));
  }
  if (settings.timer.has_value()) {
    if (settings.timer->tick_hz > OzzySettings::kMaxTimerTickHz) {
      return PathError("timer", absl::StrCat("tick_hz must be at most ",
                                             OzzySettings::kMaxTimerTickHz));
    }
    if (settings.timer->tick_hz > settings.clock_hz) {
      return PathError("timer",
                       absl::StrCat("tick_hz must be at most clock_hz (",
                                    settings.clock_hz, ")"));
    }
  }

  const int drive_count = static_cast<int>(settings.drives.size());
  if (drive_count > kOzzyDriveCount) {
    return PathError("drives", absl::StrCat("must have at most ",
                                            kOzzyDriveCount, " drives"));
  }

  // Images are compared as normalized text, so two spellings of one file may
  // still get past this.
  std::array<std::string, kOzzyDriveCount> images;
  for (int i = 0; i < drive_count; ++i) {
    const OzzyDriveSettings& drive = settings.drives[i];
    if (drive.image.empty()) {
      continue;
    }
    images[i] = gb::NormalizePath(drive.image);
    for (int j = 0; j < i; ++j) {
      if (images[j] == images[i]) {
        return PathError(
            absl::StrCat("drive ", std::string(1, GetOzzyDriveLetter(i))),
            absl::StrCat("image ", drive.image, " is also drive ",
                         std::string(1, GetOzzyDriveLetter(j)), "'s"));
      }
    }
  }
  return absl::OkStatus();
}

//==============================================================================
// Reading settings
//==============================================================================

absl::StatusOr<OzzySettings> ReadOzzySettings(std::string text,
                                              std::string_view path) {
  absl::StatusOr<gb::Config> config =
      gb::ReadConfigFromText(std::move(text), gb::kRootlessTextConfig, path);
  if (!config.ok()) {
    return config.status();
  }
  const gb::ConfigReader reader(&*config, path);
  OzzySettings settings =
      OzzySettingsReader(gb::RemoveFilename(gb::NormalizePath(path)))
          .Read(reader);
  if (absl::Status status = reader.GetStatus(); !status.ok()) {
    return status;
  }

  // The settings are no longer the text, so these errors have no location.
  if (absl::Status status = ValidateOzzySettings(settings); !status.ok()) {
    if (path.empty()) {
      return status;
    }
    return absl::InvalidArgumentError(
        absl::StrCat(path, ": ", status.message()));
  }
  return settings;
}

}  // namespace oz3
