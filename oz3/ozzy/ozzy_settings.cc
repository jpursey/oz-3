// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/ozzy_settings.h"

#include <cstdint>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "gb/config/config.h"
#include "gb/config/config_reader.h"
#include "gb/config/text_config.h"
#include "oz3/core/core_types.h"
#include "oz3/core/cpu_core.h"

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
                  kOzzyTeletypePort < OzzySettings::kMinPorts,
              "Device ports must be ports every Ozzy has");
static_assert(kOzzyKeyboardPort < kOzzyKeyboardKeyStatePort &&
                  kOzzyTeletypePort < kOzzyKeyboardKeyStatePort,
              "The key state ports must not be other devices' ports");
static_assert(kOzzyScreenInterrupt < CpuCore::kInterruptCount &&
                  kOzzyKeyboardInterrupt < CpuCore::kInterruptCount,
              "Device interrupts must be interrupts every core has");

//==============================================================================
// Keys
//==============================================================================

constexpr std::string_view kMachineKeys[] = {"clock_hz", "cores", "ports",
                                             "banks", "devices"};
constexpr std::string_view kBankKeys[] = {"ram"};
constexpr std::string_view kScreenKeys[] = {
    "type",          "columns",       "rows",      "refresh_hz",
    "slow_blink_hz", "fast_blink_hz", "characters"};
constexpr std::string_view kKeyboardKeys[] = {"type"};
constexpr std::string_view kTeletypeKeys[] = {"type", "baud", "buffer_size"};

//==============================================================================
// Device types
//==============================================================================

enum class DeviceType {
  kNone,
  kScreen,
  kKeyboard,
  kTeletype,
};

constexpr gb::ConfigEnumName<DeviceType> kDeviceTypes[] = {
    {"screen", DeviceType::kScreen},
    {"keyboard", DeviceType::kKeyboard},
    {"teletype", DeviceType::kTeletype},
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
  OzzySettings Read(const gb::ConfigReader& reader);

 private:
  void ReadBanks(const gb::ConfigReader& banks);
  void ReadDevices(const gb::ConfigReader& devices);
  void ReadDevice(const gb::ConfigReader& device);
  void ReadScreen(const gb::ConfigReader& device);
  void ReadKeyboard(const gb::ConfigReader& device);
  void ReadTeletype(const gb::ConfigReader& device);

  OzzySettings settings_;
};

OzzySettings OzzySettingsReader::Read(const gb::ConfigReader& reader) {
  if (reader.ValidateMap(kMachineKeys)) {
    reader.Get("clock_hz").ReadInt(&settings_.clock_hz);
    reader.Get("cores").ReadInt(&settings_.cores);
    reader.Get("ports").ReadInt(&settings_.ports);
    ReadBanks(reader.Get("banks"));
    ReadDevices(reader.Get("devices"));
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
  return absl::OkStatus();
}

//==============================================================================
// Reading settings
//==============================================================================

absl::StatusOr<OzzySettings> ReadOzzySettings(std::string text,
                                              std::string_view filename) {
  absl::StatusOr<gb::Config> config = gb::ReadConfigFromText(
      std::move(text), gb::kRootlessTextConfig, filename);
  if (!config.ok()) {
    return config.status();
  }
  const gb::ConfigReader reader(&*config, filename);
  OzzySettings settings = OzzySettingsReader().Read(reader);
  if (absl::Status status = reader.GetStatus(); !status.ok()) {
    return status;
  }

  // The settings are no longer the text, so these errors have no location.
  if (absl::Status status = ValidateOzzySettings(settings); !status.ok()) {
    if (filename.empty()) {
      return status;
    }
    return absl::InvalidArgumentError(
        absl::StrCat(filename, ": ", status.message()));
  }
  return settings;
}

}  // namespace oz3
