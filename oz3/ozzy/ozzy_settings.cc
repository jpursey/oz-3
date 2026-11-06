// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/ozzy_settings.h"

#include <algorithm>
#include <cstdint>
#include <limits>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/algorithm/container.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/config/config.h"
#include "gb/config/text_config.h"
#include "oz3/core/core_types.h"
#include "oz3/core/device_config.h"

namespace oz3 {

namespace {

//==============================================================================
// Keys
//==============================================================================

constexpr std::string_view kMachineKeys[] = {"clock_hz", "cores", "ports",
                                             "banks", "devices"};
constexpr std::string_view kBankKeys[] = {"ram"};
constexpr std::string_view kScreenKeys[] = {
    "type",      "bank", "page",       "interrupt",     "interrupt_cores",
    "columns",   "rows", "refresh_hz", "slow_blink_hz", "fast_blink_hz",
    "characters"};
constexpr std::string_view kKeyboardKeys[] = {"type", "port", "interrupt",
                                              "interrupt_cores"};
constexpr std::string_view kTeletypeKeys[] = {"type", "port", "baud",
                                              "buffer_size"};

//==============================================================================
// Paths and values
//==============================================================================

std::string GetKeyPath(std::string_view path, std::string_view key) {
  if (path.empty()) {
    return std::string(key);
  }
  return absl::StrCat(path, ".", key);
}

std::string GetIndexPath(std::string_view path, int index) {
  return absl::StrCat(path, "[", index, "]");
}

absl::Status PathError(std::string_view path, std::string_view message) {
  return absl::InvalidArgumentError(absl::StrCat(path, ": ", message));
}

// Returns an error for the first key in `map`, in sorted order, that isn't one
// of `keys`. `map` must be a map.
absl::Status CheckKeys(const gb::Config& map, std::string_view path,
                       absl::Span<const std::string_view> keys) {
  // The map is unordered, so the smallest unknown key is the one named, to
  // name the same one every time.
  std::optional<std::string_view> unknown_key;
  for (const auto& [key, value] : *map.GetMap()) {
    if (!absl::c_linear_search(keys, key) &&
        (!unknown_key.has_value() || key < *unknown_key)) {
      unknown_key = key;
    }
  }
  if (unknown_key.has_value()) {
    return PathError(GetKeyPath(path, *unknown_key), "unknown key");
  }
  return absl::OkStatus();
}

// Returns the int `value` holds, which must be from `min` to `max`.
absl::StatusOr<int64_t> GetInt(const gb::Config& value, std::string_view path,
                               int64_t min, int64_t max) {
  if (!value.IsInt()) {
    return PathError(path, "must be an int");
  }
  const int64_t result = value.GetInt();
  if (result < min || result > max) {
    return PathError(path, absl::StrCat("must be ", min, " to ", max));
  }
  return result;
}

// Reads the int at `key` in `map` into `value`, if `map` has it. It need only
// fit in a T, as ValidateOzzySettings, the processor, and the devices check
// the rest.
template <typename T>
absl::Status ReadInt(const gb::Config& map, std::string_view path,
                     std::string_view key, T* value) {
  const gb::Config* config = map.Get(key);
  if (config == nullptr) {
    return absl::OkStatus();
  }
  absl::StatusOr<int64_t> result =
      GetInt(*config, GetKeyPath(path, key), std::numeric_limits<T>::min(),
             std::numeric_limits<T>::max());
  if (!result.ok()) {
    return result.status();
  }
  *value = static_cast<T>(*result);
  return absl::OkStatus();
}

//==============================================================================
// OzzySettingsReader
//==============================================================================

// Reads OzzySettings from a config, one part at a time, so interrupt cores
// can be checked against the cores read before them.
class OzzySettingsReader final {
 public:
  absl::StatusOr<OzzySettings> Read(const gb::Config& config);

 private:
  absl::Status ReadBanks(const gb::Config& config);
  absl::Status ReadDevices(const gb::Config& config);
  absl::Status ReadDevice(const gb::Config& config, std::string_view path);
  absl::Status ReadScreen(const gb::Config& config, std::string_view path);
  absl::Status ReadKeyboard(const gb::Config& config, std::string_view path);
  absl::Status ReadTeletype(const gb::Config& config, std::string_view path);
  absl::Status ReadInterrupt(const gb::Config& config, std::string_view path,
                             DeviceInterrupt* interrupt);
  absl::StatusOr<uint8_t> GetCoreMask(const gb::Config& value,
                                      std::string_view path);
  absl::StatusOr<uint8_t> GetCoreBit(const gb::Config& value,
                                     std::string_view path);

  OzzySettings settings_;
};

absl::StatusOr<OzzySettings> OzzySettingsReader::Read(
    const gb::Config& config) {
  absl::Status status = CheckKeys(config, "", kMachineKeys);
  if (status.ok()) {
    status = ReadInt(config, "", "clock_hz", &settings_.clock_hz);
  }
  if (status.ok()) {
    status = ReadInt(config, "", "cores", &settings_.cores);
  }
  if (status.ok()) {
    status = ReadInt(config, "", "ports", &settings_.ports);
  }
  if (status.ok()) {
    status = ReadBanks(config);
  }
  if (status.ok()) {
    status = ReadDevices(config);
  }
  if (status.ok()) {
    status = ValidateOzzySettings(settings_);
  }
  if (!status.ok()) {
    return status;
  }
  return std::move(settings_);
}

absl::Status OzzySettingsReader::ReadBanks(const gb::Config& config) {
  const gb::Config* banks = config.Get("banks");
  if (banks == nullptr) {
    return absl::OkStatus();
  }
  if (!banks->IsArray()) {
    return PathError("banks", "must be an array");
  }
  const int bank_count = banks->GetArraySize();
  settings_.banks.assign(bank_count, 0);
  for (int i = 0; i < bank_count; ++i) {
    const std::string path = GetIndexPath("banks", i);
    const gb::Config* bank = banks->Get(i);
    if (!bank->IsMap()) {
      return PathError(path, "must be a map");
    }
    absl::Status status = CheckKeys(*bank, path, kBankKeys);
    if (status.ok()) {
      status = ReadInt(*bank, path, "ram", &settings_.banks[i]);
    }
    if (!status.ok()) {
      return status;
    }
  }
  return absl::OkStatus();
}

absl::Status OzzySettingsReader::ReadDevices(const gb::Config& config) {
  const gb::Config* devices = config.Get("devices");
  if (devices == nullptr) {
    return absl::OkStatus();
  }
  if (!devices->IsArray()) {
    return PathError("devices", "must be an array");
  }
  settings_.screen.reset();
  settings_.keyboard.reset();
  settings_.teletype.reset();
  const int device_count = devices->GetArraySize();
  for (int i = 0; i < device_count; ++i) {
    absl::Status status =
        ReadDevice(*devices->Get(i), GetIndexPath("devices", i));
    if (!status.ok()) {
      return status;
    }
  }
  return absl::OkStatus();
}

absl::Status OzzySettingsReader::ReadDevice(const gb::Config& config,
                                            std::string_view path) {
  if (!config.IsMap()) {
    return PathError(path, "must be a map");
  }
  const gb::Config* type = config.Get("type");
  if (type == nullptr) {
    return PathError(path, "missing type");
  }
  const std::string type_path = GetKeyPath(path, "type");
  if (!type->IsString()) {
    return PathError(type_path, "must be a string");
  }
  const std::string_view type_name = type->GetString();
  if (type_name == "screen") {
    return ReadScreen(config, path);
  }
  if (type_name == "keyboard") {
    return ReadKeyboard(config, path);
  }
  if (type_name == "teletype") {
    return ReadTeletype(config, path);
  }
  return PathError(
      type_path,
      absl::StrCat("unknown type \"", type_name,
                   "\" (must be \"screen\", \"keyboard\", or \"teletype\")"));
}

absl::Status OzzySettingsReader::ReadScreen(const gb::Config& config,
                                            std::string_view path) {
  if (settings_.screen.has_value()) {
    return PathError(path, "an Ozzy has only one screen");
  }
  ScreenSettings& screen = settings_.screen.emplace(GetOzzyScreenSettings());
  absl::Status status = CheckKeys(config, path, kScreenKeys);
  if (status.ok()) {
    status = ReadInt(config, path, "bank", &screen.bank);
  }
  if (status.ok()) {
    status = ReadInt(config, path, "page", &screen.page);
  }
  if (status.ok()) {
    status = ReadInterrupt(config, path, &screen.interrupt);
  }
  if (status.ok()) {
    status = ReadInt(config, path, "columns", &screen.columns);
  }
  if (status.ok()) {
    status = ReadInt(config, path, "rows", &screen.rows);
  }
  if (status.ok()) {
    status = ReadInt(config, path, "refresh_hz", &screen.refresh_hz);
  }
  if (status.ok()) {
    status = ReadInt(config, path, "slow_blink_hz", &screen.slow_blink_hz);
  }
  if (status.ok()) {
    status = ReadInt(config, path, "fast_blink_hz", &screen.fast_blink_hz);
  }
  if (!status.ok()) {
    return status;
  }
  if (const gb::Config* characters = config.Get("characters");
      characters != nullptr) {
    const std::string characters_path = GetKeyPath(path, "characters");
    if (!characters->IsArray()) {
      return PathError(characters_path, "must be an array");
    }
    std::vector<uint16_t>& words = screen.characters.emplace();
    const int count = characters->GetArraySize();
    words.reserve(count);
    for (int i = 0; i < count; ++i) {
      const gb::Config* word = characters->Get(i);
      if (!word->IsInt() || word->GetInt() < 0 || word->GetInt() > 0xFFFF) {
        // Only now build the path, as a character set is thousands of words.
        return GetInt(*word, GetIndexPath(characters_path, i), 0, 0xFFFF)
            .status();
      }
      words.push_back(static_cast<uint16_t>(word->GetInt()));
    }
  }
  return absl::OkStatus();
}

absl::Status OzzySettingsReader::ReadKeyboard(const gb::Config& config,
                                              std::string_view path) {
  if (settings_.keyboard.has_value()) {
    return PathError(path, "an Ozzy has only one keyboard");
  }
  KeyboardSettings& keyboard =
      settings_.keyboard.emplace(GetOzzyKeyboardSettings());
  absl::Status status = CheckKeys(config, path, kKeyboardKeys);
  if (status.ok()) {
    status = ReadInt(config, path, "port", &keyboard.port);
  }
  if (status.ok()) {
    status = ReadInterrupt(config, path, &keyboard.interrupt);
  }
  return status;
}

absl::Status OzzySettingsReader::ReadTeletype(const gb::Config& config,
                                              std::string_view path) {
  if (settings_.teletype.has_value()) {
    return PathError(path, "an Ozzy has only one teletype");
  }
  TeletypeSettings& teletype =
      settings_.teletype.emplace(GetOzzyTeletypeSettings());
  absl::Status status = CheckKeys(config, path, kTeletypeKeys);
  if (status.ok()) {
    status = ReadInt(config, path, "port", &teletype.port);
  }
  if (status.ok()) {
    status = ReadInt(config, path, "baud", &teletype.baud);
  }
  if (status.ok()) {
    status = ReadInt(config, path, "buffer_size", &teletype.buffer_size);
  }
  return status;
}

absl::Status OzzySettingsReader::ReadInterrupt(const gb::Config& config,
                                               std::string_view path,
                                               DeviceInterrupt* interrupt) {
  absl::Status status =
      ReadInt(config, path, "interrupt", &interrupt->interrupt);
  if (!status.ok()) {
    return status;
  }
  const gb::Config* cores = config.Get("interrupt_cores");
  if (cores == nullptr) {
    return absl::OkStatus();
  }
  absl::StatusOr<uint8_t> mask =
      GetCoreMask(*cores, GetKeyPath(path, "interrupt_cores"));
  if (!mask.ok()) {
    return mask.status();
  }
  interrupt->cores = *mask;
  return absl::OkStatus();
}

absl::StatusOr<uint8_t> OzzySettingsReader::GetCoreMask(const gb::Config& value,
                                                        std::string_view path) {
  if (value.IsString() && value.GetString() == "all") {
    return kAllCores;
  }
  if (value.IsInt()) {
    return GetCoreBit(value, path);
  }
  if (value.IsArray()) {
    uint8_t mask = 0;
    const int count = value.GetArraySize();
    for (int i = 0; i < count; ++i) {
      absl::StatusOr<uint8_t> bit =
          GetCoreBit(*value.Get(i), GetIndexPath(path, i));
      if (!bit.ok()) {
        return bit.status();
      }
      mask |= *bit;
    }
    return mask;
  }
  return PathError(path, "must be a core, an array of cores, or \"all\"");
}

absl::StatusOr<uint8_t> OzzySettingsReader::GetCoreBit(const gb::Config& value,
                                                       std::string_view path) {
  if (!value.IsInt()) {
    return PathError(path, "must be an int");
  }
  // The cores aren't validated yet, so this also keeps to the most there are.
  const int64_t core = value.GetInt();
  if (core < 0 || core >= std::min(settings_.cores, kMaxCores)) {
    return PathError(path, absl::StrCat("the Ozzy has no core ", core));
  }
  return static_cast<uint8_t>(1 << core);
}

}  // namespace

//==============================================================================
// Device defaults
//==============================================================================

ScreenSettings GetOzzyScreenSettings() {
  ScreenSettings settings;
  settings.bank = 0;
  settings.page = 15;
  settings.interrupt = {.interrupt = 6, .cores = kAllCores};
  return settings;
}

KeyboardSettings GetOzzyKeyboardSettings() {
  return {.port = 1, .interrupt = {.interrupt = 1, .cores = 1}};
}

TeletypeSettings GetOzzyTeletypeSettings() {
  TeletypeSettings settings;
  settings.port = 2;
  settings.baud = 0;
  return settings;
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
  if (settings.screen.has_value()) {
    const int bank = settings.screen->bank;
    const int page = settings.screen->page;
    if (bank < 0 || bank >= bank_count) {
      return PathError("screen.bank",
                       absl::StrCat("the Ozzy has no bank ", bank));
    }
    if (page < 0 || page >= kMemoryBankPageCount) {
      return PathError("screen.page",
                       absl::StrCat("must be 0 to ", kMemoryBankPageCount - 1));
    }
    if ((GetBootRomPages(bank) & (1 << page)) != 0) {
      return PathError("screen", absl::StrCat("page ", page, " of bank ", bank,
                                              " is the boot ROM"));
    }
    if ((settings.banks[bank] & (1 << page)) != 0) {
      return PathError(
          "screen", absl::StrCat("page ", page, " of bank ", bank, " is RAM"));
    }
  }
  return absl::OkStatus();
}

//==============================================================================
// Reading settings
//==============================================================================

absl::StatusOr<OzzySettings> ReadOzzySettings(std::string text) {
  absl::StatusOr<gb::Config> config =
      gb::ReadConfigFromText(std::move(text), gb::kRootlessTextConfig);
  if (!config.ok()) {
    return config.status();
  }
  return OzzySettingsReader().Read(*config);
}

}  // namespace oz3
