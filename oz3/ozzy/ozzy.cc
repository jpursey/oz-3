// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/ozzy.h"

#include <array>
#include <bit>
#include <cstdint>
#include <memory>
#include <optional>
#include <utility>

#include "absl/algorithm/container.h"
#include "absl/log/check.h"
#include "absl/memory/memory.h"
#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/time/time.h"
#include "absl/types/span.h"
#include "oz3/core/cpu_core.h"
#include "oz3/core/cpu_core_config.h"
#include "oz3/core/device_config.h"
#include "oz3/core/device_factory.h"
#include "oz3/core/lockable.h"
#include "oz3/core/memory_bank.h"
#include "oz3/core/memory_bank_config.h"
#include "oz3/core/processor_config.h"
#include "oz3/instruction_sets/default_instruction_set.h"
#include "oz3/ozzy/boot_rom.h"
#include "oz3/ozzy/machine_table.h"
#include "oz3/tools/program_loader.h"

namespace oz3 {

namespace {

//==============================================================================
// The machine table
//==============================================================================

// Writes a device's entry into `entry` from its wiring: its first port,
// interrupt, and page, if it has them. The interrupt cores are only the ones
// the Ozzy has, as the processor connects no others.
void WriteDeviceEntry(MachineDeviceType type, const DeviceConfig& config,
                      int cores, absl::Span<uint16_t> entry) {
  entry[kMachineDeviceType] = static_cast<uint16_t>(type);
  entry[kMachineDevicePort] = kMachineTableNone;
  entry[kMachineDeviceInterrupt] = kMachineTableNone;
  entry[kMachineDeviceBank] = kMachineTableNone;
  entry[kMachineDevicePage] = kMachineTableNone;
  if (!config.ports.empty()) {
    entry[kMachineDevicePort] = static_cast<uint16_t>(config.ports[0]);
  }
  if (!config.interrupts.empty()) {
    const DeviceInterrupt& interrupt = config.interrupts[0];
    entry[kMachineDeviceInterrupt] = static_cast<uint16_t>(interrupt.interrupt);
    entry[kMachineDeviceInterruptCores] =
        static_cast<uint16_t>(interrupt.cores & ((1 << cores) - 1));
    if (interrupt.shared) {
      entry[kMachineDeviceInterruptCores] |= kMachineInterruptShared;
    }
  }
  if (config.memory.pages.page_count > 0) {
    entry[kMachineDeviceBank] = static_cast<uint16_t>(config.memory.bank);
    entry[kMachineDevicePage] =
        static_cast<uint16_t>(config.memory.pages.start_page);
  }
}

// Returns the machine table for `settings`, which must be valid, and the
// processor config made from them.
std::array<uint16_t, kMachineTableSize> GetMachineTable(
    const OzzySettings& settings, const ProcessorConfig& config) {
  std::array<uint16_t, kMachineTableSize> table = {};
  const int bank_count = static_cast<int>(settings.banks.size());
  const absl::Span<const std::shared_ptr<const DeviceFactory>> devices =
      config.GetDevices();
  table[kMachineHeaderVersion] = kMachineTableVersion;
  table[kMachineHeaderClockHzLow] =
      static_cast<uint16_t>(settings.clock_hz & 0xFFFF);
  table[kMachineHeaderClockHzHigh] =
      static_cast<uint16_t>(settings.clock_hz >> 16);
  table[kMachineHeaderCores] = static_cast<uint16_t>(settings.cores);
  table[kMachineHeaderPorts] = static_cast<uint16_t>(settings.ports);
  table[kMachineHeaderBanks] = static_cast<uint16_t>(bank_count);
  table[kMachineHeaderDevices] = static_cast<uint16_t>(devices.size());

  absl::Span<uint16_t> entries =
      absl::MakeSpan(table).subspan(kMachineTableBanks);
  for (int i = 0; i < bank_count; ++i) {
    entries[kMachineBankRom] = GetBootRomPages(i);
    entries[kMachineBankRam] = settings.banks[i];
    entries.remove_prefix(kMachineBankSize);
  }
  entries = absl::MakeSpan(table).subspan(kMachineTableDevices);
  for (const std::shared_ptr<const DeviceFactory>& device : devices) {
    const DeviceConfig& device_config = device->GetConfig();
    if (device_config.name == Ozzy::kScreenName) {
      WriteDeviceEntry(MachineDeviceType::kScreen, device_config,
                       settings.cores, entries);
      entries[kMachineDeviceData] =
          static_cast<uint16_t>(settings.screen->columns);
      entries[kMachineDeviceData + 1] =
          static_cast<uint16_t>(settings.screen->rows);
    } else if (device_config.name == Ozzy::kKeyboardName) {
      WriteDeviceEntry(MachineDeviceType::kKeyboard, device_config,
                       settings.cores, entries);
      const std::optional<int> state_port =
          GetOzzyKeyboardSettings().state_port;
      entries[kMachineDeviceData] = state_port.has_value()
                                        ? static_cast<uint16_t>(*state_port)
                                        : kMachineTableNone;
    } else if (device_config.name == Ozzy::kTeletypeName) {
      WriteDeviceEntry(MachineDeviceType::kTeletype, device_config,
                       settings.cores, entries);
    } else {
      DCHECK(device_config.name == Ozzy::kTimerName);
      WriteDeviceEntry(MachineDeviceType::kTimer, device_config, settings.cores,
                       entries);
      entries[kMachineDeviceData] =
          static_cast<uint16_t>(settings.timer->tick_hz);
    }
    entries.remove_prefix(kMachineDeviceSize);
  }
  return table;
}

//==============================================================================
// The processor
//==============================================================================

// Returns the memory of a bank with `ram` pages, which has physical memory
// only up to its last readable page.
MemoryBankConfig GetMemoryBankConfig(int bank, uint16_t ram) {
  const uint16_t readable = ram | GetBootRomPages(bank);
  return MemoryBankConfig().SetMemPages(
      {.start_page = 0, .page_count = std::bit_width(readable)},
      {.read_mask = readable, .write_mask = ram});
}

// Returns the processor config for `settings`, which must be valid, or an
// error from a device's factory.
absl::StatusOr<ProcessorConfig> GetProcessorConfig(
    const OzzySettings& settings) {
  ProcessorConfig config;
  config.SetClockHz(settings.clock_hz);
  for (int i = 0; i < settings.cores; ++i) {
    config.AddCpuCore(CpuCoreConfig(GetDefaultInstructionSet()));
  }
  config.SetPortCount(settings.ports);
  const int bank_count = static_cast<int>(settings.banks.size());
  for (int i = 0; i < bank_count; ++i) {
    config.SetMemoryBank(i, GetMemoryBankConfig(i, settings.banks[i]));
  }
  if (settings.screen.has_value()) {
    absl::StatusOr<std::unique_ptr<ScreenFactory>> screen =
        ScreenFactory::Create(Ozzy::kScreenName,
                              GetOzzyScreenSettings(*settings.screen));
    if (!screen.ok()) {
      return screen.status();
    }
    config.AddDevice(std::move(*screen));
  }
  if (settings.keyboard) {
    absl::StatusOr<std::unique_ptr<KeyboardFactory>> keyboard =
        KeyboardFactory::Create(Ozzy::kKeyboardName, GetOzzyKeyboardSettings());
    if (!keyboard.ok()) {
      return keyboard.status();
    }
    config.AddDevice(std::move(*keyboard));
  }
  if (settings.teletype.has_value()) {
    absl::StatusOr<std::unique_ptr<TeletypeFactory>> teletype =
        TeletypeFactory::Create(Ozzy::kTeletypeName,
                                GetOzzyTeletypeSettings(*settings.teletype));
    if (!teletype.ok()) {
      return teletype.status();
    }
    config.AddDevice(std::move(*teletype));
  }
  if (settings.timer.has_value()) {
    absl::StatusOr<std::unique_ptr<TimerFactory>> timer = TimerFactory::Create(
        Ozzy::kTimerName, GetOzzyTimerSettings(*settings.timer));
    if (!timer.ok()) {
      return timer.status();
    }
    config.AddDevice(std::move(*timer));
  }
  return config;
}

}  // namespace

//==============================================================================
// Ozzy
//==============================================================================

absl::StatusOr<std::unique_ptr<Ozzy>> Ozzy::Create(
    const OzzySettings& settings) {
  absl::Status status = ValidateOzzySettings(settings);
  if (!status.ok()) {
    return status;
  }
  absl::StatusOr<ProcessorConfig> config = GetProcessorConfig(settings);
  if (!config.ok()) {
    return config.status();
  }
  status = config->Validate();
  if (!status.ok()) {
    return status;
  }
  auto processor = std::make_unique<Processor>(*config);

  // The loader writes the boot ROM into its page, as a ROM programmer would,
  // and places its stack in RAM, which the settings always have.
  ProgramLoader loader(processor.get());
  const ProgramLoader::Handle boot_rom = loader.Load(GetBootRom().GetProgram());
  CHECK_NE(boot_rom, ProgramLoader::kInvalidHandle);

  // The table is in the boot ROM's page, after its code.
  const std::array<uint16_t, kMachineTableSize> table =
      GetMachineTable(settings, *config);
  absl::Span<uint16_t> memory =
      processor->GetMemory(0)->GetMem(kMachineTableAddress, kMachineTableSize);
  DCHECK_EQ(memory.size(), table.size());
  absl::c_copy(table, memory.begin());

  // Power on: core 0 starts the boot ROM.
  CpuCore* core = processor->GetCore(0);
  std::unique_ptr<Lock> lock = core->RequestLock();
  DCHECK(lock->IsLocked());
  core->Reset(*lock, loader.GetResetParams(boot_rom));
  lock.reset();

  return absl::WrapUnique(new Ozzy(std::move(processor)));
}

Ozzy::Ozzy(std::unique_ptr<Processor> processor)
    : processor_(std::move(processor)),
      screen_(static_cast<Screen*>(processor_->GetDevice(kScreenName))),
      keyboard_(static_cast<Keyboard*>(processor_->GetDevice(kKeyboardName))),
      teletype_(static_cast<Teletype*>(processor_->GetDevice(kTeletypeName))),
      timer_(static_cast<Timer*>(processor_->GetDevice(kTimerName))) {}

Ozzy::~Ozzy() = default;

Cycles Ozzy::Run(absl::Duration time) {
  DCHECK(time != absl::InfiniteDuration());
  if (time <= absl::ZeroDuration()) {
    return 0;
  }
  const Cycles cycles =
      absl::IDivDuration(time * processor_->GetClockHz() + run_remainder_,
                         absl::Seconds(1), &run_remainder_);
  processor_->Execute(cycles);
  return cycles;
}

}  // namespace oz3
