// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_DEVICE_H_
#define OZ3_CORE_DEVICE_H_

#include <cstdint>
#include <memory>
#include <string_view>

#include "absl/log/check.h"
#include "oz3/core/core_types.h"
#include "oz3/core/device_config.h"
#include "oz3/core/execution_component.h"
#include "oz3/core/processor.h"

namespace oz3 {

//==============================================================================
// Device
//==============================================================================

// A Device is hardware outside the OZ-3 (a screen, a keyboard, a disk drive)
// that a Processor runs in step with its cores, cycle by cycle.
//
// Each Processor creates its own device from a DeviceFactory in its config,
// and runs it after its cores, in the order the devices were added. A device
// sees only what this class gives it: the ports and interrupts it is wired to,
// and any memory bank as shared memory. It uses them as a core does: through
// their locks, at its current cycle, at the start of an Execute(), and then it
// advances its cycles past what it did.
class Device : public ExecutionComponent {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  ~Device() override = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the device's name, from its DeviceConfig.
  std::string_view GetName() const { return config_.name; }

  //----------------------------------------------------------------------------
  // Component implementation
  //----------------------------------------------------------------------------

  void AttachProcessor(CoreInternal, Processor* processor) final;

 protected:
  //----------------------------------------------------------------------------
  // Derived class interface
  //----------------------------------------------------------------------------

  // Constructs a device wired as `config`, which is the config of the factory
  // creating it.
  explicit Device(DeviceConfig config);

  // Returns the number of ports the device is wired to.
  int GetPortCount() const;

  // Returns or locks one of the device's ports, numbered as in its
  // DeviceConfig. The port must be in the range [0, GetPortCount()).
  Port& GetPort(int port);
  std::unique_ptr<Lock> LockPort(int port);

  // Returns any memory bank, to lock and use as shared memory. The bank must be
  // in the range [0, kMaxMemoryBanks).
  MemoryBank* GetMemory(int bank);

  // Returns the number of interrupts the device is wired to.
  int GetInterruptCount() const;

  // Returns the cores one of the device's interrupts is connected to, among
  // those the processor has. The interrupt is numbered as in its DeviceConfig,
  // and must be in the range [0, GetInterruptCount()).
  uint8_t GetInterruptCores(int interrupt) const;

  // Raises one of the device's interrupts on the cores it is connected to, and
  // returns the cores it was newly raised on (see Processor::RaiseInterrupt).
  // The interrupt must be in the range [0, GetInterruptCount()).
  uint8_t RaiseInterrupt(int interrupt);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // The device's name and wiring.
  const DeviceConfig config_;

  // The processor the device is attached to.
  Processor* processor_ = nullptr;
};

//==============================================================================
// Device inlines
//==============================================================================

inline int Device::GetPortCount() const {
  return static_cast<int>(config_.ports.size());
}

inline Port& Device::GetPort(int port) {
  DCHECK(processor_ != nullptr);
  DCHECK(port >= 0 && port < GetPortCount());
  return processor_->GetPort(config_.ports[port]);
}

inline std::unique_ptr<Lock> Device::LockPort(int port) {
  DCHECK(processor_ != nullptr);
  DCHECK(port >= 0 && port < GetPortCount());
  return processor_->LockPort(config_.ports[port]);
}

inline MemoryBank* Device::GetMemory(int bank) {
  DCHECK(processor_ != nullptr);
  DCHECK(bank >= 0 && bank < kMaxMemoryBanks);
  return processor_->GetMemory(bank);
}

inline int Device::GetInterruptCount() const {
  return static_cast<int>(config_.interrupts.size());
}

inline uint8_t Device::GetInterruptCores(int interrupt) const {
  DCHECK(processor_ != nullptr);
  DCHECK(interrupt >= 0 && interrupt < GetInterruptCount());
  const uint8_t processor_cores =
      static_cast<uint8_t>((1 << processor_->GetNumCores()) - 1);
  return config_.interrupts[interrupt].cores & processor_cores;
}

inline uint8_t Device::RaiseInterrupt(int interrupt) {
  DCHECK(processor_ != nullptr);
  DCHECK(interrupt >= 0 && interrupt < GetInterruptCount());
  const DeviceInterrupt& wiring = config_.interrupts[interrupt];
  return processor_->RaiseInterrupt(wiring.interrupt, wiring.cores);
}

}  // namespace oz3

#endif  // OZ3_CORE_DEVICE_H_
