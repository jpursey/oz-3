// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_PROCESSOR_H_
#define OZ3_CORE_PROCESSOR_H_

#include <memory>

#include "gb/container/array.h"
#include "oz3/core/core_types.h"
#include "oz3/core/port.h"
#include "oz3/core/processor_config.h"

namespace oz3 {

//==============================================================================
// Processor
//==============================================================================

// The Processor class manages and simulates all OZ-3 components as a single
// virtual machine.
class Processor final {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a new Processor with the specified configuration.
  explicit Processor(const ProcessorConfig& config);

  Processor(const Processor&) = delete;
  Processor& operator=(const Processor&) = delete;
  ~Processor();

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the MemoryBank at the specified index.
  //
  // The index must be in the range [0, kMaxMemoryBanks). There always is a
  // fully functional MemoryBank object, even if it does not have any physical
  // memory or external memory maps.
  MemoryBank* GetMemory(int bank_index);
  const MemoryBank* GetMemory(int bank_index) const;

  // Returns the number of cores in the processor.
  int GetNumCores() const;

  // Returns the core at the specified index.
  //
  // The index must be in the range [0, GetNumCores()-1].
  CpuCore* GetCore(int core_index);
  const CpuCore* GetCore(int core_index) const;

  // Returns the number of ports in the processor.
  int GetNumPorts() const { return ports_.GetCount(); }

  // Returns the port at the specified index.
  //
  // The index must be in the range [0, GetNumPorts()-1].
  Port& GetPort(int port_index);

  // Locks the port at the specified index.
  //
  // The index must be in the range [0, GetNumPorts()-1].
  std::unique_ptr<Lock> LockPort(int port_index);

  // Returns the number of cycles that have been executed.
  Cycles GetCycles() const;

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Executes the processor for the specified number of cycles.
  //
  // This simulates all executable components of the processor in sync.
  //
  // This is a public function for the game, and components must therefore *not*
  // execute this function.
  void Execute(Cycles cycles);

  // Raise interrupt on all cores.
  void RaiseInterrupt(int interrupt_index);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  gb::Array<std::unique_ptr<MemoryBank>, kMaxMemoryBanks> banks_;
  gb::Array<std::unique_ptr<CpuCore>, kMaxCores> cores_;
  int num_cores_ = 0;
  PortBank ports_;

  Cycles cycles_ = 0;
};
static_assert(sizeof(Processor) <= 1024, "Processor is too large");

//==============================================================================
// Processor inlines
//==============================================================================

inline MemoryBank* Processor::GetMemory(int bank_index) {
  return banks_[bank_index].get();
}

inline const MemoryBank* Processor::GetMemory(int bank_index) const {
  DCHECK(bank_index >= 0 && bank_index < kMaxMemoryBanks);
  return banks_[bank_index].get();
}

inline int Processor::GetNumCores() const { return num_cores_; }

inline CpuCore* Processor::GetCore(int core_index) {
  DCHECK(core_index >= 0 && core_index < num_cores_);
  return cores_[core_index].get();
}

inline const CpuCore* Processor::GetCore(int core_index) const {
  DCHECK(core_index >= 0 && core_index < num_cores_);
  return cores_[core_index].get();
}

inline Port& Processor::GetPort(int port_index) {
  DCHECK(port_index >= 0 && port_index < ports_.GetCount());
  return ports_.GetPort(port_index);
}

inline std::unique_ptr<Lock> Processor::LockPort(int port_index) {
  DCHECK(port_index >= 0 && port_index < ports_.GetCount());
  return ports_.LockPort(port_index);
}

inline Cycles Processor::GetCycles() const { return cycles_; }

}  // namespace oz3

#endif  // OZ3_CORE_PROCESSOR_H_
