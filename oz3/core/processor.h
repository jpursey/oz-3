// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_PROCESSOR_H_
#define OZ3_CORE_PROCESSOR_H_

#include <memory>

#include "gb/container/array.h"
#include "oz3/core/processor_config.h"
#include "oz3/core/types.h"

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
  MemoryBank& GetMemory(int bank_index) { return *banks_[bank_index]; }
  const MemoryBank& GetMemory(int bank_index) const {
    return *banks_[bank_index];
  }

  // Returns the number of cycles that have been executed.
  Cycles GetCycles() const { return cycles_; }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Advances the processor by the specified number of cycles.
  //
  // This simulates all components of the processor in sync.
  void AdvanceCycles(Cycles cycles);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  gb::Array<std::unique_ptr<MemoryBank>, kMaxMemoryBanks> banks_;

  Cycles cycles_ = 0;
};

}  // namespace oz3

#endif  // OZ3_CORE_PROCESSOR_H_