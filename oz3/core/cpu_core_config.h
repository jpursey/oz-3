// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_CPU_CORE_CONFIG_H_
#define OZ3_CORE_CPU_CORE_CONFIG_H_

#include <memory>

#include "absl/types/span.h"
#include "oz3/core/instruction_set.h"

namespace oz3 {

//==============================================================================
// CpuCoreConfig
//==============================================================================

// The CpuCoreConfig struct contains configuration information for a CPU core.
class CpuCoreConfig {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Constructs a default core configuration using the base OZ-3 instruction
  // set.
  static CpuCoreConfig Default() { return CpuCoreConfig(); }

  // Constructs a core with all banks mapped to memory bank 0.
  CpuCoreConfig();

  // Sets the instruction set for this core.
  //
  // By default, this is the full instruction set defined by the OZ-3
  // specifications. Custom implementations may choose to override this with a
  // custom instruction set.
  CpuCoreConfig& SetInstructionSet(
      std::shared_ptr<const InstructionSet> instructions);

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the instruction set for this core.
  std::shared_ptr<const InstructionSet> GetInstructions() const {
    return instructions_;
  }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  std::shared_ptr<const InstructionSet> instructions_;
};

}  // namespace oz3

#endif  // OZ3_CORE_CPU_CORE_CONFIG_H_
