// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/component.h"
#include "oz3/core/types.h"

namespace oz3 {

//==============================================================================
// ExecutionComponent
//==============================================================================

// An ExecutionComponent is a Component that executes over time.
//
// Time is measured in processor cycles, which are advanced by the Processor,
// and passed to each ExecutionComponent to do their simulated updates.
class ExecutionComponent : public Component {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  ExecutionComponent() = default;
  ExecutionComponent(const ExecutionComponent&) = delete;
  ExecutionComponent& operator=(const ExecutionComponent&) = delete;
  ~ExecutionComponent() override = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the number of cycles that have been executed by this component.
  Cycles GetCycles() const { return cycles_; }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Allows the component to continue execution for a number of cycles
  // appropriate to the component.
  //
  // This is called by the Processor once it has reached the cycle time returned
  // by GetCycles(). The component must execute at least one cycle.
  //
  // There are several factors that may affect how many cycles are executed:
  // - If the component is waiting on a component lock, it should generally
  //   execute one cycle, as the lock may be released after that cycle.
  // - If the component is waiting on an external event, it should generally
  //   execute one cycle, so it can respond to the event (if it occurs) at the
  //   correct cycle time.
  // - If the component has any externally visible side effects to its
  //   execution, either they must logically occur immediately, or execution
  //   should pause immediately before those side effects have taken place. This
  //   ensures other components don't see the side effects until the appropriate
  //   time.
  // - The component should keep non-trivial real execution to a minimum. This
  //   ensures the Processor can run in a timely manner. Usually there is a
  //   natural breakpoint (completion of a single operation). Inifite loops are
  //   not allowed.
  virtual void Execute() = 0;

 protected:
  //---------------------------------------------------------------------------
  // Derived class interface
  //---------------------------------------------------------------------------

  // Advances the component by the specified number of cycles.
  void AdvanceCycles(Cycles cycles) { cycles_ += cycles; }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  Cycles cycles_ = 0;
};

}  // namespace oz3
