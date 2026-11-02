// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_EXECUTION_COMPONENT_H_
#define OZ3_CORE_EXECUTION_COMPONENT_H_

#include <algorithm>

#include "oz3/core/component.h"
#include "oz3/core/core_types.h"

namespace oz3 {

//==============================================================================
// ExecutionComponent
//==============================================================================

// An ExecutionComponent is a Component that executes over time.
//
// Time is measured in processor cycles, which are advanced by the Processor,
// and passed to each ExecutionComponent to do their simulated updates.
//
// A component with nothing to do can end an Execute() asleep, and isn't run
// again until it is woken: by Wake(), from anywhere, or by the deadline it gave
// Sleep(). A woken component runs at the cycle after the wake, whoever woke it.
// While it sleeps, its cycles stay where it fell asleep, and waking catches
// them up to the processor's, so they never go backward.
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

  // Returns the cycle the component runs at next: GetCycles(), unless it is
  // asleep, when it is the cycle it wakes at (kSleepForever if nothing has
  // woken it, and it gave no deadline).
  Cycles GetDueCycle() const { return due_cycle_; }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Wakes the component if it is asleep, so it runs at the next cycle. Safe to
  // call from anywhere. Does nothing if it is awake, or not attached to a
  // processor.
  void Wake() {
    // The processor's next cycle is the one at its count, unless the component
    // fell asleep later, or is already due to wake sooner. An awake component's
    // due cycle is its cycles, so this leaves it as it is.
    if (clock_ != nullptr) {
      due_cycle_ = std::max(cycles_, std::min(due_cycle_, *clock_));
    }
  }

  // Called by the Processor when the component is attached, with the
  // processor's cycle count, which Wake() needs.
  void AttachClock(CoreInternal, const Cycles* processor_cycles) {
    clock_ = processor_cycles;
  }

  // Called by the Processor once it has reached GetDueCycle(): catches a woken
  // component's cycles up (an awake one's already are), then calls Execute().
  void Step(CoreInternal) {
    cycles_ = due_cycle_;
    Execute();
  }

 protected:
  //---------------------------------------------------------------------------
  // Derived class interface
  //---------------------------------------------------------------------------

  // Allows the component to continue execution for a number of cycles
  // appropriate to the component.
  //
  // This is called by Step(), once the Processor has reached the cycle time
  // returned by GetDueCycle(), with GetCycles() caught up to it. The component
  // must execute at least one cycle, or sleep.
  //
  // There are several factors that may affect how many cycles are executed:
  // - If the component is waiting on a component lock, it should generally
  //   execute one cycle, as the lock may be released after that cycle.
  // - If the component is waiting on an external event, it should generally
  //   execute one cycle, so it can respond to the event (if it occurs) at the
  //   correct cycle time, or sleep until whatever the event comes from wakes
  //   it.
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

  // Advances the component by the specified number of cycles.
  void AdvanceCycles(Cycles cycles) {
    cycles_ += cycles;
    due_cycle_ = cycles_;
  }

  // Ends this Execute() asleep: the component runs again only once woken, or
  // after `max_cycles` (which must be at least one) if it is given. Called last
  // in Execute(), after any AdvanceCycles().
  void Sleep(Cycles max_cycles = kSleepForever) {
    due_cycle_ = max_cycles < kSleepForever - cycles_ ? cycles_ + max_cycles
                                                      : kSleepForever;
  }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  Cycles cycles_ = 0;  // Cycles executed.

  // When to run next: cycles_ while awake, and later while asleep, as a sleep
  // is at least one cycle.
  Cycles due_cycle_ = 0;

  const Cycles* clock_ = nullptr;  // The processor's cycles, once attached.
};

}  // namespace oz3

#endif  // OZ3_CORE_EXECUTION_COMPONENT_H_
