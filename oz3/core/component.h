// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_COMPONENT_H_
#define OZ3_CORE_COMPONENT_H_

#include <memory>

#include "oz3/core/core_types.h"
#include "oz3/core/lockable.h"

namespace oz3 {

//==============================================================================
// Component
//==============================================================================

// The Component class is a base class for all components that can be attached
// to a Processor.
//
// Components themselves are Lockables, which allows them to support functions
// that require exclusive locking.
class Component : public Lockable {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------
  Component(const Component&) = delete;
  Component& operator=(const Component&) = delete;
  ~Component() override = default;

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Called by the processor when the component is attached.
  //
  // After the call the component is owned solely by this processor, and the
  // Processor* pointer will remain valid for the life of the component.
  virtual void AttachProcessor(CoreInternal, Processor* processor) {}

 protected:
  Component() = default;
};

}  // namespace oz3

#endif  // OZ3_CORE_COMPONENT_H_
