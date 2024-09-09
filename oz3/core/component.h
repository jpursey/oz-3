// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_COMPONENT_H_
#define OZ3_CORE_COMPONENT_H_

#include <memory>

#include "gb/container/queue.h"
#include "oz3/core/core_types.h"

namespace oz3 {

namespace internal {
// Internal class used to coordinate between Component and ComponentLock.
class ComponentLockPtr {
 public:
  explicit ComponentLockPtr(ComponentLock* lock = nullptr)
      : ptr_(std::make_shared<ComponentLock*>(lock)) {}

  void Clear() { *ptr_ = nullptr; }
  bool Exists() const { return *ptr_ != nullptr; }
  ComponentLock* operator->() const { return *ptr_; }

 private:
  std::shared_ptr<ComponentLock*> ptr_;
};
}  // namespace internal

//==============================================================================
// ComponentLock
//==============================================================================

// The ComponentLock class is a lock for a component that ensures eventual
// exclusive access to a component. It may start unlocked, but will
// automatically become locked when the component is available.
class ComponentLock final {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------
  ComponentLock() : self_ptr_(this) {}
  ComponentLock(const ComponentLock&) = delete;
  ComponentLock& operator=(const ComponentLock&) = delete;
  ~ComponentLock();

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Returns true if this lock is currently locked.
  bool IsLocked() const;

  // Returns true if this lock is currently locked for the specified component.
  bool IsLocked(const Component& component) const;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------
  friend class Component;
  using LockPtr = internal::ComponentLockPtr;

  Component* component_ = nullptr;
  LockPtr self_ptr_;
};

//==============================================================================
// Component
//==============================================================================

// The Component class is a base class for all components that can be attached
// to a Processor.
//
// Components support unique locks that can be used to access the component
// exclusively. Derived classes can enforce this by taking a ComponentLock
// reference as a parameter and verifying it is a lock for them with IsLocked.
class Component {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------
  Component();
  Component(const Component&) = delete;
  Component& operator=(const Component&) = delete;
  virtual ~Component();

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Called by the processor when the component is attached.
  //
  // After the call the component is owned solely by this processor, and the
  // Processor* pointer will remain valid for the life of the component.
  virtual void AttachProcessor(CoreInternal, Processor* processor) {}

  // Returns true if this component is currently locked.
  bool IsLocked() const;

  // Requests a lock for this component.
  //
  // If the component is not locked, the ComponentLock may be used immediately
  // to access the lock-guarded methods of the Component. If the component was
  // already locked, the returned lock remains unlocked, and is queued to become
  // locked when the Component is available. Requested locks are granted in the
  // order they are requested.
  std::unique_ptr<ComponentLock> Lock();

 protected:
  //----------------------------------------------------------------------------
  // Derived class interface
  //----------------------------------------------------------------------------

  // Prevents the component from being locked. All calls to Lock will return an
  // unlocked lock. Returns false if the component is already locked.
  bool PreventLock();

  // Allows the component to be locked again. If there are pending locks, the
  // oldest pending lock will be granted.
  void AllowLock();

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------
  friend class ComponentLock;
  using LockPtr = internal::ComponentLockPtr;

  // Called by ComponentLock to unlock the component.
  void Unlock();

  bool allow_locks_ = true;
  LockPtr lock_;
  gb::Queue<LockPtr> pending_locks_;
};

//==============================================================================
// Component Inlines
//==============================================================================

inline bool Component::IsLocked() const { return lock_.Exists(); }

//==============================================================================
// ComponentLock Inlines
//==============================================================================

inline ComponentLock::~ComponentLock() {
  self_ptr_.Clear();
  if (component_ != nullptr) {
    component_->Unlock();
    component_ = nullptr;
  }
}

inline bool ComponentLock::IsLocked() const { return component_ != nullptr; }

inline bool ComponentLock::IsLocked(const Component& component) const {
  return component_ == &component;
}

}  // namespace oz3

#endif  // OZ3_CORE_COMPONENT_H_
