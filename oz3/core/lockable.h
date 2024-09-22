// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_LOCKABLE_H_
#define OZ3_CORE_LOCKABLE_H_

#include <memory>

#include "gb/container/queue.h"
#include "oz3/core/core_types.h"

namespace oz3 {

namespace internal {
// Internal class used to coordinate between Lockable and Lock.
class LockPtr {
 public:
  explicit LockPtr(Lock* lock = nullptr)
      : ptr_(std::make_shared<Lock*>(lock)) {}

  void Clear() { *ptr_ = nullptr; }
  bool Exists() const { return *ptr_ != nullptr; }
  Lock* operator->() const { return *ptr_; }

 private:
  std::shared_ptr<Lock*> ptr_;
};
}  // namespace internal

//==============================================================================
// Lock
//==============================================================================

// The Lock class is a lock for a lockable that ensures eventual
// exclusive access to a lockable. It may start unlocked, but will
// automatically become locked when the lockable is available.
class Lock final {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------
  Lock() : self_ptr_(this) {}
  Lock(const Lock&) = delete;
  Lock& operator=(const Lock&) = delete;
  ~Lock();

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Returns true if this lock is currently locked.
  bool IsLocked() const;

  // Returns true if this lock is currently locked for the specified lockable.
  bool IsLocked(const Lockable& lockable) const;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------
  friend class Lockable;
  using LockPtr = internal::LockPtr;

  Lockable* lockable_ = nullptr;
  LockPtr self_ptr_;
};

//==============================================================================
// Lockable
//==============================================================================

// The Lockable class is a base class for all OZ-3 objects that can be locked.
//
// Lockables support unique locks that can be used to access the object
// exclusively. Derived classes can enforce this by taking a Lock reference as a
// parameter and verifying it is a lock for them with IsLocked.
class Lockable {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------
  Lockable();
  Lockable(const Lockable&) = delete;
  Lockable& operator=(const Lockable&) = delete;
  virtual ~Lockable();

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Returns true if this lockable is currently locked.
  bool IsLocked() const;

  // Requests a lock for this lockable.
  //
  // If the lockable is not locked, the Lock may be used immediately
  // to access the lock-guarded methods of the Lockable. If the lockable was
  // already locked, the returned lock remains unlocked, and is queued to become
  // locked when the Lockable is available. Requested locks are granted in the
  // order they are requested.
  std::unique_ptr<Lock> RequestLock();

 protected:
  //----------------------------------------------------------------------------
  // Derived class interface
  //----------------------------------------------------------------------------

  // Prevents the lockable from being locked. All calls to Lock will return an
  // unlocked lock. Returns false if the lockable is already locked.
  bool PreventLock();

  // Allows the lockable to be locked again. If there are pending locks, the
  // oldest pending lock will be granted.
  void AllowLock();

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------
  friend class Lock;
  using LockPtr = internal::LockPtr;

  // Called by Lock to unlock the lockable.
  void Unlock();

  bool allow_locks_ = true;
  LockPtr lock_;
  gb::Queue<LockPtr> pending_locks_;
};

//==============================================================================
// Lockable Inlines
//==============================================================================

inline bool Lockable::IsLocked() const { return lock_.Exists(); }

//==============================================================================
// Lock Inlines
//==============================================================================

inline Lock::~Lock() {
  self_ptr_.Clear();
  if (lockable_ != nullptr) {
    lockable_->Unlock();
    lockable_ = nullptr;
  }
}

inline bool Lock::IsLocked() const { return lockable_ != nullptr; }

inline bool Lock::IsLocked(const Lockable& lockable) const {
  return lockable_ == &lockable;
}

}  // namespace oz3

#endif  // OZ3_CORE_LOCKABLE_H_
