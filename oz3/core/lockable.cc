// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/lockable.h"

#include <memory>
#include "glog/logging.h"

namespace oz3 {

Lockable::Lockable() : pending_locks_(32) {}

Lockable::~Lockable() {
  if (lock_.Exists()) {
    lock_->lockable_ = nullptr;
  }
}

bool Lockable::PreventLock() {
  if (lock_.Exists()) {
    return false;
  }
  allow_locks_ = false;
  return true;
}

void Lockable::AllowLock() {
  allow_locks_ = true;
  if (!lock_.Exists()) {
    Unlock();
  }
}

std::unique_ptr<Lock> Lockable::RequestLock() {
  auto new_lock = std::make_unique<Lock>();
  if (lock_.Exists() || !allow_locks_) {
    pending_locks_.push(new_lock->self_ptr_);
  } else {
    new_lock->lockable_ = this;
    lock_ = new_lock->self_ptr_;
  }
  return new_lock;
}

void Lockable::Unlock() {
  while (!pending_locks_.empty()) {
    lock_ = std::move(pending_locks_.front());
    pending_locks_.pop();
    if (lock_.Exists()) {
      lock_->lockable_ = this;
      break;
    }
  }
  if (!lock_.Exists()) {
    DCHECK(pending_locks_.empty());
    OnUnlocked();
  }
}

}  // namespace oz3
