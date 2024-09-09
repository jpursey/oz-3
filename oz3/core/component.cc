// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/component.h"

#include <memory>

namespace oz3 {

Component::Component() : pending_locks_(32) {}

Component::~Component() {
  if (lock_.Exists()) {
    lock_->component_ = nullptr;
  }
}

bool Component::PreventLock() {
  if (lock_.Exists()) {
    return false;
  }
  allow_locks_ = false;
  return true;
}

void Component::AllowLock() {
  allow_locks_ = true;
  if (!lock_.Exists()) {
    Unlock();
  }
}

std::unique_ptr<ComponentLock> Component::Lock() {
  auto new_lock = std::make_unique<ComponentLock>();
  if (lock_.Exists() || !allow_locks_) {
    pending_locks_.push(new_lock->self_ptr_);
  } else {
    new_lock->component_ = this;
    lock_ = new_lock->self_ptr_;
  }
  return new_lock;
}

void Component::Unlock() {
  while (!pending_locks_.empty()) {
    lock_ = std::move(pending_locks_.front());
    pending_locks_.pop();
    if (lock_.Exists()) {
      lock_->component_ = this;
      break;
    }
  }
}

}  // namespace oz3
