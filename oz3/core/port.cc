// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/port.h"

#include "absl/memory/memory.h"
#include "oz3/core/core_types.h"

namespace oz3 {

namespace internal {

void PortLockable::OnUnlocked() { bank_->OnPortUnlocked(port_index_); }

}  // namespace internal

uint16_t Port::LoadWord(const Lock& lock, uint16_t mode, uint16_t& value) {
  DCHECK(lock.IsLocked(*lockable_));
  const uint16_t status = status_;
  if (mode & T & (~status & 1)) {
    return 0;
  }
  value = value_[address_];
  if (mode & S) {
    status_ = 0;
  }
  if (mode & A) {
    address_ = 1 - address_;
  }
  return status;
}

uint16_t Port::StoreWord(const Lock& lock, uint16_t mode, uint16_t value) {
  DCHECK(lock.IsLocked(*lockable_));
  const uint16_t status = status_;
  if (mode & T & status) {
    return 1;
  }
  value_[address_] = value;
  if (mode & S) {
    status_ = 1;
  }
  if (mode & A) {
    address_ = 1 - address_;
  }
  return status;
}

PortBank::PortBank(int num_ports) : ports_(num_ports) {
  DCHECK(num_ports >= 0 && num_ports <= kMaxPorts);
}

std::unique_ptr<Lock> PortBank::LockPort(int index) {
  DCHECK(index >= 0 && index < GetCount());
  Port& port = ports_[index];
  if (port.lockable_ == nullptr) {
    if (!free_lockables_.empty()) {
      port.lockable_ = std::move(free_lockables_.back());
      port.lockable_->port_index_ = index;
      free_lockables_.pop_back();
    } else {
      port.lockable_ =
          absl::WrapUnique(new internal::PortLockable(this, index));
    }
  }
  port.address_ = 0;
  return port.lockable_->RequestLock();
}

void PortBank::OnPortUnlocked(int index) {
  DCHECK(index >= 0 && index < GetCount());
  Port& port = ports_[index];
  free_lockables_.push_back(std::move(port.lockable_));
  port.lockable_ = nullptr;
}

}  // namespace oz3