// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdint>
#include <optional>
#include <vector>

#include "glog/logging.h"
#include "oz3/core/core_types.h"
#include "oz3/core/lockable.h"

namespace oz3 {

namespace internal {
// Internal class to support caching of lockables in a PortBank
class PortLockable : public Lockable {
 public:
  ~PortLockable() override = default;

 protected:
  void OnUnlocked() override;

 private:
  friend class PortBank;

  PortLockable(PortBank* bank, int port_index)
      : bank_(bank), port_index_(port_index) {}

  PortBank* bank_;
  int port_index_;
};
}  // namespace internal

//==============================================================================
// Port
//==============================================================================

// This class represents a bidirectional port of an OZ-3 Processor.
class Port final {
 public:
  //----------------------------------------------------------------------------
  // Constants
  //----------------------------------------------------------------------------

  // Mode flags for Load and Store operations.
  static constexpr uint16_t T = 1;  // Load if status set; Store if status clear
  static constexpr uint16_t S = 2;  // Load clears status; Store sets status
  static constexpr uint16_t A = 4;  // Advance port address

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  Port() = default;
  Port(const Port&) = delete;
  Port& operator=(const Port&) = delete;
  ~Port() = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns true if the port is locked.
  bool IsLocked() const {
    return lockable_ != nullptr && lockable_->IsLocked();
  }

  // Direct access to the port address, status, and values. This does not honor
  // the lock, so if the caller does not own a locked Lock for this port, these
  // methods can return results that are not yet ready (by whoever has the
  // lock).
  uint16_t GetAddress() const { return address_; }
  uint16_t GetStatus() const { return status_; }
  uint16_t GetValue(int address) const {
    DCHECK(address == 0 || address == 1);
    return value_[address];
  }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Loads thhe port value honoring the specified mode.
  //
  // Returns the port status (zero or 1) prior to the load.
  uint16_t LoadWord(const Lock& lock, uint16_t mode, uint16_t& value);

  // Stores the port value honoring the specified mode.
  //
  // Returns the port status (zero or 1) prior to the store.
  uint16_t StoreWord(const Lock& lock, uint16_t mode, uint16_t value);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------
  friend class PortBank;

  uint16_t address_ = 0;        // Address of the port value (0 or 1).
  uint16_t status_ = 0;         // Status of the port (0 or 1).
  uint16_t value_[2] = {0, 0};  // Value of the port.
  std::unique_ptr<internal::PortLockable> lockable_;
};

//==============================================================================
// PortBank
//==============================================================================

// A port bank is the set of ports held by a Processor.
//
// It is provided to CpuCores who have full access to all ports. Devices are
// mapped to individual ports.
class PortBank final {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Constructs a PortBank with the specified number of ports.
  //
  // The number of ports must be in the range [0, kMaxPorts).
  explicit PortBank(int num_ports);

  PortBank(const PortBank&) = delete;
  PortBank& operator=(const PortBank&) = delete;
  ~PortBank() = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns the number of ports in the bank
  int GetCount() const { return static_cast<int>(ports_.size()); }

  // Returns the port for the specified index.
  //
  // The index must be in the range [0, GetCount()-1].
  Port& GetPort(int index) {
    DCHECK(index >= 0 && index < static_cast<int>(ports_.size()));
    return ports_[index];
  }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Locks the port at the specified index, and resets the port address.
  //
  // The index must be in the range [0, GetCount()-1].
  std::unique_ptr<Lock> LockPort(int index);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------
  friend class internal::PortLockable;

  void OnPortUnlocked(int index);

  std::vector<Port> ports_;
  std::vector<std::unique_ptr<internal::PortLockable>> free_lockables_;
};

}  // namespace oz3
