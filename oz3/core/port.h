// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_PORT_H_
#define OZ3_CORE_PORT_H_

#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "absl/log/check.h"
#include "absl/log/log.h"
#include "oz3/core/core_types.h"
#include "oz3/core/execution_component.h"
#include "oz3/core/lockable.h"

namespace oz3 {

class PortBank;

namespace internal {
// Internal class to support caching of lockables in a PortBank
class PortLockable : public Lockable {
 public:
  ~PortLockable() override = default;

 protected:
  void OnLocked() override;
  void OnUnlocked() override;

 private:
  friend class oz3::PortBank;

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

  // Wires a component to this port, so LoadWord and StoreWord wake it. Called
  // by the Processor as it creates devices. Does nothing if the component is
  // already wired to it.
  void AddComponent(CoreInternal, ExecutionComponent* component);

  // Loads the port value honoring the specified mode, and wakes the components
  // wired to the port, even if nothing is loaded.
  //
  // Returns the port status (zero or 1) prior to the load.
  uint16_t LoadWord(const Lock& lock, uint16_t mode, uint16_t& value);

  // Stores the port value honoring the specified mode, and wakes the
  // components wired to the port, even if nothing is stored.
  //
  // Returns the port status (zero or 1) prior to the store.
  uint16_t StoreWord(const Lock& lock, uint16_t mode, uint16_t value);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------
  friend class oz3::PortBank;

  void WakeComponents() {
    for (ExecutionComponent* component : components_) {
      component->Wake();
    }
  }

  uint16_t address_ = 0;        // Address of the port value (0 or 1).
  uint16_t status_ = 0;         // Status of the port (0 or 1).
  uint16_t value_[2] = {0, 0};  // Value of the port.
  std::unique_ptr<internal::PortLockable> lockable_;
  std::vector<ExecutionComponent*> components_;  // Woken by Load and Store.
};

//==============================================================================
// PortWords
//==============================================================================

// The two words of a port, as a device moves them: both at once, word 0 and
// then word 1. Word 0 is a dword's low word.
class PortWords final {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Both words are 0.
  PortWords() = default;

  // Returns `word` as word 0, with word 1 as 0.
  static PortWords FromWord(uint16_t word) { return PortWords(word, 0); }

  // Returns `word0` and `word1` as words 0 and 1.
  static PortWords FromWords(uint16_t word0, uint16_t word1) {
    return PortWords(word0, word1);
  }

  // Returns `value`'s low word as word 0, and its high word as word 1.
  static PortWords FromDword(uint32_t value) {
    return PortWords(static_cast<uint16_t>(value),
                     static_cast<uint16_t>(value >> 16));
  }

  bool operator==(const PortWords&) const = default;

  // Prints as "{abcdh, 0001h}".
  template <typename Sink>
  friend void AbslStringify(Sink& sink, const PortWords& words) {
    sink.Append(words.ToString());
  }

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns word `index`, which must be 0 or 1.
  uint16_t GetWord(int index = 0) const {
    DCHECK(index == 0 || index == 1);
    return words_[index];
  }

  // Returns both words as a dword.
  uint32_t GetDword() const {
    return words_[0] | static_cast<uint32_t>(words_[1]) << 16;
  }

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Loads both words from `port`, which `lock` holds, starting at the port's
  // address (port word 0 once a lock is granted), and leaves the address where
  // it was. `mode` applies to the pair: with T, neither word is loaded unless
  // the status is set, and S applies after the second word. A is ignored, as
  // the address never changes. Returns the port status prior to the load, as
  // Port::LoadWord does.
  uint16_t Load(Port& port, const Lock& lock, uint16_t mode);

  // Stores both words to `port`, which `lock` holds, as Load loads them: with
  // T, neither word is stored unless the status is clear, S applies after the
  // second word, A is ignored, and the address is left where it was. Returns
  // the port status prior to the store, as Port::StoreWord does.
  uint16_t Store(Port& port, const Lock& lock, uint16_t mode) const;

 private:
  PortWords(uint16_t word0, uint16_t word1) : words_{word0, word1} {}

  std::string ToString() const;

  std::array<uint16_t, 2> words_ = {0, 0};
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

  // Requests a lock on the port at the specified index. The port address is
  // reset to word 0 when the lock is granted, which may be later if the port
  // is already locked.
  //
  // The index must be in the range [0, GetCount()-1].
  std::unique_ptr<Lock> LockPort(int index);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------
  friend class internal::PortLockable;

  void OnPortLocked(int index);
  void OnPortUnlocked(int index);

  std::vector<Port> ports_;
  std::vector<std::unique_ptr<internal::PortLockable>> free_lockables_;
};

}  // namespace oz3

#endif  // OZ3_CORE_PORT_H_
