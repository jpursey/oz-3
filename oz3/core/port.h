// Copyright 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include <cstdint>
#include <optional>
#include <vector>

#include "absl/types/span.h"
#include "glog/logging.h"
#include "oz3/core/component.h"

namespace oz3 {

//==============================================================================
// Port
//==============================================================================

// This class represents a bidirectional port of an OZ-3 Processor.
class Port final : public Component {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------
  Port() = default;
  ~Port() override = default;

  //----------------------------------------------------------------------------
  // Input/Output interface
  //
  // Input and output are terms relative to the CpuCore and Coprocessor. Input
  // data is read from the port by the CpuCore/Coprocessor, and output data is
  // data that is written to the port by the CpuCore/Coprocessor. Devices are
  // the reverse, where they read from output and set the input.
  //----------------------------------------------------------------------------

  // Returns true if the port has input data.
  bool HasInput(const Lock& lock);

  // Reads the input word if it exist (otherwise it reads zero). This clears the
  // input word, resetting it to zero.
  uint16_t ReadInput(const Lock& lock);

  // Sets the input word to the specified.
  void SetInput(const Lock& lock, uint16_t value);

  // Returns true if the port has output data.
  bool HasOutput(const Lock& lock);

  // Reads the output word if it exist (otherwise it reads zero). This clears
  // the output word, resetting it to zero.
  uint16_t ReadOutput(const Lock& lock);

  // Sets the output word to the specified.
  void SetOutput(const Lock& lock, uint16_t value);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  std::optional<uint16_t> input_;
  std::optional<uint16_t> output_;
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

  PortBank(int num_ports) : ports_(num_ports) {}
  PortBank(const PortBank&) = delete;
  PortBank& operator=(const PortBank&) = delete;
  ~PortBank() = default;

  //----------------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------------

  // Returns all ports as a span.
  absl::Span<Port> GetPorts() { return absl::MakeSpan(ports_); }

  // Returns the number of ports in the bank
  int GetCount() const { return static_cast<int>(ports_.size()); }

  // Returns the port for the specified index.
  //
  // The index must be in the range [0, GetCount()-1].
  Port& GetPort(int index) {
    DCHECK(index >= 0 && index < static_cast<int>(ports_.size()));
    return ports_[index];
  }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  std::vector<Port> ports_;
};

//==============================================================================
// Port inlines
//==============================================================================

inline bool Port::HasInput(const Lock& lock) {
  DCHECK(lock.IsLocked(*this));
  return input_.has_value();
}

inline uint16_t Port::ReadInput(const Lock& lock) {
  DCHECK(lock.IsLocked(*this));
  uint16_t value = input_.value_or(0);
  input_.reset();
  return value;
}

inline void Port::SetInput(const Lock& lock, uint16_t value) {
  DCHECK(lock.IsLocked(*this));
  input_ = value;
}

inline bool Port::HasOutput(const Lock& lock) {
  DCHECK(lock.IsLocked(*this));
  return output_.has_value();
}

inline uint16_t Port::ReadOutput(const Lock& lock) {
  DCHECK(lock.IsLocked(*this));
  uint16_t value = output_.value_or(0);
  output_.reset();
  return value;
}

inline void Port::SetOutput(const Lock& lock, uint16_t value) {
  DCHECK(lock.IsLocked(*this));
  output_ = value;
}

}  // namespace oz3
