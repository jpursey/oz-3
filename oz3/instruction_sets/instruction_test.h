// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_INSTRUCTION_SETS_INSTRUCTION_TEST_H_
#define OZ3_INSTRUCTION_SETS_INSTRUCTION_TEST_H_

#include <cstdint>
#include <utility>
#include <vector>

#include "absl/log/check.h"
#include "oz3/core/base_core_test.h"
#include "oz3/core/port.h"
#include "oz3/instruction_sets/default_instruction_set.h"

namespace oz3 {

class InstructionTest : public BaseCoreTest {
 protected:
  // The size of the values a fake device reads or writes on a port.
  enum class PortSize { kWord, kDword };

  // A fake device that feeds values to a port one at a time: whenever the port
  // is unlocked and its status is clear, it writes the next value and sets the
  // status, as WritePort or WritePort32 do. Update() must be called after
  // every cycle (see CyclesUntilIp).
  class PortFeeder {
   public:
    // `test` must outlive this.
    PortFeeder(InstructionTest* test, int port, PortSize size,
               std::vector<uint32_t> values)
        : test_(test), port_(port), size_(size), values_(std::move(values)) {}

    void Update() {
      if (next_ == static_cast<int>(values_.size())) {
        return;
      }
      const Port& port = test_->GetPort(port_);
      if (port.IsLocked() || port.GetStatus() != 0) {
        return;
      }
      if (size_ == PortSize::kWord) {
        test_->WritePort(port_, static_cast<uint16_t>(values_[next_]));
      } else {
        test_->WritePort32(port_, values_[next_]);
      }
      ++next_;
    }

   private:
    InstructionTest* const test_;
    const int port_;
    const PortSize size_;
    const std::vector<uint32_t> values_;
    int next_ = 0;
  };

  // A fake device that drains values from a port one at a time: whenever the
  // port is unlocked and its status is set, it reads the value and clears the
  // status, as ReadPort or ReadPort32 do, until it has read `count` values.
  // Update() must be called after every cycle (see CyclesUntilIp).
  class PortDrainer {
   public:
    // `test` must outlive this.
    PortDrainer(InstructionTest* test, int port, PortSize size, int count)
        : test_(test), port_(port), size_(size), count_(count) {}

    // Returns the values read so far.
    const std::vector<uint32_t>& GetValues() const { return values_; }

    void Update() {
      if (static_cast<int>(values_.size()) == count_) {
        return;
      }
      const Port& port = test_->GetPort(port_);
      if (port.IsLocked() || port.GetStatus() == 0) {
        return;
      }
      if (size_ == PortSize::kWord) {
        values_.push_back(test_->ReadPort(port_));
      } else {
        values_.push_back(test_->ReadPort32(port_));
      }
    }

   private:
    InstructionTest* const test_;
    const int port_;
    const PortSize size_;
    const int count_;
    std::vector<uint32_t> values_;
  };

  InstructionTest()
      : BaseCoreTest(GetDefaultInstructionSetDef(),
                     GetDefaultInstructionSet()) {}

  // Writes `value` to word 0 of the port and sets the port status, as a device
  // would. The port must not be locked.
  void WritePort(int port, uint16_t value) {
    auto lock = LockPort(port);
    CHECK(lock->IsLocked());
    GetPort(port).StoreWord(*lock, Port::S, value);
  }

  // Writes the low word of `value` to word 0 of the port and the high word to
  // word 1, and sets the port status, as a device would. The port must not be
  // locked.
  void WritePort32(int port_index, uint32_t value) {
    auto lock = LockPort(port_index);
    CHECK(lock->IsLocked());
    Port& port = GetPort(port_index);
    port.StoreWord(*lock, Port::A, value & 0xFFFF);
    port.StoreWord(*lock, Port::S, value >> 16);
  }

  // Reads word 0 of the port and clears the port status, as a device would.
  // The port must not be locked.
  uint16_t ReadPort(int port) {
    auto lock = LockPort(port);
    CHECK(lock->IsLocked());
    uint16_t value = 0;
    GetPort(port).LoadWord(*lock, Port::S, value);
    return value;
  }

  // Reads word 0 of the port as the low word and word 1 as the high word, and
  // clears the port status, as a device would. The port must not be locked.
  uint32_t ReadPort32(int port_index) {
    auto lock = LockPort(port_index);
    CHECK(lock->IsLocked());
    Port& port = GetPort(port_index);
    uint16_t low = 0;
    uint16_t high = 0;
    port.LoadWord(*lock, Port::A, low);
    port.LoadWord(*lock, Port::S, high);
    return low | (static_cast<uint32_t>(high) << 16);
  }
};

}  // namespace oz3

#endif  // OZ3_INSTRUCTION_SETS_INSTRUCTION_TEST_H_
