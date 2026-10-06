// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_CORE_INSTRUCTION_TEST_H_
#define OZ3_CORE_INSTRUCTION_TEST_H_

#include "absl/log/check.h"
#include "oz3/core/base_core_test.h"
#include "oz3/core/port.h"

namespace oz3 {

class InstructionTest : public BaseCoreTest {
 protected:
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
};

}  // namespace oz3

#endif  // OZ3_CORE_INSTRUCTION_TEST_H_
