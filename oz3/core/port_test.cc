// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/port.h"

#include "gtest/gtest.h"
#include "oz3/core/core_types.h"

namespace oz3 {
namespace {

TEST(PortTest, PortBankNoPorts) {
  PortBank bank(0);
  EXPECT_EQ(bank.GetCount(), 0);
}

TEST(PortTest, PortBankWithMaxPorts) {
  PortBank bank(kMaxPorts);
  EXPECT_EQ(bank.GetCount(), kMaxPorts);

  Port& port0 = bank.GetPort(0);
  EXPECT_EQ(port0.GetAddress(), 0);
  EXPECT_EQ(port0.GetStatus(), 0);
  EXPECT_EQ(port0.GetValue(0), 0);
  EXPECT_EQ(port0.GetValue(1), 0);

  Port& portMax = bank.GetPort(kMaxPorts - 1);
  EXPECT_EQ(portMax.GetAddress(), 0);
  EXPECT_EQ(portMax.GetStatus(), 0);
  EXPECT_EQ(portMax.GetValue(0), 0);
  EXPECT_EQ(portMax.GetValue(1), 0);
}

TEST(PortTest, StoreWordMode0) {
  PortBank bank(1);
  auto lock = bank.LockPort(0);
  Port& port = bank.GetPort(0);

  EXPECT_EQ(port.StoreWord(*lock, 0, 1), 0);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, 0, 0xFFFF), 0);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 0xFFFF);
  EXPECT_EQ(port.GetValue(1), 0);

  lock.reset();
}

TEST(PortTest, StoreWordModeS) {
  PortBank bank(1);
  auto lock = bank.LockPort(0);
  Port& port = bank.GetPort(0);

  EXPECT_EQ(port.StoreWord(*lock, Port::S, 1), 0);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, Port::S, 0xFFFF), 1);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 0xFFFF);
  EXPECT_EQ(port.GetValue(1), 0);

  lock.reset();
}

TEST(PortTest, StoreWordModeA) {
  PortBank bank(1);
  auto lock = bank.LockPort(0);
  Port& port = bank.GetPort(0);

  EXPECT_EQ(port.StoreWord(*lock, Port::A, 1), 0);
  EXPECT_EQ(port.GetAddress(), 1);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, Port::A, 0xFFFF), 0);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0xFFFF);

  lock.reset();
}

TEST(PortTest, StoreWordModeT) {
  PortBank bank(1);
  auto lock = bank.LockPort(0);
  Port& port = bank.GetPort(0);

  EXPECT_EQ(port.StoreWord(*lock, Port::T | Port::S, 1), 0);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, Port::T, 0xFFFF), 1);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  lock.reset();
}

TEST(PortTest, StoreWordModeTSA) {
  PortBank bank(1);
  auto lock = bank.LockPort(0);
  Port& port = bank.GetPort(0);

  EXPECT_EQ(port.StoreWord(*lock, Port::T | Port::S | Port::A, 1), 0);
  EXPECT_EQ(port.GetAddress(), 1);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, Port::T | Port::S | Port::A, 0xFFFF), 1);
  EXPECT_EQ(port.GetAddress(), 1);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, Port::A, 0xFFFF), 1);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0xFFFF);

  lock.reset();
}

TEST(PortTest, LoadWordMode0) {
  PortBank bank(1);
  auto lock = bank.LockPort(0);
  Port& port = bank.GetPort(0);
  uint16_t value = 0;

  EXPECT_EQ(port.StoreWord(*lock, 0, 1), 0);
  EXPECT_EQ(port.LoadWord(*lock, 0, value), 0);
  EXPECT_EQ(value, 1);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, Port::S, 0xFFFF), 0);
  EXPECT_EQ(port.LoadWord(*lock, 0, value), 1);
  EXPECT_EQ(value, 0xFFFF);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 0xFFFF);
  EXPECT_EQ(port.GetValue(1), 0);

  lock.reset();
}

TEST(PortTest, LoadWordModeS) {
  PortBank bank(1);
  auto lock = bank.LockPort(0);
  Port& port = bank.GetPort(0);
  uint16_t value = 0;

  EXPECT_EQ(port.StoreWord(*lock, 0, 1), 0);
  EXPECT_EQ(port.LoadWord(*lock, Port::S, value), 0);
  EXPECT_EQ(value, 1);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, Port::S, 0xFFFF), 0);
  EXPECT_EQ(port.LoadWord(*lock, Port::S, value), 1);
  EXPECT_EQ(value, 0xFFFF);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 0xFFFF);
  EXPECT_EQ(port.GetValue(1), 0);

  lock.reset();
}

TEST(PortTest, LoadWordModeA) {
  PortBank bank(1);
  auto lock = bank.LockPort(0);
  Port& port = bank.GetPort(0);
  uint16_t value = 0;

  port.StoreWord(*lock, Port::A | Port::S, 1);
  port.StoreWord(*lock, Port::A, 0xFFFF);
  EXPECT_EQ(port.GetStatus(), 1);

  EXPECT_EQ(port.LoadWord(*lock, Port::A, value), 1);
  EXPECT_EQ(value, 1);
  EXPECT_EQ(port.GetAddress(), 1);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0xFFFF);

  EXPECT_EQ(port.LoadWord(*lock, Port::A, value), 1);
  EXPECT_EQ(value, 0xFFFF);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0xFFFF);

  lock.reset();
}

TEST(PortTest, LoadWordModeT) {
  PortBank bank(1);
  auto lock = bank.LockPort(0);
  Port& port = bank.GetPort(0);
  uint16_t value = 0;

  EXPECT_EQ(port.StoreWord(*lock, 0, 1), 0);
  EXPECT_EQ(port.LoadWord(*lock, Port::T, value), 0);
  EXPECT_EQ(value, 0);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, Port::S, 0xFFFF), 0);
  EXPECT_EQ(port.LoadWord(*lock, Port::T, value), 1);
  EXPECT_EQ(value, 0xFFFF);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 1);
  EXPECT_EQ(port.GetValue(0), 0XFFFF);
  EXPECT_EQ(port.GetValue(1), 0);

  lock.reset();
}

TEST(PortTest, LoadWordModeTSA) {
  PortBank bank(1);
  auto lock = bank.LockPort(0);
  Port& port = bank.GetPort(0);
  uint16_t value = 0;

  EXPECT_EQ(port.StoreWord(*lock, 0, 1), 0);
  EXPECT_EQ(port.LoadWord(*lock, Port::T | Port::S | Port::A, value), 0);
  EXPECT_EQ(value, 0);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 1);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, Port::S, 0xFFFF), 0);
  EXPECT_EQ(port.LoadWord(*lock, Port::T | Port::S | Port::A, value), 1);
  EXPECT_EQ(value, 0xFFFF);
  EXPECT_EQ(port.GetAddress(), 1);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 0xFFFF);
  EXPECT_EQ(port.GetValue(1), 0);

  EXPECT_EQ(port.StoreWord(*lock, Port::S, 42), 0);
  EXPECT_EQ(port.LoadWord(*lock, Port::T | Port::S | Port::A, value), 1);
  EXPECT_EQ(value, 42);
  EXPECT_EQ(port.GetAddress(), 0);
  EXPECT_EQ(port.GetStatus(), 0);
  EXPECT_EQ(port.GetValue(0), 0xFFFF);
  EXPECT_EQ(port.GetValue(1), 42);

  lock.reset();
}

TEST(PortTest, PortsLockIndependently) {
  PortBank bank(2);

  auto lock1 = bank.LockPort(0);
  EXPECT_TRUE(lock1->IsLocked());
  auto lock2 = bank.LockPort(1);
  EXPECT_TRUE(lock2->IsLocked());

  Port& port1 = bank.GetPort(0);
  EXPECT_EQ(port1.StoreWord(*lock1, 0, 1), 0);
  EXPECT_EQ(port1.GetValue(0), 1);

  Port& port2 = bank.GetPort(1);
  EXPECT_EQ(port2.StoreWord(*lock2, 0, 2), 0);
  EXPECT_EQ(port2.GetValue(0), 2);

  lock1.reset();
  lock2.reset();
}

TEST(PortTest, PortHasExclusiveLock) {
  PortBank bank(1);
  Port& port = bank.GetPort(0);

  auto lock1 = bank.LockPort(0);
  EXPECT_TRUE(lock1->IsLocked());
  auto lock2 = bank.LockPort(0);
  EXPECT_FALSE(lock2->IsLocked());

  EXPECT_EQ(port.StoreWord(*lock1, 0, 1), 0);
  EXPECT_EQ(port.GetValue(0), 1);
  lock1.reset();

  EXPECT_TRUE(lock2->IsLocked());
  EXPECT_EQ(port.StoreWord(*lock2, 0, 2), 0);
  EXPECT_EQ(port.GetValue(0), 2);
  lock2.reset();
}

TEST(PortTest, SequentialPortLocking) {
  PortBank bank(1);
  Port& port = bank.GetPort(0);

  auto lock1 = bank.LockPort(0);
  EXPECT_TRUE(lock1->IsLocked());
  EXPECT_EQ(port.StoreWord(*lock1, 0, 1), 0);
  EXPECT_EQ(port.GetValue(0), 1);
  lock1.reset();

  auto lock2 = bank.LockPort(0);
  EXPECT_TRUE(lock2->IsLocked());
  EXPECT_EQ(port.StoreWord(*lock2, 0, 2), 0);
  EXPECT_EQ(port.GetValue(0), 2);
  lock2.reset();

  auto lock3 = bank.LockPort(0);
  EXPECT_TRUE(lock3->IsLocked());
  EXPECT_EQ(port.StoreWord(*lock3, 0, 3), 0);
  EXPECT_EQ(port.GetValue(0), 3);
  lock3.reset();
}

}  // namespace
}  // namespace oz3