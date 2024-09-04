// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/component.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

TEST(ComponentTest, LockWhenUnlocked) {
  Component component;
  EXPECT_FALSE(component.IsLocked());
  auto lock = component.Lock();
  EXPECT_TRUE(component.IsLocked());
  ASSERT_NE(lock, nullptr);
  EXPECT_TRUE(lock->IsLocked());
  EXPECT_TRUE(lock->IsLocked(component));
}

TEST(ComponentTest, LockWhenLocked) {
  Component component;

  auto lock1 = component.Lock();
  auto lock2 = component.Lock();
  EXPECT_TRUE(component.IsLocked());
  ASSERT_NE(lock2, nullptr);
  EXPECT_FALSE(lock2->IsLocked());

  lock1.reset();
  EXPECT_TRUE(component.IsLocked());
  EXPECT_TRUE(lock2->IsLocked());

  lock2.reset();
  EXPECT_FALSE(component.IsLocked());
}

TEST(ComponentTest, LockIsComponentSpecific) {
  Component component1;
  Component component2;

  auto lock1 = component1.Lock();
  ASSERT_NE(lock1, nullptr);
  EXPECT_TRUE(lock1->IsLocked(component1));
  EXPECT_FALSE(lock1->IsLocked(component2));
}

TEST(ComponentTest, LockDestroyedBeforeBeingLocked) {
  Component component;

  auto lock1 = component.Lock();
  auto lock2 = component.Lock();
  auto lock3 = component.Lock();
  EXPECT_TRUE(lock1->IsLocked());
  EXPECT_FALSE(lock2->IsLocked());
  EXPECT_FALSE(lock3->IsLocked());

  lock2.reset();
  EXPECT_TRUE(lock1->IsLocked());
  EXPECT_FALSE(lock3->IsLocked());

  lock1.reset();
  EXPECT_TRUE(lock3->IsLocked());
}

}  // namespace
}  // namespace oz3