// Copyright (c) 2024 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/lockable.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace oz3 {
namespace {

TEST(LockableTest, LockWhenUnlocked) {
  Lockable lockable;
  EXPECT_FALSE(lockable.IsLocked());
  auto lock = lockable.RequestLock();
  EXPECT_TRUE(lockable.IsLocked());
  ASSERT_NE(lock, nullptr);
  EXPECT_TRUE(lock->IsLocked());
  EXPECT_TRUE(lock->IsLocked(lockable));
}

TEST(LockableTest, LockWhenLocked) {
  Lockable lockable;

  auto lock1 = lockable.RequestLock();
  auto lock2 = lockable.RequestLock();
  auto lock3 = lockable.RequestLock();
  EXPECT_TRUE(lockable.IsLocked());
  ASSERT_NE(lock2, nullptr);
  EXPECT_FALSE(lock2->IsLocked());
  ASSERT_NE(lock3, nullptr);
  EXPECT_FALSE(lock3->IsLocked());

  lock1.reset();
  EXPECT_TRUE(lockable.IsLocked());
  EXPECT_TRUE(lock2->IsLocked());
  EXPECT_FALSE(lock3->IsLocked());

  lock2.reset();
  EXPECT_TRUE(lockable.IsLocked());
  EXPECT_TRUE(lock3->IsLocked());

  lock3.reset();
  EXPECT_FALSE(lockable.IsLocked());
}

TEST(LockableTest, LockIsLockableSpecific) {
  Lockable lockable1;
  Lockable lockable2;

  auto lock1 = lockable1.RequestLock();
  ASSERT_NE(lock1, nullptr);
  EXPECT_TRUE(lock1->IsLocked(lockable1));
  EXPECT_FALSE(lock1->IsLocked(lockable2));
}

TEST(LockableTest, LockDestroyedBeforeBeingLocked) {
  Lockable lockable;

  auto lock1 = lockable.RequestLock();
  auto lock2 = lockable.RequestLock();
  auto lock3 = lockable.RequestLock();
  EXPECT_TRUE(lock1->IsLocked());
  EXPECT_FALSE(lock2->IsLocked());
  EXPECT_FALSE(lock3->IsLocked());

  lock2.reset();
  EXPECT_TRUE(lock1->IsLocked());
  EXPECT_FALSE(lock3->IsLocked());

  lock1.reset();
  EXPECT_TRUE(lock3->IsLocked());
}

class DerivedLockable : public Lockable {
 public:
  DerivedLockable() = default;

  // Make protected methods public for testing.
  using Lockable::AllowLock;
  using Lockable::PreventLock;

  int GetUnlockedCount() const { return unlocked_count_; }

 protected:
  void OnUnlocked() override { ++unlocked_count_; }

 private:
  int unlocked_count_ = 0;
};

TEST(LockableTest, PreventLock) {
  DerivedLockable lockable;
  EXPECT_TRUE(lockable.PreventLock());
  auto lock = lockable.RequestLock();
  EXPECT_FALSE(lock->IsLocked());
}

TEST(LockableTest, PreventLockFailsIfLockExists) {
  DerivedLockable lockable;
  auto lock = lockable.RequestLock();
  EXPECT_FALSE(lockable.PreventLock());
}

TEST(LockableTest, AllowLockLocksPendingLock) {
  DerivedLockable lockable;
  EXPECT_TRUE(lockable.PreventLock());
  auto lock1 = lockable.RequestLock();
  auto lock2 = lockable.RequestLock();
  EXPECT_FALSE(lock1->IsLocked());
  EXPECT_FALSE(lock2->IsLocked());
  lockable.AllowLock();
  EXPECT_TRUE(lock1->IsLocked());
  EXPECT_FALSE(lock2->IsLocked());
}

TEST(LockableTest, OnUnlockedCalled) {
  DerivedLockable lockable;
  auto lock = lockable.RequestLock();
  EXPECT_TRUE(lock->IsLocked());
  lock.reset();
  EXPECT_EQ(lockable.GetUnlockedCount(), 1);
}

TEST(LockableTest, OnUnlockedNotCalledIfPendingLocks) {
  DerivedLockable lockable;
  auto lock1 = lockable.RequestLock();
  auto lock2 = lockable.RequestLock();
  EXPECT_TRUE(lock1->IsLocked());
  EXPECT_FALSE(lock2->IsLocked());
  lock1.reset();
  EXPECT_EQ(lockable.GetUnlockedCount(), 0);
  lock2.reset();
  EXPECT_EQ(lockable.GetUnlockedCount(), 1);
}

TEST(LockableTest, OnUnlockedCalledIfPendingLockResetFirst) {
  DerivedLockable lockable;
  auto lock1 = lockable.RequestLock();
  auto lock2 = lockable.RequestLock();
  EXPECT_TRUE(lock1->IsLocked());
  EXPECT_FALSE(lock2->IsLocked());
  lock2.reset();
  EXPECT_EQ(lockable.GetUnlockedCount(), 0);
  lock1.reset();
  EXPECT_EQ(lockable.GetUnlockedCount(), 1);
}

TEST(LockableTest, OnUnlockedCalledMultipleTimes) {
  DerivedLockable lockable;
  auto lock1 = lockable.RequestLock();
  EXPECT_TRUE(lock1->IsLocked());
  lock1.reset();
  EXPECT_EQ(lockable.GetUnlockedCount(), 1);
  auto lock2 = lockable.RequestLock();
  EXPECT_TRUE(lock2->IsLocked());
  lock2.reset();
  EXPECT_EQ(lockable.GetUnlockedCount(), 2);
}

}  // namespace
}  // namespace oz3