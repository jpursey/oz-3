// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/console_frontend.h"

#include <memory>
#include <optional>
#include <string>
#include <utility>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/app/frontend.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_testing.h"
#include "oz3/ozzy/term/console.h"
#include "oz3/ozzy/term/console_display.h"
#include "oz3/ozzy/term/console_text.h"
#include "oz3/ozzy/term/fake_console.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::EndsWith;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::StartsWith;

// Tests of a frontend over a fake console, which the test keeps a pointer to.
class ConsoleFrontendTest : public ::testing::Test {
 protected:
  // Creates `frontend_` over `console_`, in `mode`.
  void CreateFrontend(ConsoleMode mode = ConsoleMode::kScreen) {
    auto console = std::make_unique<FakeConsole>(mode);
    console_ = console.get();
    frontend_ = std::make_unique<ConsoleFrontend>(std::move(console));
  }

  FakeConsole* console_ = nullptr;  // Owned by `frontend_`.
  std::unique_ptr<ConsoleFrontend> frontend_;
};

TEST_F(ConsoleFrontendTest, ScreenDrawnAfterARun) {
  CreateFrontend();
  std::unique_ptr<Ozzy> ozzy = CreateTestOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->PowerOn(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Update();
  const std::string text = console_->TakeText();
  EXPECT_EQ(text, DrawConsoleText(ozzy->GetScreen()->GetPicture(), nullptr));
  EXPECT_THAT(text, HasSubstr(kBootVersion));
}

TEST_F(ConsoleFrontendTest, PowerOffShowsTheLastChanges) {
  CreateFrontend();
  std::unique_ptr<Ozzy> ozzy = CreateTestOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->PowerOn(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->PowerOff(std::nullopt);
  EXPECT_EQ(console_->TakeText(),
            DrawConsoleText(ozzy->GetScreen()->GetPicture(), nullptr));
}

TEST_F(ConsoleFrontendTest, PowerOffWithAnExitCodeShowsTheShutDown) {
  CreateFrontend(ConsoleMode::kLines);
  std::unique_ptr<Ozzy> ozzy = CreateTestOzzy(GetTeletypeSettings(9600));
  ASSERT_NE(ozzy, nullptr);
  frontend_->PowerOn(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->PowerOff(7);

  // The teletype's last output is shown before the shutdown.
  FakeConsole expected(ConsoleMode::kLines);
  ShowOzzyShutDown(expected, 7);
  const std::string text = console_->TakeText();
  EXPECT_THAT(text, StartsWith(kBootVersion));
  EXPECT_THAT(text, EndsWith(expected.TakeText()));
}

TEST_F(ConsoleFrontendTest, NextPowerOnDrawsTheNewOzzyWhole) {
  CreateFrontend();
  std::unique_ptr<Ozzy> ozzy = CreateTestOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->PowerOn(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Update();
  console_->TakeText();
  frontend_->PowerOff(std::nullopt);
  ozzy.reset();

  // The new Ozzy's screen is the same as the last's, so only drawing it whole
  // writes anything.
  ozzy = CreateTestOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->PowerOn(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Update();
  EXPECT_EQ(console_->TakeText(),
            DrawConsoleText(ozzy->GetScreen()->GetPicture(), nullptr));
}

TEST_F(ConsoleFrontendTest, KeysAreTheConsoles) {
  CreateFrontend();
  console_->SetKeys(
      {.typed = {kKeyEnter}, .changes = {KeyChange{.key = 'a', .held = true}}});
  const FrontendKeys keys = frontend_->TakeKeys();
  EXPECT_THAT(keys.typed, ElementsAre(kKeyEnter));
  EXPECT_THAT(keys.changes, ElementsAre(KeyChange{.key = 'a', .held = true}));

  // They are taken.
  EXPECT_THAT(frontend_->TakeKeys().typed, IsEmpty());
}

TEST_F(ConsoleFrontendTest, QuitIsTheConsoles) {
  CreateFrontend();
  EXPECT_FALSE(frontend_->IsQuitRequested());
  console_->SetQuitRequested();
  EXPECT_TRUE(frontend_->IsQuitRequested());
}

}  // namespace
}  // namespace oz3
