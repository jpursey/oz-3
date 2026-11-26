// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/console_frontend.h"

#include <memory>
#include <string>
#include <string_view>
#include <utility>

#include "absl/status/statusor.h"
#include "absl/time/time.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/app/frontend.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"
#include "oz3/ozzy/machine/ozzy_testing.h"
#include "oz3/ozzy/term/console.h"
#include "oz3/ozzy/term/console_display.h"
#include "oz3/ozzy/term/console_text.h"
#include "oz3/ozzy/term/fake_console.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::HasSubstr;
using ::testing::IsEmpty;

// Long enough for the boot ROM to show its banner.
constexpr absl::Duration kBootTime = absl::Seconds(1);

// The boot ROM's banner's first line.
constexpr std::string_view kVersion = "Ozzy 1.0";

// Tests of a frontend over a fake console, which the test keeps a pointer to.
class ConsoleFrontendTest : public ::testing::Test {
 protected:
  // Creates `frontend_` over `console_`, in `mode`.
  void CreateFrontend(ConsoleMode mode = ConsoleMode::kScreen) {
    auto console = std::make_unique<FakeConsole>();
    console_ = console.get();
    frontend_ = std::make_unique<ConsoleFrontend>(std::move(console), mode);
  }

  // Returns an Ozzy with `settings`, or null after failing the test.
  std::unique_ptr<Ozzy> CreateOzzy(const OzzySettings& settings = {}) {
    absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
        Ozzy::Create(settings, WithPowerCallback());
    EXPECT_TRUE(ozzy.ok()) << ozzy.status();
    return ozzy.ok() ? *std::move(ozzy) : nullptr;
  }

  FakeConsole* console_ = nullptr;  // Owned by `frontend_`.
  std::unique_ptr<ConsoleFrontend> frontend_;
};

TEST_F(ConsoleFrontendTest, NothingShownBeforePowerOn) {
  CreateFrontend();
  frontend_->Update();
  EXPECT_EQ(console_->TakeText(), "");
}

TEST_F(ConsoleFrontendTest, ScreenDrawnAfterARun) {
  CreateFrontend();
  std::unique_ptr<Ozzy> ozzy = CreateOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->PowerOn(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Update();
  const std::string text = console_->TakeText();
  EXPECT_EQ(text, DrawConsoleText(ozzy->GetScreen()->GetPicture(), nullptr));
  EXPECT_THAT(text, HasSubstr(kVersion));
}

TEST_F(ConsoleFrontendTest, PowerOffWithAnExitCodeShowsTheShutDown) {
  CreateFrontend();
  std::unique_ptr<Ozzy> ozzy = CreateOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->PowerOn(ozzy.get());
  frontend_->PowerOff(-3);
  ozzy.reset();

  FakeConsole expected;
  ShowOzzyShutDown(expected, ConsoleMode::kScreen, -3);
  EXPECT_EQ(console_->TakeText(), expected.TakeText());

  // Nothing is shown until the next power on.
  frontend_->Update();
  EXPECT_EQ(console_->TakeText(), "");
}

TEST_F(ConsoleFrontendTest, ShutDownInTheConsolesMode) {
  CreateFrontend(ConsoleMode::kLines);
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(GetTeletypeSettings(9600));
  ASSERT_NE(ozzy, nullptr);
  frontend_->PowerOn(ozzy.get());
  frontend_->PowerOff(7);

  FakeConsole expected;
  ShowOzzyShutDown(expected, ConsoleMode::kLines, 7);
  EXPECT_EQ(console_->TakeText(), expected.TakeText());
}

TEST_F(ConsoleFrontendTest, PowerOffWithoutAnExitCodeShowsNothing) {
  CreateFrontend();
  std::unique_ptr<Ozzy> ozzy = CreateOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->PowerOn(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->PowerOff(std::nullopt);
  ozzy.reset();
  frontend_->Update();
  EXPECT_EQ(console_->TakeText(), "");
}

TEST_F(ConsoleFrontendTest, NextPowerOnDrawsTheNewOzzyWhole) {
  CreateFrontend();
  std::unique_ptr<Ozzy> ozzy = CreateOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->PowerOn(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Update();
  console_->TakeText();
  frontend_->PowerOff(std::nullopt);
  ozzy.reset();

  // The new Ozzy's screen is the same as the last's, so only drawing it whole
  // writes anything.
  ozzy = CreateOzzy();
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
