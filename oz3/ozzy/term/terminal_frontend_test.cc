// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/terminal_frontend.h"

#include <memory>
#include <optional>
#include <string>
#include <utility>

#include "absl/status/statusor.h"
#include "gb/cli/cli.h"
#include "gb/cli/cli_command_line.h"
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
using ::testing::EndsWith;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::StartsWith;

// Tests of a frontend over a fake console, which the test keeps a pointer to.
class TerminalFrontendTest : public ::testing::Test {
 protected:
  // Creates `frontend_` over `console_`, in `mode`.
  void CreateFrontend(ConsoleMode mode = ConsoleMode::kScreen) {
    auto console = std::make_unique<FakeConsole>(mode);
    console_ = console.get();
    frontend_ = std::make_unique<TerminalFrontend>(std::move(console));
  }

  FakeConsole* console_ = nullptr;  // Owned by `frontend_`.
  std::unique_ptr<TerminalFrontend> frontend_;
};

TEST_F(TerminalFrontendTest, UpdateWithNoOzzyShowsNothing) {
  CreateFrontend();
  frontend_->Update();
  EXPECT_EQ(console_->TakeText(), "");

  // Nor after one is detached.
  std::unique_ptr<Ozzy> ozzy = CreateTestOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->Attach(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Detach(std::nullopt);
  ozzy.reset();
  console_->TakeText();
  frontend_->Update();
  EXPECT_EQ(console_->TakeText(), "");
}

TEST_F(TerminalFrontendTest, ScreenDrawnAfterARun) {
  CreateFrontend();
  std::unique_ptr<Ozzy> ozzy = CreateTestOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->Attach(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Update();
  const std::string text = console_->TakeText();
  EXPECT_EQ(text, DrawConsoleText(ozzy->GetScreen()->GetPicture(), nullptr));
  EXPECT_THAT(text, HasSubstr(kBootVersion));
}

TEST_F(TerminalFrontendTest, DetachShowsTheLastChanges) {
  CreateFrontend();
  std::unique_ptr<Ozzy> ozzy = CreateTestOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->Attach(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Detach(std::nullopt);
  EXPECT_EQ(console_->TakeText(),
            DrawConsoleText(ozzy->GetScreen()->GetPicture(), nullptr));
}

TEST_F(TerminalFrontendTest, DetachWithAnExitCodeShowsTheShutDown) {
  CreateFrontend(ConsoleMode::kLines);
  std::unique_ptr<Ozzy> ozzy = CreateTestOzzy(GetTeletypeSettings(9600));
  ASSERT_NE(ozzy, nullptr);
  frontend_->Attach(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Detach(7);

  // The teletype's last output is shown before the shutdown.
  FakeConsole expected(ConsoleMode::kLines);
  ShowOzzyShutDown(expected, 7);
  const std::string text = console_->TakeText();
  EXPECT_THAT(text, StartsWith(kBootVersion));
  EXPECT_THAT(text, EndsWith(expected.TakeText()));
}

TEST_F(TerminalFrontendTest, NextAttachDrawsTheNewOzzyWhole) {
  CreateFrontend();
  std::unique_ptr<Ozzy> ozzy = CreateTestOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->Attach(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Update();
  console_->TakeText();
  frontend_->Detach(std::nullopt);
  ozzy.reset();

  // The new Ozzy's screen is the same as the last's, so only drawing it whole
  // writes anything.
  ozzy = CreateTestOzzy();
  ASSERT_NE(ozzy, nullptr);
  frontend_->Attach(ozzy.get());
  ozzy->Run(kBootTime);
  frontend_->Update();
  EXPECT_EQ(console_->TakeText(),
            DrawConsoleText(ozzy->GetScreen()->GetPicture(), nullptr));
}

TEST_F(TerminalFrontendTest, KeysAreTheConsoles) {
  CreateFrontend();
  console_->SetKeys(
      {.typed = {kKeyEnter}, .changes = {KeyChange{.key = 'a', .held = true}}});
  const FrontendKeys keys = frontend_->TakeKeys();
  EXPECT_THAT(keys.typed, ElementsAre(kKeyEnter));
  EXPECT_THAT(keys.changes, ElementsAre(KeyChange{.key = 'a', .held = true}));

  // They are taken.
  EXPECT_THAT(frontend_->TakeKeys().typed, IsEmpty());
}

TEST_F(TerminalFrontendTest, QuitIsTheConsoles) {
  CreateFrontend();
  EXPECT_FALSE(frontend_->IsQuitRequested());
  console_->SetQuitRequested();
  EXPECT_TRUE(frontend_->IsQuitRequested());
}

TEST(TerminalFrontendTypeTest, CommandLineIsOzzys) {
  TerminalFrontendType type;
  EXPECT_EQ(type.GetProgramName(), "ozzy");
  EXPECT_THAT(type.GetProgramHelp(), StartsWith("Runs an Ozzy computer"));
  EXPECT_THAT(type.GetFlags(), IsEmpty());
}

// The program the frontend's command line is parsed with, which has no flags.
const gb::CliCommand kProgram = {.name = "ozzy"};

TEST(TerminalFrontendTypeTest, ScreenDrawsNoPixels) {
  absl::StatusOr<gb::CliCommandLine> command_line =
      gb::ParseCommandLine(&kProgram, {});
  ASSERT_TRUE(command_line.ok()) << command_line.status();
  TerminalFrontendType type;
  OzzySettings settings;
  ASSERT_TRUE(settings.screen.has_value());
  settings.screen->draw_pixels = true;
  absl::StatusOr<std::unique_ptr<FrontendFactory>> factory =
      type.Configure(&settings, *command_line);
  ASSERT_TRUE(factory.ok()) << factory.status();
  EXPECT_NE(*factory, nullptr);
  EXPECT_FALSE(settings.screen->draw_pixels);
}

TEST(TerminalFrontendTypeTest, ConfiguresAnOzzyWithNoScreen) {
  absl::StatusOr<gb::CliCommandLine> command_line =
      gb::ParseCommandLine(&kProgram, {});
  ASSERT_TRUE(command_line.ok()) << command_line.status();
  TerminalFrontendType type;
  OzzySettings settings = GetTeletypeSettings(9600);
  ASSERT_FALSE(settings.screen.has_value());
  absl::StatusOr<std::unique_ptr<FrontendFactory>> factory =
      type.Configure(&settings, *command_line);
  ASSERT_TRUE(factory.ok()) << factory.status();
  EXPECT_NE(*factory, nullptr);
  EXPECT_FALSE(settings.screen.has_value());
}

}  // namespace
}  // namespace oz3
