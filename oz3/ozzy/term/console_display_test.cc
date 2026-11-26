// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/console_display.h"

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/time/time.h"
#include "gb/base/context.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/screen.h"
#include "oz3/ozzy/machine/ozzy.h"
#include "oz3/ozzy/machine/ozzy_settings.h"
#include "oz3/ozzy/machine/ozzy_testing.h"
#include "oz3/ozzy/term/console.h"
#include "oz3/ozzy/term/console_text.h"
#include "oz3/ozzy/term/fake_console.h"

namespace oz3 {
namespace {

using ::testing::HasSubstr;
using ::testing::Not;

// Long enough for the boot ROM to show its banner.
constexpr absl::Duration kBootTime = absl::Seconds(1);

// The boot ROM's banner's first line.
constexpr std::string_view kVersion = "Ozzy 1.0";

// What the boot ROM shows when Enter is pressed in an Ozzy with no drives.
constexpr std::string_view kNoDrive = "ERROR: No drive A:";

// Long enough for the boot ROM to answer Enter, and the screen to refresh.
constexpr absl::Duration kKeyTime = absl::Milliseconds(100);

std::unique_ptr<Ozzy> CreateOzzy(const OzzySettings& settings) {
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy =
      Ozzy::Create(settings, WithPowerCallback());
  EXPECT_TRUE(ozzy.ok()) << ozzy.status();
  if (!ozzy.ok()) {
    return nullptr;
  }
  return std::move(*ozzy);
}

OzzySettings TeletypeOnlySettings() {
  OzzySettings settings;
  settings.screen.reset();
  settings.teletype = OzzyTeletypeSettings();
  return settings;
}

TEST(ConsoleDisplayTest, ModeFollowsTheScreen) {
  EXPECT_EQ(GetOzzyConsoleMode(OzzySettings()), ConsoleMode::kScreen);
  EXPECT_EQ(GetOzzyConsoleMode(TeletypeOnlySettings()), ConsoleMode::kLines);
}

TEST(ConsoleDisplayTest, NothingBeforeTheFirstRefresh) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  ConsoleDisplay display(*ozzy, console);
  display.Update();
  EXPECT_EQ(console.TakeText(), "");
}

TEST(ConsoleDisplayTest, ScreenDrawnWholeFirst) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  ConsoleDisplay display(*ozzy, console);
  ozzy->Run(kBootTime);
  display.Update();
  const std::string text = console.TakeText();
  EXPECT_EQ(text, DrawConsoleText(ozzy->GetScreen()->GetPicture(), nullptr));
  EXPECT_THAT(text, HasSubstr(kVersion));
}

TEST(ConsoleDisplayTest, ScreenRedrawnOnlyWhereChanged) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  ConsoleDisplay display(*ozzy, console);
  ozzy->Run(kBootTime);
  display.Update();
  console.TakeText();

  // Nothing changes without a refresh.
  display.Update();
  EXPECT_EQ(console.TakeText(), "");

  const ScreenPicture before = ozzy->GetScreen()->GetPicture();
  ozzy->GetKeyboard()->TypeKey(kKeyEnter);
  ozzy->Run(kKeyTime);
  display.Update();
  const std::string text = console.TakeText();
  EXPECT_EQ(text, DrawConsoleText(ozzy->GetScreen()->GetPicture(), &before));
  EXPECT_THAT(text, HasSubstr(kNoDrive));
  EXPECT_THAT(text, Not(HasSubstr(kVersion)));
}

TEST(ConsoleDisplayTest, MessageWhileTooNarrow) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  console.SetSize({.columns = kDefaultScreenSize.columns - 1, .rows = 30});
  ConsoleDisplay display(*ozzy, console);
  ozzy->Run(kBootTime);
  display.Update();
  const std::string text = console.TakeText();
  EXPECT_THAT(
      text, HasSubstr(absl::StrCat("The console is ",
                                   kDefaultScreenSize.columns - 1, " by 30")));
  EXPECT_THAT(
      text, HasSubstr(absl::StrCat("the screen is ", kDefaultScreenSize.columns,
                                   " by ", kDefaultScreenSize.rows)));
  EXPECT_THAT(text, Not(HasSubstr(kVersion)));

  // The message isn't drawn again, and the picture isn't drawn, as it changes.
  display.Update();
  EXPECT_EQ(console.TakeText(), "");
  ozzy->GetKeyboard()->TypeKey('x');
  ozzy->Run(kKeyTime);
  display.Update();
  EXPECT_EQ(console.TakeText(), "");
}

TEST(ConsoleDisplayTest, MessageWhileTooShort) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  console.SetSize({.columns = 100, .rows = kDefaultScreenSize.rows - 1});
  ConsoleDisplay display(*ozzy, console);
  ozzy->Run(kBootTime);
  display.Update();
  EXPECT_THAT(console.TakeText(),
              HasSubstr(absl::StrCat("The console is 100 by ",
                                     kDefaultScreenSize.rows - 1)));
}

TEST(ConsoleDisplayTest, MessageRedrawnWhenTheConsoleResizes) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  console.SetSize({.columns = 40, .rows = 20});
  ConsoleDisplay display(*ozzy, console);
  ozzy->Run(kBootTime);
  display.Update();
  console.TakeText();

  console.SetSize({.columns = 30, .rows = 10});
  display.Update();
  EXPECT_THAT(console.TakeText(), HasSubstr("The console is 30 by 10"));
}

TEST(ConsoleDisplayTest, ScreenDrawnWholeOnceTheConsoleFits) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  ConsoleDisplay display(*ozzy, console);
  ozzy->Run(kBootTime);
  display.Update();
  console.TakeText();

  // The picture is drawn whole, though it didn't change, as the message was
  // drawn over it.
  console.SetSize({.columns = 40, .rows = 20});
  display.Update();
  console.TakeText();
  console.SetSize(kDefaultScreenSize);
  display.Update();
  EXPECT_EQ(console.TakeText(),
            DrawConsoleText(ozzy->GetScreen()->GetPicture(), nullptr));

  // Once drawn, the message is shown again when the console is too small.
  console.SetSize({.columns = 40, .rows = 20});
  display.Update();
  EXPECT_THAT(console.TakeText(), HasSubstr("The console is 40 by 20"));
}

TEST(ConsoleDisplayTest, ScreenDrawnWholeWhenTheConsoleResizes) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  ConsoleDisplay display(*ozzy, console);
  ozzy->Run(kBootTime);
  display.Update();
  console.TakeText();

  console.SetSize({.columns = kDefaultScreenSize.columns + 1,
                   .rows = kDefaultScreenSize.rows});
  display.Update();
  EXPECT_EQ(console.TakeText(),
            DrawConsoleText(ozzy->GetScreen()->GetPicture(), nullptr));
  display.Update();
  EXPECT_EQ(console.TakeText(), "");
}

TEST(ConsoleDisplayTest, NothingBeforeTheFirstRefreshInASmallConsole) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  console.SetSize({.columns = 40, .rows = 20});
  ConsoleDisplay display(*ozzy, console);
  display.Update();
  EXPECT_EQ(console.TakeText(), "");
}

TEST(ConsoleDisplayTest, TeletypeOutputWritten) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(TeletypeOnlySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  ConsoleDisplay display(*ozzy, console);
  ozzy->Run(kBootTime);
  display.Update();
  EXPECT_THAT(console.TakeText(), HasSubstr(kVersion));

  ozzy->GetKeyboard()->TypeKey(kKeyEnter);
  ozzy->Run(kKeyTime);
  display.Update();
  EXPECT_EQ(console.TakeText(), absl::StrCat("\r\nLoading...\r\n", kNoDrive,
                                             "\r\n\r\nPress Enter to Boot..."));
}

TEST(ConsoleDisplayTest, ShutDownInPlaceOfTheScreen) {
  FakeConsole console;
  ShowOzzyShutDown(console, ConsoleMode::kScreen, -3);
  const std::string lines[] = {"Ozzy shut down, exit code -3.",
                               "Ctrl+Alt+Q quits."};
  EXPECT_EQ(console.TakeText(), DrawConsoleLines(lines));
}

TEST(ConsoleDisplayTest, ShutDownOnALineAfterTheTeletype) {
  FakeConsole console;
  ShowOzzyShutDown(console, ConsoleMode::kLines, 7);
  EXPECT_EQ(console.TakeText(),
            "\r\nOzzy shut down, exit code 7. Ctrl+Alt+Q quits.\r\n");
}

TEST(ConsoleDisplayTest, NeitherScreenNorTeletype) {
  OzzySettings settings;
  settings.screen.reset();
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(settings);
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  ConsoleDisplay display(*ozzy, console);
  EXPECT_EQ(console.TakeText(),
            "This Ozzy has neither a screen nor a teletype.\r\n");
  ozzy->Run(kBootTime);
  display.Update();
  EXPECT_EQ(console.TakeText(), "");
}

}  // namespace
}  // namespace oz3
