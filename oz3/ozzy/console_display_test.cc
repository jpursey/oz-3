// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/console_display.h"

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/time/time.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/devices/keyboard.h"
#include "oz3/devices/screen.h"
#include "oz3/ozzy/console.h"
#include "oz3/ozzy/console_text.h"
#include "oz3/ozzy/ozzy.h"
#include "oz3/ozzy/ozzy_settings.h"

namespace oz3 {
namespace {

using ::testing::HasSubstr;
using ::testing::Not;

// A console that keeps what is written to it.
class FakeConsole final : public Console {
 public:
  // Returns what was written since the last call.
  std::string TakeText() { return std::exchange(text_, {}); }

  void Write(std::string_view text) override { absl::StrAppend(&text_, text); }
  std::vector<uint16_t> TakeKeys() override { return {}; }
  bool IsQuitRequested() const override { return false; }

 private:
  std::string text_;
};

// Long enough for the boot ROM to show its banner.
constexpr absl::Duration kBootTime = absl::Seconds(1);

// Long enough for a key to be echoed, and the screen refreshed.
constexpr absl::Duration kKeyTime = absl::Milliseconds(100);

std::unique_ptr<Ozzy> CreateOzzy(const OzzySettings& settings) {
  absl::StatusOr<std::unique_ptr<Ozzy>> ozzy = Ozzy::Create(settings);
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
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(OzzySettings());
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(GetOzzyConsoleMode(*ozzy), ConsoleMode::kScreen);

  ozzy = CreateOzzy(TeletypeOnlySettings());
  ASSERT_NE(ozzy, nullptr);
  EXPECT_EQ(GetOzzyConsoleMode(*ozzy), ConsoleMode::kLines);
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
  EXPECT_THAT(text, HasSubstr("OZZY 0.1"));
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
  ozzy->GetKeyboard()->TypeKey('x');
  ozzy->Run(kKeyTime);
  display.Update();
  const std::string text = console.TakeText();
  EXPECT_EQ(text, DrawConsoleText(ozzy->GetScreen()->GetPicture(), &before));
  EXPECT_THAT(text, HasSubstr("x"));
  EXPECT_THAT(text, Not(HasSubstr("OZZY 0.1")));
}

TEST(ConsoleDisplayTest, TeletypeOutputWritten) {
  std::unique_ptr<Ozzy> ozzy = CreateOzzy(TeletypeOnlySettings());
  ASSERT_NE(ozzy, nullptr);
  FakeConsole console;
  ConsoleDisplay display(*ozzy, console);
  ozzy->Run(kBootTime);
  display.Update();
  EXPECT_THAT(console.TakeText(), HasSubstr("OZZY 0.1"));

  ozzy->GetKeyboard()->TypeKey('x');
  ozzy->Run(kKeyTime);
  display.Update();
  EXPECT_EQ(console.TakeText(), "x");
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
