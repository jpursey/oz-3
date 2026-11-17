// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/posix_console.h"

#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/console.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

//==============================================================================
// Console
//==============================================================================

// Runs a console on a pseudo terminal, which the test is the other end of.
class PosixTerminalTest : public ::testing::Test {
 protected:
  void SetUp() override {
    main_ = posix_openpt(O_RDWR | O_NOCTTY);
    ASSERT_GE(main_, 0);
    ASSERT_EQ(grantpt(main_), 0);
    ASSERT_EQ(unlockpt(main_), 0);
    terminal_ = open(ptsname(main_), O_RDWR | O_NOCTTY);
    ASSERT_GE(terminal_, 0);

    // The test reads the terminal's output without waiting for it.
    fcntl(main_, F_SETFL, fcntl(main_, F_GETFL) | O_NONBLOCK);
    ASSERT_EQ(tcgetattr(terminal_, &mode_), 0);
  }

  void TearDown() override {
    close(terminal_);
    close(main_);
  }

  // Returns a console on the terminal, or null after failing the test.
  std::unique_ptr<Console> CreateConsole(ConsoleMode mode) {
    absl::StatusOr<std::unique_ptr<Console>> console =
        CreateTerminalConsole(terminal_, terminal_, mode);
    EXPECT_TRUE(console.ok()) << console.status();
    return console.ok() ? *std::move(console) : nullptr;
  }

  // Returns what the console wrote since this was last called.
  std::string ReadOutput() {
    std::string output;
    char buffer[256];
    int64_t count = 0;
    while ((count = read(main_, buffer, sizeof(buffer))) > 0) {
      output.append(buffer, count);
    }
    return output;
  }

  // Types `text` at the terminal.
  void Type(std::string_view text) {
    ASSERT_EQ(write(main_, text.data(), text.size()),
              static_cast<int64_t>(text.size()));
  }

  // Returns the terminal's mode now.
  termios GetMode() {
    termios mode = {};
    EXPECT_EQ(tcgetattr(terminal_, &mode), 0);
    return mode;
  }

  int main_ = -1;      // The test's end.
  int terminal_ = -1;  // The console's end.
  termios mode_ = {};  // The terminal's mode before the test.
};

TEST_F(PosixTerminalTest, NotATerminal) {
  int pipe_ends[2];
  ASSERT_EQ(pipe(pipe_ends), 0);
  EXPECT_EQ(CreateTerminalConsole(pipe_ends[0], terminal_, ConsoleMode::kLines)
                .status()
                .code(),
            absl::StatusCode::kFailedPrecondition);
  EXPECT_EQ(CreateTerminalConsole(terminal_, pipe_ends[1], ConsoleMode::kLines)
                .status()
                .code(),
            absl::StatusCode::kFailedPrecondition);
  close(pipe_ends[0]);
  close(pipe_ends[1]);
}

TEST_F(PosixTerminalTest, OnlyOneConsole) {
  std::unique_ptr<Console> console = CreateConsole(ConsoleMode::kLines);
  ASSERT_NE(console, nullptr);
  EXPECT_EQ(CreateTerminalConsole(terminal_, terminal_, ConsoleMode::kLines)
                .status()
                .code(),
            absl::StatusCode::kFailedPrecondition);
  console.reset();
  console = CreateConsole(ConsoleMode::kLines);
  EXPECT_NE(console, nullptr);
}

TEST_F(PosixTerminalTest, RawModeIsGivenBack) {
  std::unique_ptr<Console> console = CreateConsole(ConsoleMode::kLines);
  ASSERT_NE(console, nullptr);
  const termios raw = GetMode();
  EXPECT_EQ(raw.c_lflag & (ECHO | ICANON | ISIG | IEXTEN), 0);
  EXPECT_EQ(raw.c_iflag & (ICRNL | IXON), 0);
  EXPECT_EQ(raw.c_oflag & OPOST, 0);

  console.reset();
  const termios given_back = GetMode();
  EXPECT_EQ(given_back.c_lflag, mode_.c_lflag);
  EXPECT_EQ(given_back.c_iflag, mode_.c_iflag);
  EXPECT_EQ(given_back.c_oflag, mode_.c_oflag);
  EXPECT_EQ(given_back.c_cc[VMIN], mode_.c_cc[VMIN]);
}

TEST_F(PosixTerminalTest, ScreenIsTheAlternateScreen) {
  std::unique_ptr<Console> console = CreateConsole(ConsoleMode::kScreen);
  ASSERT_NE(console, nullptr);
  EXPECT_EQ(ReadOutput(), "\x1B[?1049h\x1B[?7l");
  console->Write("Hi\n");
  EXPECT_EQ(ReadOutput(), "Hi\n");
  console.reset();
  EXPECT_EQ(ReadOutput(), "\x1B[0m\x1B[0 q\x1B[?25h\x1B[?7h\x1B[?1049l");
}

TEST_F(PosixTerminalTest, LinesEndTheLastLine) {
  for (std::string_view text : {"Hi", "Hi\r", "Hi\n", "\t", "Hi\r\nA\b"}) {
    std::unique_ptr<Console> console = CreateConsole(ConsoleMode::kLines);
    ASSERT_NE(console, nullptr);
    console->Write(text);
    console.reset();
    EXPECT_EQ(ReadOutput(), absl::StrCat(text, "\r\n")) << text;
  }
}

TEST_F(PosixTerminalTest, LinesEndedAreLeft) {
  for (std::string_view text :
       {"", "Hi\r\n", "Hi\r\n\x1B[1mBold\x1B[0m\r\n", "Hi\b\b\r\n"}) {
    std::unique_ptr<Console> console = CreateConsole(ConsoleMode::kLines);
    ASSERT_NE(console, nullptr);
    console->Write(text);
    console.reset();
    EXPECT_EQ(ReadOutput(), text) << text;
  }
}

TEST_F(PosixTerminalTest, Size) {
  winsize size = {.ws_row = 24, .ws_col = 80};
  ASSERT_EQ(ioctl(main_, TIOCSWINSZ, &size), 0);
  std::unique_ptr<Console> console = CreateConsole(ConsoleMode::kScreen);
  ASSERT_NE(console, nullptr);
  EXPECT_EQ(console->GetSize(), (ConsoleSize{.columns = 80, .rows = 24}));

  size = {.ws_row = 50, .ws_col = 132};
  ASSERT_EQ(ioctl(main_, TIOCSWINSZ, &size), 0);
  EXPECT_EQ(console->GetSize(), (ConsoleSize{.columns = 132, .rows = 50}));
}

TEST_F(PosixTerminalTest, KeysAreTypedAndTapped) {
  std::unique_ptr<Console> console = CreateConsole(ConsoleMode::kLines);
  ASSERT_NE(console, nullptr);
  EXPECT_THAT(console->TakeKeys().typed, IsEmpty());

  // Ctrl+C and Ctrl+Z are keys, as is Enter, which isn't a line feed.
  Type("a\x03\x1A\r\x1B[A");
  const ConsoleKeys keys = console->TakeKeys();
  EXPECT_THAT(keys.typed, ElementsAre('a', 0x03 | kKeyCtrl, 0x1A | kKeyCtrl,
                                      kKeyEnter, kKeyUp));
  EXPECT_EQ(keys.changes.size(), 14);
  EXPECT_FALSE(console->IsQuitRequested());

  // Nothing was echoed.
  EXPECT_EQ(ReadOutput(), "");
}

TEST_F(PosixTerminalTest, KeysNotTakenAreDropped) {
  std::unique_ptr<Console> console = CreateConsole(ConsoleMode::kLines);
  ASSERT_NE(console, nullptr);
  Type("abc");
  console.reset();
  console = CreateConsole(ConsoleMode::kLines);
  ASSERT_NE(console, nullptr);
  EXPECT_THAT(console->TakeKeys().typed, IsEmpty());
}

TEST_F(PosixTerminalTest, CtrlAltQQuits) {
  std::unique_ptr<Console> console = CreateConsole(ConsoleMode::kLines);
  ASSERT_NE(console, nullptr);
  Type("a\x1B\x11");
  const ConsoleKeys keys = console->TakeKeys();
  EXPECT_THAT(keys.typed, ElementsAre('a'));
  EXPECT_THAT(keys.changes,
              ElementsAre(KeyChange{.key = 'a', .held = true},
                          KeyChange{.key = 'a', .held = false},
                          KeyChange{.key = kKeyLeftCtrl, .held = true},
                          KeyChange{.key = kKeyLeftAlt, .held = true},
                          KeyChange{.key = 'q', .held = true},
                          KeyChange{.key = 'q', .held = false},
                          KeyChange{.key = kKeyLeftCtrl, .held = false},
                          KeyChange{.key = kKeyLeftAlt, .held = false}));
  EXPECT_TRUE(console->IsQuitRequested());

  // A new console starts without a quit.
  console.reset();
  console = CreateConsole(ConsoleMode::kLines);
  ASSERT_NE(console, nullptr);
  EXPECT_FALSE(console->IsQuitRequested());
}

TEST_F(PosixTerminalTest, QuitSignals) {
  for (const int signal : {SIGHUP, SIGTERM}) {
    std::unique_ptr<Console> console = CreateConsole(ConsoleMode::kLines);
    ASSERT_NE(console, nullptr);
    EXPECT_FALSE(console->IsQuitRequested());
    ASSERT_EQ(raise(signal), 0);
    EXPECT_TRUE(console->IsQuitRequested()) << signal;
    console.reset();

    // The signal's action is given back.
    struct sigaction action = {};
    ASSERT_EQ(sigaction(signal, nullptr, &action), 0);
    EXPECT_EQ(action.sa_handler, SIG_DFL) << signal;
  }
}

}  // namespace
}  // namespace oz3
