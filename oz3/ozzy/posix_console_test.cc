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
// Keys
//==============================================================================

// Returns the keys a new reader reads from `input` and then no more bytes.
std::vector<uint16_t> ReadAllKeys(std::string_view input) {
  TerminalKeyReader reader;
  std::vector<uint16_t> keys = reader.ReadKeys(input);
  const std::vector<uint16_t> rest = reader.ReadKeys("");
  keys.insert(keys.end(), rest.begin(), rest.end());
  return keys;
}

// Returns the changes from tapping `key`.
std::vector<KeyChange> GetKeyTap(uint16_t key) {
  std::vector<KeyChange> changes;
  AddKeyTap(key, changes);
  return changes;
}

TEST(PosixConsoleTest, CharactersAreAscii) {
  EXPECT_THAT(ReadAllKeys("aZ1 ~"), ElementsAre('a', 'Z', '1', ' ', '~'));
}

TEST(PosixConsoleTest, ControlCodesAreCtrl) {
  EXPECT_THAT(ReadAllKeys("\x01\x03\x11\x1A"),
              ElementsAre(0x01 | kKeyCtrl, 0x03 | kKeyCtrl, 0x11 | kKeyCtrl,
                          0x1A | kKeyCtrl));
  EXPECT_THAT(ReadAllKeys("\n"), ElementsAre(0x0A | kKeyCtrl));
  EXPECT_THAT(ReadAllKeys(std::string_view("\0", 1)),
              ElementsAre(' ' | kKeyCtrl));
}

TEST(PosixConsoleTest, NamedControlCodes) {
  EXPECT_THAT(
      ReadAllKeys("\r\t\x7F\b"),
      ElementsAre(kKeyEnter, kKeyTab, kKeyBackspace, kKeyBackspace | kKeyCtrl));
}

TEST(PosixConsoleTest, BytesThatAreNotKeys) {
  // UTF-8 for é, and the control codes past Ctrl+Z.
  EXPECT_THAT(ReadAllKeys("\xC3\xA9\x1C\x1D\x1E\x1F"), IsEmpty());
}

TEST(PosixConsoleTest, ArrowsAndPages) {
  EXPECT_THAT(ReadAllKeys("\x1B[A\x1B[B\x1B[C\x1B[D"),
              ElementsAre(kKeyUp, kKeyDown, kKeyRight, kKeyLeft));
  EXPECT_THAT(
      ReadAllKeys("\x1B[H\x1B[F\x1B[1~\x1B[4~\x1B[7~\x1B[8~"),
      ElementsAre(kKeyHome, kKeyEnd, kKeyHome, kKeyEnd, kKeyHome, kKeyEnd));
  EXPECT_THAT(ReadAllKeys("\x1B[2~\x1B[3~\x1B[5~\x1B[6~"),
              ElementsAre(kKeyInsert, kKeyDelete, kKeyPageUp, kKeyPageDown));

  // In application cursor mode, the arrows, Home, and End are SS3 sequences.
  EXPECT_THAT(ReadAllKeys("\x1BOA\x1BOD\x1BOH\x1BOF"),
              ElementsAre(kKeyUp, kKeyLeft, kKeyHome, kKeyEnd));
}

TEST(PosixConsoleTest, FunctionKeys) {
  EXPECT_THAT(ReadAllKeys("\x1BOP\x1BOQ\x1BOR\x1BOS"),
              ElementsAre(kKeyF1, kKeyF2, kKeyF3, kKeyF4));
  EXPECT_THAT(ReadAllKeys("\x1B[11~\x1B[15~\x1B[17~\x1B[21~\x1B[23~\x1B[24~"),
              ElementsAre(kKeyF1, kKeyF5, kKeyF6, kKeyF10, kKeyF11, kKeyF12));
}

TEST(PosixConsoleTest, ModifiersOfSequences) {
  EXPECT_THAT(
      ReadAllKeys("\x1B[1;2A\x1B[1;3B\x1B[1;5C\x1B[1;8D"),
      ElementsAre(kKeyUp | kKeyShift, kKeyDown | kKeyAlt, kKeyRight | kKeyCtrl,
                  kKeyLeft | kKeyShift | kKeyAlt | kKeyCtrl));
  EXPECT_THAT(ReadAllKeys("\x1B[3;5~\x1B[1;2P\x1B[15;6~"),
              ElementsAre(kKeyDelete | kKeyCtrl, kKeyF1 | kKeyShift,
                          kKeyF5 | kKeyShift | kKeyCtrl));

  // Shift+Tab is a sequence of its own.
  EXPECT_THAT(ReadAllKeys("\x1B[Z"), ElementsAre(kKeyTab | kKeyShift));
}

TEST(PosixConsoleTest, EscapeBeforeAKeyIsAlt) {
  EXPECT_THAT(ReadAllKeys("\x1Bx\x1BX\x1B\x11\x1B\r"),
              ElementsAre('x' | kKeyAlt, 'X' | kKeyAlt,
                          0x11 | kKeyCtrl | kKeyAlt, kKeyEnter | kKeyAlt));
  EXPECT_THAT(ReadAllKeys("\x1B\x1B[A"), ElementsAre(kKeyUp | kKeyAlt));
  EXPECT_THAT(ReadAllKeys("\x1B\x1B"), ElementsAre(kKeyEscape | kKeyAlt));
}

TEST(PosixConsoleTest, SequencesThatAreNotKeys) {
  // A private sequence (a focus or mouse report), one with an intermediate
  // byte, and unknown finals and numbers are read and dropped.
  EXPECT_THAT(ReadAllKeys("\x1B[I\x1B[<0;1;1M\x1B[?1u\x1B[ q\x1B[9~a"),
              ElementsAre('a'));
}

TEST(PosixConsoleTest, BytesThatAreNotSequences) {
  // ESC [ before a control code isn't a control sequence, so it is Alt+[.
  EXPECT_THAT(ReadAllKeys("\x1B[\x01"),
              ElementsAre('[' | kKeyAlt, 0x01 | kKeyCtrl));

  // ESC O before a byte that isn't a key's is Alt+O.
  EXPECT_THAT(ReadAllKeys("\x1BOx"), ElementsAre('O' | kKeyAlt, 'x'));
}

TEST(PosixConsoleTest, EscapeWaitsForTheNextBytes) {
  TerminalKeyReader reader;
  EXPECT_THAT(reader.ReadKeys("a\x1B"), ElementsAre('a'));
  EXPECT_THAT(reader.ReadKeys(""), ElementsAre(kKeyEscape));
  EXPECT_THAT(reader.ReadKeys(""), IsEmpty());

  // Escape before the next bytes is Alt with them.
  EXPECT_THAT(reader.ReadKeys("\x1B"), IsEmpty());
  EXPECT_THAT(reader.ReadKeys("b"), ElementsAre('b' | kKeyAlt));
}

TEST(PosixConsoleTest, SequencesSplitAcrossReads) {
  TerminalKeyReader reader;
  EXPECT_THAT(reader.ReadKeys("\x1B["), IsEmpty());
  EXPECT_THAT(reader.ReadKeys("1;5"), IsEmpty());
  EXPECT_THAT(reader.ReadKeys("A\x1BO"), ElementsAre(kKeyUp | kKeyCtrl));
  EXPECT_THAT(reader.ReadKeys("P"), ElementsAre(kKeyF1));
}

TEST(PosixConsoleTest, UnfinishedSequencesAreReadAsTheyAre) {
  TerminalKeyReader reader;
  EXPECT_THAT(reader.ReadKeys("\x1B[1;"), IsEmpty());
  EXPECT_THAT(reader.ReadKeys(""), ElementsAre('[' | kKeyAlt, '1', ';'));
  EXPECT_THAT(reader.ReadKeys("\x1BO"), IsEmpty());
  EXPECT_THAT(reader.ReadKeys(""), ElementsAre('O' | kKeyAlt));
}

TEST(PosixConsoleTest, CtrlAltQQuits) {
  const std::vector<uint16_t> keys = ReadAllKeys("\x1B\x11");
  ASSERT_EQ(keys.size(), 1);
  EXPECT_TRUE(IsQuitKey(keys[0]));
  EXPECT_FALSE(IsQuitKey(ReadAllKeys("\x11")[0]));
  EXPECT_FALSE(IsQuitKey(ReadAllKeys("\x1Bq")[0]));
}

TEST(PosixConsoleTest, KeyCodesOfCharacters) {
  EXPECT_EQ(GetKeyCode('a'), 'a');
  EXPECT_EQ(GetKeyCode('A'), 'a');
  EXPECT_EQ(GetKeyCode('z' | kKeyAlt), 'z');
  EXPECT_EQ(GetKeyCode(' '), ' ');
  EXPECT_EQ(GetKeyCode('7'), '7');
  EXPECT_EQ(GetKeyCode('/'), '/');
  EXPECT_EQ(GetKeyCode('`'), '`');
}

TEST(PosixConsoleTest, KeyCodesOfShiftedCharacters) {
  EXPECT_EQ(GetKeyCode('!'), '1');
  EXPECT_EQ(GetKeyCode(')'), '0');
  EXPECT_EQ(GetKeyCode('_'), '-');
  EXPECT_EQ(GetKeyCode('+'), '=');
  EXPECT_EQ(GetKeyCode('{'), '[');
  EXPECT_EQ(GetKeyCode('|'), '\\');
  EXPECT_EQ(GetKeyCode(':'), ';');
  EXPECT_EQ(GetKeyCode('"'), '\'');
  EXPECT_EQ(GetKeyCode('?'), '/');
  EXPECT_EQ(GetKeyCode('~'), '`');
}

TEST(PosixConsoleTest, KeyCodesOfControlCodes) {
  EXPECT_EQ(GetKeyCode(0x01 | kKeyCtrl), 'a');
  EXPECT_EQ(GetKeyCode(0x1A | kKeyCtrl), 'z');
  EXPECT_EQ(GetKeyCode(0x0A | kKeyCtrl), 'j');
  EXPECT_EQ(GetKeyCode(kKeyBackspace | kKeyCtrl), kKeyBackspace);
  EXPECT_EQ(GetKeyCode(kKeyTab | kKeyShift), kKeyTab);
  EXPECT_EQ(GetKeyCode(kKeyEnter), kKeyEnter);
  EXPECT_EQ(GetKeyCode(kKeyEscape), kKeyEscape);
}

TEST(PosixConsoleTest, KeyCodesOfNamedKeys) {
  EXPECT_EQ(GetKeyCode(kKeyDelete), kKeyDelete);
  EXPECT_EQ(GetKeyCode(kKeyUp | kKeyCtrl), kKeyUp);
  EXPECT_EQ(GetKeyCode(kKeyInsert), kKeyInsert);
  EXPECT_EQ(GetKeyCode(kKeyF1), kKeyF1);
  EXPECT_EQ(GetKeyCode(kKeyF12), kKeyF12);
}

TEST(PosixConsoleTest, KeysWithoutCodes) {
  EXPECT_EQ(GetKeyCode(0), 0);
  EXPECT_EQ(GetKeyCode(0x89), 0);
  EXPECT_EQ(GetKeyCode(0x8F), 0);
  EXPECT_EQ(GetKeyCode(0x9C), 0);
}

TEST(PosixConsoleTest, TapsPressAndReleaseTheKey) {
  EXPECT_THAT(GetKeyTap('A'),
              ElementsAre(KeyChange{.key = 'a', .held = true},
                          KeyChange{.key = 'a', .held = false}));
  EXPECT_THAT(GetKeyTap(kKeyUp),
              ElementsAre(KeyChange{.key = kKeyUp, .held = true},
                          KeyChange{.key = kKeyUp, .held = false}));
}

TEST(PosixConsoleTest, TapsHoldTheModifiersAroundTheKey) {
  EXPECT_THAT(GetKeyTap(kKeyF5 | kKeyShift | kKeyCtrl | kKeyAlt),
              ElementsAre(KeyChange{.key = kKeyLeftShift, .held = true},
                          KeyChange{.key = kKeyLeftCtrl, .held = true},
                          KeyChange{.key = kKeyLeftAlt, .held = true},
                          KeyChange{.key = kKeyF5, .held = true},
                          KeyChange{.key = kKeyF5, .held = false},
                          KeyChange{.key = kKeyLeftShift, .held = false},
                          KeyChange{.key = kKeyLeftCtrl, .held = false},
                          KeyChange{.key = kKeyLeftAlt, .held = false}));
}

TEST(PosixConsoleTest, NoTapForAKeyWithoutACode) {
  EXPECT_THAT(GetKeyTap(0x8F | kKeyCtrl), IsEmpty());
}

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
