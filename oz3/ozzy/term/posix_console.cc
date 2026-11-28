// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/posix_console.h"

#include <signal.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

#include <array>
#include <atomic>
#include <cerrno>
#include <cstdint>
#include <iterator>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "oz3/devices/keyboard/keyboard.h"
#include "oz3/ozzy/app/frontend_keys.h"
#include "oz3/ozzy/term/console.h"
#include "oz3/ozzy/term/terminal_keys.h"

namespace oz3 {

namespace {

//==============================================================================
// PosixConsole
//==============================================================================

static_assert(std::atomic<bool>::is_always_lock_free,
              "a signal handler may only set a lock free atomic");

// Whether the user asked to quit, which HandleSignal sets too.
std::atomic<bool> g_quit_requested = false;

// Whether a PosixConsole exists.
bool g_console_exists = false;

// The signals that ask to quit: the terminal closing (SIGHUP), and the program
// asked to end from outside, as the terminal sends no signals for its keys.
constexpr int kQuitSignals[] = {SIGHUP, SIGINT, SIGQUIT, SIGTERM};

// Handles the signals that ask to quit, rather than ending the program at
// once, so it gives the terminal back.
void HandleSignal(int) { g_quit_requested = true; }

// Returns an error for a function that failed to do `action`, from errno.
absl::Status ErrnoError(std::string_view action) {
  return absl::ErrnoToStatus(errno, absl::StrCat("failed to ", action));
}

// Returns the size of the window of the terminal `output`, or nothing if it
// can't be read.
std::optional<ConsoleSize> ReadWindowSize(int output) {
  winsize size = {};
  if (ioctl(output, TIOCGWINSZ, &size) != 0) {
    return std::nullopt;
  }
  return ConsoleSize{.columns = size.ws_col, .rows = size.ws_row};
}

class PosixConsole final : public Console {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a console that gives back the terminal `input` and `output`, with
  // the input's mode as it was, `input_mode`.
  PosixConsole(int input, const termios& input_mode, int output);
  ~PosixConsole() override;

  // Takes over the terminal, as Console::Create describes.
  absl::Status TakeOver(ConsoleMode mode);

  //----------------------------------------------------------------------------
  // Console implementation
  //----------------------------------------------------------------------------

  // Only Create returns a console, once it has taken over.
  ConsoleMode GetMode() const override { return *mode_; }

  void Write(std::string_view text) override;
  ConsoleSize GetSize() override;
  FrontendKeys TakeKeys() override;
  bool IsQuitRequested() const override { return g_quit_requested; }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // Follows where `text` leaves the cursor on its line, so the destructor
  // knows whether the line is unended, as a terminal can't be read back.
  void FollowLine(std::string_view text);

  const int input_;
  const termios input_mode_;
  const int output_;
  std::optional<ConsoleMode> mode_;  // Once it has taken over.
  ConsoleSize size_;                 // The size last read.
  TerminalKeyReader key_reader_;

  // The quit signals' actions before, once it has taken over.
  std::array<struct sigaction, std::size(kQuitSignals)> old_actions_ = {};

  // In lines, the cursor's column as written, ignoring the edge, and whether
  // there is text on its line, as a carriage return alone leaves.
  int column_ = 0;
  bool line_has_text_ = false;
};

PosixConsole::PosixConsole(int input, const termios& input_mode, int output)
    : input_(input), input_mode_(input_mode), output_(output) {
  g_console_exists = true;
  g_quit_requested = false;
}

PosixConsole::~PosixConsole() {
  if (mode_ == ConsoleMode::kScreen) {
    // The default style, the cursor's default shape, shown, and wrapping
    // again, before going back to the main screen and its cursor.
    Write("\x1B[0m\x1B[0 q\x1B[?25h\x1B[?7h\x1B[?1049l");
  } else if (mode_ == ConsoleMode::kLines && (column_ != 0 || line_has_text_)) {
    Write("\r\n");
  }

  // Keys not taken are dropped, rather than left for whatever runs next.
  // tcflush drops those the terminal hasn't passed on to be read yet too,
  // which TCSAFLUSH would leave.
  tcflush(input_, TCIFLUSH);
  tcsetattr(input_, TCSADRAIN, &input_mode_);
  if (mode_.has_value()) {
    for (int i = 0; i < static_cast<int>(std::size(kQuitSignals)); ++i) {
      sigaction(kQuitSignals[i], &old_actions_[i], nullptr);
    }
  }
  g_console_exists = false;
}

absl::Status PosixConsole::TakeOver(ConsoleMode mode) {
  // Keys are read as the bytes the terminal sends, without echoing them, and
  // without the terminal acting on any (such as Ctrl+C, Ctrl+Z, or Ctrl+S), so
  // every key is a key. A read returns at once with the bytes there are. A
  // line feed only moves down, as on a teletype.
  termios raw = input_mode_;
  raw.c_iflag &=
      ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
  raw.c_oflag &= ~OPOST;
  raw.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 0;
  if (tcsetattr(input_, TCSANOW, &raw) != 0) {
    return ErrnoError("set the terminal's mode");
  }

  // The size is read here first, so GetSize can take a later failure to read
  // it as no change.
  const std::optional<ConsoleSize> size = ReadWindowSize(output_);
  if (!size.has_value()) {
    return ErrnoError("read the terminal's size");
  }
  size_ = *size;

  // Without wrapping, a row wider than the terminal is cut off at its edge,
  // rather than continuing on the next.
  if (mode == ConsoleMode::kScreen) {
    Write("\x1B[?1049h\x1B[?7l");
  }

  // The quit signals are handled last, once nothing can fail, as the
  // destructor gives back their actions only once it has taken over.
  struct sigaction action = {};
  action.sa_handler = HandleSignal;
  sigemptyset(&action.sa_mask);
  for (int i = 0; i < static_cast<int>(std::size(kQuitSignals)); ++i) {
    sigaction(kQuitSignals[i], &action, &old_actions_[i]);
  }
  mode_ = mode;
  return absl::OkStatus();
}

void PosixConsole::Write(std::string_view text) {
  if (mode_ == ConsoleMode::kLines) {
    FollowLine(text);
  }
  while (!text.empty()) {
    const int64_t written = write(output_, text.data(), text.size());
    if (written < 0 && errno == EINTR) {
      continue;
    }
    if (written <= 0) {
      return;
    }
    text.remove_prefix(written);
  }
}

void PosixConsole::FollowLine(std::string_view text) {
  const int size = static_cast<int>(text.size());
  for (int i = 0; i < size; ++i) {
    const int byte = static_cast<unsigned char>(text[i]);
    switch (byte) {
      case kKeyEscape:
        // A control sequence ends at its final byte, and other escape
        // sequences at the byte after the escape.
        if (i + 1 < size && text[i + 1] == '[') {
          i += 2;
          while (i < size && (text[i] < 0x40 || text[i] > 0x7E)) {
            ++i;
          }
        } else {
          ++i;
        }
        break;
      case '\r':
        column_ = 0;
        break;
      case '\n':
        line_has_text_ = false;
        break;
      case '\b':
        if (column_ > 0) {
          --column_;
        }
        break;
      case '\t':
        column_ = (column_ / 8 + 1) * 8;
        break;
      default:
        // Each character is one column, which is its first byte in UTF-8.
        if (byte >= 0x20 && byte != 0x7F && (byte & 0xC0) != 0x80) {
          ++column_;
          line_has_text_ = true;
        }
        break;
    }
  }
}

ConsoleSize PosixConsole::GetSize() {
  if (const std::optional<ConsoleSize> size = ReadWindowSize(output_);
      size.has_value()) {
    size_ = *size;
  }
  return size_;
}

FrontendKeys PosixConsole::TakeKeys() {
  std::string input;
  char buffer[256];
  while (true) {
    const int64_t count = read(input_, buffer, sizeof(buffer));
    if (count < 0 && errno == EINTR) {
      continue;
    }
    if (count <= 0) {
      break;
    }
    input.append(buffer, count);
  }

  // Every key with a code is tapped, the keys that quit too.
  FrontendKeys keys;
  for (const uint16_t key : key_reader_.ReadKeys(input)) {
    AddKeyTap(key, keys.changes);
    if (IsQuitKey(key)) {
      g_quit_requested = true;
    } else {
      keys.typed.push_back(key);
    }
  }
  return keys;
}

}  // namespace

//==============================================================================
// Console
//==============================================================================

absl::StatusOr<std::unique_ptr<Console>> CreateTerminalConsole(
    int input, int output, ConsoleMode mode) {
  if (g_console_exists) {
    return absl::FailedPreconditionError("a console already exists");
  }
  termios input_mode;
  if (!isatty(input) || !isatty(output) || tcgetattr(input, &input_mode) != 0) {
    return absl::FailedPreconditionError(
        "the input and output must be a terminal");
  }

  // The console gives back what it changed, even if taking over fails.
  auto console = std::make_unique<PosixConsole>(input, input_mode, output);
  if (absl::Status status = console->TakeOver(mode); !status.ok()) {
    return status;
  }
  return console;
}

absl::StatusOr<std::unique_ptr<Console>> Console::Create(ConsoleMode mode) {
  return CreateTerminalConsole(STDIN_FILENO, STDOUT_FILENO, mode);
}

}  // namespace oz3
