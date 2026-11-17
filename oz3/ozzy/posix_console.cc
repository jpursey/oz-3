// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/posix_console.h"

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
#include "absl/strings/numbers.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"
#include "absl/types/span.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/console.h"

namespace oz3 {

namespace {

//==============================================================================
// Keys
//==============================================================================

constexpr char kEscape = '\x1B';

// The characters a US keyboard types with Shift, and in the same order, the
// characters its keys type without it, which are their key codes.
constexpr std::string_view kShiftedCharacters = "~!@#$%^&*()_+{}|:\"<>?";
constexpr std::string_view kUnshiftedCharacters = "`1234567890-=[]\\;',./";

// The left modifier keys tapped with a key, by their typed key event bits.
struct ModifierKey {
  uint16_t bit;
  int key;
};
constexpr ModifierKey kModifierKeys[] = {
    {kKeyShift, kKeyLeftShift},
    {kKeyCtrl, kKeyLeftCtrl},
    {kKeyAlt, kKeyLeftAlt},
};

// The keys of control sequences by their final byte, which are the same after
// SS3 (ESC O) as after CSI (ESC [).
struct FinalKey {
  char final_byte;
  uint16_t key;
};
constexpr FinalKey kFinalKeys[] = {
    {'A', kKeyUp},   {'B', kKeyDown}, {'C', kKeyRight}, {'D', kKeyLeft},
    {'H', kKeyHome}, {'F', kKeyEnd},  {'P', kKeyF1},    {'Q', kKeyF2},
    {'R', kKeyF3},   {'S', kKeyF4},
};

// The keys of control sequences ending in ~, by their first parameter.
struct NumberKey {
  int number;
  uint16_t key;
};
constexpr NumberKey kNumberKeys[] = {
    {1, kKeyHome},   {2, kKeyInsert},   {3, kKeyDelete}, {4, kKeyEnd},
    {5, kKeyPageUp}, {6, kKeyPageDown}, {7, kKeyHome},   {8, kKeyEnd},
    {11, kKeyF1},    {12, kKeyF2},      {13, kKeyF3},    {14, kKeyF4},
    {15, kKeyF5},    {17, kKeyF6},      {18, kKeyF7},    {19, kKeyF8},
    {20, kKeyF9},    {21, kKeyF10},     {23, kKeyF11},   {24, kKeyF12},
};

// The length ReadControlSequence and ReadSs3 return for bytes that aren't a
// control sequence.
constexpr int kNotSequence = -1;

// Returns the key of the byte `byte` on its own, or 0 if it is none.
uint16_t GetByteKey(char byte) {
  const int value = static_cast<unsigned char>(byte);
  switch (value) {
    case 0x00:
      return ' ' | kKeyCtrl;
    case kKeyBackspace:
      return kKeyBackspace | kKeyCtrl;
    case kKeyTab:
    case kKeyEnter:
    case kKeyEscape:
      return static_cast<uint16_t>(value);
    case kKeyDelete:
      return kKeyBackspace;
  }
  if (value <= 'Z' - 'A' + 1) {
    return static_cast<uint16_t>(value | kKeyCtrl);
  }
  if (value >= 0x20 && value < 0x7F) {
    return static_cast<uint16_t>(value);
  }
  return 0;
}

// Returns the key with the final byte `final_byte` in kFinalKeys, or 0 if
// there is none.
uint16_t FindFinalKey(char final_byte) {
  for (const FinalKey& entry : kFinalKeys) {
    if (entry.final_byte == final_byte) {
      return entry.key;
    }
  }
  return 0;
}

// Returns the key with `number` in kNumberKeys, or 0 if there is none.
uint16_t FindNumberKey(int number) {
  for (const NumberKey& entry : kNumberKeys) {
    if (entry.number == number) {
      return entry.key;
    }
  }
  return 0;
}

// Returns the control sequence parameter at `index` in `parameters`, without
// its subparameters, or 1 if it is left out.
int GetParameter(absl::Span<const std::string_view> parameters, int index) {
  if (index >= static_cast<int>(parameters.size())) {
    return 1;
  }
  const std::string_view parameter =
      parameters[index].substr(0, parameters[index].find(':'));
  int value = 0;
  if (parameter.empty() || !absl::SimpleAtoi(parameter, &value)) {
    return 1;
  }
  return value;
}

// Returns the modifier bits of the control sequence parameter `parameter`,
// which is 1 plus a mask of Shift (1), Alt (2), and Ctrl (4).
uint16_t GetModifiers(int parameter) {
  const int mask = parameter - 1;
  uint16_t modifiers = 0;
  if ((mask & 1) != 0) {
    modifiers |= kKeyShift;
  }
  if ((mask & 2) != 0) {
    modifiers |= kKeyAlt;
  }
  if ((mask & 4) != 0) {
    modifiers |= kKeyCtrl;
  }
  return modifiers;
}

// Returns the key of the control sequence with `parameters` (its parameter
// and intermediate bytes) and `final_byte`, or 0 if it isn't a key's.
uint16_t GetSequenceKey(std::string_view parameters, char final_byte) {
  // Private parameters (starting with < to ?) and intermediate bytes aren't in
  // any key's sequence.
  for (const char byte : parameters) {
    if (byte < '0' || byte > ';') {
      return 0;
    }
  }
  const std::vector<std::string_view> fields = absl::StrSplit(parameters, ';');
  uint16_t key = 0;
  if (final_byte == '~') {
    key = FindNumberKey(GetParameter(fields, 0));
  } else if (final_byte == 'Z') {
    key = kKeyTab | kKeyShift;
  } else {
    key = FindFinalKey(final_byte);
  }
  if (key == 0) {
    return 0;
  }
  return key | GetModifiers(GetParameter(fields, 1));
}

// Reads the control sequence at the start of `input`, which starts with CSI
// (ESC [), setting `key` to its key. Returns its length, 0 if `input` ends
// partway through it, or kNotSequence.
int ReadControlSequence(std::string_view input, uint16_t& key) {
  const int size = static_cast<int>(input.size());
  int end = 2;
  while (end < size && input[end] >= 0x20 && input[end] <= 0x3F) {
    ++end;
  }
  if (end == size) {
    return 0;
  }
  const char final_byte = input[end];
  if (final_byte < 0x40 || final_byte > 0x7E) {
    return kNotSequence;
  }
  key = GetSequenceKey(input.substr(2, end - 2), final_byte);
  return end + 1;
}

// Reads the key at the start of `input`, which starts with SS3 (ESC O),
// setting `key` to it. Returns its length, 0 if `input` ends before it, or
// kNotSequence.
int ReadSs3(std::string_view input, uint16_t& key) {
  if (input.size() < 3) {
    return 0;
  }
  key = FindFinalKey(input[2]);
  return key != 0 ? 3 : kNotSequence;
}

// Reads the key at the start of `input`, which isn't empty, setting `key` to
// it, or 0 for bytes that aren't a key. Returns the bytes read, or 0 if
// `input` ends partway through a key and `more` bytes may follow. Without
// more, bytes that start a key are read as they are.
int ReadKey(std::string_view input, bool more, uint16_t& key) {
  key = 0;
  if (input[0] != kEscape) {
    key = GetByteKey(input[0]);
    return 1;
  }
  if (input.size() == 1) {
    if (more) {
      return 0;
    }
    key = kKeyEscape;
    return 1;
  }
  if (input[1] == '[' || input[1] == 'O') {
    const int length =
        input[1] == '[' ? ReadControlSequence(input, key) : ReadSs3(input, key);
    if (length > 0 || (length == 0 && more)) {
      return length;
    }
  }

  // Escape before a key that isn't a control sequence is Alt with the key.
  const int length = ReadKey(input.substr(1), more, key);
  if (length == 0) {
    return 0;
  }
  if (key != 0) {
    key |= kKeyAlt;
  }
  return length + 1;
}

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
// asked to end (SIGTERM).
constexpr int kQuitSignals[] = {SIGHUP, SIGTERM};

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

  void Write(std::string_view text) override;
  ConsoleSize GetSize() override;
  ConsoleKeys TakeKeys() override;
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

  // The quit signals' actions before, once they are handled.
  std::optional<std::array<struct sigaction, std::size(kQuitSignals)>>
      old_actions_;

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
  if (old_actions_.has_value()) {
    for (int i = 0; i < static_cast<int>(std::size(kQuitSignals)); ++i) {
      sigaction(kQuitSignals[i], &(*old_actions_)[i], nullptr);
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

  struct sigaction action = {};
  action.sa_handler = HandleSignal;
  sigemptyset(&action.sa_mask);
  std::array<struct sigaction, std::size(kQuitSignals)> old_actions;
  for (int i = 0; i < static_cast<int>(std::size(kQuitSignals)); ++i) {
    sigaction(kQuitSignals[i], &action, &old_actions[i]);
  }
  old_actions_ = old_actions;

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
      case kEscape:
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

ConsoleKeys PosixConsole::TakeKeys() {
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
  ConsoleKeys keys;
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
// Keys
//==============================================================================

int GetKeyCode(uint16_t key) {
  const int code = key & 0xFF;
  if ((key & kKeyCtrl) != 0 && code >= 1 && code <= 'Z' - 'A' + 1 &&
      code != kKeyBackspace && code != kKeyTab && code != kKeyEnter) {
    return 'a' + code - 1;
  }
  if (code >= 'A' && code <= 'Z') {
    return code - 'A' + 'a';
  }
  const int shifted_count = static_cast<int>(kShiftedCharacters.size());
  for (int i = 0; i < shifted_count; ++i) {
    if (kShiftedCharacters[i] == code) {
      return kUnshiftedCharacters[i];
    }
  }
  if ((code >= 'a' && code <= 'z') || code == ' ' ||
      kUnshiftedCharacters.find(static_cast<char>(code)) !=
          std::string_view::npos ||
      code == kKeyBackspace || code == kKeyTab || code == kKeyEnter ||
      code == kKeyEscape || code == kKeyDelete ||
      (code >= kKeyUp && code <= kKeyInsert) ||
      (code >= kKeyF1 && code <= kKeyF12)) {
    return code;
  }
  return 0;
}

void AddKeyTap(uint16_t key, std::vector<KeyChange>& changes) {
  const int code = GetKeyCode(key);
  if (code == 0) {
    return;
  }
  for (const ModifierKey& modifier : kModifierKeys) {
    if ((key & modifier.bit) != 0) {
      changes.push_back({.key = modifier.key, .held = true});
    }
  }
  changes.push_back({.key = code, .held = true});
  changes.push_back({.key = code, .held = false});
  for (const ModifierKey& modifier : kModifierKeys) {
    if ((key & modifier.bit) != 0) {
      changes.push_back({.key = modifier.key, .held = false});
    }
  }
}

//==============================================================================
// TerminalKeyReader
//==============================================================================

std::vector<uint16_t> TerminalKeyReader::ReadKeys(std::string_view input) {
  const bool more = !input.empty();
  pending_.append(input);
  std::vector<uint16_t> keys;
  const std::string_view bytes = pending_;
  const int size = static_cast<int>(bytes.size());
  int read = 0;
  while (read < size) {
    uint16_t key = 0;
    const int length = ReadKey(bytes.substr(read), more, key);
    if (length == 0) {
      break;
    }
    if (key != 0) {
      keys.push_back(key);
    }
    read += length;
  }
  pending_.erase(0, read);
  return keys;
}

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
