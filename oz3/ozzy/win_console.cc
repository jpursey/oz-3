// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/win_console.h"

#include <atomic>
#include <cstdint>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "absl/strings/str_cat.h"
#include "absl/types/span.h"
#include "gb/base/unicode.h"
#include "gb/base/win_platform.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/console.h"

// MUST be last, as windows pollutes the global namespace with many macros.
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

namespace oz3 {

namespace {

//==============================================================================
// Keys
//==============================================================================

// The keys that are their virtual key, whatever character they type.
struct VirtualKey {
  uint16_t virtual_key;
  uint16_t key;
};
constexpr VirtualKey kVirtualKeys[] = {
    {VK_RETURN, kKeyEnter},  {VK_TAB, kKeyTab},       {VK_BACK, kKeyBackspace},
    {VK_ESCAPE, kKeyEscape}, {VK_DELETE, kKeyDelete}, {VK_UP, kKeyUp},
    {VK_DOWN, kKeyDown},     {VK_LEFT, kKeyLeft},     {VK_RIGHT, kKeyRight},
    {VK_HOME, kKeyHome},     {VK_END, kKeyEnd},       {VK_PRIOR, kKeyPageUp},
    {VK_NEXT, kKeyPageDown}, {VK_INSERT, kKeyInsert},
};

// Returns the key, without modifiers, for a key press, or 0 if it is none.
uint16_t GetKey(uint16_t virtual_key, char32_t character, bool ctrl) {
  for (const VirtualKey& entry : kVirtualKeys) {
    if (entry.virtual_key == virtual_key) {
      return entry.key;
    }
  }
  if (virtual_key >= VK_F1 && virtual_key <= VK_F12) {
    return static_cast<uint16_t>(kKeyF1 + (virtual_key - VK_F1));
  }
  if (character >= 0x20 && character < 0x7F) {
    return static_cast<uint16_t>(character);
  }

  // Ctrl with a letter is its control code. Windows types it as the
  // character, but not with Alt held too, so it comes from the virtual key.
  if (ctrl && virtual_key >= 'A' && virtual_key <= 'Z') {
    return static_cast<uint16_t>(virtual_key - 'A' + 1);
  }
  return 0;
}

// The main keys' rows, by scan code: the keys from the first scan code on, as
// the characters a US keyboard types without Shift, which are their key codes.
struct ScanCodeRow {
  uint16_t first_scan_code;
  std::string_view keys;
};
constexpr ScanCodeRow kScanCodeRows[] = {
    {0x02, "1234567890-="},
    {0x10, "qwertyuiop[]"},
    {0x1E, "asdfghjkl;'`"},
    {0x2B, "\\zxcvbnm,./"},
};

// F1 to F10's scan codes, which are consecutive.
constexpr uint16_t kScanCodeF1 = 0x3B;
constexpr uint16_t kScanCodeF10 = 0x44;

// The other keys with key codes, by scan code, without ENHANCED_KEY and with
// it.
struct ScanCode {
  uint16_t scan_code;
  uint16_t key;
};
constexpr ScanCode kScanCodes[] = {
    {0x01, kKeyEscape},     {0x0E, kKeyBackspace}, {0x0F, kKeyTab},
    {0x1C, kKeyEnter},      {0x1D, kKeyLeftCtrl},  {0x2A, kKeyLeftShift},
    {0x36, kKeyRightShift}, {0x38, kKeyLeftAlt},   {0x39, ' '},
    {0x57, kKeyF11},        {0x58, kKeyF12},
};
constexpr ScanCode kExtendedScanCodes[] = {
    {0x1D, kKeyRightCtrl}, {0x38, kKeyRightAlt}, {0x47, kKeyHome},
    {0x48, kKeyUp},        {0x49, kKeyPageUp},   {0x4B, kKeyLeft},
    {0x4D, kKeyRight},     {0x4F, kKeyEnd},      {0x50, kKeyDown},
    {0x51, kKeyPageDown},  {0x52, kKeyInsert},   {0x53, kKeyDelete},
};

// The keys held as the control key state says, rather than by their scan codes.
struct ControlKey {
  DWORD flag;
  int key;
};
constexpr ControlKey kControlKeys[] = {
    {LEFT_CTRL_PRESSED, kKeyLeftCtrl},
    {RIGHT_CTRL_PRESSED, kKeyRightCtrl},
    {LEFT_ALT_PRESSED, kKeyLeftAlt},
    {RIGHT_ALT_PRESSED, kKeyRightAlt},
};

// Returns whether the key with code `key` is held as the control key state
// says.
bool IsControlKey(int key) {
  for (const ControlKey& control_key : kControlKeys) {
    if (control_key.key == key) {
      return true;
    }
  }
  return false;
}

// Returns the key in `scan_codes` with `scan_code`, or 0 if there is none.
int FindScanCode(absl::Span<const ScanCode> scan_codes, uint16_t scan_code) {
  for (const ScanCode& entry : scan_codes) {
    if (entry.scan_code == scan_code) {
      return entry.key;
    }
  }
  return 0;
}

//==============================================================================
// WinConsole
//==============================================================================

// Whether the user asked to quit, which HandleControl sets on a thread of its
// own.
std::atomic<bool> g_quit_requested = false;

// Whether a WinConsole exists.
bool g_console_exists = false;

// Handles the console's control signals, on a thread the system starts for
// each. Ctrl+C is a key press, as input isn't processed.
BOOL WINAPI HandleControl(DWORD control_type) {
  switch (control_type) {
    case CTRL_BREAK_EVENT:
      g_quit_requested = true;
      return TRUE;
    case CTRL_CLOSE_EVENT:
      // The system ends the program as soon as this returns, so it never
      // does: the program finishes and exits on its own, which ends this
      // thread too. If it takes more than a few seconds, the system ends it
      // anyway.
      g_quit_requested = true;
      Sleep(INFINITE);
      return TRUE;
  }
  return FALSE;
}

// Returns an error for a Windows function that failed to do `action`.
absl::Status WindowsError(std::string_view action) {
  return absl::InternalError(
      absl::StrCat("failed to ", action, ": ", gb::GetWindowsError()));
}

class WinConsole final : public Console {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a console that gives back the standard `input` and `output` with
  // their modes as they were, `input_mode` and `output_mode`.
  WinConsole(HANDLE input, DWORD input_mode, HANDLE output, DWORD output_mode);
  ~WinConsole() override;

  // Takes over the console, as Console::Create describes.
  absl::Status TakeOver(ConsoleMode mode);

  //----------------------------------------------------------------------------
  // Console implementation
  //----------------------------------------------------------------------------

  void Write(std::string_view text) override;
  ConsoleKeys TakeKeys() override;
  bool IsQuitRequested() const override { return g_quit_requested; }

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  const HANDLE input_;
  const DWORD input_mode_;
  const HANDLE output_;
  const DWORD output_mode_;
  bool alternate_buffer_ = false;  // Whether it is in the alternate buffer.
  ConsoleKeyState key_state_;      // The keys reported held.
};

WinConsole::WinConsole(HANDLE input, DWORD input_mode, HANDLE output,
                       DWORD output_mode)
    : input_(input),
      input_mode_(input_mode),
      output_(output),
      output_mode_(output_mode) {
  g_console_exists = true;
  g_quit_requested = false;
}

WinConsole::~WinConsole() {
  if (alternate_buffer_) {
    // The default style, and the cursor's default shape, shown, before going
    // back to the main screen buffer and its cursor.
    Write("\x1B[0m\x1B[0 q\x1B[?25h\x1B[?1049l");
  }

  // Keys not taken are dropped, rather than left for whatever runs next.
  FlushConsoleInputBuffer(input_);
  SetConsoleCtrlHandler(HandleControl, FALSE);
  SetConsoleMode(output_, output_mode_);

  // Quick edit only changes with the extended flags, which the mode may lack.
  SetConsoleMode(input_, input_mode_ | ENABLE_EXTENDED_FLAGS);
  g_console_exists = false;
}

absl::Status WinConsole::TakeOver(ConsoleMode mode) {
  // Keys are read as key events rather than lines, without echoing them, and
  // Ctrl+C is a key. Quick edit is off, so a click doesn't pause the program.
  if (!SetConsoleMode(input_, ENABLE_EXTENDED_FLAGS)) {
    return WindowsError("set the console's input mode");
  }

  // A line feed only moves down, as on a teletype. Without wrapping, a row
  // wider than the console is cut off at its edge, rather than continuing on
  // the next.
  DWORD output_mode = ENABLE_PROCESSED_OUTPUT |
                      ENABLE_VIRTUAL_TERMINAL_PROCESSING |
                      DISABLE_NEWLINE_AUTO_RETURN;
  if (mode == ConsoleMode::kLines) {
    output_mode |= ENABLE_WRAP_AT_EOL_OUTPUT;
  }
  if (!SetConsoleMode(output_, output_mode)) {
    return WindowsError("turn on virtual terminal sequences");
  }
  if (!SetConsoleCtrlHandler(HandleControl, TRUE)) {
    return WindowsError("handle Ctrl+Break");
  }
  if (mode == ConsoleMode::kScreen) {
    Write("\x1B[?1049h");
    alternate_buffer_ = true;
  }
  return absl::OkStatus();
}

void WinConsole::Write(std::string_view text) {
  // The console is written in UTF-16, which it takes whatever its code page.
  const std::u16string wide_text = gb::ToUtf16(text);
  const wchar_t* data = reinterpret_cast<const wchar_t*>(wide_text.data());
  DWORD size = static_cast<DWORD>(wide_text.size());
  while (size > 0) {
    DWORD written = 0;
    if (!WriteConsoleW(output_, data, size, &written, nullptr) ||
        written == 0) {
      return;
    }
    data += written;
    size -= written;
  }
}

ConsoleKeys WinConsole::TakeKeys() {
  ConsoleKeys keys;
  constexpr DWORD kMaxRecords = 64;
  INPUT_RECORD records[kMaxRecords];
  DWORD count = 0;
  while (GetNumberOfConsoleInputEvents(input_, &count) && count > 0 &&
         ReadConsoleInputW(input_, records, kMaxRecords, &count)) {
    for (const INPUT_RECORD& record : std::span(records, count)) {
      if (record.EventType == FOCUS_EVENT) {
        if (!record.Event.FocusEvent.bSetFocus) {
          key_state_.ReleaseKeys(keys.changes);
        }
        continue;
      }
      if (record.EventType != KEY_EVENT) {
        continue;
      }

      // Every key with a code is a change, the keys that quit too.
      const KEY_EVENT_RECORD& event = record.Event.KeyEvent;
      const bool key_down = event.bKeyDown != FALSE;
      key_state_.AddKeyEvent(event.wVirtualScanCode, key_down,
                             event.dwControlKeyState, keys.changes);
      if (!key_down) {
        continue;
      }
      const uint16_t key =
          GetKeyEvent(event.wVirtualKeyCode, event.uChar.UnicodeChar,
                      event.dwControlKeyState);
      if (IsQuitKey(key)) {
        g_quit_requested = true;
      } else if (key != 0) {
        keys.typed.insert(keys.typed.end(), event.wRepeatCount, key);
      }
    }
  }
  return keys;
}

}  // namespace

//==============================================================================
// Keys
//==============================================================================

uint16_t GetKeyEvent(uint16_t virtual_key, char32_t character,
                     uint32_t control_keys) {
  const bool ctrl =
      (control_keys & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)) != 0;
  uint16_t key = GetKey(virtual_key, character, ctrl);
  if (key == 0) {
    return 0;
  }
  if ((control_keys & SHIFT_PRESSED) != 0) {
    key |= kKeyShift;
  }
  if (ctrl) {
    key |= kKeyCtrl;
  }
  if ((control_keys & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED)) != 0) {
    key |= kKeyAlt;
  }
  return key;
}

bool IsQuitKey(uint16_t key) {
  constexpr uint16_t kCtrlQ = 'Q' - 'A' + 1;
  return (key & ~kKeyShift) == (kCtrlQ | kKeyCtrl | kKeyAlt);
}

int GetKeyCode(uint16_t scan_code, bool extended) {
  if (extended) {
    return FindScanCode(kExtendedScanCodes, scan_code);
  }
  for (const ScanCodeRow& row : kScanCodeRows) {
    const int index = scan_code - row.first_scan_code;
    if (index >= 0 && index < static_cast<int>(row.keys.size())) {
      return row.keys[index];
    }
  }
  if (scan_code >= kScanCodeF1 && scan_code <= kScanCodeF10) {
    return kKeyF1 + (scan_code - kScanCodeF1);
  }
  return FindScanCode(kScanCodes, scan_code);
}

//==============================================================================
// ConsoleKeyState
//==============================================================================

void ConsoleKeyState::AddKeyEvent(uint16_t scan_code, bool key_down,
                                  uint32_t control_keys,
                                  std::vector<KeyChange>& changes) {
  // Ctrl and Alt first, as they are held with the key.
  for (const ControlKey& control_key : kControlKeys) {
    ChangeKey(control_key.key, (control_keys & control_key.flag) != 0, changes);
  }
  const int key = GetKeyCode(scan_code, (control_keys & ENHANCED_KEY) != 0);
  if (!IsControlKey(key)) {
    ChangeKey(key, key_down, changes);
  }
}

void ConsoleKeyState::ReleaseKeys(std::vector<KeyChange>& changes) {
  for (int key = 0; key < Keyboard::kKeyStateKeys; ++key) {
    ChangeKey(key, false, changes);
  }
}

void ConsoleKeyState::ChangeKey(int key, bool held,
                                std::vector<KeyChange>& changes) {
  if (key == 0 || held_[key] == held) {
    return;
  }
  held_[key] = held;
  changes.push_back({.key = key, .held = held});
}

//==============================================================================
// Console
//==============================================================================

absl::StatusOr<std::unique_ptr<Console>> Console::Create(ConsoleMode mode) {
  if (g_console_exists) {
    return absl::FailedPreconditionError("a console already exists");
  }
  HANDLE input = GetStdHandle(STD_INPUT_HANDLE);
  HANDLE output = GetStdHandle(STD_OUTPUT_HANDLE);
  DWORD input_mode = 0;
  DWORD output_mode = 0;
  if (!GetConsoleMode(input, &input_mode) ||
      !GetConsoleMode(output, &output_mode)) {
    return absl::FailedPreconditionError(
        "the standard input and output must be a console");
  }

  // The console gives back what it changed, even if taking over fails.
  auto console =
      std::make_unique<WinConsole>(input, input_mode, output, output_mode);
  if (absl::Status status = console->TakeOver(mode); !status.ok()) {
    return status;
  }
  return console;
}

}  // namespace oz3
