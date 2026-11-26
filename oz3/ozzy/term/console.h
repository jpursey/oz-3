// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_TERM_CONSOLE_H_
#define OZ3_OZZY_TERM_CONSOLE_H_

#include <cstdint>
#include <memory>
#include <string_view>

#include "absl/status/statusor.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/app/frontend.h"

namespace oz3 {

// How a Console shows what is written to it.
enum class ConsoleMode {
  // A screen, drawn in the console's alternate screen buffer, so the
  // console's own text is back once the Console is destroyed. Rows don't wrap,
  // so a row wider than the console is cut off at its edge.
  kScreen,

  // Lines, as a teletype prints them, in the console's own buffer, so they are
  // still there once the Console is destroyed. Lines wrap at the console's
  // edge. A last line left unended is ended when the Console is destroyed, so
  // what writes to the console next starts on a line of its own.
  kLines,
};

// The size of a console's window, in characters.
struct ConsoleSize {
  bool operator==(const ConsoleSize&) const = default;

  int columns = 0;
  int rows = 0;
};

// Returns whether the typed key event `key` is Ctrl+Alt+Q, which quits rather
// than going to the Ozzy. Shift may be held too. AltGr is Ctrl+Alt, so where
// AltGr with Q types a character (such as @), that character is typed rather
// than quitting.
inline bool IsQuitKey(uint16_t key) {
  constexpr uint16_t kCtrlQ = 'Q' - 'A' + 1;
  return (key & ~kKeyShift) == (kCtrlQ | kKeyCtrl | kKeyAlt);
}

// The host's text console, which the ozzy app runs an Ozzy in: it draws the
// screen's picture as text, and its key presses are the keyboard's.
//
// Each platform implements this in its own file: win_console.cc for the
// Windows console, and posix_console.cc for a terminal on any other platform.
class Console {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Takes over the console, to show text in `mode`: virtual terminal output
  // in UTF-8, and raw key input, so that every key (Ctrl+C too) is a key
  // press. Returns an error if the standard input and output aren't a
  // console, or a Console already exists, as there is only one. The console is
  // given back as it was when the Console is destroyed.
  static absl::StatusOr<std::unique_ptr<Console>> Create(ConsoleMode mode);

  Console(const Console&) = delete;
  Console& operator=(const Console&) = delete;
  virtual ~Console() = default;

  //----------------------------------------------------------------------------
  // Console interface
  //----------------------------------------------------------------------------

  // Writes `text`, which is UTF-8 and may hold virtual terminal sequences.
  virtual void Write(std::string_view text) = 0;

  // Returns the size of the console's window, which the user may change at any
  // time.
  virtual ConsoleSize GetSize() = 0;

  // Returns the keys since the last call: the typed key events, and each key
  // pressed and released, by where it is. A key held when the console loses
  // focus is released then, as its release goes elsewhere. A terminal sends
  // only what is typed, so there each typed key with a key code is pressed and
  // released at once, as a tap. It doesn't wait for keys.
  virtual FrontendKeys TakeKeys() = 0;

  // Returns whether the user asked to quit, with Ctrl+Alt+Q (see IsQuitKey),
  // Ctrl+Break in a Windows console, or by closing the console's window, or
  // the program was asked to end by a signal (such as SIGTERM). The keys that
  // quit aren't typed key events, but are pressed and released as other keys
  // are. Once the window is closing, the program should finish and exit
  // promptly.
  virtual bool IsQuitRequested() const = 0;

 protected:
  Console() = default;
};

}  // namespace oz3

#endif  // OZ3_OZZY_TERM_CONSOLE_H_
