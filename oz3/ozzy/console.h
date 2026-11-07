// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_CONSOLE_H_
#define OZ3_OZZY_CONSOLE_H_

#include <cstdint>
#include <memory>
#include <string_view>
#include <vector>

#include "absl/status/statusor.h"

namespace oz3 {

// How a Console shows what is written to it.
enum class ConsoleMode {
  // A screen, drawn in the console's alternate screen buffer, so the
  // console's own text is back once the Console is destroyed. Rows don't wrap,
  // so a row wider than the console is cut off at its edge.
  kScreen,

  // Lines, as a teletype prints them, in the console's own buffer, so they are
  // still there once the Console is destroyed. Lines wrap at the console's
  // edge.
  kLines,
};

// A key pressed or released, by its key code (see keyboard.h).
struct KeyChange {
  bool operator==(const KeyChange&) const = default;

  int key = 0;
  bool held = false;
};

// The keys a Console took since they were last taken, each oldest first.
struct ConsoleKeys {
  std::vector<uint16_t> typed;     // Typed key events (see keyboard.h).
  std::vector<KeyChange> changes;  // Keys pressed and released.
};

// The host's text console, which the ozzy app runs an Ozzy in: it draws the
// screen's picture as text, and its key presses are the keyboard's.
//
// Each platform implements this in its own file (win_console.cc).
class Console {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Takes over the console, to show text in `mode`: virtual terminal output
  // in UTF-8, and raw key input, so that every key (Ctrl+C too) is a key
  // press. Returns an error if the program has no console, or a Console
  // already exists, as there is only one. The console is given back as it
  // was when the Console is destroyed.
  static absl::StatusOr<std::unique_ptr<Console>> Create(ConsoleMode mode);

  Console(const Console&) = delete;
  Console& operator=(const Console&) = delete;
  virtual ~Console() = default;

  //----------------------------------------------------------------------------
  // Console interface
  //----------------------------------------------------------------------------

  // Writes `text`, which is UTF-8 and may hold virtual terminal sequences.
  virtual void Write(std::string_view text) = 0;

  // Returns the keys since the last call: the typed key events, and each key
  // pressed and released, by where it is. A key held when the console loses
  // focus is released then, as its release goes elsewhere. It doesn't wait for
  // keys.
  virtual ConsoleKeys TakeKeys() = 0;

  // Returns whether the user asked to quit, with Ctrl+Break, Ctrl+Alt+Q, or by
  // closing the console's window. The keys that quit aren't typed key events,
  // but are pressed and released as other keys are. Once the window is
  // closing, the program should finish and exit promptly.
  virtual bool IsQuitRequested() const = 0;

 protected:
  Console() = default;
};

}  // namespace oz3

#endif  // OZ3_OZZY_CONSOLE_H_
