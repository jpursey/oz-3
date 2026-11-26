// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_TERM_TERMINAL_KEYS_H_
#define OZ3_OZZY_TERM_TERMINAL_KEYS_H_

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "oz3/ozzy/term/console.h"

namespace oz3 {

// Returns the key code (see keyboard.h) of the key that types the typed key
// event `key` on a US keyboard, or 0 for one without a key code. Characters
// are their keys whether Shift typed them or not: 'A' is A's key, and '!' is
// 1's. Ctrl with A to Z is its letter's key, but for the control codes that
// are keys of their own, Backspace, Tab, and Enter.
int GetTypedKeyCode(uint16_t key);

// Adds a tap of the key that types the typed key event `key` to `changes`, as
// a terminal doesn't say when keys are pressed and released: the left Shift,
// Ctrl, and Alt for the modifiers `key` holds are pressed, then its key (see
// GetTypedKeyCode) is pressed and released, and then the modifiers are
// released. Adds nothing for a key without a key code.
void AddKeyTap(uint16_t key, std::vector<KeyChange>& changes);

// Reads typed key events (see keyboard.h) from the bytes a terminal sends for
// the keys typed in it, with its input in raw mode.
//
// Characters are their ASCII codes, and Ctrl with A to Z the control codes,
// with Ctrl. Other bytes outside ASCII, such as UTF-8's, aren't keys. A
// character holds no modifiers, as the terminal sends the character the keys
// made, so 'A' is 'A' without Shift. The named keys (arrows, Home, End, Page
// Up, Page Down, Insert, Delete, and F1 to F12) are the escape sequences xterm
// and the terminals like it send, with the Shift, Ctrl, and Alt they hold.
// Escape before a key is Alt with that key. Backspace is DEL (7Fh), and BS
// (08h) is Ctrl+Backspace, which Ctrl+H is too. Ctrl with Space is NUL.
class TerminalKeyReader {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  TerminalKeyReader() = default;
  TerminalKeyReader(const TerminalKeyReader&) = delete;
  TerminalKeyReader& operator=(const TerminalKeyReader&) = delete;
  ~TerminalKeyReader() = default;

  //----------------------------------------------------------------------------
  // Keys
  //----------------------------------------------------------------------------

  // Returns the typed key events in `input`, the bytes the terminal sent since
  // the last call, oldest first.
  //
  // A key whose sequence `input` ends partway through waits for the next
  // call's bytes. If the next call has none, the bytes are all the terminal
  // sent, so they are read as they are: Escape alone is the Escape key, a call
  // later.
  std::vector<uint16_t> ReadKeys(std::string_view input);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  std::string pending_;  // The bytes of a key not yet read whole.
};

}  // namespace oz3

#endif  // OZ3_OZZY_TERM_TERMINAL_KEYS_H_
