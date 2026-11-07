// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_WIN_CONSOLE_H_
#define OZ3_OZZY_WIN_CONSOLE_H_

#include <bitset>
#include <cstdint>
#include <vector>

#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/console.h"

namespace oz3 {

// Returns the typed key event (see keyboard.h) for a Windows console key press,
// or 0 if it is none, such as a modifier key alone or a character outside
// ASCII. `virtual_key`, `character`, and `control_keys` are the key event
// record's virtual key code, Unicode character, and control key state.
//
// Every key event holds the modifiers held with it, so Shift with A is 'A'
// with Shift. Ctrl with a letter is its control code, 01h to 1Ah, even when
// Alt is held too, unless they type a character, as AltGr does.
uint16_t GetKeyEvent(uint16_t virtual_key, char32_t character,
                     uint32_t control_keys);

// Returns whether the typed key event `key` (from GetKeyEvent) is Ctrl+Alt+Q,
// which quits rather than going to the Ozzy, as many keyboards have no Break
// key for Ctrl+Break. Shift may be held too. AltGr with Q types a character on
// some layouts (such as @), which is that character rather than Ctrl+Alt+Q.
bool IsQuitKey(uint16_t key);

// Returns the key code (see keyboard.h) of a Windows console key, or 0 for a
// key without one, such as the keypad's. `scan_code` is the key event record's
// virtual scan code, and `extended` whether its control key state has
// ENHANCED_KEY, which tells the arrows and the six keys above them, Right Ctrl,
// Right Alt, and the keypad's Enter and / from the keys that share their scan
// codes. A scan code is where the key is, whatever the layout, so the key code
// is too.
int GetKeyCode(uint16_t scan_code, bool extended);

// The keys held, from a Windows console's key event records, as the changes
// to them.
//
// A key is held from its key down to its key up, by its key code from
// GetKeyCode, except Ctrl and Alt, which are held as each record's control key
// state says. Windows Terminal gives Right Alt's key up without ENHANCED_KEY,
// so by its scan code it would be Left Alt's, but its control key state is
// right, and is in every record, so a Ctrl or Alt released while the console
// didn't have focus is caught up on the next key. The control key state has no
// left and right Shift, so Shift is held by its scan code.
class ConsoleKeyState {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  ConsoleKeyState() = default;
  ConsoleKeyState(const ConsoleKeyState&) = delete;
  ConsoleKeyState& operator=(const ConsoleKeyState&) = delete;
  ~ConsoleKeyState() = default;

  //----------------------------------------------------------------------------
  // Keys
  //----------------------------------------------------------------------------

  // Adds the changes from a key event record to `changes`: its key, and Ctrl
  // and Alt from its control key state. `scan_code`, `key_down`, and
  // `control_keys` are the record's virtual scan code, whether it is a key
  // down, and its control key state. A key down for a key already held is a
  // repeat, and a key up for a key not held is one released before, so
  // neither is a change.
  void AddKeyEvent(uint16_t scan_code, bool key_down, uint32_t control_keys,
                   std::vector<KeyChange>& changes);

  // Releases every key held, adding the changes to `changes`, for when the
  // console loses focus, as it doesn't get the key ups of keys released then.
  void ReleaseKeys(std::vector<KeyChange>& changes);

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  // Adds a change to `changes` if the key with code `key` (0 for none) isn't
  // already `held`.
  void ChangeKey(int key, bool held, std::vector<KeyChange>& changes);

  std::bitset<Keyboard::kKeyStateKeys> held_;  // By key code.
};

}  // namespace oz3

#endif  // OZ3_OZZY_WIN_CONSOLE_H_
