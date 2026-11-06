// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_WIN_CONSOLE_H_
#define OZ3_OZZY_WIN_CONSOLE_H_

#include <cstdint>

namespace oz3 {

// Returns the Ozzy key event (see keyboard.h) for a Windows console key press,
// or 0 if it is none, such as a modifier key alone or a character outside
// ASCII. `virtual_key`, `character`, and `control_keys` are the key event
// record's virtual key code, Unicode character, and control key state.
//
// Every key event holds the modifiers held with it, so Shift with A is 'A'
// with Shift. Ctrl with a letter is its control code, 01h to 1Ah, even when
// Alt is held too, unless they type a character, as AltGr does.
uint16_t GetOzzyKey(uint16_t virtual_key, char32_t character,
                    uint32_t control_keys);

// Returns whether the Ozzy key event `key` (from GetOzzyKey) is Ctrl+Alt+Q,
// which quits rather than going to the Ozzy, as many keyboards have no Break
// key for Ctrl+Break. Shift may be held too. AltGr with Q types a character on
// some layouts (such as @), which is that character rather than Ctrl+Alt+Q.
bool IsQuitKey(uint16_t key);

}  // namespace oz3

#endif  // OZ3_OZZY_WIN_CONSOLE_H_
