// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_CONSOLE_TEXT_H_
#define OZ3_OZZY_CONSOLE_TEXT_H_

#include <cstdint>
#include <string>
#include <string_view>

#include "oz3/devices/screen.h"

namespace oz3 {

// Returns the UTF-8 text of the Unicode character closest to the built-in
// character set's glyph for `character` (see GetDefaultScreenCharacters). Every
// character has text, which is one printable character, never a control
// character.
std::string_view GetCharacterText(uint8_t character);

// Returns the text that draws `picture` in a console from its top left, with
// virtual terminal sequences. It redraws only the rows whose cells differ from
// `last`, the picture drawn before, or clears the console and draws every row
// if `last` is null or a different size. It is empty if nothing changed.
//
// Each cell is drawn as its character's text (see GetCharacterText), with
// inverse, half intensity (faint), blink, the bottom line (underline), and the
// top line (overline). The left and right lines and the alternate character
// set can't be shown as text, so they are left out. The console's cursor is
// put at the picture's, with its shape and whether it blinks, or hidden if the
// picture's is off. The text expects the console's style (its colors and
// attributes) to be the default, and leaves it so.
std::string DrawConsoleText(const ScreenPicture& picture,
                            const ScreenPicture* last);

}  // namespace oz3

#endif  // OZ3_OZZY_CONSOLE_TEXT_H_
