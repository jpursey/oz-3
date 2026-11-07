// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DEVICES_SCREEN_CHARACTERS_H_
#define OZ3_DEVICES_SCREEN_CHARACTERS_H_

#include <cstdint>

#include "absl/types/span.h"

namespace oz3 {

// Returns the built-in character set, kScreenCharacterSetSize words, which a
// screen starts with when ScreenSettings::characters is unset. Its glyphs are:
// - 00h to 1Fh: block and line graphics.
// - 20h to 7Eh: ASCII, and 7Fh a hatch.
// - 80h to BFh: sextants: 80h plus the sixths of the cell lit, where bit 0 is
//   the top left sixth, bit 1 the top right, and so on to bit 5, the bottom
//   right. The sixths are rows of 3, 4, and 3 pixels.
// - C0h to CAh: single lines: horizontal, vertical, down and right, down and
//   left, up and right, up and left, vertical and right, vertical and left,
//   down and horizontal, up and horizontal, and vertical and horizontal.
// - CBh to D5h: double lines, in the same order.
// - D6h to D9h: half lines: left, up, right, and down.
// - DAh to DDh: rounded corners: down and right, down and left, up and left,
//   and up and right.
// - DEh to E0h: light, medium, and dark shades.
// - E1h and E2h: the upper and lower halves.
// - E3h to E5h: diagonals: rising, falling, and crossed.
// - E6h to E9h: triangles filling the lower right, lower left, upper left,
//   and upper right.
// - EAh to EDh: arrows: left, up, right, and down.
// - EEh to F1h: spades, hearts, diamonds, and clubs.
// - F2h to FFh: a bullet, circle, ring, square, box, check mark, times,
//   divide, plus-minus, degree, pound, note, smile, and up-down arrow.
// The alternate glyphs, 256 to 511, are the same as the first 256.
absl::Span<const uint16_t> GetDefaultScreenCharacters();

}  // namespace oz3

#endif  // OZ3_DEVICES_SCREEN_CHARACTERS_H_
