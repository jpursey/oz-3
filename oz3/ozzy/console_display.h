// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_CONSOLE_DISPLAY_H_
#define OZ3_OZZY_CONSOLE_DISPLAY_H_

#include <cstdint>

#include "oz3/devices/screen.h"
#include "oz3/devices/teletype.h"
#include "oz3/ozzy/console.h"
#include "oz3/ozzy/ozzy.h"

namespace oz3 {

// Returns the mode of the Console that shows `ozzy`: a screen if it has one,
// or else lines.
ConsoleMode GetOzzyConsoleMode(Ozzy& ozzy);

// Shows an Ozzy in a Console, which is in the mode GetOzzyConsoleMode
// returns: its screen's picture, or without a screen, its teletype's output.
// With a screen, the teletype's output is dropped, as the console shows only
// one of them.
class ConsoleDisplay final {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Creates a display of `ozzy` in `console`, which both outlive it. An Ozzy
  // with neither a screen nor a teletype shows nothing, so the console says
  // so.
  ConsoleDisplay(Ozzy& ozzy, Console& console);
  ConsoleDisplay(const ConsoleDisplay&) = delete;
  ConsoleDisplay& operator=(const ConsoleDisplay&) = delete;
  ~ConsoleDisplay() = default;

  //----------------------------------------------------------------------------
  // Operations
  //----------------------------------------------------------------------------

  // Shows what changed since the last call: the screen's picture, if it was
  // refreshed, or the teletype's output. Called between Ozzy::Run calls.
  void Update();

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  Console& console_;
  Screen* const screen_;
  Teletype* const teletype_;

  // The screen's frame count when last drawn, which is 0 until the first
  // refresh, so nothing is drawn before it.
  int64_t frame_count_ = 0;

  // The last picture drawn, which is 0 by 0 before the first, so the first is
  // drawn whole.
  ScreenPicture last_;
};

}  // namespace oz3

#endif  // OZ3_OZZY_CONSOLE_DISPLAY_H_
