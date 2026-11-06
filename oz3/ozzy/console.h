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

// The host's text console, which the ozzy app runs an Ozzy in: it draws the
// screen's picture as text, and its key presses are the keyboard's.
//
// Each platform implements this in its own file (win_console.cc).
class Console {
 public:
  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  // Takes over the console: virtual terminal output in UTF-8, the alternate
  // screen buffer, and raw key input, so that every key (Ctrl+C too) is a key
  // press. Returns an error if the program has no console, or a Console
  // already exists, as there is only one. The console is given back as it
  // was when the Console is destroyed.
  static absl::StatusOr<std::unique_ptr<Console>> Create();

  Console(const Console&) = delete;
  Console& operator=(const Console&) = delete;
  virtual ~Console() = default;

  //----------------------------------------------------------------------------
  // Console interface
  //----------------------------------------------------------------------------

  // Writes `text`, which is UTF-8 and may hold virtual terminal sequences.
  virtual void Write(std::string_view text) = 0;

  // Returns the key events pressed since the last call, oldest first, in
  // Ozzy's key codes (see keyboard.h). It doesn't wait for keys.
  virtual std::vector<uint16_t> TakeKeys() = 0;

  // Returns whether the user asked to quit, with Ctrl+Break or by closing the
  // console's window. Once the window is closing, the program should finish
  // and exit promptly.
  virtual bool IsQuitRequested() const = 0;

 protected:
  Console() = default;
};

}  // namespace oz3

#endif  // OZ3_OZZY_CONSOLE_H_
