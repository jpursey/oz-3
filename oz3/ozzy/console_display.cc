// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/console_display.h"

#include <string>

#include "oz3/ozzy/console_text.h"

namespace oz3 {

ConsoleMode GetOzzyConsoleMode(Ozzy& ozzy) {
  return ozzy.GetScreen() != nullptr ? ConsoleMode::kScreen
                                     : ConsoleMode::kLines;
}

ConsoleDisplay::ConsoleDisplay(Ozzy& ozzy, Console& console)
    : console_(console),
      screen_(ozzy.GetScreen()),
      teletype_(ozzy.GetTeletype()) {
  if (screen_ == nullptr && teletype_ == nullptr) {
    console_.Write("This Ozzy has neither a screen nor a teletype.\r\n");
  }
}

void ConsoleDisplay::Update() {
  // The teletype's output is taken even when it is dropped, so it doesn't
  // fill up and stall the program writing it.
  if (teletype_ != nullptr) {
    const std::string output = teletype_->TakeOutput();
    if (screen_ == nullptr) {
      console_.Write(GetTeletypeText(output));
    }
  }

  // The picture changes only at a refresh.
  if (screen_ == nullptr || screen_->GetFrameCount() == frame_count_) {
    return;
  }
  frame_count_ = screen_->GetFrameCount();
  const ScreenPicture& picture = screen_->GetPicture();
  console_.Write(DrawConsoleText(picture, &last_));
  last_ = picture;
}

}  // namespace oz3
