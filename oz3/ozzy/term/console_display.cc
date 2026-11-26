// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/console_display.h"

#include <cstdint>
#include <string>

#include "absl/strings/str_cat.h"
#include "oz3/ozzy/term/console.h"
#include "oz3/ozzy/term/console_text.h"

namespace oz3 {

ConsoleMode GetOzzyConsoleMode(const OzzySettings& settings) {
  return settings.screen.has_value() ? ConsoleMode::kScreen
                                     : ConsoleMode::kLines;
}

void ShowOzzyShutDown(Console& console, ConsoleMode mode, int exit_code) {
  const std::string lines[] = {
      absl::StrCat("Ozzy shut down, exit code ", exit_code, "."),
      "Ctrl+Alt+Q quits.",
  };
  if (mode == ConsoleMode::kScreen) {
    console.Write(DrawConsoleLines(lines));
    return;
  }

  // The teletype's last line may be unended.
  console.Write(absl::StrCat("\r\n", lines[0], " ", lines[1], "\r\n"));
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

  // Nothing is drawn before the first refresh.
  if (screen_ == nullptr) {
    return;
  }
  const int64_t frame_count = screen_->GetFrameCount();
  if (frame_count == 0) {
    return;
  }
  const ScreenPicture& picture = screen_->GetPicture();

  // Whatever is shown is drawn whole again when the console resizes, as the
  // console may lose or move what was drawn.
  const ConsoleSize size = console_.GetSize();
  const bool resized = size != size_;
  size_ = size;
  if (size.columns < picture.columns || size.rows < picture.rows) {
    if (resized || !message_shown_) {
      ShowTooSmall(size, picture);
      message_shown_ = true;
    }
    return;
  }

  // The picture changes only at a refresh, but it is drawn whole over the
  // message, or after a resize.
  const bool whole = resized || message_shown_;
  if (!whole && frame_count == frame_count_) {
    return;
  }
  frame_count_ = frame_count;
  console_.Write(DrawConsoleText(picture, whole ? nullptr : &last_));
  last_ = picture;
  message_shown_ = false;
}

void ConsoleDisplay::ShowTooSmall(ConsoleSize size,
                                  const ScreenPicture& picture) {
  // The lines are short, as the console doesn't wrap them.
  const std::string lines[] = {
      absl::StrCat("The console is ", size.columns, " by ", size.rows, ","),
      absl::StrCat("but the screen is ", picture.columns, " by ", picture.rows,
                   "."),
      "Make the console bigger to",
      "see the screen.",
  };
  console_.Write(DrawConsoleLines(lines));
}

}  // namespace oz3
