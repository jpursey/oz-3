// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_TERM_FAKE_CONSOLE_H_
#define OZ3_OZZY_TERM_FAKE_CONSOLE_H_

#include <string>
#include <string_view>
#include <utility>

#include "absl/strings/str_cat.h"
#include "oz3/devices/screen.h"
#include "oz3/ozzy/app/frontend.h"
#include "oz3/ozzy/term/console.h"

namespace oz3 {

// The size of the default Ozzy's screen.
inline const ConsoleSize kDefaultScreenSize = {
    .columns = ScreenSettings().columns, .rows = ScreenSettings().rows};

// A console for tests, which keeps what is written to it, and gives the keys
// and quit a test sets. It is the size of the default Ozzy's screen unless it
// is set.
class FakeConsole final : public Console {
 public:
  FakeConsole() = default;

  // Returns what was written since the last call.
  std::string TakeText() { return std::exchange(text_, {}); }

  void SetSize(ConsoleSize size) { size_ = size; }

  // Sets the keys the next TakeKeys returns.
  void SetKeys(FrontendKeys keys) { keys_ = std::move(keys); }

  void SetQuitRequested() { quit_requested_ = true; }

  void Write(std::string_view text) override { absl::StrAppend(&text_, text); }
  ConsoleSize GetSize() override { return size_; }
  FrontendKeys TakeKeys() override { return std::exchange(keys_, {}); }
  bool IsQuitRequested() const override { return quit_requested_; }

 private:
  std::string text_;
  ConsoleSize size_ = kDefaultScreenSize;
  FrontendKeys keys_;
  bool quit_requested_ = false;
};

}  // namespace oz3

#endif  // OZ3_OZZY_TERM_FAKE_CONSOLE_H_
