// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_POSIX_CONSOLE_H_
#define OZ3_OZZY_POSIX_CONSOLE_H_

#include <memory>

#include "absl/status/statusor.h"
#include "oz3/ozzy/console.h"

namespace oz3 {

// Takes over the terminal that `input` and `output` (file descriptors) are, as
// Console::Create does for the standard input and output, which it calls this
// with. They must be the same terminal, as its mode is set through `input`,
// and stay open until the Console is destroyed.
absl::StatusOr<std::unique_ptr<Console>> CreateTerminalConsole(
    int input, int output, ConsoleMode mode);

}  // namespace oz3

#endif  // OZ3_OZZY_POSIX_CONSOLE_H_
