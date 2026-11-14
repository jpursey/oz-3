// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "absl/status/status.h"
#include "oz3/ozzy/console.h"

namespace oz3 {

absl::StatusOr<std::unique_ptr<Console>> Console::Create(ConsoleMode mode) {
  return absl::UnimplementedError("there is no console on this platform");
}

}  // namespace oz3
