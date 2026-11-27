// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_APP_FRONTEND_KEYS_H_
#define OZ3_OZZY_APP_FRONTEND_KEYS_H_

#include <cstdint>
#include <vector>

namespace oz3 {

// A key pressed or released, by its key code (see keyboard.h).
struct KeyChange {
  bool operator==(const KeyChange&) const = default;

  int key = 0;
  bool held = false;
};

// The keys a frontend took since they were last taken, each oldest first.
struct FrontendKeys {
  std::vector<uint16_t> typed;     // Typed key events (see keyboard.h).
  std::vector<KeyChange> changes;  // Keys pressed and released.
};

}  // namespace oz3

#endif  // OZ3_OZZY_APP_FRONTEND_KEYS_H_
