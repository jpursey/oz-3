// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_OZZY_OZZY_TESTING_H_
#define OZ3_OZZY_OZZY_TESTING_H_

#include "gb/base/context.h"

namespace oz3 {

// Returns `context` with a power request callback that ignores requests, as
// every Ozzy's context needs one.
gb::Context WithPowerCallback(gb::Context context = {});

}  // namespace oz3

#endif  // OZ3_OZZY_OZZY_TESTING_H_
