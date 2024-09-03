## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_core_SOURCE
  memory_bank.cc memory_bank.h
  memory_map.h
  types.h
)
set(oz3_core_DEPS
  absl::memory
  absl::span
)
set(oz3_core_LIBS
  glog
)

set(oz3_core_TEST_SOURCE
  memory_bank_test.cc
)

gb_add_library(oz3_core)
