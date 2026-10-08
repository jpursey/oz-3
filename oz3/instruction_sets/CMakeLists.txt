## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_instruction_sets_SOURCE
  default_instruction_set.cc default_instruction_set.h
  default_instruction_set.inc
  default_instruction_set.izm
)
set(oz3_instruction_sets_DEPS
  oz3_core
)

set(oz3_instruction_sets_TEST_SOURCE
  default_instruction_set_test.cc
  instruction_test.h
  instruction_test_bits.cc
  instruction_test_block.cc
  instruction_test_branch.cc
  instruction_test_core.cc
  instruction_test_interrupt.cc
  instruction_test_load_store.cc
  instruction_test_logic.cc
  instruction_test_math.cc
  instruction_test_misc.cc
  instruction_test_multiply.cc
  instruction_test_port.cc
  instruction_test_rotate.cc
  instruction_test_shift.cc
)
set(oz3_instruction_sets_TEST_DEPS
  absl::span
  absl::strings
  oz3_core_testing
)

gb_add_library(oz3_instruction_sets)
