## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_core_SOURCE
  component.h
  core_types.h
  cpu_core.cc cpu_core.h
  cpu_core_config.cc cpu_core_config.h
  default_instruction_set.cc default_instruction_set.h
  execution_component.h
  instruction_def.cc instruction_def.h
  instruction_compiler.cc instruction_compiler.h
  instruction_set.cc instruction_set.h
  lockable.cc lockable.h
  memory_bank.cc memory_bank.h
  memory_bank_config.cc memory_bank_config.h
  memory_map.h
  microcode.cc microcode.h
  port.cc port.h
  processor.cc processor.h
  processor_config.cc processor_config.h
)
set(oz3_core_DEPS
  absl::memory
  absl::span
  gb_container
)
set(oz3_core_LIBS
  absl::check
  absl::log
)

set(oz3_core_TEST_SOURCE
  base_core_test.cc base_core_test.h
  cpu_core_test.cc
  default_instruction_set_test.cc
  instruction_compiler_test.cc
  instruction_set_test.cc
  instruction_test.h
  instruction_test_bits.cc
  instruction_test_branch.cc
  instruction_test_load_store.cc
  instruction_test_logic.cc
  instruction_test_math.cc
  instruction_test_misc.cc
  instruction_test_rotate.cc
  instruction_test_shift.cc
  lockable_test.cc
  memory_bank_test.cc
  port_test.cc
  processor_test.cc
)

gb_add_library(oz3_core)
