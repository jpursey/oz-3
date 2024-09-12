## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_core_SOURCE
  component.cc component.h
  core_types.h
  cpu_core.cc cpu_core.h
  cpu_core_config.cc cpu_core_config.h
  execution_component.h
  instruction_set.cc instruction_set.h
  memory_bank.cc memory_bank.h
  memory_bank_config.cc memory_bank_config.h
  memory_map.h
  microcode.cc microcode.h
  processor.cc processor.h
  processor_config.cc processor_config.h
)
set(oz3_core_DEPS
  absl::memory
  absl::span
  gb_container
)
set(oz3_core_LIBS
  glog
)

set(oz3_core_TEST_SOURCE
  component_test.cc
  cpu_core_test.cc
  memory_bank_test.cc
  microcode_test.cc
  processor_test.cc
)

gb_add_library(oz3_core)
