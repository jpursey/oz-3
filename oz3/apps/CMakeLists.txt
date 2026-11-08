## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

# oz3ism doesn't link oz3_instruction_sets, so it builds even when the default
# instruction set's generated source doesn't compile.
set(oz3ism_SOURCE
  oz3ism.cc
)

set(oz3ism_DEPS
  absl::strings
  gb_base
  gb_cli
  gb_file
  gb_parse
  oz3_core
  oz3_tools
)

set(oz3ism_LIBS
  absl::check
  absl::log
)

gb_add_executable(oz3ism)

set(oz3asm_SOURCE
  oz3asm.cc
)

set(oz3asm_DEPS
  absl::algorithm_container
  absl::status
  absl::strings
  gb_base
  gb_cli
  gb_file
  gb_parse
  oz3_core
  oz3_instruction_sets
  oz3_tools
)

gb_add_executable(oz3asm)

set(ozzy_SOURCE
  ozzy_main.cc
)

set(ozzy_DEPS
  absl::civil_time
  absl::status
  absl::statusor
  absl::str_format
  absl::strings
  absl::time
  gb_base
  gb_cli
  gb_file
  oz3_core
  oz3_devices
  oz3_ozzy
)

gb_add_executable(ozzy)
