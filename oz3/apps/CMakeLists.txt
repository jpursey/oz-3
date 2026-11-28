## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

# oz3ism doesn't link oz3_instructions, so it builds even when the default
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
  oz3_microasm
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
  absl::statusor
  absl::strings
  gb_base
  gb_cli
  gb_file
  gb_parse
  oz3_asm
  oz3_core
  oz3_instructions
  oz3_microasm
  oz3_program
  oz3_syntax
)

gb_add_executable(oz3asm)

set(oz3dsk_SOURCE
  oz3dsk.cc
)

set(oz3dsk_DEPS
  absl::status
  absl::statusor
  absl::strings
  gb_base
  gb_cli
  gb_file
  oz3_disk
  oz3_tools
)

gb_add_executable(oz3dsk)

set(ozzyterm_SOURCE
  ozzyterm_main.cc
)

set(ozzyterm_DEPS
  gb_base
  gb_cli
  gb_file
  oz3_ozzy_app
  oz3_ozzy_term
)

gb_add_executable(ozzyterm)
