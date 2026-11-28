## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_microasm_SOURCE
  instruction_assembler.cc instruction_assembler.h
  instruction_def_exporter.cc instruction_def_exporter.h
)
set(oz3_microasm_DEPS
  absl::flat_hash_map
  absl::flat_hash_set
  absl::memory
  absl::statusor
  absl::strings
  gb_file
  gb_parse
  oz3_core
  oz3_syntax
)
set(oz3_microasm_LIBS
  absl::check
  absl::log
)

set(oz3_microasm_TEST_SOURCE
  instruction_assembler_test.cc
  instruction_def_exporter_test.cc
)

gb_add_library(oz3_microasm)
