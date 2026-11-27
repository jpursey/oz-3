## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_program_SOURCE
  bare_program.cc bare_program.h
  program.cc program.h
  program_file.cc program_file.h
  program_listing.cc program_listing.h
  program_loader.cc program_loader.h
  program_map.cc program_map.h
  program_map_chunks.h
  program_map_file.cc program_map_file.h
)
set(oz3_program_DEPS
  absl::algorithm_container
  absl::flat_hash_map
  absl::flat_hash_set
  absl::span
  absl::status
  absl::statusor
  absl::str_format
  absl::strings
  gb_file
  oz3_core
)
set(oz3_program_LIBS
  absl::check
)

# The listing's tests assemble the programs they list, so they use tools.
set(oz3_program_TEST_SOURCE
  bare_program_test.cc
  program_file_test.cc
  program_listing_test.cc
  program_loader_test.cc
  program_map_file_test.cc
  program_map_test.cc
  program_test.cc
)
set(oz3_program_TEST_DEPS
  gb_parse
  oz3_core_testing
  oz3_tools
)

gb_add_library(oz3_program)
