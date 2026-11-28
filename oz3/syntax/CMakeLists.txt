## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_syntax_SOURCE
  instruction_set_syntax.cc instruction_set_syntax.h
  program_lexer.cc program_lexer.h
  source_pattern.cc source_pattern.h
)
set(oz3_syntax_DEPS
  absl::algorithm_container
  absl::flat_hash_map
  absl::flat_hash_set
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::str_format
  absl::strings
  gb_base
  gb_parse
  oz3_core
)
set(oz3_syntax_LIBS
  absl::check
)

# The instruction set syntax's tests assemble the instruction sets they read,
# so they use microasm.
set(oz3_syntax_TEST_SOURCE
  instruction_set_syntax_test.cc
  program_lexer_test.cc
  source_pattern_test.cc
)
set(oz3_syntax_TEST_DEPS
  oz3_microasm
)

gb_add_library(oz3_syntax)
