## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_asm_SOURCE
  expression.cc expression.h
  program_assembler.cc program_assembler.h
  program_grammar.cc program_grammar.h
  program_macro.cc program_macro.h
)
set(oz3_asm_DEPS
  absl::algorithm_container
  absl::flat_hash_map
  absl::flat_hash_set
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::strings
  gb_file
  gb_parse
  oz3_core
  oz3_program
  oz3_syntax
)
set(oz3_asm_LIBS
  absl::check
)

# The assembler's and grammar's tests build their instruction sets with
# InstructionAssembler, so they use microasm.
set(oz3_asm_TEST_SOURCE
  expression_test.cc
  program_assembler_test.cc
  program_grammar_test.cc
  program_macro_test.cc
)
set(oz3_asm_TEST_DEPS
  oz3_microasm
)

gb_add_library(oz3_asm)
