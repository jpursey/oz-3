## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(GB_INCLUDE_DIRS "${CURRENT_DIR}")

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  add_compile_options(-Werror)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
  add_compile_options(/WX)
endif()

add_subdirectory(core)
