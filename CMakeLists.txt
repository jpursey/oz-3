## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

cmake_minimum_required (VERSION 3.8)

###############################################################################
## Set project configuration
###############################################################################

project(OZ3 C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

###############################################################################
# Game Bits configuration
###############################################################################

if(NOT DEFINED GB_DIR)
  if(NOT DEFINED ENV{GB_DIR})
    message(FATAL_ERROR "GB_DIR environment variable not defined.")
  endif()
  set(GB_DIR "$ENV{GB_DIR}")
endif()

STRING(REGEX REPLACE "\\\\" "/" CURRENT_DIR ${CMAKE_CURRENT_LIST_DIR})
STRING(REGEX REPLACE "\\\\" "/" GB_DIR ${GB_DIR})

set(GB_BIN_DIR "${CURRENT_DIR}/bin")
set(GB_BUILD_TESTS OFF)
add_subdirectory("${GB_DIR}" gb)
gb_config()
gb_external_libraries()

###############################################################################
## Sub Directories
###############################################################################

enable_testing()
add_subdirectory(oz3)

set_property(DIRECTORY PROPERTY VS_STARTUP_PROJECT oz3)
