## Copyright (c) 2024 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

cmake_minimum_required (VERSION 3.15)

###############################################################################
## Set project configuration
###############################################################################

project(OZ3 C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

###############################################################################
# Game Bits configuration
###############################################################################

# Game Bits is found through the GB_DIR environment variable, or else beside
# this project, as it is checked out in cloud sessions and CI.
if(NOT DEFINED GB_DIR)
  if(DEFINED ENV{GB_DIR})
    set(GB_DIR "$ENV{GB_DIR}")
  elseif(EXISTS "${CMAKE_CURRENT_LIST_DIR}/../game-bits/CMakeLists.txt")
    get_filename_component(GB_DIR "${CMAKE_CURRENT_LIST_DIR}/../game-bits"
                           ABSOLUTE)
  else()
    message(FATAL_ERROR "GB_DIR environment variable not defined, and there "
                        "is no game-bits directory beside this one.")
  endif()
endif()

STRING(REGEX REPLACE "\\\\" "/" CURRENT_DIR ${CMAKE_CURRENT_LIST_DIR})
STRING(REGEX REPLACE "\\\\" "/" GB_DIR ${GB_DIR})

set(GB_BIN_DIR "${CURRENT_DIR}/bin")
set(GB_BUILD_TESTS OFF)
add_subdirectory("${GB_DIR}" gb EXCLUDE_FROM_ALL)
gb_config()
gb_external_libraries()

###############################################################################
## Sub Directories
###############################################################################

enable_testing()
add_subdirectory(oz3)

set_property(DIRECTORY PROPERTY VS_STARTUP_PROJECT oz3)
