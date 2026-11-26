## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

# The console's implementation for the platform: the Windows console, or
# elsewhere a POSIX terminal.
if (WIN32)
  set(oz3_ozzy_term_internal_SOURCE
    win_console.cc win_console.h
  )
  set(oz3_ozzy_term_internal_TEST_SOURCE
    win_console_test.cc
  )
else()
  set(oz3_ozzy_term_internal_SOURCE
    posix_console.cc posix_console.h
  )
  set(oz3_ozzy_term_internal_TEST_SOURCE
    posix_console_test.cc
  )
endif()

set(oz3_ozzy_term_SOURCE
  console.h
  console_display.cc console_display.h
  console_frontend.cc console_frontend.h
  console_text.cc console_text.h
  terminal_keys.cc terminal_keys.h
  ${oz3_ozzy_term_internal_SOURCE}
)
set(oz3_ozzy_term_DEPS
  absl::no_destructor
  absl::span
  absl::status
  absl::statusor
  absl::strings
  absl::time
  gb_base
  gb_cli
  oz3_core
  oz3_devices
  oz3_ozzy_app
  oz3_ozzy_machine
)
set(oz3_ozzy_term_LIBS
  absl::check
)
set(oz3_ozzy_term_TEST_SOURCE
  console_display_test.cc
  console_frontend_test.cc
  console_text_test.cc
  fake_console.h
  terminal_keys_test.cc
  ${oz3_ozzy_term_internal_TEST_SOURCE}
)
set(oz3_ozzy_term_TEST_DEPS
  oz3_ozzy_testing
)

gb_add_library(oz3_ozzy_term)
