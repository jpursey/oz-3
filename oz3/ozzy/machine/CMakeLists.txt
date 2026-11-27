## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_ozzy_machine_SOURCE
  boot_rom.cc boot_rom.h
  machine_table.h
  ozzy.cc ozzy.h
  ozzy_settings.cc ozzy_settings.h
)
set(oz3_ozzy_machine_DEPS
  absl::algorithm_container
  absl::civil_time
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::strings
  absl::time
  gb_base
  gb_config
  gb_file
  gb_parse
  oz3_core
  oz3_devices
  oz3_disk
  oz3_instructions
  oz3_tools
)
set(oz3_ozzy_machine_LIBS
  absl::check
)
set(oz3_ozzy_machine_TEST_SOURCE
  boot_rom_test.cc
  loader_test.cc
  ozzy_settings_test.cc
  ozzy_test.cc
)
set(oz3_ozzy_machine_TEST_DEPS
  oz3_disk_testing
  oz3_ozzy_testing
)

gb_add_library(oz3_ozzy_machine)

# The boot ROM's source, which GetBootRom assembles.
gb_embed_data(oz3_ozzy_machine boot_rom.oza oz3/ozzy/machine/boot_rom_source.h
              oz3::kBootRomSource)

# The configs checked in at the top level, which the tests read, so they stay
# valid as the settings change.
gb_embed_data(oz3_ozzy_machine_test ${PROJECT_SOURCE_DIR}/configs/console.ozzy
              oz3/configs/console_config.h oz3::kConsoleConfig)
gb_embed_data(oz3_ozzy_machine_test ${PROJECT_SOURCE_DIR}/configs/teletype.ozzy
              oz3/configs/teletype_config.h oz3::kTeletypeConfig)

# The default loader's source, which the tests assemble and boot.
gb_embed_data(oz3_ozzy_machine_test loader.oza oz3/ozzy/machine/loader_source.h
              oz3::kLoaderSource)

# Test support: an Ozzy to run programs on, for tests of the machine and of
# what is built on it.
set(oz3_ozzy_testing_SOURCE
  ozzy_testing.cc ozzy_testing.h
)
set(oz3_ozzy_testing_DEPS
  absl::algorithm_container
  absl::span
  absl::status
  absl::statusor
  absl::strings
  absl::time
  gb_base
  gb_file
  gb_parse
  gmock
  oz3_core
  oz3_devices
  oz3_disk
  oz3_instructions
  oz3_ozzy_machine
  oz3_tools
)
set(oz3_ozzy_testing_LIBS
  absl::check
)

gb_add_library(oz3_ozzy_testing)
