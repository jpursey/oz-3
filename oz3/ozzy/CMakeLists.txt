## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_ozzy_SOURCE
  boot_rom.cc boot_rom.h
  machine_table.h
  ozzy.cc ozzy.h
  ozzy_settings.cc ozzy_settings.h
)
set(oz3_ozzy_DEPS
  absl::algorithm_container
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::strings
  absl::time
  gb_config
  gb_parse
  oz3_core
  oz3_devices
  oz3_instruction_sets
  oz3_tools
)
set(oz3_ozzy_LIBS
  absl::check
)
set(oz3_ozzy_TEST_SOURCE
  boot_rom_test.cc
  ozzy_settings_test.cc
  ozzy_test.cc
)

gb_add_library(oz3_ozzy)

# The boot ROM's source, which GetBootRom assembles.
gb_embed_data(oz3_ozzy boot_rom.oza oz3/ozzy/boot_rom_source.h
              oz3::kBootRomSource)
