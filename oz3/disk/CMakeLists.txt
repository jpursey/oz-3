## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_disk_SOURCE
  disk_check.cc disk_check.h
  disk_dir_entry.cc disk_dir_entry.h
  disk_fat.cc disk_fat.h
  disk_file_system.cc disk_file_system.h
  disk_filename.cc disk_filename.h
  disk_hardware.cc disk_hardware.h
  disk_image.cc disk_image.h
  disk_label.cc disk_label.h
  disk_layout.cc disk_layout.h
  disk_media.cc disk_media.h
  disk_packed_text.cc disk_packed_text.h
  disk_sector_buffer.h
  disk_status.h
  disk_superblock.cc disk_superblock.h
)
set(oz3_disk_DEPS
  absl::btree
  absl::function_ref
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::strings
  absl::time
  gb_file
  oz3_core
)
set(oz3_disk_LIBS
  absl::check
)

set(oz3_disk_TEST_SOURCE
  disk_check_test.cc
  disk_dir_entry_test.cc
  disk_fat_test.cc
  disk_file_system_test.cc
  disk_filename_test.cc
  disk_hardware_test.cc
  disk_image_test.cc
  disk_label_test.cc
  disk_layout_test.cc
  disk_superblock_test.cc
  faulty_file_protocol.cc faulty_file_protocol.h
)

gb_add_library(oz3_disk)
