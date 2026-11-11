## Copyright (c) 2026 John Pursey
##
## Use of this source code is governed by an MIT-style License that can be found
## in the LICENSE file or at https://opensource.org/licenses/MIT.

set(oz3_disk_SOURCE
  disk_image.cc disk_image.h
  disk_status.h
)
set(oz3_disk_DEPS
  absl::memory
  absl::span
  absl::status
  absl::statusor
  absl::strings
  gb_file
)
set(oz3_disk_LIBS
  absl::check
)

set(oz3_disk_TEST_SOURCE
  disk_image_test.cc
  faulty_file_protocol.cc faulty_file_protocol.h
)

gb_add_library(oz3_disk)
