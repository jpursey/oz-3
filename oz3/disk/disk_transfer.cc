// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/disk_transfer.h"

#include <string>
#include <string_view>

#include "absl/strings/str_replace.h"

namespace oz3 {

std::string DiskTextFromHost(std::string_view host_text) {
  return absl::StrReplaceAll(host_text, {{"\r\n", "\n"}, {"\r", "\n"}});
}

std::string HostTextFromDisk(std::string_view disk_text) {
  std::string text(disk_text);
  std::erase(text, '\0');
  return text;
}

}  // namespace oz3
