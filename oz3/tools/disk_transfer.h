// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_TOOLS_DISK_TRANSFER_H_
#define OZ3_TOOLS_DISK_TRANSFER_H_

#include <string>
#include <string_view>

namespace oz3 {

//==============================================================================
// Host text transfer
//
// Text as a host file holds it, to and from the bytes of an OZ-3 file. An OZ-3
// file's bytes are its words, low byte first, as strings are packed, so text
// never needs reordering: only line endings and padding differ.
//==============================================================================

// Returns host text as it is written to a disk: each CRLF and lone CR becomes
// LF. Writing an odd number of bytes pads the file's last word with a zero.
std::string DiskTextFromHost(std::string_view host_text);

// Returns text read from a disk as a host file holds it: every zero byte is
// dropped, such as the padding at the end of an odd length. Line endings stay
// LF.
std::string HostTextFromDisk(std::string_view disk_text);

}  // namespace oz3

#endif  // OZ3_TOOLS_DISK_TRANSFER_H_
