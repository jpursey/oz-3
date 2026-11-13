// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#ifndef OZ3_DISK_FAULTY_FILE_PROTOCOL_H_
#define OZ3_DISK_FAULTY_FILE_PROTOCOL_H_

#include <cstdint>
#include <memory>
#include <string_view>
#include <vector>

#include "gb/file/file_types.h"
#include "gb/file/memory_file_protocol.h"
#include "gb/file/raw_file.h"

namespace oz3 {

// For tests: a gb::MemoryFileProtocol whose files fail as a failing host's
// would, once told to. Until then, it is a memory protocol like any other. It
// also records where each write lands, so tests can check the order of writes.
class FaultyFileProtocol final : public gb::MemoryFileProtocol {
 public:
  //----------------------------------------------------------------------------
  // Types
  //----------------------------------------------------------------------------

  // How many more of each kind of operation on the protocol's files succeed,
  // counted across all of them, before every later one fails. -1 is no limit.
  struct Limits {
    int64_t reads = -1;
    int64_t writes = -1;
    int64_t flushes = -1;
  };

  //----------------------------------------------------------------------------
  // Construction / Destruction
  //----------------------------------------------------------------------------

  FaultyFileProtocol() = default;
  ~FaultyFileProtocol() override = default;

  //----------------------------------------------------------------------------
  // Faults
  //----------------------------------------------------------------------------

  // Sets the limits, replacing those set before, including what is left of
  // them.
  void SetLimits(const Limits& limits) { limits_ = limits; }

  //----------------------------------------------------------------------------
  // Writes
  //----------------------------------------------------------------------------

  // Returns the byte offset each write to the protocol's files started at,
  // since the last call, in order, and forgets them. Failed writes are left
  // out. A file's position is taken to start at 0, as it does unless the file
  // is opened to append.
  std::vector<int64_t> CollectWriteOffsets();

 protected:
  //----------------------------------------------------------------------------
  // gb::MemoryFileProtocol overrides
  //----------------------------------------------------------------------------

  std::unique_ptr<gb::RawFile> BasicOpenFile(std::string_view protocol_name,
                                             std::string_view path,
                                             gb::FileFlags flags) override;

 private:
  //----------------------------------------------------------------------------
  // Implementation
  //----------------------------------------------------------------------------

  class FaultyFile;

  Limits limits_;
  std::vector<int64_t> write_offsets_;
};

}  // namespace oz3

#endif  // OZ3_DISK_FAULTY_FILE_PROTOCOL_H_
