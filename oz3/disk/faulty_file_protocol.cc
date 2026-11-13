// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/faulty_file_protocol.h"

#include <cstdint>
#include <utility>
#include <vector>

namespace oz3 {

namespace {

// Returns whether `limit` (see FaultyFileProtocol::Limits) allows one more
// operation, counting it if so.
bool Take(int64_t& limit) {
  if (limit == 0) {
    return false;
  }
  if (limit > 0) {
    --limit;
  }
  return true;
}

}  // namespace

// A memory file that fails its operations once its protocol's limits run out,
// and records where its writes start.
class FaultyFileProtocol::FaultyFile final : public gb::RawFile {
 public:
  FaultyFile(std::unique_ptr<gb::RawFile> file, FaultyFileProtocol* protocol)
      : file_(std::move(file)), protocol_(*protocol) {}
  ~FaultyFile() override = default;

  int64_t SeekEnd() override { return MoveTo(file_->SeekEnd()); }
  int64_t SeekTo(int64_t position) override {
    return MoveTo(file_->SeekTo(position));
  }

  int64_t Write(const void* buffer, int64_t size) override {
    if (!Take(protocol_.limits_.writes)) {
      return 0;
    }
    const int64_t written = file_->Write(buffer, size);
    if (written > 0) {
      protocol_.write_offsets_.push_back(position_);
      position_ += written;
    }
    return written;
  }

  bool Flush() override {
    return Take(protocol_.limits_.flushes) && file_->Flush();
  }

  int64_t Read(void* buffer, int64_t size) override {
    if (!Take(protocol_.limits_.reads)) {
      return -1;
    }
    const int64_t read = file_->Read(buffer, size);
    if (read > 0) {
      position_ += read;
    }
    return read;
  }

 private:
  // Keeps `position`, as a seek returned it, unless the seek failed, and
  // returns it.
  int64_t MoveTo(int64_t position) {
    if (position >= 0) {
      position_ = position;
    }
    return position;
  }

  std::unique_ptr<gb::RawFile> file_;
  FaultyFileProtocol& protocol_;
  int64_t position_ = 0;
};

std::vector<int64_t> FaultyFileProtocol::CollectWriteOffsets() {
  return std::exchange(write_offsets_, {});
}

std::unique_ptr<gb::RawFile> FaultyFileProtocol::BasicOpenFile(
    std::string_view protocol_name, std::string_view path,
    gb::FileFlags flags) {
  std::unique_ptr<gb::RawFile> file =
      MemoryFileProtocol::BasicOpenFile(protocol_name, path, flags);
  if (file == nullptr) {
    return nullptr;
  }
  return std::make_unique<FaultyFile>(std::move(file), this);
}

}  // namespace oz3
