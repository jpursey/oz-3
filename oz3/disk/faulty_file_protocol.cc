// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/disk/faulty_file_protocol.h"

#include <utility>

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

// A memory file that fails its operations once its protocol's limits run out.
class FaultyFileProtocol::FaultyFile final : public gb::RawFile {
 public:
  FaultyFile(std::unique_ptr<gb::RawFile> file, Limits* limits)
      : file_(std::move(file)), limits_(limits) {}
  ~FaultyFile() override = default;

  int64_t SeekEnd() override { return file_->SeekEnd(); }
  int64_t SeekTo(int64_t position) override { return file_->SeekTo(position); }

  int64_t Write(const void* buffer, int64_t size) override {
    return Take(limits_->writes) ? file_->Write(buffer, size) : 0;
  }

  bool Flush() override { return Take(limits_->flushes) && file_->Flush(); }

  int64_t Read(void* buffer, int64_t size) override {
    return Take(limits_->reads) ? file_->Read(buffer, size) : -1;
  }

 private:
  std::unique_ptr<gb::RawFile> file_;
  Limits* const limits_;
};

std::unique_ptr<gb::RawFile> FaultyFileProtocol::BasicOpenFile(
    std::string_view protocol_name, std::string_view path,
    gb::FileFlags flags) {
  std::unique_ptr<gb::RawFile> file =
      MemoryFileProtocol::BasicOpenFile(protocol_name, path, flags);
  if (file == nullptr) {
    return nullptr;
  }
  return std::make_unique<FaultyFile>(std::move(file), &limits_);
}

}  // namespace oz3
