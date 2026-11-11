// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/core/date_time.h"

namespace oz3 {

namespace {

// Returns `high` and `low` as the high and low bytes of a word, each truncated
// to 8 bits.
uint16_t GetWord(int high, int low) {
  return static_cast<uint16_t>(((high & 0xFF) << 8) | (low & 0xFF));
}

}  // namespace

DateTime DateTime::FromCivil(absl::CivilSecond time) {
  return DateTime({static_cast<uint16_t>(time.year()),
                   GetWord(time.month(), time.day()),
                   static_cast<uint16_t>(time.hour()),
                   GetWord(time.minute(), time.second())});
}

DateTime DateTime::FromWords(const Words& words) { return DateTime(words); }

std::optional<absl::CivilSecond> DateTime::ToCivil() const {
  if (!IsSet()) {
    return std::nullopt;
  }
  return absl::CivilSecond(words_[0], words_[1] >> 8, words_[1] & 0xFF,
                           words_[2], words_[3] >> 8, words_[3] & 0xFF);
}

}  // namespace oz3
