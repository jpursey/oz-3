// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/terminal_keys.h"

#include <algorithm>
#include <cstdint>
#include <string_view>
#include <vector>

#include "absl/strings/ascii.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_split.h"
#include "absl/types/span.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/term/console.h"

namespace oz3 {

namespace {

//==============================================================================
// Reading keys
//==============================================================================

// The characters a US keyboard types with Shift, and in the same order, the
// characters its keys type without it, which are their key codes.
constexpr std::string_view kShiftedCharacters = "~!@#$%^&*()_+{}|:\"<>?";
constexpr std::string_view kUnshiftedCharacters = "`1234567890-=[]\\;',./";

// The left modifier keys tapped with a key, by their typed key event bits.
struct ModifierKey {
  uint16_t bit;
  int key;
};
constexpr ModifierKey kModifierKeys[] = {
    {kKeyShift, kKeyLeftShift},
    {kKeyCtrl, kKeyLeftCtrl},
    {kKeyAlt, kKeyLeftAlt},
};

// The keys of control sequences by their final byte, which are the same after
// SS3 (ESC O) as after CSI (ESC [).
struct FinalKey {
  char final_byte;
  uint16_t key;
};
constexpr FinalKey kFinalKeys[] = {
    {'A', kKeyUp},   {'B', kKeyDown}, {'C', kKeyRight}, {'D', kKeyLeft},
    {'H', kKeyHome}, {'F', kKeyEnd},  {'P', kKeyF1},    {'Q', kKeyF2},
    {'R', kKeyF3},   {'S', kKeyF4},
};

// The keys of control sequences ending in ~, by their first parameter.
struct NumberKey {
  int number;
  uint16_t key;
};
constexpr NumberKey kNumberKeys[] = {
    {1, kKeyHome},   {2, kKeyInsert},   {3, kKeyDelete}, {4, kKeyEnd},
    {5, kKeyPageUp}, {6, kKeyPageDown}, {7, kKeyHome},   {8, kKeyEnd},
    {11, kKeyF1},    {12, kKeyF2},      {13, kKeyF3},    {14, kKeyF4},
    {15, kKeyF5},    {17, kKeyF6},      {18, kKeyF7},    {19, kKeyF8},
    {20, kKeyF9},    {21, kKeyF10},     {23, kKeyF11},   {24, kKeyF12},
};

// The length ReadControlSequence and ReadSs3 return for bytes that aren't a
// control sequence.
constexpr int kNotSequence = -1;

// Returns the key of the byte `byte`, which isn't Escape, on its own, or 0 if
// it is none.
uint16_t GetByteKey(char byte) {
  const int value = static_cast<unsigned char>(byte);
  switch (value) {
    case 0x00:
      return ' ' | kKeyCtrl;
    case kKeyTab:
    case kKeyEnter:
      return static_cast<uint16_t>(value);
    case kKeyDelete:
      return kKeyBackspace;
  }
  if (value <= 'Z' - 'A' + 1) {
    return static_cast<uint16_t>(value | kKeyCtrl);
  }
  if (absl::ascii_isprint(static_cast<unsigned char>(value))) {
    return static_cast<uint16_t>(value);
  }
  return 0;
}

// Returns the key with the final byte `final_byte` in kFinalKeys, or 0 if
// there is none.
uint16_t FindFinalKey(char final_byte) {
  for (const FinalKey& entry : kFinalKeys) {
    if (entry.final_byte == final_byte) {
      return entry.key;
    }
  }
  return 0;
}

// Returns the key with `number` in kNumberKeys, or 0 if there is none.
uint16_t FindNumberKey(int number) {
  for (const NumberKey& entry : kNumberKeys) {
    if (entry.number == number) {
      return entry.key;
    }
  }
  return 0;
}

// Returns the control sequence parameter at `index` in `parameters`, or 1 if
// it is left out.
int GetParameter(absl::Span<const std::string_view> parameters, int index) {
  int value = 0;
  if (index >= static_cast<int>(parameters.size()) ||
      !absl::SimpleAtoi(parameters[index], &value)) {
    return 1;
  }
  return value;
}

// Returns the modifier bits of the control sequence parameter `parameter`,
// which is 1 plus a mask of Shift (1), Alt (2), and Ctrl (4). Less than 1 is
// none.
uint16_t GetModifiers(int parameter) {
  const int mask = std::max(parameter - 1, 0);
  uint16_t modifiers = 0;
  if ((mask & 1) != 0) {
    modifiers |= kKeyShift;
  }
  if ((mask & 2) != 0) {
    modifiers |= kKeyAlt;
  }
  if ((mask & 4) != 0) {
    modifiers |= kKeyCtrl;
  }
  return modifiers;
}

// Returns the key of the control sequence with `parameters` (its parameter
// and intermediate bytes) and `final_byte`, or 0 if it isn't a key's.
uint16_t GetSequenceKey(std::string_view parameters, char final_byte) {
  // Keys' sequences only have numbers separated by semicolons.
  for (const char byte : parameters) {
    if (!absl::ascii_isdigit(static_cast<unsigned char>(byte)) && byte != ';') {
      return 0;
    }
  }
  const std::vector<std::string_view> fields = absl::StrSplit(parameters, ';');
  uint16_t key = 0;
  if (final_byte == '~') {
    key = FindNumberKey(GetParameter(fields, 0));
  } else if (final_byte == 'Z') {
    key = kKeyTab | kKeyShift;
  } else {
    key = FindFinalKey(final_byte);
  }
  if (key == 0) {
    return 0;
  }
  return key | GetModifiers(GetParameter(fields, 1));
}

// Reads the control sequence at the start of `input`, which starts with CSI
// (ESC [), setting `key` to its key. Returns its length, 0 if `input` ends
// partway through it, or kNotSequence.
int ReadControlSequence(std::string_view input, uint16_t& key) {
  const int size = static_cast<int>(input.size());
  int end = 2;
  while (end < size && input[end] >= 0x20 && input[end] <= 0x3F) {
    ++end;
  }
  if (end == size) {
    return 0;
  }
  const char final_byte = input[end];
  if (final_byte < 0x40 || final_byte > 0x7E) {
    return kNotSequence;
  }
  key = GetSequenceKey(input.substr(2, end - 2), final_byte);
  return end + 1;
}

// Reads the key at the start of `input`, which starts with SS3 (ESC O),
// setting `key` to it. Returns its length, 0 if `input` ends before it, or
// kNotSequence.
int ReadSs3(std::string_view input, uint16_t& key) {
  if (input.size() < 3) {
    return 0;
  }
  key = FindFinalKey(input[2]);
  return key != 0 ? 3 : kNotSequence;
}

// Reads the key at the start of `input`, which isn't empty, setting `key` to
// it, or 0 for bytes that aren't a key. Returns the bytes read, or 0 if
// `input` ends partway through a key and `more` bytes may follow. Without
// more, bytes that start a key are read as they are.
int ReadKey(std::string_view input, bool more, uint16_t& key) {
  key = 0;
  if (input[0] != kKeyEscape) {
    key = GetByteKey(input[0]);
    return 1;
  }
  if (input.size() == 1) {
    if (more) {
      return 0;
    }
    key = kKeyEscape;
    return 1;
  }
  int length = kNotSequence;
  if (input[1] == '[') {
    length = ReadControlSequence(input, key);
  } else if (input[1] == 'O') {
    length = ReadSs3(input, key);
  }
  if (length > 0 || (length == 0 && more)) {
    return length;
  }

  // Escape before a key that isn't a control sequence is Alt with the key.
  length = ReadKey(input.substr(1), more, key);
  if (length == 0) {
    return 0;
  }
  if (key != 0) {
    key |= kKeyAlt;
  }
  return length + 1;
}

}  // namespace

//==============================================================================
// Keys
//==============================================================================

int GetTypedKeyCode(uint16_t key) {
  const int code = key & 0xFF;
  if ((key & kKeyCtrl) != 0 && code >= 1 && code <= 'Z' - 'A' + 1 &&
      code != kKeyBackspace && code != kKeyTab && code != kKeyEnter) {
    return 'a' + code - 1;
  }
  if (code < 0x80) {
    const char character = static_cast<char>(code);
    if (absl::ascii_isupper(character)) {
      return absl::ascii_tolower(character);
    }
    const int shifted_count = static_cast<int>(kShiftedCharacters.size());
    for (int i = 0; i < shifted_count; ++i) {
      if (kShiftedCharacters[i] == character) {
        return kUnshiftedCharacters[i];
      }
    }
  }

  // Every other character, and Delete, is its own key on a US keyboard.
  if ((code >= ' ' && code <= kKeyDelete) || code == kKeyBackspace ||
      code == kKeyTab || code == kKeyEnter || code == kKeyEscape ||
      (code >= kKeyUp && code <= kKeyInsert) ||
      (code >= kKeyF1 && code <= kKeyF12)) {
    return code;
  }
  return 0;
}

void AddKeyTap(uint16_t key, std::vector<KeyChange>& changes) {
  const int code = GetTypedKeyCode(key);
  if (code == 0) {
    return;
  }
  for (const ModifierKey& modifier : kModifierKeys) {
    if ((key & modifier.bit) != 0) {
      changes.push_back({.key = modifier.key, .held = true});
    }
  }
  changes.push_back({.key = code, .held = true});
  changes.push_back({.key = code, .held = false});
  for (const ModifierKey& modifier : kModifierKeys) {
    if ((key & modifier.bit) != 0) {
      changes.push_back({.key = modifier.key, .held = false});
    }
  }
}

//==============================================================================
// TerminalKeyReader
//==============================================================================

std::vector<uint16_t> TerminalKeyReader::ReadKeys(std::string_view input) {
  const bool more = !input.empty();
  pending_.append(input);
  std::vector<uint16_t> keys;
  const std::string_view bytes = pending_;
  const int size = static_cast<int>(bytes.size());
  int read = 0;
  while (read < size) {
    uint16_t key = 0;
    const int length = ReadKey(bytes.substr(read), more, key);
    if (length == 0) {
      break;
    }
    if (key != 0) {
      keys.push_back(key);
    }
    read += length;
  }
  pending_.erase(0, read);
  return keys;
}

}  // namespace oz3
