// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/term/terminal_keys.h"

#include <cstdint>
#include <string_view>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/app/frontend_keys.h"
#include "oz3/ozzy/term/console.h"

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

// Returns the keys a new reader reads from `input` and then no more bytes.
std::vector<uint16_t> ReadAllKeys(std::string_view input) {
  TerminalKeyReader reader;
  std::vector<uint16_t> keys = reader.ReadKeys(input);
  const std::vector<uint16_t> rest = reader.ReadKeys("");
  keys.insert(keys.end(), rest.begin(), rest.end());
  return keys;
}

// Returns the changes from tapping `key`.
std::vector<KeyChange> GetKeyTap(uint16_t key) {
  std::vector<KeyChange> changes;
  AddKeyTap(key, changes);
  return changes;
}

TEST(TerminalKeysTest, CharactersAreAscii) {
  EXPECT_THAT(ReadAllKeys("aZ1 ~"), ElementsAre('a', 'Z', '1', ' ', '~'));
}

TEST(TerminalKeysTest, ControlCodesAreCtrl) {
  EXPECT_THAT(ReadAllKeys("\x01\x03\x11\x1A"),
              ElementsAre(0x01 | kKeyCtrl, 0x03 | kKeyCtrl, 0x11 | kKeyCtrl,
                          0x1A | kKeyCtrl));
  EXPECT_THAT(ReadAllKeys("\n"), ElementsAre(0x0A | kKeyCtrl));
  EXPECT_THAT(ReadAllKeys(std::string_view("\0", 1)),
              ElementsAre(' ' | kKeyCtrl));
}

TEST(TerminalKeysTest, NamedControlCodes) {
  EXPECT_THAT(
      ReadAllKeys("\r\t\x7F\b"),
      ElementsAre(kKeyEnter, kKeyTab, kKeyBackspace, kKeyBackspace | kKeyCtrl));
}

TEST(TerminalKeysTest, BytesThatAreNotKeys) {
  // UTF-8 for é, and the control codes past Ctrl+Z.
  EXPECT_THAT(ReadAllKeys("\xC3\xA9\x1C\x1D\x1E\x1F"), IsEmpty());
}

TEST(TerminalKeysTest, ArrowsAndPages) {
  EXPECT_THAT(ReadAllKeys("\x1B[A\x1B[B\x1B[C\x1B[D"),
              ElementsAre(kKeyUp, kKeyDown, kKeyRight, kKeyLeft));
  EXPECT_THAT(
      ReadAllKeys("\x1B[H\x1B[F\x1B[1~\x1B[4~\x1B[7~\x1B[8~"),
      ElementsAre(kKeyHome, kKeyEnd, kKeyHome, kKeyEnd, kKeyHome, kKeyEnd));
  EXPECT_THAT(ReadAllKeys("\x1B[2~\x1B[3~\x1B[5~\x1B[6~"),
              ElementsAre(kKeyInsert, kKeyDelete, kKeyPageUp, kKeyPageDown));

  // In application cursor mode, the arrows, Home, and End are SS3 sequences.
  EXPECT_THAT(ReadAllKeys("\x1BOA\x1BOD\x1BOH\x1BOF"),
              ElementsAre(kKeyUp, kKeyLeft, kKeyHome, kKeyEnd));
}

TEST(TerminalKeysTest, FunctionKeys) {
  EXPECT_THAT(ReadAllKeys("\x1BOP\x1BOQ\x1BOR\x1BOS"),
              ElementsAre(kKeyF1, kKeyF2, kKeyF3, kKeyF4));
  EXPECT_THAT(ReadAllKeys("\x1B[11~\x1B[15~\x1B[17~\x1B[21~\x1B[23~\x1B[24~"),
              ElementsAre(kKeyF1, kKeyF5, kKeyF6, kKeyF10, kKeyF11, kKeyF12));
}

TEST(TerminalKeysTest, ModifiersOfSequences) {
  EXPECT_THAT(
      ReadAllKeys("\x1B[1;2A\x1B[1;3B\x1B[1;5C\x1B[1;8D"),
      ElementsAre(kKeyUp | kKeyShift, kKeyDown | kKeyAlt, kKeyRight | kKeyCtrl,
                  kKeyLeft | kKeyShift | kKeyAlt | kKeyCtrl));
  EXPECT_THAT(ReadAllKeys("\x1B[3;5~\x1B[1;2P\x1B[15;6~"),
              ElementsAre(kKeyDelete | kKeyCtrl, kKeyF1 | kKeyShift,
                          kKeyF5 | kKeyShift | kKeyCtrl));

  // A modifier parameter of 0 is none, as 1 is.
  EXPECT_THAT(ReadAllKeys("\x1B[1;0A"), ElementsAre(kKeyUp));

  // Shift+Tab is a sequence of its own.
  EXPECT_THAT(ReadAllKeys("\x1B[Z"), ElementsAre(kKeyTab | kKeyShift));
}

TEST(TerminalKeysTest, EscapeBeforeAKeyIsAlt) {
  EXPECT_THAT(ReadAllKeys("\x1Bx\x1BX\x1B\x11\x1B\r"),
              ElementsAre('x' | kKeyAlt, 'X' | kKeyAlt,
                          0x11 | kKeyCtrl | kKeyAlt, kKeyEnter | kKeyAlt));
  EXPECT_THAT(ReadAllKeys("\x1B\x1B[A"), ElementsAre(kKeyUp | kKeyAlt));
  EXPECT_THAT(ReadAllKeys("\x1B\x1B"), ElementsAre(kKeyEscape | kKeyAlt));
}

TEST(TerminalKeysTest, SequencesThatAreNotKeys) {
  // A private sequence (a focus or mouse report), one with an intermediate
  // byte, and unknown finals and numbers are read and dropped.
  EXPECT_THAT(ReadAllKeys("\x1B[I\x1B[<0;1;1M\x1B[?1u\x1B[ q\x1B[9~a"),
              ElementsAre('a'));
}

TEST(TerminalKeysTest, BytesThatAreNotSequences) {
  // ESC [ before a control code isn't a control sequence, so it is Alt+[.
  EXPECT_THAT(ReadAllKeys("\x1B[\x01"),
              ElementsAre('[' | kKeyAlt, 0x01 | kKeyCtrl));

  // ESC O before a byte that isn't a key's is Alt+O.
  EXPECT_THAT(ReadAllKeys("\x1BOx"), ElementsAre('O' | kKeyAlt, 'x'));
}

TEST(TerminalKeysTest, EscapeWaitsForTheNextBytes) {
  TerminalKeyReader reader;
  EXPECT_THAT(reader.ReadKeys("a\x1B"), ElementsAre('a'));
  EXPECT_THAT(reader.ReadKeys(""), ElementsAre(kKeyEscape));
  EXPECT_THAT(reader.ReadKeys(""), IsEmpty());

  // Escape before the next bytes is Alt with them.
  EXPECT_THAT(reader.ReadKeys("\x1B"), IsEmpty());
  EXPECT_THAT(reader.ReadKeys("b"), ElementsAre('b' | kKeyAlt));
}

TEST(TerminalKeysTest, SequencesSplitAcrossReads) {
  TerminalKeyReader reader;
  EXPECT_THAT(reader.ReadKeys("\x1B["), IsEmpty());
  EXPECT_THAT(reader.ReadKeys("1;5"), IsEmpty());
  EXPECT_THAT(reader.ReadKeys("A\x1BO"), ElementsAre(kKeyUp | kKeyCtrl));
  EXPECT_THAT(reader.ReadKeys("P"), ElementsAre(kKeyF1));
}

TEST(TerminalKeysTest, UnfinishedSequencesAreReadAsTheyAre) {
  TerminalKeyReader reader;
  EXPECT_THAT(reader.ReadKeys("\x1B[1;"), IsEmpty());
  EXPECT_THAT(reader.ReadKeys(""), ElementsAre('[' | kKeyAlt, '1', ';'));
  EXPECT_THAT(reader.ReadKeys("\x1BO"), IsEmpty());
  EXPECT_THAT(reader.ReadKeys(""), ElementsAre('O' | kKeyAlt));
}

TEST(TerminalKeysTest, CtrlAltQQuits) {
  const std::vector<uint16_t> keys = ReadAllKeys("\x1B\x11");
  ASSERT_EQ(keys.size(), 1);
  EXPECT_TRUE(IsQuitKey(keys[0]));
  EXPECT_FALSE(IsQuitKey(ReadAllKeys("\x11")[0]));
  EXPECT_FALSE(IsQuitKey(ReadAllKeys("\x1Bq")[0]));
}

TEST(TerminalKeysTest, KeyCodesOfCharacters) {
  EXPECT_EQ(GetTypedKeyCode('a'), 'a');
  EXPECT_EQ(GetTypedKeyCode('A'), 'a');
  EXPECT_EQ(GetTypedKeyCode('z' | kKeyAlt), 'z');
  EXPECT_EQ(GetTypedKeyCode(' '), ' ');
  EXPECT_EQ(GetTypedKeyCode('7'), '7');
  EXPECT_EQ(GetTypedKeyCode('/'), '/');
  EXPECT_EQ(GetTypedKeyCode('`'), '`');
}

TEST(TerminalKeysTest, KeyCodesOfShiftedCharacters) {
  EXPECT_EQ(GetTypedKeyCode('!'), '1');
  EXPECT_EQ(GetTypedKeyCode(')'), '0');
  EXPECT_EQ(GetTypedKeyCode('_'), '-');
  EXPECT_EQ(GetTypedKeyCode('+'), '=');
  EXPECT_EQ(GetTypedKeyCode('{'), '[');
  EXPECT_EQ(GetTypedKeyCode('|'), '\\');
  EXPECT_EQ(GetTypedKeyCode(':'), ';');
  EXPECT_EQ(GetTypedKeyCode('"'), '\'');
  EXPECT_EQ(GetTypedKeyCode('?'), '/');
  EXPECT_EQ(GetTypedKeyCode('~'), '`');
}

TEST(TerminalKeysTest, KeyCodesOfControlCodes) {
  EXPECT_EQ(GetTypedKeyCode(0x01 | kKeyCtrl), 'a');
  EXPECT_EQ(GetTypedKeyCode(0x1A | kKeyCtrl), 'z');
  EXPECT_EQ(GetTypedKeyCode(0x0A | kKeyCtrl), 'j');
  EXPECT_EQ(GetTypedKeyCode(kKeyBackspace | kKeyCtrl), kKeyBackspace);
  EXPECT_EQ(GetTypedKeyCode(kKeyTab | kKeyShift), kKeyTab);
  EXPECT_EQ(GetTypedKeyCode(kKeyEnter), kKeyEnter);
  EXPECT_EQ(GetTypedKeyCode(kKeyEscape), kKeyEscape);
}

TEST(TerminalKeysTest, KeyCodesOfNamedKeys) {
  EXPECT_EQ(GetTypedKeyCode(kKeyDelete), kKeyDelete);
  EXPECT_EQ(GetTypedKeyCode(kKeyUp | kKeyCtrl), kKeyUp);
  EXPECT_EQ(GetTypedKeyCode(kKeyInsert), kKeyInsert);
  EXPECT_EQ(GetTypedKeyCode(kKeyF1), kKeyF1);
  EXPECT_EQ(GetTypedKeyCode(kKeyF12), kKeyF12);
}

TEST(TerminalKeysTest, KeysWithoutCodes) {
  EXPECT_EQ(GetTypedKeyCode(0), 0);
  EXPECT_EQ(GetTypedKeyCode(0x89), 0);
  EXPECT_EQ(GetTypedKeyCode(0x8F), 0);
  EXPECT_EQ(GetTypedKeyCode(0x9C), 0);
}

TEST(TerminalKeysTest, TapsPressAndReleaseTheKey) {
  EXPECT_THAT(GetKeyTap('A'),
              ElementsAre(KeyChange{.key = 'a', .held = true},
                          KeyChange{.key = 'a', .held = false}));
  EXPECT_THAT(GetKeyTap(kKeyUp),
              ElementsAre(KeyChange{.key = kKeyUp, .held = true},
                          KeyChange{.key = kKeyUp, .held = false}));
}

TEST(TerminalKeysTest, TapsHoldTheModifiersAroundTheKey) {
  EXPECT_THAT(GetKeyTap(kKeyF5 | kKeyShift | kKeyCtrl | kKeyAlt),
              ElementsAre(KeyChange{.key = kKeyLeftShift, .held = true},
                          KeyChange{.key = kKeyLeftCtrl, .held = true},
                          KeyChange{.key = kKeyLeftAlt, .held = true},
                          KeyChange{.key = kKeyF5, .held = true},
                          KeyChange{.key = kKeyF5, .held = false},
                          KeyChange{.key = kKeyLeftShift, .held = false},
                          KeyChange{.key = kKeyLeftCtrl, .held = false},
                          KeyChange{.key = kKeyLeftAlt, .held = false}));
}

TEST(TerminalKeysTest, NoTapForAKeyWithoutACode) {
  EXPECT_THAT(GetKeyTap(0x8F | kKeyCtrl), IsEmpty());
}

}  // namespace
}  // namespace oz3
