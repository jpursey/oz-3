// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/win_console.h"

#include <cstdint>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "oz3/devices/keyboard.h"
#include "oz3/ozzy/console.h"

// MUST be last, as windows pollutes the global namespace with many macros.
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

namespace oz3 {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

constexpr uint32_t kCtrl = LEFT_CTRL_PRESSED;
constexpr uint32_t kAlt = LEFT_ALT_PRESSED;

// Scan codes of keys in the ConsoleKeyState tests.
constexpr uint16_t kScanA = 0x1E;
constexpr uint16_t kScanD = 0x20;
constexpr uint16_t kScanEnter = 0x1C;
constexpr uint16_t kScanCtrl = 0x1D;
constexpr uint16_t kScanAlt = 0x38;
constexpr uint16_t kScanLeftShift = 0x2A;
constexpr uint16_t kScanRightShift = 0x36;
constexpr uint16_t kScanLeftWindows = 0x5B;

// Returns the changes from adding a key event record to `state`.
std::vector<KeyChange> AddKeyEvent(ConsoleKeyState& state, uint16_t scan_code,
                                   bool key_down, uint32_t control_keys) {
  std::vector<KeyChange> changes;
  state.AddKeyEvent(scan_code, key_down, control_keys, changes);
  return changes;
}

// Returns the changes from releasing every key in `state`.
std::vector<KeyChange> ReleaseKeys(ConsoleKeyState& state) {
  std::vector<KeyChange> changes;
  state.ReleaseKeys(changes);
  return changes;
}

TEST(WinConsoleTest, CharactersAreAscii) {
  EXPECT_EQ(GetKeyEvent('A', 'a', 0), 'a');
  EXPECT_EQ(GetKeyEvent('Z', 'z', 0), 'z');
  EXPECT_EQ(GetKeyEvent('1', '1', 0), '1');
  EXPECT_EQ(GetKeyEvent(VK_SPACE, ' ', 0), ' ');
  EXPECT_EQ(GetKeyEvent(VK_OEM_3, '~', SHIFT_PRESSED), '~' | kKeyShift);
  EXPECT_EQ(GetKeyEvent(VK_NUMPAD5, '5', NUMLOCK_ON), '5');
  EXPECT_EQ(GetKeyEvent(VK_DIVIDE, '/', ENHANCED_KEY), '/');
}

TEST(WinConsoleTest, NamedKeys) {
  EXPECT_EQ(GetKeyEvent(VK_RETURN, '\r', 0), kKeyEnter);
  EXPECT_EQ(GetKeyEvent(VK_RETURN, '\r', ENHANCED_KEY), kKeyEnter);
  EXPECT_EQ(GetKeyEvent(VK_TAB, '\t', 0), kKeyTab);
  EXPECT_EQ(GetKeyEvent(VK_BACK, '\b', 0), kKeyBackspace);
  EXPECT_EQ(GetKeyEvent(VK_ESCAPE, 0x1B, 0), kKeyEscape);
  EXPECT_EQ(GetKeyEvent(VK_DELETE, 0, ENHANCED_KEY), kKeyDelete);
  EXPECT_EQ(GetKeyEvent(VK_INSERT, 0, ENHANCED_KEY), kKeyInsert);
}

TEST(WinConsoleTest, NamedKeysWhateverTheyType) {
  // Ctrl+Enter types a line feed, and Ctrl+Backspace a delete.
  EXPECT_EQ(GetKeyEvent(VK_RETURN, '\n', kCtrl), kKeyEnter | kKeyCtrl);
  EXPECT_EQ(GetKeyEvent(VK_BACK, 0x7F, kCtrl), kKeyBackspace | kKeyCtrl);
}

TEST(WinConsoleTest, ArrowsAndPages) {
  EXPECT_EQ(GetKeyEvent(VK_UP, 0, ENHANCED_KEY), kKeyUp);
  EXPECT_EQ(GetKeyEvent(VK_DOWN, 0, ENHANCED_KEY), kKeyDown);
  EXPECT_EQ(GetKeyEvent(VK_LEFT, 0, ENHANCED_KEY), kKeyLeft);
  EXPECT_EQ(GetKeyEvent(VK_RIGHT, 0, ENHANCED_KEY), kKeyRight);
  EXPECT_EQ(GetKeyEvent(VK_HOME, 0, ENHANCED_KEY), kKeyHome);
  EXPECT_EQ(GetKeyEvent(VK_END, 0, ENHANCED_KEY), kKeyEnd);
  EXPECT_EQ(GetKeyEvent(VK_PRIOR, 0, ENHANCED_KEY), kKeyPageUp);
  EXPECT_EQ(GetKeyEvent(VK_NEXT, 0, ENHANCED_KEY), kKeyPageDown);

  // The keypad's, without Num Lock.
  EXPECT_EQ(GetKeyEvent(VK_UP, 0, 0), kKeyUp);
  EXPECT_EQ(GetKeyEvent(VK_NEXT, 0, 0), kKeyPageDown);
}

TEST(WinConsoleTest, FunctionKeys) {
  EXPECT_EQ(GetKeyEvent(VK_F1, 0, 0), kKeyF1);
  EXPECT_EQ(GetKeyEvent(VK_F2, 0, 0), kKeyF2);
  EXPECT_EQ(GetKeyEvent(VK_F11, 0, 0), kKeyF11);
  EXPECT_EQ(GetKeyEvent(VK_F12, 0, 0), kKeyF12);
  EXPECT_EQ(GetKeyEvent(VK_F13, 0, 0), 0);
}

TEST(WinConsoleTest, CtrlWithLetters) {
  EXPECT_EQ(GetKeyEvent('A', 0x01, kCtrl), 0x01 | kKeyCtrl);
  EXPECT_EQ(GetKeyEvent('C', 0x03, kCtrl), 0x03 | kKeyCtrl);
  EXPECT_EQ(GetKeyEvent('Z', 0x1A, kCtrl), 0x1A | kKeyCtrl);
  EXPECT_EQ(GetKeyEvent('Z', 0x1A, kCtrl | SHIFT_PRESSED),
            0x1A | kKeyCtrl | kKeyShift);

  // Windows types nothing for Ctrl+Alt with a letter.
  EXPECT_EQ(GetKeyEvent('C', 0, kCtrl | kAlt), 0x03 | kKeyCtrl | kKeyAlt);

  // AltGr is Ctrl+Alt, and types characters on many layouts.
  EXPECT_EQ(GetKeyEvent('Q', '@', LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED),
            '@' | kKeyCtrl | kKeyAlt);
}

TEST(WinConsoleTest, EachModifier) {
  EXPECT_EQ(GetKeyEvent('A', 'A', SHIFT_PRESSED), 'A' | kKeyShift);
  EXPECT_EQ(GetKeyEvent(VK_UP, 0, LEFT_CTRL_PRESSED), kKeyUp | kKeyCtrl);
  EXPECT_EQ(GetKeyEvent(VK_UP, 0, RIGHT_CTRL_PRESSED), kKeyUp | kKeyCtrl);
  EXPECT_EQ(GetKeyEvent('A', 'a', LEFT_ALT_PRESSED), 'a' | kKeyAlt);
  EXPECT_EQ(GetKeyEvent('A', 'a', RIGHT_ALT_PRESSED), 'a' | kKeyAlt);
  EXPECT_EQ(GetKeyEvent(VK_F5, 0, SHIFT_PRESSED | kCtrl | kAlt),
            kKeyF5 | kKeyShift | kKeyCtrl | kKeyAlt);

  // The other control key states aren't modifiers.
  EXPECT_EQ(GetKeyEvent('A', 'A', CAPSLOCK_ON | NUMLOCK_ON | SCROLLLOCK_ON),
            'A');
}

TEST(WinConsoleTest, CtrlAltQQuits) {
  EXPECT_TRUE(IsQuitKey(GetKeyEvent('Q', 0, kCtrl | kAlt)));
  EXPECT_TRUE(IsQuitKey(GetKeyEvent(
      'Q', 0, RIGHT_CTRL_PRESSED | LEFT_ALT_PRESSED | SHIFT_PRESSED)));

  // Q without both Ctrl and Alt, and other keys with them.
  EXPECT_FALSE(IsQuitKey(GetKeyEvent('Q', 'q', 0)));
  EXPECT_FALSE(IsQuitKey(GetKeyEvent('Q', 0x11, kCtrl)));
  EXPECT_FALSE(IsQuitKey(GetKeyEvent('Q', 'q', kAlt)));
  EXPECT_FALSE(IsQuitKey(GetKeyEvent('W', 0, kCtrl | kAlt)));
  EXPECT_FALSE(IsQuitKey(GetKeyEvent(VK_END, 0, kCtrl | kAlt)));
  EXPECT_FALSE(IsQuitKey(0));

  // AltGr with Q types @ on a German layout.
  EXPECT_FALSE(
      IsQuitKey(GetKeyEvent('Q', '@', LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED)));
}

TEST(WinConsoleTest, KeysThatAreNone) {
  EXPECT_EQ(GetKeyEvent(VK_SHIFT, 0, SHIFT_PRESSED), 0);
  EXPECT_EQ(GetKeyEvent(VK_CONTROL, 0, kCtrl), 0);
  EXPECT_EQ(GetKeyEvent(VK_MENU, 0, kAlt), 0);
  EXPECT_EQ(GetKeyEvent(VK_CAPITAL, 0, CAPSLOCK_ON), 0);
  EXPECT_EQ(GetKeyEvent(VK_CANCEL, 0, kCtrl), 0);

  // Characters outside ASCII, and control codes other than Ctrl's letters.
  EXPECT_EQ(GetKeyEvent('E', 0xE9, 0), 0);
  EXPECT_EQ(GetKeyEvent(VK_OEM_4, 0x1B, kCtrl), 0);
}

TEST(WinConsoleTest, KeyCodesOfMainKeys) {
  // Each row's first and last keys, and the keys between that aren't letters.
  EXPECT_EQ(GetKeyCode(0x02, false), '1');
  EXPECT_EQ(GetKeyCode(0x0A, false), '9');
  EXPECT_EQ(GetKeyCode(0x0B, false), '0');
  EXPECT_EQ(GetKeyCode(0x0C, false), '-');
  EXPECT_EQ(GetKeyCode(0x0D, false), '=');
  EXPECT_EQ(GetKeyCode(0x10, false), 'q');
  EXPECT_EQ(GetKeyCode(0x19, false), 'p');
  EXPECT_EQ(GetKeyCode(0x1A, false), '[');
  EXPECT_EQ(GetKeyCode(0x1B, false), ']');
  EXPECT_EQ(GetKeyCode(0x1E, false), 'a');
  EXPECT_EQ(GetKeyCode(0x26, false), 'l');
  EXPECT_EQ(GetKeyCode(0x27, false), ';');
  EXPECT_EQ(GetKeyCode(0x28, false), '\'');
  EXPECT_EQ(GetKeyCode(0x29, false), '`');
  EXPECT_EQ(GetKeyCode(0x2B, false), '\\');
  EXPECT_EQ(GetKeyCode(0x2C, false), 'z');
  EXPECT_EQ(GetKeyCode(0x32, false), 'm');
  EXPECT_EQ(GetKeyCode(0x33, false), ',');
  EXPECT_EQ(GetKeyCode(0x34, false), '.');
  EXPECT_EQ(GetKeyCode(0x35, false), '/');
  EXPECT_EQ(GetKeyCode(0x39, false), ' ');

  // Keys are where they are, so the key US keyboards mark W is 'w', whatever
  // the layout says it is.
  EXPECT_EQ(GetKeyCode(0x11, false), 'w');
}

TEST(WinConsoleTest, KeyCodesOfNamedKeys) {
  EXPECT_EQ(GetKeyCode(0x01, false), kKeyEscape);
  EXPECT_EQ(GetKeyCode(0x0E, false), kKeyBackspace);
  EXPECT_EQ(GetKeyCode(0x0F, false), kKeyTab);
  EXPECT_EQ(GetKeyCode(0x1C, false), kKeyEnter);
  EXPECT_EQ(GetKeyCode(0x3B, false), kKeyF1);
  EXPECT_EQ(GetKeyCode(0x44, false), kKeyF10);
  EXPECT_EQ(GetKeyCode(0x57, false), kKeyF11);
  EXPECT_EQ(GetKeyCode(0x58, false), kKeyF12);
}

TEST(WinConsoleTest, KeyCodesOfModifiers) {
  // Right Ctrl and Right Alt are their left twins' scan codes, extended.
  EXPECT_EQ(GetKeyCode(0x2A, false), kKeyLeftShift);
  EXPECT_EQ(GetKeyCode(0x36, false), kKeyRightShift);
  EXPECT_EQ(GetKeyCode(0x1D, false), kKeyLeftCtrl);
  EXPECT_EQ(GetKeyCode(0x1D, true), kKeyRightCtrl);
  EXPECT_EQ(GetKeyCode(0x38, false), kKeyLeftAlt);
  EXPECT_EQ(GetKeyCode(0x38, true), kKeyRightAlt);
}

TEST(WinConsoleTest, KeyCodesOfExtendedKeys) {
  EXPECT_EQ(GetKeyCode(0x48, true), kKeyUp);
  EXPECT_EQ(GetKeyCode(0x50, true), kKeyDown);
  EXPECT_EQ(GetKeyCode(0x4B, true), kKeyLeft);
  EXPECT_EQ(GetKeyCode(0x4D, true), kKeyRight);
  EXPECT_EQ(GetKeyCode(0x47, true), kKeyHome);
  EXPECT_EQ(GetKeyCode(0x4F, true), kKeyEnd);
  EXPECT_EQ(GetKeyCode(0x49, true), kKeyPageUp);
  EXPECT_EQ(GetKeyCode(0x51, true), kKeyPageDown);
  EXPECT_EQ(GetKeyCode(0x52, true), kKeyInsert);
  EXPECT_EQ(GetKeyCode(0x53, true), kKeyDelete);

  // Without ENHANCED_KEY, they are their keypad twins, which have no code.
  for (const uint16_t scan_code :
       {0x48, 0x50, 0x4B, 0x4D, 0x47, 0x4F, 0x49, 0x51, 0x52, 0x53}) {
    EXPECT_EQ(GetKeyCode(scan_code, false), 0) << scan_code;
  }

  // The keypad's Enter and / are the main keys' scan codes, extended.
  EXPECT_EQ(GetKeyCode(0x1C, true), 0);
  EXPECT_EQ(GetKeyCode(0x35, true), 0);

  // Main keys aren't extended.
  EXPECT_EQ(GetKeyCode(0x1E, true), 0);
}

TEST(WinConsoleTest, KeyCodesThatAreNone) {
  EXPECT_EQ(GetKeyCode(0x00, false), 0);  // Some injected keys
  EXPECT_EQ(GetKeyCode(0x3A, false), 0);  // Caps Lock
  EXPECT_EQ(GetKeyCode(0x45, false), 0);  // Num Lock or Pause
  EXPECT_EQ(GetKeyCode(0x46, false), 0);  // Scroll Lock
  EXPECT_EQ(GetKeyCode(0x37, false), 0);  // Keypad *
  EXPECT_EQ(GetKeyCode(0x37, true), 0);   // Print Screen
  EXPECT_EQ(GetKeyCode(0x4A, false), 0);  // Keypad -
  EXPECT_EQ(GetKeyCode(0x4C, false), 0);  // Keypad 5
  EXPECT_EQ(GetKeyCode(0x4E, false), 0);  // Keypad +
  EXPECT_EQ(GetKeyCode(0x56, false), 0);  // The key beside Left Shift (ISO)
  EXPECT_EQ(GetKeyCode(0x5B, true), 0);   // Left Windows
  EXPECT_EQ(GetKeyCode(0x5D, true), 0);   // Menu
}

//==============================================================================
// ConsoleKeyState
//
// The records are as Windows Terminal and the console host gave them.
//==============================================================================

TEST(ConsoleKeyStateTest, KeyHeldFromDownToUp) {
  ConsoleKeyState state;
  EXPECT_THAT(AddKeyEvent(state, kScanA, true, 0),
              ElementsAre(KeyChange{.key = 'a', .held = true}));

  // Repeats are more key downs.
  EXPECT_THAT(AddKeyEvent(state, kScanA, true, 0), IsEmpty());
  EXPECT_THAT(AddKeyEvent(state, kScanA, false, 0),
              ElementsAre(KeyChange{.key = 'a', .held = false}));
}

TEST(ConsoleKeyStateTest, KeyUpOfKeyNotHeld) {
  // The Enter that started the program comes up once it is running.
  ConsoleKeyState state;
  EXPECT_THAT(AddKeyEvent(state, kScanEnter, false, 0), IsEmpty());
}

TEST(ConsoleKeyStateTest, KeysWithoutCodes) {
  ConsoleKeyState state;
  EXPECT_THAT(AddKeyEvent(state, kScanLeftWindows, true, ENHANCED_KEY),
              IsEmpty());
  EXPECT_THAT(AddKeyEvent(state, 0, true, 0), IsEmpty());
}

TEST(ConsoleKeyStateTest, ShiftByScanCode) {
  // The control key state has one Shift for both.
  ConsoleKeyState state;
  EXPECT_THAT(AddKeyEvent(state, kScanRightShift, true, SHIFT_PRESSED),
              ElementsAre(KeyChange{.key = kKeyRightShift, .held = true}));
  EXPECT_THAT(AddKeyEvent(state, kScanLeftShift, true, SHIFT_PRESSED),
              ElementsAre(KeyChange{.key = kKeyLeftShift, .held = true}));
  EXPECT_THAT(AddKeyEvent(state, kScanRightShift, false, SHIFT_PRESSED),
              ElementsAre(KeyChange{.key = kKeyRightShift, .held = false}));
}

TEST(ConsoleKeyStateTest, CtrlAndAltByControlKeyState) {
  // Left Ctrl, then Left Alt, then Left Ctrl up, and Left Alt up.
  ConsoleKeyState state;
  EXPECT_THAT(AddKeyEvent(state, kScanCtrl, true, LEFT_CTRL_PRESSED),
              ElementsAre(KeyChange{.key = kKeyLeftCtrl, .held = true}));
  EXPECT_THAT(
      AddKeyEvent(state, kScanAlt, true, LEFT_CTRL_PRESSED | LEFT_ALT_PRESSED),
      ElementsAre(KeyChange{.key = kKeyLeftAlt, .held = true}));
  EXPECT_THAT(AddKeyEvent(state, kScanCtrl, false, LEFT_ALT_PRESSED),
              ElementsAre(KeyChange{.key = kKeyLeftCtrl, .held = false}));
  EXPECT_THAT(AddKeyEvent(state, kScanAlt, false, 0),
              ElementsAre(KeyChange{.key = kKeyLeftAlt, .held = false}));

  // The same on the right, which is extended.
  EXPECT_THAT(
      AddKeyEvent(state, kScanCtrl, true, ENHANCED_KEY | RIGHT_CTRL_PRESSED),
      ElementsAre(KeyChange{.key = kKeyRightCtrl, .held = true}));
  EXPECT_THAT(
      AddKeyEvent(state, kScanAlt, true,
                  ENHANCED_KEY | RIGHT_CTRL_PRESSED | RIGHT_ALT_PRESSED),
      ElementsAre(KeyChange{.key = kKeyRightAlt, .held = true}));
  EXPECT_THAT(
      AddKeyEvent(state, kScanCtrl, false, ENHANCED_KEY | RIGHT_ALT_PRESSED),
      ElementsAre(KeyChange{.key = kKeyRightCtrl, .held = false}));
  EXPECT_THAT(AddKeyEvent(state, kScanAlt, false, ENHANCED_KEY),
              ElementsAre(KeyChange{.key = kKeyRightAlt, .held = false}));
}

TEST(ConsoleKeyStateTest, RightAltUpWithoutEnhancedKey) {
  // Windows Terminal gives Right Alt's key up without ENHANCED_KEY, which by
  // its scan code is Left Alt's.
  ConsoleKeyState state;
  EXPECT_THAT(
      AddKeyEvent(state, kScanAlt, true, ENHANCED_KEY | RIGHT_ALT_PRESSED),
      ElementsAre(KeyChange{.key = kKeyRightAlt, .held = true}));
  EXPECT_THAT(AddKeyEvent(state, kScanAlt, false, 0),
              ElementsAre(KeyChange{.key = kKeyRightAlt, .held = false}));
}

TEST(ConsoleKeyStateTest, CtrlAndAltCaughtUpByAnyKey) {
  // A Ctrl pressed before the console had focus has no key down, so it is held
  // by the next key's control key state, before that key.
  ConsoleKeyState state;
  EXPECT_THAT(AddKeyEvent(state, kScanA, true, LEFT_CTRL_PRESSED),
              ElementsAre(KeyChange{.key = kKeyLeftCtrl, .held = true},
                          KeyChange{.key = 'a', .held = true}));

  // Ctrl released while the console didn't have focus.
  EXPECT_THAT(AddKeyEvent(state, kScanA, false, 0),
              ElementsAre(KeyChange{.key = kKeyLeftCtrl, .held = false},
                          KeyChange{.key = 'a', .held = false}));
}

TEST(ConsoleKeyStateTest, ReleaseKeys) {
  ConsoleKeyState state;
  AddKeyEvent(state, kScanD, true, 0);
  AddKeyEvent(state, kScanA, true, 0);
  AddKeyEvent(state, kScanAlt, true, ENHANCED_KEY | RIGHT_ALT_PRESSED);

  // Released in key code order.
  EXPECT_THAT(ReleaseKeys(state),
              ElementsAre(KeyChange{.key = 'a', .held = false},
                          KeyChange{.key = 'd', .held = false},
                          KeyChange{.key = kKeyRightAlt, .held = false}));
  EXPECT_THAT(ReleaseKeys(state), IsEmpty());

  // A key up once focus is back, for a key already released.
  EXPECT_THAT(AddKeyEvent(state, kScanD, false, 0), IsEmpty());

  // A key held through it is held again by its next repeat.
  EXPECT_THAT(AddKeyEvent(state, kScanA, true, 0),
              ElementsAre(KeyChange{.key = 'a', .held = true}));
}

}  // namespace
}  // namespace oz3
