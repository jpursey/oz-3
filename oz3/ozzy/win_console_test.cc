// Copyright (c) 2026 John Pursey
//
// Use of this source code is governed by an MIT-style License that can be found
// in the LICENSE file or at https://opensource.org/licenses/MIT.

#include "oz3/ozzy/win_console.h"

#include <cstdint>

#include "gtest/gtest.h"
#include "oz3/devices/keyboard.h"

// MUST be last, as windows pollutes the global namespace with many macros.
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

namespace oz3 {
namespace {

constexpr uint32_t kCtrl = LEFT_CTRL_PRESSED;
constexpr uint32_t kAlt = LEFT_ALT_PRESSED;

TEST(WinConsoleTest, CharactersAreAscii) {
  EXPECT_EQ(GetOzzyKey('A', 'a', 0), 'a');
  EXPECT_EQ(GetOzzyKey('Z', 'z', 0), 'z');
  EXPECT_EQ(GetOzzyKey('1', '1', 0), '1');
  EXPECT_EQ(GetOzzyKey(VK_SPACE, ' ', 0), ' ');
  EXPECT_EQ(GetOzzyKey(VK_OEM_3, '~', SHIFT_PRESSED), '~' | kKeyShift);
  EXPECT_EQ(GetOzzyKey(VK_NUMPAD5, '5', NUMLOCK_ON), '5');
  EXPECT_EQ(GetOzzyKey(VK_DIVIDE, '/', ENHANCED_KEY), '/');
}

TEST(WinConsoleTest, NamedKeys) {
  EXPECT_EQ(GetOzzyKey(VK_RETURN, '\r', 0), kKeyEnter);
  EXPECT_EQ(GetOzzyKey(VK_RETURN, '\r', ENHANCED_KEY), kKeyEnter);
  EXPECT_EQ(GetOzzyKey(VK_TAB, '\t', 0), kKeyTab);
  EXPECT_EQ(GetOzzyKey(VK_BACK, '\b', 0), kKeyBackspace);
  EXPECT_EQ(GetOzzyKey(VK_ESCAPE, 0x1B, 0), kKeyEscape);
  EXPECT_EQ(GetOzzyKey(VK_DELETE, 0, ENHANCED_KEY), kKeyDelete);
  EXPECT_EQ(GetOzzyKey(VK_INSERT, 0, ENHANCED_KEY), kKeyInsert);
}

TEST(WinConsoleTest, NamedKeysWhateverTheyType) {
  // Ctrl+Enter types a line feed, and Ctrl+Backspace a delete.
  EXPECT_EQ(GetOzzyKey(VK_RETURN, '\n', kCtrl), kKeyEnter | kKeyCtrl);
  EXPECT_EQ(GetOzzyKey(VK_BACK, 0x7F, kCtrl), kKeyBackspace | kKeyCtrl);
}

TEST(WinConsoleTest, ArrowsAndPages) {
  EXPECT_EQ(GetOzzyKey(VK_UP, 0, ENHANCED_KEY), kKeyUp);
  EXPECT_EQ(GetOzzyKey(VK_DOWN, 0, ENHANCED_KEY), kKeyDown);
  EXPECT_EQ(GetOzzyKey(VK_LEFT, 0, ENHANCED_KEY), kKeyLeft);
  EXPECT_EQ(GetOzzyKey(VK_RIGHT, 0, ENHANCED_KEY), kKeyRight);
  EXPECT_EQ(GetOzzyKey(VK_HOME, 0, ENHANCED_KEY), kKeyHome);
  EXPECT_EQ(GetOzzyKey(VK_END, 0, ENHANCED_KEY), kKeyEnd);
  EXPECT_EQ(GetOzzyKey(VK_PRIOR, 0, ENHANCED_KEY), kKeyPageUp);
  EXPECT_EQ(GetOzzyKey(VK_NEXT, 0, ENHANCED_KEY), kKeyPageDown);

  // The keypad's, without Num Lock.
  EXPECT_EQ(GetOzzyKey(VK_UP, 0, 0), kKeyUp);
  EXPECT_EQ(GetOzzyKey(VK_NEXT, 0, 0), kKeyPageDown);
}

TEST(WinConsoleTest, FunctionKeys) {
  EXPECT_EQ(GetOzzyKey(VK_F1, 0, 0), kKeyF1);
  EXPECT_EQ(GetOzzyKey(VK_F2, 0, 0), kKeyF2);
  EXPECT_EQ(GetOzzyKey(VK_F11, 0, 0), kKeyF11);
  EXPECT_EQ(GetOzzyKey(VK_F12, 0, 0), kKeyF12);
  EXPECT_EQ(GetOzzyKey(VK_F13, 0, 0), 0);
}

TEST(WinConsoleTest, CtrlWithLetters) {
  EXPECT_EQ(GetOzzyKey('A', 0x01, kCtrl), 0x01 | kKeyCtrl);
  EXPECT_EQ(GetOzzyKey('C', 0x03, kCtrl), 0x03 | kKeyCtrl);
  EXPECT_EQ(GetOzzyKey('Z', 0x1A, kCtrl), 0x1A | kKeyCtrl);
  EXPECT_EQ(GetOzzyKey('Z', 0x1A, kCtrl | SHIFT_PRESSED),
            0x1A | kKeyCtrl | kKeyShift);

  // Windows types nothing for Ctrl+Alt with a letter.
  EXPECT_EQ(GetOzzyKey('C', 0, kCtrl | kAlt), 0x03 | kKeyCtrl | kKeyAlt);

  // AltGr is Ctrl+Alt, and types characters on many layouts.
  EXPECT_EQ(GetOzzyKey('Q', '@', LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED),
            '@' | kKeyCtrl | kKeyAlt);
}

TEST(WinConsoleTest, EachModifier) {
  EXPECT_EQ(GetOzzyKey('A', 'A', SHIFT_PRESSED), 'A' | kKeyShift);
  EXPECT_EQ(GetOzzyKey(VK_UP, 0, LEFT_CTRL_PRESSED), kKeyUp | kKeyCtrl);
  EXPECT_EQ(GetOzzyKey(VK_UP, 0, RIGHT_CTRL_PRESSED), kKeyUp | kKeyCtrl);
  EXPECT_EQ(GetOzzyKey('A', 'a', LEFT_ALT_PRESSED), 'a' | kKeyAlt);
  EXPECT_EQ(GetOzzyKey('A', 'a', RIGHT_ALT_PRESSED), 'a' | kKeyAlt);
  EXPECT_EQ(GetOzzyKey(VK_F5, 0, SHIFT_PRESSED | kCtrl | kAlt),
            kKeyF5 | kKeyShift | kKeyCtrl | kKeyAlt);

  // The other control key states aren't modifiers.
  EXPECT_EQ(GetOzzyKey('A', 'A', CAPSLOCK_ON | NUMLOCK_ON | SCROLLLOCK_ON),
            'A');
}

TEST(WinConsoleTest, CtrlAltQQuits) {
  EXPECT_TRUE(IsQuitKey(GetOzzyKey('Q', 0, kCtrl | kAlt)));
  EXPECT_TRUE(IsQuitKey(GetOzzyKey(
      'Q', 0, RIGHT_CTRL_PRESSED | LEFT_ALT_PRESSED | SHIFT_PRESSED)));

  // Q without both Ctrl and Alt, and other keys with them.
  EXPECT_FALSE(IsQuitKey(GetOzzyKey('Q', 'q', 0)));
  EXPECT_FALSE(IsQuitKey(GetOzzyKey('Q', 0x11, kCtrl)));
  EXPECT_FALSE(IsQuitKey(GetOzzyKey('Q', 'q', kAlt)));
  EXPECT_FALSE(IsQuitKey(GetOzzyKey('W', 0, kCtrl | kAlt)));
  EXPECT_FALSE(IsQuitKey(GetOzzyKey(VK_END, 0, kCtrl | kAlt)));
  EXPECT_FALSE(IsQuitKey(0));

  // AltGr with Q types @ on a German layout.
  EXPECT_FALSE(
      IsQuitKey(GetOzzyKey('Q', '@', LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED)));
}

TEST(WinConsoleTest, KeysThatAreNone) {
  EXPECT_EQ(GetOzzyKey(VK_SHIFT, 0, SHIFT_PRESSED), 0);
  EXPECT_EQ(GetOzzyKey(VK_CONTROL, 0, kCtrl), 0);
  EXPECT_EQ(GetOzzyKey(VK_MENU, 0, kAlt), 0);
  EXPECT_EQ(GetOzzyKey(VK_CAPITAL, 0, CAPSLOCK_ON), 0);
  EXPECT_EQ(GetOzzyKey(VK_CANCEL, 0, kCtrl), 0);

  // Characters outside ASCII, and control codes other than Ctrl's letters.
  EXPECT_EQ(GetOzzyKey('E', 0xE9, 0), 0);
  EXPECT_EQ(GetOzzyKey(VK_OEM_4, 0x1B, kCtrl), 0);
}

}  // namespace
}  // namespace oz3
