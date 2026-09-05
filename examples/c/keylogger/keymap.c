// Hand-authored US-QWERTY layout table. Unlike constants.c, this is not
// derived from the kernel header -- the shifted symbol for each key isn't
// data the kernel exposes, it's a property of the keyboard layout.
#include <linux/input-event-codes.h>

#include "keymap.h"

typedef struct {
  char normal;
  char shifted;
} KeyChars;

static const KeyChars key_chars[KEY_CNT] = {
  [KEY_SPACE] = {' ', ' '},

  [KEY_1] = {'1', '!'},
  [KEY_2] = {'2', '@'},
  [KEY_3] = {'3', '#'},
  [KEY_4] = {'4', '$'},
  [KEY_5] = {'5', '%'},
  [KEY_6] = {'6', '^'},
  [KEY_7] = {'7', '&'},
  [KEY_8] = {'8', '*'},
  [KEY_9] = {'9', '('},
  [KEY_0] = {'0', ')'},

  [KEY_MINUS]      = {'-', '_'},
  [KEY_EQUAL]      = {'=', '+'},
  [KEY_LEFTBRACE]  = {'[', '{'},
  [KEY_RIGHTBRACE] = {']', '}'},
  [KEY_SEMICOLON]  = {';', ':'},
  [KEY_APOSTROPHE] = {'\'', '"'},
  [KEY_GRAVE]      = {'`', '~'},
  [KEY_BACKSLASH]  = {'\\', '|'},
  [KEY_COMMA]      = {',', '<'},
  [KEY_DOT]        = {'.', '>'},
  [KEY_SLASH]      = {'/', '?'},

  [KEY_Q] = {'q', 'Q'},
  [KEY_W] = {'w', 'W'},
  [KEY_E] = {'e', 'E'},
  [KEY_R] = {'r', 'R'},
  [KEY_T] = {'t', 'T'},
  [KEY_Y] = {'y', 'Y'},
  [KEY_U] = {'u', 'U'},
  [KEY_I] = {'i', 'I'},
  [KEY_O] = {'o', 'O'},
  [KEY_P] = {'p', 'P'},
  [KEY_A] = {'a', 'A'},
  [KEY_S] = {'s', 'S'},
  [KEY_D] = {'d', 'D'},
  [KEY_F] = {'f', 'F'},
  [KEY_G] = {'g', 'G'},
  [KEY_H] = {'h', 'H'},
  [KEY_J] = {'j', 'J'},
  [KEY_K] = {'k', 'K'},
  [KEY_L] = {'l', 'L'},
  [KEY_Z] = {'z', 'Z'},
  [KEY_X] = {'x', 'X'},
  [KEY_C] = {'c', 'C'},
  [KEY_V] = {'v', 'V'},
  [KEY_B] = {'b', 'B'},
  [KEY_N] = {'n', 'N'},
  [KEY_M] = {'m', 'M'},
};

char key_to_char(unsigned int code, int shift_pressed) {
  if (code >= KEY_CNT) {
    return '\0';
  }

  KeyChars chars = key_chars[code];
  return shift_pressed ? chars.shifted : chars.normal;
}
