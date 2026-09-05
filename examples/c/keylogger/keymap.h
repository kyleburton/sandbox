#ifndef KEYMAP_H
#define KEYMAP_H

// Returns the printable US-QWERTY character produced by pressing `code`
// with `shift_pressed` reflecting current shift key state, or '\0' if the
// key does not produce a printable character (e.g. KEY_ESC, KEY_ENTER).
char key_to_char(unsigned int code, int shift_pressed);

#endif
