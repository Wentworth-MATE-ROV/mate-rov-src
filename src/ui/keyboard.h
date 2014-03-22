// Joe Jevnik
// 2014.3.18
// Keyboard operation handling.

#ifndef ROV_KEYBOARD_H
#define ROV_KEYBOARD_H

#include "../controls/keybinds.h"

// Macros for the keys:
#define RELOAD_KEYBINDS 'l'

// Reloads the keybinds from the .keybinds file.
void screen_reload_keybinds(rov_screen*,rov_arduino*,bool);

// Handles all keyboard presses.
// Pass the screen.
void *process_keyboard(void*);

#endif
