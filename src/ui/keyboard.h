/* keyboard.h --- Functions that handle keyboard input.
   Copyright (c) Joe Jevnik

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along with
   this program; if not, write to the Free Software Foundation, Inc., 51
   Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

#ifndef ROV_KEYBOARD_H
#define ROV_KEYBOARD_H

#include "../controls/keybinds.h"
#include "../controls/pinlayout.h"

// Macros for the keys:
#define RELOAD_KEYBINDS 'l'
#define RELOAD_PINS     'p'
#define QUIT_PROG       ']'

// Reloads the keybinds from the .keybinds file and prints differences.
void screen_reload_keybinds(rov_screen*,rov_arduino*,const char*,bool);

// Reloads the pinlayout from the .pins file and prints differences.
void screen_reload_pinlayout(rov_screen*,rov_arduino*,const char*,bool);

// Handles all keyboard presses.
void process_keyboard(rov_screen*,rov_arduino*,const char*,const char*);

#endif
