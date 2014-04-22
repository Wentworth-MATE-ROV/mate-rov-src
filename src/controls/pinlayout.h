/* pinlayout.h --- Contstruction of pinlayouts.
   Copyright (c) Joe Jevnik

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along with
   this program; if not, write to the Free Software Foundation, Inc., 51
   Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

#ifndef ROV_PINLAYOUT_H
#define ROV_PINLAYOUT_H

#include "../common.h"
#include "../comm/comm.h"

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <sys/stat.h>
#include <ctype.h>
#include <unistd.h>

// The strings used when parsing the pins.
extern const char* const pin_laser_str;
extern const char* const pin_headlight_str;
extern const char* const pin_sidelight_str;
extern const char* const pin_leftmotor_str;
extern const char* const pin_rightmotor_str;
extern const char* const pin_frontmotor_str;
extern const char* const pin_backmotor_str;

// Zeros the pinlayout.
void init_pinlayout(rov_pinlayout*);

// Reads sexpr, updating the layout as needed.
// return: 0 on success, non-zero on failure.
int pin_read_scm_line(rov_pinlayout*,char*);

// Parse a pin layout config out of a file (pass the path).
// If the file is NULL, or there is an error, returns a pointer to the default
// keybinds config.
// IO WARNING: Calls out to './pin-parser.scm' which requires guile.
// return: 0 on success, non-zero on failure.
int parse_pinlayout(rov_pinlayout*,const char*);

// Sets the pinmodes for the needed pins.
void pinmode_sync(rov_arduino*);

#endif
