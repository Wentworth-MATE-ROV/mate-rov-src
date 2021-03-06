/* keybinds.h --- Parsing of the .keybinds file.
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

#ifndef ROV_KEYBINDS_H
#define ROV_KEYBINDS_H

#include "controls.h"

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <sys/stat.h>
#include <ctype.h>
#include <unistd.h>
#include <libguile.h>

// The default 6 axis that exist on the joystick.
extern rov_jsaxis x_axis;            // Joystick's x axis.
extern rov_jsaxis y_axis;            // Joystick's y axis.
extern rov_jsaxis twist_axis;        // Joystick's twisting axis.
extern rov_jsaxis slider_axis;       // Joystick's slider.
extern rov_jsaxis hat_x_axis;        // Hat switch x axis.
extern rov_jsaxis hat_y_axis;        // Hat switch y axis.

extern rov_jsaxis trans_y_pair_axis; // Axis pair to translate y;
extern rov_jsaxis rot_z_pair_axis;   // Axis to rotate about the z.

// The default set of keybinds.
extern rov_keybinds default_keybinds;

// Strings used in parsing opeerations:
extern const char* const claw_open_str;
extern const char* const claw_close_str;
extern const char* const claw_x_str;
extern const char* const claw_y_str;
extern const char* const rotate_z_str;
extern const char* const rotate_y_str;
extern const char* const transpose_x_str;
extern const char* const transpose_y_str;
extern const char* const laser_toggle_str;
extern const char* const headlight_toggle_str;
extern const char* const sidelight_toggle_str;

// Sets up the initial default keybind values, call before parsing!
void init_keybinds(void);

// Reads the SCM, updating the keybinds as needed.
// return: 0 on success, non-zero on failure.
int keybinds_read_scm_line(rov_keybinds*,SCM);

// Parse a keybinds config out of a file (pass the path).
// If the file is NULL, or there is an error, returns a pointer to the default
// keybinds config.
// IO WARNING: Calls out to './keybinds-parser.scm'.
// return: 0 on success, non-zero on failure.
int parse_keybinds(rov_keybinds*,const char*);

#endif
