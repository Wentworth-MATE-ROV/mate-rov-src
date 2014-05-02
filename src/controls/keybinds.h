// Joe Jevnik
// 2014.3.13
// Keybind defaults and parsing.

#ifndef ROV_KEYBINDS_H
#define ROV_KEYBINDS_H

#include "controls.h"

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <sys/stat.h>
#include <ctype.h>
#include <unistd.h>

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

// Reads sexpr, updating the keybinds as needed.
// return: 0 on success, non-zero on failure.
int keybinds_read_scm_line(rov_keybinds*,char*);

// Parses the params from a sexpr.
void keybinds_parse_scm_params(char*,         // The operation
                               const char*,   // The operation to check against.
                               size_t,        // The amount of params.
                               void*,         // A pointer to list of params.
                               size_t*,       // A pointer to the op's size.
                               bool);         // Is this param a button?

// Parse a keybinds config out of a file (pass the path).
// If the file is NULL, or there is an error, returns a pointer to the default
// keybinds config.
// IO WARNING: Calls out to './keybinds-parser.scm' which requires guile.
// return: 0 on success, non-zero on failure.
int parse_keybinds(rov_keybinds*,const char*);

#endif
