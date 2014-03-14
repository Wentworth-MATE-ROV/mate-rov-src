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

// An axis structure that is either a pair of buttons or an actual axis.
typedef struct{
    bool is_pair;              // Is this axis created out of (pair a b).
    union{
        struct{
            unsigned char pos; // Button relating to the postive dir.
            unsigned char neg; // Button relating to the negative dir.
        };
        unsigned char axis;    // The axis that this relates to.
    };
}rov_jsaxis;

// The default 6 axis that exist on the joystick.
extern rov_jsaxis x_axis;
extern rov_jsaxis y_axis;
extern rov_jsaxis twist_axis;
extern rov_jsaxis slider_axis;
extern rov_jsaxis hat_x_axis;
extern rov_jsaxis hat_y_axis;

// A structure to hold all the keybindings in memory.
typedef struct{
    size_t        claw_openc;      // The amount of bindings to claw_open.
    unsigned char claw_openv[12];  // Buttons to open the claw.
    size_t        claw_closec;     // The amount of bindings to claw_close.
    unsigned char claw_closev[12]; // Buttons to close the claw.
    size_t        rotate_xc;       // The amount of bindings to rotate_x.
    rov_jsaxis    rotate_xv[6];    // Axis to rotate about the x.
    size_t        rotate_yc;       // The amount of bindings to rotate_y.
    rov_jsaxis    rotate_yv[6];    // Axis to rotate about the y.
    size_t        transposec;      // The amount of bindings to transpose.
    rov_jsaxis    transposev[6];   // Axis to transpose the robot.
    size_t        turnc;           // The amount of bindings to turn.
    rov_jsaxis    turnv[6];        // Axis to turn the robot without rotating.
    size_t        thrust_modc;     // The amount of bindings to thrus_mod
    rov_jsaxis    thrust_modv[6];  // Axis to adjust the thrust power.
}rov_keybinds;

// The default set of keybinds.
extern rov_keybinds default_keybinds;

// Strings used in parsing opeerations:
extern const char* const claw_open_str;
extern const char* const claw_close_str;
extern const char* const rotate_x_str;
extern const char* const rotate_y_str;
extern const char* const transpose_str;
extern const char* const turn_str;
extern const char* const thrust_mod_str;

// Sets up the initial default keybind values, call before parsing!
void init_keybinds();

// Frees the key lists.
void destroy_keybinds(rov_keybinds*);

// Reads sexpr, updating the keybinds as needed.
// return: 0 on success, non-zero on failure.
int read_scm_line(rov_keybinds*,char*);

// Parses the params from a sexpr.
void parse_scm_params(rov_keybinds*, // The keybinds struct to modify.
                      char*,         // The operation
                      const char*,   // The operation to check against.
                      size_t,        // The amount of params.
                      void*,         // A pointer to the list of params.
                      size_t*,       // A pointer to the size of that op's size.
                      bool);         // Is this param a button?

// Parse a keybinds config out of a file (pass the path).
// If the file is NULL, or there is an error, returns a pointer to the default
// keybinds config.
// IO WARNING: Calls out to './keybinds-parser.scm' which requires guile.
// return: 0 on success, non-zero on failure.
int parse_keybinds(rov_keybinds*,const char*);

#endif
