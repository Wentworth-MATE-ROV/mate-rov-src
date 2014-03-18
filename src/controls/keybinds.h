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
extern rov_jsaxis x_axis;            // Joystick's x axis.
extern rov_jsaxis y_axis;            // Joystick's y axis.
extern rov_jsaxis twist_axis;        // Joystick's twisting axis.
extern rov_jsaxis slider_axis;       // Joystick's slider.
extern rov_jsaxis hat_x_axis;        // Hat switch x axis.
extern rov_jsaxis hat_y_axis;        // Hat switch y axis.

extern rov_jsaxis trans_y_pair_axis; // Axis pair to translate y;
extern rov_jsaxis rot_z_pair_axis;   // Axis to rotate about the z.

// A structure to hold all the keybindings in memory.
typedef struct{
    size_t        claw_openc;        // The amount of bindings to claw_open.
    unsigned char claw_openv[12];    // Buttons to open the claw.
    size_t        claw_closec;       // The amount of bindings to claw_close.
    unsigned char claw_closev[12];   // Buttons to close the claw.
    size_t        claw_xc;           // The amounf of bindings to claw_x;
    rov_jsaxis    claw_xv[6];        // Axes that move the claw along the x.
    size_t        claw_yc;           // The amount of bindings to claw_y.
    rov_jsaxis    claw_yv[6];        // Axes that move the claw along the y.
    size_t        rotate_zc;         // The amount of bindings to rotate_z.
    rov_jsaxis    rotate_zv[6];      // Axes to rotate about the z.
    size_t        rotate_yc;         // The amount of bindings to rotate_y.
    rov_jsaxis    rotate_yv[6];      // Axes to rotate about the y.
    size_t        transpose_xc;      // The amount of bindings to transpose_x.
    rov_jsaxis    transpose_xv[6];   // Axes to transpose the robot along the x.
    size_t        transpose_yc;      // The amount of bindings to transpose_y.
    rov_jsaxis    transpose_yv[6];   // Axes to transpose the robot along the y.
    size_t        turn_yc;           // The amount of bindings to turn around y.
    rov_jsaxis    turn_yv[6];        // Axes to turn the robot without rotating.
    size_t        thrust_modc;       // The amount of bindings to thrus_mod
    rov_jsaxis    thrust_modv[6];    // Axes to adjust the thrust power.
    size_t        laser_onc;         // The amount of bindings to laser_on.
    unsigned char laser_onv[12];     // Buttons that turn the lasers on.
    size_t        laser_offc;        // The amount of bindings to laser_off.
    unsigned char laser_offv[12];    // Buttons that turn the lasers off.
    size_t        laser_togglec;     // The amount of bindings to laser_toggle.
    unsigned char laser_togglev[12]; // Buttons that toggle the laser's state.
}rov_keybinds;

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
extern const char* const turn_y_str;
extern const char* const thrust_mod_str;
extern const char* const laser_on_str;
extern const char* const laser_off_str;
extern const char* const laser_toggle_str;
// Sets up the initial default keybind values, call before parsing!
void init_keybinds(void);

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
