// Joe Jevnik
// 2014.3.13
// Keybind defaults and parsing.

#ifndef ROV_KEYBINDS_H
#define ROV_KEYBINDS_H

#include "controls.h"

#include <stdbool.h>

// An axis structure that is either a pair of buttons or an actual axis.
typedef struct{
    const bool is_pair;              // Is this axis created out of (pair a b).
    union{
        struct{
            const unsigned char pos; // Button relating to the postive dir.
            const unsigned char neg; // Button relating to the negative dir.
        };
        const unsigned char axis;    // The axis that this relates to.
    };
}rov_jsaxis;

// The default 6 axis that exist on the joystick.
const rov_jsaxis x_axis      = { .is_pair = false,
                                 .axis    = ROV_JS_X };
const rov_jsaxis y_axis      = { .is_pair = false,
                                 .axis    = ROV_JS_Y };
const rov_jsaxis twist_axis  = { .is_pair = false,
                                 .axis    = ROV_JS_T };
const rov_jsaxis slider_axis = { .is_pair = false,
                                 .axis    = ROV_JS_S };
const rov_jsaxis hat_x_axis  = { .is_pair = false,
                                 .axis    = ROV_JS_HX };
const rov_jsaxis hat_y_axis  = { .is_pair = false,
                                 .axis    = ROV_JS_HY };

// A structure to hold all the keybindings in memory.
typedef struct{
    unsigned char claw_open;  // Button to open the claw.
    unsigned char claw_close; // Button to close the claw.
    rov_jsaxis    rotate_x;   // Axis to rotate about the x.
    rov_jsaxis    rotate_y;   // Axis to rotate about the y.
    rov_jsaxis    transpose;  // Axis to transpose the robot.
    rov_jsaxis    turn;       // Axis to turn the robot without rotating.
    rov_jsaxis    thrust_mod; // Axis to adjust the thrust power.
}rov_keybinds;

const rov_jsaxis test_pair_axis = { .is_pair = true,
                                    .pos     = 3,
                                    .neg     = 4 };

// The default set of keybinds.
const rov_keybinds default_keybinds = { .claw_open  = 5,
                                        .claw_close = 6,
                                        .rotate_x   = twist_axis,
                                        .rotate_y   = test_pair_axis,
                                        .transpose  = y_axis,
                                        .turn       = x_axis,
                                        .thrust_mod = slider_axis };

#endif
