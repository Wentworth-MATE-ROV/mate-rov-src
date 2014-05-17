/* controls.h --- Where the controls of the robot are handled.
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

#ifndef ROV_CONTROLS_H
#define ROV_CONTROLS_H

#include <libguile.h>
#include <limits.h>

#include "../common.h"

// A type representing a pre-proccessed (keybind mappings applied) joystick.
typedef struct{
    bool  claw_open;        // Claw open.
    bool  claw_close;       // Claw close.
    bool  laser_toggle;     // Laser toggle.
    bool  headlight_toggle; // Headlights toggle.
    bool  sidelight_toggle; // Sidelights toggle.
    short claw_x;           // Claw x-axis.
    short claw_y;           // Claw y-axis.
    short rotate_z;         // Rotation about z.
    short rotate_y;         // Rotation about y.
    short transpose_x;      // Transposition over x.
    short transpose_y;      // Transposition over y.
}rov_clean_js;

// Applies the keybinds to the joystick to clean it, storing the result in c.
void clean_joystick(rov_joystick*,rov_keybinds*,rov_clean_js*);

// Syncs the local control state back to the arduino if it has changed.
void sync_ctrlstate(rov_arduino*,rov_ctrlstate*);

// Process joystick input forever.
void *process_joystick(void*);

// Holds the thread until all ssg's have completed or overide is set to true.
void wait_ssgs(rov_arduino*,bool*);

// Converts a (sanatized) ctrl-state to an rov_controlstate.
// WARNING: Does no type checking, call sanatize-ctrl-state first.
void ctrl_from_scm(SCM,rov_ctrlstate*);

// The scheme that will setup the records and sanatize function.
extern const char* const scm_init_logic_str;

// return: A scheme js-state built from a clean joystick.
SCM scm_from_cjs(rov_clean_js*);

#endif
