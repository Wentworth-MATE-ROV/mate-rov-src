/* controls.h --- Where the controls of the robot are handled.
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

#ifndef ROV_CONTROLS_H
#define ROV_CONTROLS_H

#include <libguile.h>
#include <limits.h>
#include <sys/time.h>

#include "../common.h"

// A type representing a pre-proccessed (keybind mappings applied) joystick.
// We use ints here so that we can in-place average the values from the many
// inputs. We call trunc_cleaned_val() on all the ints before returning from
// clean_joystick() so the range of all ints is that of a short.
typedef struct{
    bool claw_open;        // Claw open.
    bool claw_close;       // Claw close.
    bool laser_toggle;     // Laser toggle.
    bool headlight_toggle; // Headlights toggle.
    bool sidelight_toggle; // Sidelights toggle.
    int  claw_x;           // Claw x-axis.
    int  claw_y;           // Claw y-axis.
    int  rotate_z;         // Rotation about z.
    int  rotate_y;         // Rotation about y.
    int  transpose_x;      // Transposition over x.
    int  transpose_y;      // Transposition over y.
}rov_clean_js;

// Applies the keybinds to the joystick to clean it, storing the result in c.
void clean_joystick(rov_joystick*,rov_keybinds*,rov_clean_js*);

// Syncs the local control state back to the arduino if it has changed.
void sync_ctrlstate(rov_arduino*,rov_ctrlstate*);

// Process robot logic forever.
void *process_logic(void*);

// Converts a (sanatized) ctrl-state to an rov_controlstate.
// WARNING: Does no type checking, call sanatize-ctrl-state first.
void ctrl_from_scm(SCM,rov_ctrlstate*);

// return: A scheme js-state built from a clean joystick.
SCM scm_from_cjs(rov_clean_js*);

#endif
