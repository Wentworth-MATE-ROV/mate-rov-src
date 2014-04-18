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

#include "../common.h"

// The states we can be in.
typedef struct{
    bool  clawopen;     // The state of the claw being open or closed.
    bool  laser;        // The state of the laser.
    bool  headlight;    // Are the headlights on or off.
    bool  sideligh;     // Are the sidelights on or off.
    char  clawposition; // The positions of the claw.
    short powerleft;    // Power to the left motor.
    short powerright;   // Power to the right motor.
    short powerfront;   // Power to the front motor.
    short powerback;    // Power to the back motor.
}rov_ctrlstate;


void compose_ctrlstate(rov_controlstate*,rov_joystick*,keybinds*);

// Process joystick input forever.
void *process_joystick(void*);

#endif
