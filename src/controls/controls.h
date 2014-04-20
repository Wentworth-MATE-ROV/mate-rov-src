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

#define HEADLIGHT_PIN 50

// The states we can be in.
typedef struct{
    bool  clawgrip;     // The state of the claw's grip.
    bool  lasers;       // The state of the lasers.
    bool  headlights;   // The state of the headlights.
    bool  sidelights;   // The the state of the side lights.
    char  clawpos;      // The claw's position.
    short powerleft;    // Power to the left motor.
    short powerright;   // Power to the right motor.
    short powerfront;   // Power to the front motor.
    short powerback;    // Power to the back motor.
}rov_ctrlstate;

// Process joystick input forever.
void *process_joystick(void*);

#endif
