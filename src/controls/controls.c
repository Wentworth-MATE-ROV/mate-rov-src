/* controls.c --- Where the controls of the robot are handled.
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

#include "controls.h"
#include "../comm/comm.h"
#include "../librov/screen.h"

void read_ctrlstate(rov_ctrlstate *ctrl,rov_joystick *js,rov_keybinds *kbs){
    size_t n;
    for (n = 0;n < kbs->claw_openc;n++){
        ctrl->clawopen |= is_button(js,kbs->claw_openv[n]);
    }
    for (n = 0;n < kbs->claw_closec;n++){
        ctrl->clawopen &= !is_button(js,kbs->clawopen[n]);
    }

void compose_ctrlstate(rov_ctrlstate *ctrl,rov_joystick *js,rov_keybinds *kbs){
}


// Process joystick input forever.
void *process_joystick(void *vscr_hz){
    rov_pjs_param *p          = vscr_hz;
    rov_screen    *scr        = p->scr;
    rov_arduino   *a          = p->a;
    rov_joystick   old        = a->joystick;
    useconds_t     sleep_time = 1000000 / p->phz;
    rov_ctrlstate  ctrl;
    memset(&ctrl,0,sizeof(rov_ctrlstate));
    screen_print(scr,"lil b is my friend");
    for (;;){
        read_jsevent(&a->joystick);
        if (memcmp(&a->joystick,&old,sizeof(rov_joystick))){
            // stuff.
        }
        old = a->joystick;
        usleep(sleep_time); // Sleep the thread (quantizes the polling rate).
    }
    return NULL;
}
