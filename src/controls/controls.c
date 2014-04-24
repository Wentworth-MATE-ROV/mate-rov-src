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

// return: scales the value from the range of unsigned char to [0,256).
unsigned char scale_axisval(short v){
    return ((32767 + v) / 65534.0) * 255;
}

// return: truncates the power value in the range [-1024,1024].
unsigned char trunc_powerval(int v){
    return (v > 255) ? 255 : (v < -255) ? -255 : v;
}

// Re-reads the state from the joystick and keybinds.
void read_ctrlstate(rov_arduino *a){
    size_t n;
    short  lx,rx,lr,rr;
    for (n = 0;n < a->keybinds.headlight_togglec;n++){
        if (is_button(&a->joystick,a->keybinds.headlight_togglev[n])){
            a->headlights = !a->headlights;
            break;
        }
    }
    for (n = 0;n < a->keybinds.sidelight_togglec;n++){
        if (is_button(&a->joystick,a->keybinds.sidelight_togglev[n])){
            a->sidelights = !a->sidelights;
            break;
        }
    }
    for (n = 0;n < a->keybinds.laser_togglec;n++){
        if (is_button(&a->joystick,a->keybinds.laser_togglev[n])){
            a->lasers = !a->lasers;
            break;
        }
    }
    for (n = 0;n < a->keybinds.claw_openc;n++){
        if (is_button(&a->joystick,a->keybinds.claw_openv[n])){
            a->clawgrip = false;
            break;
        }
    }
    for (n = 0;n < a->keybinds.claw_closec;n++){
        if (is_button(&a->joystick,a->keybinds.claw_openv[n])){
            a->clawgrip = true;
            break;
        }
    }
    for (n = 0;n < a->keybinds.transpose_xc;n++){
        if (a->keybinds.transpose_xv[n].is_pair){
            if (is_button(&a->joystick,a->keybinds.transpose_xv[n].pos)){
                lx += 1023;
            }else if (is_button(&a->joystick,a->keybinds.transpose_xv[n].neg)){
                lx -= 1023;
            }
        }else{
            lx += scale_axisval(a->joystick.axes[a->keybinds
                                                 .transpose_xv[n].axis]);
        }
    }
    lx /= a->keybinds.transpose_xc;
    rx = lx;
    for (n = 0;n < a->keybinds.rotate_yc;n++){
        if (a->keybinds.rotate_yv[n].is_pair){
            if (is_button(&a->joystick,a->keybinds.rotate_yv[n].pos)){
                lr += 1023;
            }else if (is_button(&a->joystick,a->keybinds.rotate_yv[n].neg)){
                lr -= 1023;
            }
        }else{
            lr += scale_axisval(a->joystick.axes[a->keybinds
                                                 .rotate_yv[n].axis]);
        }
    }
    lr            /= a->keybinds.rotate_yc;
    rr            =  -lr;
    a->leftmotor  =  trunc_powerval(lx + lr);
    a->rightmotor =  trunc_powerval(rx + rr);
}


void sync_ctrlstate(rov_arduino *a){
    size_t n;
    for (n = 0;n < a->layout.laserc;n++){
        digital_write(a,a->layout.laserv[n],a->lasers);
    }
    for (n = 0;n < a->layout.headlightc;n++){
        digital_write(a,a->layout.headlightv[n],a->headlights);
    }
    for (n = 0;n < a->layout.sidelightc;n++){
        digital_write(a,a->layout.sidelightv[n],a->sidelights);
    }
    for (n = 0;n < a->layout.leftmotorc;n++){
        analog_write(a,a->layout.leftmotorv[n],a->leftmotor);
    }
}

// Process joystick input forever.
void *process_joystick(void *vscr_hz){
    rov_pjs_param *p          = vscr_hz;
    rov_screen    *scr        = p->scr;
    rov_arduino   *a          = p->a;
    rov_joystick   old        = a->joystick;
    useconds_t     sleep_time = p->phz / 1000000;
    for (;;){
        read_jsevent(&a->joystick);
        if (memcmp(&a->joystick,&old,sizeof(rov_joystick))){
            read_ctrlstate(a);
            sync_ctrlstate(a);
            screen_printf(scr,"%d",a->leftmotor);
        }
        old = a->joystick;
        usleep(sleep_time); // Sleep the thread (quantizes the polling rate).
    }
    return NULL;
}
