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
#include "../ui/stats.h"

// return: scales the value from the range of unsigned char to [0,180].
short scale_axisval(short v){
    return (v / 32767.0) * 180;
}

// return: truncates the power value in the range [-180,180].
short trunc_powerval(short v){
    return (v > 180) ? 180 : (v < -180) ? -180 : v;
}

// Re-reads the state from the joystick and keybinds.
void read_ctrlstate(rov_arduino *a){
    size_t n;
    int    lx,rx,lr,rr,fy,by,fr,br;
    lx = rx = lr = rr = fy = by = fr = br = 0;
    for (n = 0;n < a->keybinds.headlight_togglec;n++){
        if (is_button(&a->joystick,a->keybinds.headlight_togglev[n])){
            a->ctrl.headlights = !a->ctrl.headlights;
            break;
        }
    }
    for (n = 0;n < a->keybinds.sidelight_togglec;n++){
        if (is_button(&a->joystick,a->keybinds.sidelight_togglev[n])){
            a->ctrl.sidelights = !a->ctrl.sidelights;
            break;
        }
    }
    for (n = 0;n < a->keybinds.laser_togglec;n++){
        if (is_button(&a->joystick,a->keybinds.laser_togglev[n])){
            a->ctrl.lasers = !a->ctrl.lasers;
            break;
        }
    }
    for (n = 0;n < a->keybinds.claw_openc;n++){
        if (is_button(&a->joystick,a->keybinds.claw_openv[n])){
            a->ctrl.clawgrip = false;
            break;
        }
    }
    for (n = 0;n < a->keybinds.claw_closec;n++){
        if (is_button(&a->joystick,a->keybinds.claw_closev[n])){
            a->ctrl.clawgrip = true;
            break;
        }
    }
    for (n = 0;n < a->keybinds.transpose_xc;n++){
        if (a->keybinds.transpose_xv[n].is_pair){
            if (is_button(&a->joystick,a->keybinds.transpose_xv[n].pos)){
                lx += 180;
            }
            if (is_button(&a->joystick,a->keybinds.transpose_xv[n].neg)){
                lx -= 180;
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
                lr += 180;
            }
            if (is_button(&a->joystick,a->keybinds.rotate_yv[n].neg)){
                lr -= 180;
            }
        }else{
            lr += scale_axisval(a->joystick.axes[a->keybinds
                                                 .rotate_yv[n].axis]);
        }
    }
    for (n = 0;n < a->keybinds.transpose_yc;n++){
        if (a->keybinds.transpose_yv[n].is_pair){
            if (is_button(&a->joystick,a->keybinds.transpose_yv[n].pos)){
                fy += 180;
            }
            if (is_button(&a->joystick,a->keybinds.transpose_yv[n].neg)){
                fy -= 180;
            }
        }else{
            fy += scale_axisval(a->joystick.axes[a->keybinds
                                                 .transpose_yv[n].axis]);
        }
    }
    for (n = 0;n < a->keybinds.rotate_zc;n++){
        if (a->keybinds.rotate_zv[n].is_pair){
            if (is_button(&a->joystick,a->keybinds.rotate_zv[n].pos)){
                fr += 180;
            }
            if (is_button(&a->joystick,a->keybinds.rotate_zv[n].neg)){
                fr -= 180;
            }
        }else{
            fr += scale_axisval(a->joystick.axes[a->keybinds
                                                 .rotate_zv[n].axis]);
        }
    }
    lr                 /= a->keybinds.rotate_yc;
    rr                 =  -lr;
    by                 =  fy;
    br                 =  -fr;
    a->ctrl.leftmotor  =  trunc_powerval(lx + lr);
    a->ctrl.rightmotor =  trunc_powerval(rx + rr);
    a->ctrl.frontmotor =  trunc_powerval(fy + fr);
    a->ctrl.backmotor  =  trunc_powerval(by + br);
}

// Syncs the local control state back to the arduino if it has changed.
void sync_ctrlstate(rov_arduino *a,rov_ctrlstate *old){
    size_t n;
    short  v;
    bool   b;
    for (n = 0;n < a->layout.laserc;n++){
        digital_write(a,a->layout.laserv[n],a->ctrl.lasers);
    }
    for (n = 0;n < a->layout.headlightc;n++){
        digital_write(a,a->layout.headlightv[n],a->ctrl.headlights);
    }
    for (n = 0;n < a->layout.sidelightc;n++){
        digital_write(a,a->layout.sidelightv[n],a->ctrl.sidelights);
    }
    for (n = 0;n < a->layout.clawgripc;n++){
        digital_write(a,a->layout.clawgripv[n],a->ctrl.clawgrip);
    }
    if ((a->ctrl.leftmotor > 0) != (old->leftmotor > 0)){
        b = a->ctrl.leftmotor > 0;
        for (n = 0;n < a->layout.leftmotordc;n++){
            digital_write(a,a->layout.leftmotordv[n],b);
        }
    }
    v = abs(a->ctrl.leftmotor);
    for (n = 0;n < a->layout.leftmotorc;n++){
        servo_write(a,a->layout.leftmotorv[n],v);
    }
    if ((a->ctrl.rightmotor > 0) != (old->rightmotor > 0)){
        b = a->ctrl.rightmotor > 0;
        for (n = 0;n < a->layout.rightmotordc;n++){
            digital_write(a,a->layout.rightmotordv[n],b);
        }
    }
    v = abs(a->ctrl.rightmotor);
    for (n = 0;n < a->layout.rightmotorc;n++){
        servo_write(a,a->layout.rightmotorv[n],v);
    }
    if ((a->ctrl.frontmotor > 0) != (old->frontmotor > 0)){
        b = a->ctrl.frontmotor > 0;
        for (n = 0;n < a->layout.frontmotordc;n++){
            digital_write(a,a->layout.frontmotordv[v],b);
        }
    }
    v = abs(a->ctrl.frontmotor);
    for (n = 0;n < a->layout.frontmotorc;n++){
        servo_write(a,a->layout.frontmotorv[n],v);
    }
    if ((a->ctrl.backmotor > 0) != (old->backmotor > 0)){
        b = a->ctrl.backmotor > 0;
        for (n = 0;n < a->layout.backmotordc;n++){
            digital_write(a,a->layout.backmotordv[n],b);
        }
    }
    v = abs(a->ctrl.backmotor);
    for (n = 0;n < a->layout.backmotorc;n++){
        servo_write(a,a->layout.backmotorv[n],v);
    }
}

// Process joystick input forever.
void *process_joystick(void *vscr_hz){
    rov_pjs_param *p          = vscr_hz;
    rov_screen    *scr        = p->scr;
    rov_arduino   *a          = p->a;
    rov_joystick   oldjs      = a->joystick;
    rov_ctrlstate  oldctrl    = a->ctrl;
    useconds_t     sleep_time = p->phz / 1000000;
    update_stats(scr,a);
    for (;;){
        read_jsevent(&a->joystick);
        if (memcmp(&a->joystick,&oldjs,sizeof(rov_joystick))){
            oldctrl = a->ctrl;
            read_ctrlstate(a);
            sync_ctrlstate(a,&oldctrl);
            diff_update_stats(scr,a,&oldctrl);
        }
        oldjs = a->joystick;
        usleep(sleep_time); // Sleep the thread (quantizes the polling rate).
    }
    return NULL;
}
