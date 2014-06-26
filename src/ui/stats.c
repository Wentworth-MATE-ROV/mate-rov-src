/* stats.c --- Manages the printing of the robot's stats.
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

#include "stats.h"

char *statv[STATC] = { "left motor: ",
                       "right motor:",
                       "front motor:",
                       "back motor: ",
                       "headlights: ",
                       "sidelights: ",
                       "lasers:     ",
                       "claw-grip:",
                       "cp:    ",
                       "",
                       "miswrites: " };

// return: the appropriate color for the given motor value.
int getstatcol(rov_motor p){
    if (!p){
        return GREEN_PAIR | A_BOLD;
    }
    p = abs(p);
    if (p <= 25){
        return GREEN_PAIR;
    }
    if (p <= 50){
        return YELLOW_PAIR;
    }
    if (p < 75 && p != 100){
        return RED_PAIR;
    }
    return RED_PAIR | A_BOLD;
}

void update_claw_pos_stat(rov_screen *scr,rov_ctrlstate *ctrl){
    unsigned char s = 0;
    if (ctrl->claw_90){
        s |= 1;
    }
    if (ctrl->claw_180){
        s |= 2;
    }
    switch(s){
    case 0:
        update_statvattr(scr,CLAW_POS_STAT,RED_PAIR | A_BOLD,"(#f . #f)");
        return;
    case 1:
        update_statvattr(scr,CLAW_POS_STAT,RED_PAIR,"(#t . #f)");
        return;
    case 2:
        update_statvattr(scr,CLAW_POS_STAT,GREEN_PAIR,"(#f . #t)");
        return;
    case 3:
        update_statvattr(scr,CLAW_POS_STAT,GREEN_PAIR | A_BOLD,"(#t . #t)");
        return;
    }
}

// Updates all the stats.
void update_stats(rov_screen *scr,rov_arduino *a){
    bool   b;
    double v;
    update_statvfattr(scr,LEFTMOTOR_STAT,getstatcol(a->ctrl.leftmotor),
                      "%+d",a->ctrl.leftmotor);
    update_statvfattr(scr,RIGHTMOTOR_STAT,getstatcol(a->ctrl.rightmotor),
                      "%+d",a->ctrl.rightmotor);
    update_statvfattr(scr,FRONTMOTOR_STAT,getstatcol(a->ctrl.frontmotor),
                      "%+d",a->ctrl.frontmotor);
    update_statvfattr(scr,BACKMOTOR_STAT,getstatcol(a->ctrl.backmotor),
                      "%+d",a->ctrl.backmotor);
    b = a->ctrl.headlights;
    update_statvattr(scr,HEADLIGHTS_STAT,(b) ? GREEN_PAIR : RED_PAIR,
                     (b) ? "on" : "off");
    b = a->ctrl.sidelights;
    update_statvattr(scr,SIDELIGHTS_STAT,(b) ? GREEN_PAIR : RED_PAIR,
                     (b) ? "on" : "off");
    b = a->ctrl.lasers;
    update_statvattr(scr,LASERS_STAT,(b) ? GREEN_PAIR : RED_PAIR,
                     (b) ? "on" : "off");
    b = a->ctrl.clawgrip;
    update_statvattr(scr,CLAW_STAT,(b) ? RED_PAIR : GREEN_PAIR,
                         (b) ? "closed" : "  open");
    update_claw_pos_stat(scr,&a->ctrl);
    v = ((double) a->queue.miswrites) / a->queue.writes;
    update_statvfattr(scr,MISWRITES_STAT,
                      (v > 0.05) ? RED_PAIR : GREEN_PAIR,"%.2lf",v);
}

// Updates the stats that have changed.
void diff_update_stats(rov_screen *scr,rov_arduino *a,rov_ctrlstate *old){
    bool   b;
    double v;
    if (a->ctrl.leftmotor != old->leftmotor){
        update_statvfattr(scr,LEFTMOTOR_STAT,getstatcol(a->ctrl.leftmotor),
                          "%+d",a->ctrl.leftmotor);
    }
    if (a->ctrl.rightmotor != old->rightmotor){
        update_statvfattr(scr,RIGHTMOTOR_STAT,getstatcol(a->ctrl.rightmotor),
                          "%+d",a->ctrl.rightmotor);
    }
    if (a->ctrl.frontmotor != old->frontmotor){
        update_statvfattr(scr,FRONTMOTOR_STAT,getstatcol(a->ctrl.frontmotor),
                          "%+d",a->ctrl.frontmotor);
    }
    if (a->ctrl.backmotor != old->backmotor){
        update_statvfattr(scr,BACKMOTOR_STAT,getstatcol(a->ctrl.backmotor),
                          "%+d",a->ctrl.backmotor);
    }
    if (a->ctrl.headlights != old->headlights){
        b = a->ctrl.headlights;
        update_statvattr(scr,HEADLIGHTS_STAT,(b) ? GREEN_PAIR : RED_PAIR,
                         (b) ? "on" : "off");
    }
    if (a->ctrl.sidelights != old->sidelights){
        b = a->ctrl.sidelights;
        update_statvattr(scr,SIDELIGHTS_STAT,(b) ? GREEN_PAIR : RED_PAIR,
                         (b) ? "on" : "off");
    }
    if (a->ctrl.lasers != old->lasers){
        b = a->ctrl.lasers;
        update_statvattr(scr,LASERS_STAT,(b) ? GREEN_PAIR : RED_PAIR,
                         (b) ? "on" : "off");
    }
    if (a->ctrl.clawgrip != old->clawgrip){
        b = a->ctrl.clawgrip;
        update_statvattr(scr,CLAW_STAT,(b) ? RED_PAIR : GREEN_PAIR,
                         (b) ? "closed" : "  open");
    }
    v = ((double) a->queue.miswrites) / a->queue.writes;
    if (a->ctrl.claw_90 != old->claw_90 || a->ctrl.claw_180 != old->claw_180){
        update_claw_pos_stat(scr,&a->ctrl);
    }
    update_statvfattr(scr,MISWRITES_STAT,
                      (v > 0.05) ? RED_PAIR : GREEN_PAIR,"%.2lf",v);
}
