/* keyboard.c --- Functions that handle keyboard input.
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

#include "keyboard.h"

// Quick macro to faclitate printing a string to scr as boldgreen.
#define boldgreenprint(scr,str) screen_printattr(scr,GREEN_PAIR | A_BOLD,str)


// Test print for an axis.
static void print_jsaxis(rov_screen *scr,rov_jsaxis *j){
    if (j->is_pair){
        screen_printfattr(scr,GREEN_PAIR,"axis-pair: (%u %u) ",j->pos,j->neg);
    }else{
        screen_printfattr(scr,GREEN_PAIR,"axis: %u ",j->axis);
    }
}

// Quick macros to print if values differ.
#define DIFFCMPBUTTON(str,val,count)                                    \
    if (!diff || memcmp(&old.val,kbs->val,6)){                          \
        boldgreenprint(scr,str);                                        \
        for (n = 0;n < kbs->count;n++){                                 \
            screen_printfattr(scr,GREEN_PAIR,"%u ",kbs->val[n]);        \
        }                                                               \
        ++c;                                                            \
    }
#define DIFFCMPAXIS(str,val,count)                                      \
    if (!diff || memcmp(&old.val,kbs->val,12 * sizeof(rov_jsaxis))){    \
        boldgreenprint(scr,str);                                        \
        for (n = 0;n < kbs->count;n++){                                 \
            print_jsaxis(scr,&kbs->val[n]);                             \
        }                                                               \
        ++c;                                                            \
    }

// Reloads the keybinds from the .keybinds file.
void screen_reload_keybinds(rov_screen *scr,rov_arduino *a,bool diff){
    int           n,c = 0;
    rov_keybinds  old = a->keybinds;
    rov_keybinds *kbs = &a->keybinds;
    char          line[81];
    if (parse_keybinds(kbs,".keybinds")){
        screen_printattr(scr,RED_PAIR,"Failed to reload keybinds!");
        return;
    }
    memset(line,'-',80);
    line[81] = '\0';
    screen_printattr(scr,GREEN_PAIR,line);
    DIFFCMPBUTTON("claw-open: ",claw_openv,claw_openc);
    DIFFCMPBUTTON("claw-close: ",claw_closev,claw_closec);
    DIFFCMPAXIS("claw-x: ",claw_xv,claw_xc);
    DIFFCMPAXIS("claw-y: ",claw_yv,claw_yc);
    DIFFCMPAXIS("rotate-z: ",rotate_zv,rotate_zc);
    DIFFCMPAXIS("rotate-y: ",rotate_yv,rotate_yc);
    DIFFCMPAXIS("transpose-x: ",transpose_xv,transpose_xc);
    DIFFCMPAXIS("transpose-y: ",transpose_yv,transpose_yc);
    DIFFCMPAXIS("turn-y: ",turn_yv,turn_yc);
    DIFFCMPAXIS("thrust-mod: ",thrust_modv,thrust_modc);
    DIFFCMPBUTTON("laser-toggle: ",laser_togglev,laser_togglec);
    DIFFCMPBUTTON("headlight-toggle",headlight_togglev,headlight_togglec);
    DIFFCMPBUTTON("sidelight-toggle",sidelight_togglev,sidelight_togglec);
    if (!c){
        screen_printattr(scr,GREEN_PAIR | A_BOLD,"Nothing to reload!");
    }
}

// Quick macro to print only if two pin value differ.
#define DIFFCMPPIN(str,val,count)                                       \
    if (!diff || memcmp(&old.val,l->val,52)){                           \
        boldgreenprint(scr,str);                                        \
        for (n = 0;n < l->count;n++){                                   \
            screen_printfattr(scr,GREEN_PAIR,"%u ",l->val[n]);          \
        }                                                               \
        ++c;                                                            \
    }

void screen_reload_pinlayout(rov_screen *scr,rov_arduino *a,bool diff){
    int            n,c = 0;
    rov_pinlayout  old = a->layout;
    rov_pinlayout *l   = &a->layout;
    char           line[81];
    if (parse_pinlayout(l,".pins")){
        screen_printattr(scr,RED_PAIR,"Failed to reload pin layout!");
        return;
    }
    memset(line,'-',80);
    line[81] = '\0';
    screen_printattr(scr,GREEN_PAIR,line);
    DIFFCMPPIN("lasers: ",laserv,laserc);
    DIFFCMPPIN("headlights: ",headlightv,headlightc);
    DIFFCMPPIN("sidelights: ",sidelightv,sidelightc);
    DIFFCMPPIN("left-motor: ",leftmotorv,leftmotorc);
    DIFFCMPPIN("right-motor: ",rightmotorv,rightmotorc);
    DIFFCMPPIN("front-motor: ",frontmotorv,frontmotorc);
    DIFFCMPPIN("back-motor: ",backmotorv,backmotorc);
}

// Handles all keyboard presses.
// Pass the screen.
void process_keyboard(rov_screen *scr,rov_arduino *a){
    int c;
    while ((c = getch())){
        switch(c){
        case RELOAD_KEYBINDS:
            screen_reload_keybinds(scr,a,true);
            break;
        case RELOAD_PINS:
            screen_reload_pinlayout(scr,a,true);
            pinmode_sync(a);
            break;
        case QUIT_PROG:
            return;
        }
    }
}
