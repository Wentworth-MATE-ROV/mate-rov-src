/* main.c --- Testing the main function.
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

#include "../common.h"
#include "../comm/comm.h"
#include "../controls/controls.h"
#include "../controls/keybinds.h"
#include "keyboard.h"
#include "../controls/pinlayout.h"

#include <stdio.h>
#include <pthread.h>

int main(void){
    rov_arduino   a;
    rov_screen    scr;
    size_t        motorc,servoc;
    pthread_t     jst;
    rov_pjs_param p;
    init_keybinds();
    motorc = servoc = 0;
    if (init_arduino(&a,"/dev/ttyACM0","/dev/input/js0",
                      motorc,NULL,servoc,NULL,NULL,NULL,NULL)){
        fputs("Could not initialize the arduino\n",stderr);
        return -1;
    }
    init_screen(&scr,fopen("/dev/null","w"),NULL,0);
    p.a   = &a;
    p.scr = &scr;
    p.phz = 1000;
    p.shz = 200;
    print_staticui(&scr);
    screen_reload_keybinds(&scr,&a,false);
    init_pinlayout(&a.layout);
    parse_pinlayout(&a.layout,".pins");
    pthread_create(&jst,NULL,process_joystick,&p);
    process_keyboard(&scr,&a);
    destroy_screen(&scr);
    destroy_arduino(&a);
    return 0;
}
