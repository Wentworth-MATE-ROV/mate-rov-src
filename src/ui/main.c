/* main.c --- Testing the main function.
Copyright (c) Joe Jevnik

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51
Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

#include "../common.h"
#include "../comm/comm.h"
#include "keyboard.h"
#include "stats.h"
#include "../controls/pinlayout.h"

#include <stdio.h>
#include <pthread.h>

char *statv[STATC];

int main(void){
    rov_arduino   a;
    rov_screen    scr;
    pthread_t     jst;
    rov_pjs_param pjp;
    rov_pq_param  pqp;
    init_keybinds();
    if (init_arduino(&a,"/dev/ttyACM0","/dev/input/js0")){
        fputs("Could not initialize the arduino\n",stderr);
        return -1;
    }
    init_screen(&scr,fopen("/dev/null","w"),STATC,YELLOW_PAIR,statv);
    print_staticui(&scr);
    screen_printattr(&scr,GREEN_PAIR,"Waiting 2s for Arduino...");
    sleep(2);
    pqp.scr = &scr;
    pqp.q   = &a.queue;
    pthread_create(&a.qt,NULL,process_queue,&pqp);
    pjp.a   = &a;
    pjp.scr = &scr;
    pjp.phz = 100;
    pjp.shz = 100;
    screen_reload_keybinds(&scr,&a,false);
    screen_reload_pinlayout(&scr,&a,false);
    pinmode_sync(&a);
    pthread_create(&jst,NULL,process_joystick,&pjp);
    process_keyboard(&scr,&a);
    destroy_screen(&scr);
    destroy_arduino(&a);
    return 0;
}
