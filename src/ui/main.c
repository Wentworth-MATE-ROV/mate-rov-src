// Joe Jevnik
// 2014.2.27
// Testing main

#include "../common.h"
#include "../comm/comm.h"
#include "screen.h"
#include "../controls/controls.h"

#include <stdio.h>
#include <pthread.h>

int main(void){
    rov_arduino a;
    rov_screen scr;
    size_t motorc,servoc;
    pthread_t pt;
    motorc = servoc = 0;
    if (init_arduino(&a,"/dev/ttyACM0","/dev/input/js0",
                      motorc,NULL,servoc,NULL,NULL,NULL,NULL)){
        fputs("Could not initialize the arduino\n",stderr);
        return -1;
    }
    init_screen(&scr,&a,fopen("/dev/null","w"));
    pthread_create(&pt,NULL,process_joystick,&scr);
    print_staticui(&scr);
    getch();
    pthread_cancel(pt);
    destroy_screen(&scr);
    destroy_arduino(&a);
    return 0;
}
