// Joe Jevnik
// 2014.2.27
// Testing main

#include "../common.h"
#include "../comm/comm.h"
#include "../controls/controls.h"
#include "../controls/keybinds.h"
#include "keyboard.h"

#include <stdio.h>
#include <pthread.h>

int main(void){
    rov_arduino a;
    rov_screen scr;
    size_t motorc,servoc;
    init_keybinds();
    motorc = servoc = 0;
    if (init_arduino(&a,"/dev/ttyACM0","/dev/input/js0",
                      motorc,NULL,servoc,NULL,NULL,NULL,NULL)){
        fputs("Could not initialize the arduino\n",stderr);
        return -1;
    }
    init_screen(&scr,fopen("/dev/null","w"),NULL,0,process_keyboard,&scr);
    print_staticui(&scr);
    screen_reload_keybinds(&scr,&a,false);
    process_keyboard(&scr);
    destroy_screen(&scr);
    destroy_arduino(&a);
    return 0;
}
