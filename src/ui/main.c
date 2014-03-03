// Joe Jevnik
// 2014.2.27
// Testing main

#include "../common.h"
#include "../comm/comm.h"
#include "screen.h"

#include <stdio.h>

int main(void){
    rov_arduino a;
    rov_screen scr;
    size_t motorc,servoc;
    motorc = servoc = 0;
    if (!init_arduino(&a,"/dev/ttyACM0",motorc,NULL,servoc,NULL,NULL,NULL,NULL)){
        fputs("Could not initialize the arduino",stderr);
        return -1;
    }
    init_screen(&scr,&a,fopen("/dev/null","w"));
    print_staticui(&scr);
    getch();
    writeln(&scr,"test message");
    getch();
    writeln_attr(&scr,"test bold",A_BOLD);
    getch();
    writeln_attr(&scr,"test red",RED_PAIR);
    getch();
    destroy_screen(&scr);
    destroy_arduino(&a);
    return 0;
}
