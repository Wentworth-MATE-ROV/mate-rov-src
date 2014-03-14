// Joe Jevnik
// 2014.2.27
// Testing main

#include "../common.h"
#include "../comm/comm.h"
#include "screen.h"
#include "../controls/controls.h"
#include "../controls/keybinds.h"

#include <stdio.h>
#include <pthread.h>

// Test print for an axis.
void print_jsaxis(rov_jsaxis *j){
    if (j->is_pair){
        printf("axis-pair: (%u %u) ",j->pos,j->neg);
    }else{
        printf("axis: %u ",j->axis);
    }
}

// Test main for the keybinds parsing.
int main(void){
    int n;
    rov_keybinds kbs;
    init_keybinds();
    parse_keybinds(&kbs,".keybinds_rand");
    printf("claw-open: ");
    for (n = 0;n < kbs.claw_openc;n++){
        printf("%u ",kbs.claw_openv[n]);
    }
    putchar('\n');
    printf("claw-close: ");
    for (n = 0;n < kbs.claw_closec;n++){
        printf("%u ",kbs.claw_closev[n]);
    }
    putchar('\n');
    printf("rotate-x: ");
    for (n = 0;n < kbs.rotate_xc;n++){
        print_jsaxis(&kbs.rotate_xv[n]);
    }
    putchar('\n');
    printf("rotate-y: ");
    for (n = 0;n < kbs.rotate_yc;n++){
        print_jsaxis(&kbs.rotate_yv[n]);
    }
    putchar('\n');
    printf("transpose: ");
    for (n = 0;n < kbs.transposec;n++){
        print_jsaxis(&kbs.transposev[n]);
    }
    putchar('\n');
    printf("turn: ");
    for (n = 0;n < kbs.turnc;n++){
        print_jsaxis(&kbs.turnv[n]);
    }
    putchar('\n');
    printf("thrust-mod: ");
    for (n = 0;n < kbs.thrust_modc;n++){
        print_jsaxis(&kbs.thrust_modv[n]);
    }
    putchar('\n');
    return 0;
}

/*
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
*/
