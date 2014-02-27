// Joe Jevnik
// 2014.2.27
// Testing main

#include "../common.h"
#include "../comm/comm.h"

#include <stdio.h>

int main(void){
    rov_arduino a;
    size_t motorc,servoc;
    motorc = servoc = 0;
    if (!init_arduino(&a,"/dev/ttyACM0",motorc,NULL,servoc,NULL,NULL,NULL,NULL)){
        fputs("Could not initialize the arduino",stderr);
        return -1;
    }
    return 0;
}
