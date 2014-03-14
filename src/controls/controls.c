// Joe Jevnik
// 2014.3.6
// Joystick control implementations.

#include "controls.h"
#include "../ui/screen.h"

// read a joystick event of the joystick.
// return: 0 on success, non-zero on failure.
int read_jsevent(rov_arduino *a){
    unsigned char b[8];
    read(a->jsfd,b,9);
    if (checkbits(b[6],ROV_JSAXIS)){
        setaxis(&a->joystick,b[7],*((short*) &b[4]));
    }else if (checkbits(b[6],ROV_JSBUTTON)){
        a->joystick.buttons = (b[4] == ROV_JSPRESSED)
            ? a->joystick.buttons | (1 << (b[7]))
            : a->joystick.buttons & ~(1 << (b[7]));
    }
    return 0;
}

//return: is button number b being pressed (trigger = 1).
bool is_button(rov_arduino *a,unsigned char b){
    if (b < 1 || b > 12){
        return false;
    }
    return (1 << (b - 1)) & a->joystick.buttons;
}

// return: true iff the all the bits in f are true in v.
bool checkbits(unsigned char v,unsigned char f){
    return (v & f) == f;
}

// Set the value of a given axis on the joystick.
void setaxis(rov_joystick *js,unsigned char a,short v){
    switch(a){
    case ROV_JS_X:
        js->x = v;
        return;
    case ROV_JS_Y:
        js->y = v;
        return;
    case ROV_JS_T:
        js->twist = v;
        return;
    case ROV_JS_S:
        js->slider = v;
        return;
    case ROV_JS_HX:
        js->hat_x = v;
        return;
    case ROV_JS_HY:
        js->hat_y = v;
        return;
    }
    return;
}

// Process joystick input forever.
void *process_joystick(void *vscr_hz){
    rov_pjs_param *p      = vscr_hz;
    rov_screen *scr       = p->scr;
    rov_arduino *a        = scr->arduino;
    rov_joystick old      = a->joystick;
    useconds_t sleep_time = 1000000 / p->phz;
    for (;;){
        read_jsevent(a);
        if (memcmp(&a->joystick,&old,sizeof(rov_joystick))){
            // PROCESS JOYSTICK
        }
        old = a->joystick;
        usleep(sleep_time); // Sleep the thread (fixes the polling rate).
    }
    return NULL;
}
