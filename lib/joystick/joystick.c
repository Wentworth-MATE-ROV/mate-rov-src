/* joystick.h --- Implementation for librov_joystick.
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

#include "joystick.h"

// Initializes a new joystick structure that is linked with the given device.
// return: 0 on success, non-zero on failure.
int init_joystick(rov_joystick *js,const char *dev){
    memset(js,0,sizeof(rov_joystick)); // zero the struct.
    return (js->fd = open(dev,O_RDONLY | O_NONBLOCK)) != -1;
}

void destroy_joystick(rov_joystick *js){
    close(js->fd);
}

// return: true iff the all the bits in f are true in v.
static bool checkbits(unsigned char v,unsigned char f){
    return (v & f) == f;
}

// Read a joystick event off the joystick.
// return: 0 on success, non-zero on failure.
int read_jsevent(rov_joystick *js){
    unsigned char b[8];
    read(js->fd,b,8);
    if (checkbits(b[6],ROV_JSAXIS)){
        js->axes[b[7]] = *((short*) &b[4]);
    }else if (checkbits(b[6],ROV_JSBUTTON)){
        js->buttons = (b[4] == ROV_JSPRESSED)
            ? js->buttons | (1 << (b[7]))
            : js->buttons & ~(1 << (b[7]));
    }
    return 0;
}

// return: is button number b being pressed (trigger = 1).
bool is_button(rov_joystick *js,unsigned char b){
    if (b < 1 || b > 12){
        return false;
    }
    return (1 << b) & js->buttons;
}
