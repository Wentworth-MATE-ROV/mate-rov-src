/* motor.c --- Functions that wrap basic motor control.
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

#include "motor.h"

// Sets up a motor on a given pin.
void init_motor(rov_motor *m,rov_pin pin){
    m->pin   = pin;
    m->power = 0;
}

// Sets up a servo on a given pin with a max degree of rotation.
void init_servo(rov_servo *s,rov_pin pin,int d){
    s->pin = pin;
    s->loc = 0;
    s->max = d;
}

// Sets the power on a motor in the range of [-100,100]
// This automagically scales [-100,100] -> [0,1023].
// return: 0 on success, non-zero on failure.
int m_setpower(rov_arduino *a,size_t n,char p){
    if (n >= a->motorc || p < -100 || p > 100){
        return -1;
    }
    analog_write(a,a->motorv[n]->pin,(p + 100) * 5.115 + 0.5);
    a->motorv[n]->power = p;
    return 0;;
}

// Sets a servo to an angle in degrees
// This automagically scales [0,max degrees] -> [0,1023].
// return: 0 on success, non-zero on failure.
int s_setangle(rov_arduino *a,size_t n,int d){
    if (n >= a->servoc || d < 0 || d > a->servov[n]->max){
        return -1;
    }
    analog_write(a,a->servov[n]->pin,(d / 360.0) * 1023);
    a->servov[n]->loc = d;
    return 0;
}
