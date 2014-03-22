/* motor.h --- Interface to working with motors.
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

#ifndef ROV_MOTOR_H
#define ROV_MOTOR_H

#include "../common.h"
#include "../comm/comm.h"

// Sets up a motor on a given pin.
void init_motor(rov_motor*,rov_pin);

// Sets up a servo on a given pin with a max degree of rotation.
void init_servo(rov_servo*,rov_pin,int);

// Sets the power on a motor in the range of [-100,100]
// This automagically scales [-100,100] -> [0,1023].
// return: 0 on success, non-zero on failure.
int m_setpower(rov_arduino*,size_t,char);

// Procedure to run in a pthread to manage the message queue.
// This automagically scales [0,max degrees] -> [0,1023].
// return: 0 on success, non-zero on failure.
int s_setangle(rov_arduino*,size_t,int);

#endif
