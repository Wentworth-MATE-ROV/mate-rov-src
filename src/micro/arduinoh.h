/* arduinoh.h --- Arduino side server control.
   Copyright (c) Joe Jevnik

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along with
   this program; if not, write to the Free Software Foundation, Inc., 51
   Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

#ifndef ROV_ARDUINO_H
#define ROV_ARDUINO_H

#include <Servo.h>

#define OP_SHOULDWAIT    0x50
#define OP_SHOULDSTART   0x60

#define OP_DIGITAL_ON    0x00
#define OP_DIGITAL_OFF   0x10
#define OP_DIGITAL_READ  0x90
#define OP_ANALOG_WRITE  0x30
#define OP_ANALOG_READ   0x40
#define OP_SERVO_WRITE   0x80

#define OP_SET_PINSTATE  0x70

// The number of servos.
#define SERVOC           4

// The states the pins may be in.
typedef enum{
    ROV_INPUT        = 0x00,
    ROV_OUTPUT       = 0x01,
    ROV_SERVO        = 0x02,
    ROV_INPUT_PULLUP = 0x03
}rov_pinstate;

// The pair of the servo and the pin it is on.
typedef struct{
    Servo s; // The servo object.
    char  p; // The pin it is on.
}rov_servo;

#endif
