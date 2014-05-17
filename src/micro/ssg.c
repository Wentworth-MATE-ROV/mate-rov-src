/* hw11ssg.c --- Start up sequence generator for the hw11.
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

#include <Servo.h>

#define HWPIN      3
#define PHOENIXPIN 11
#define ARDPIN     2

void setup(){
    Servo hw,phoenix;
    hw.attach(HWPIN);
    phoenix.attach(PHOENIXPIN);
    pinMode(8,OUTPUT);
    pinMode(7,OUTPUT);
    pinMode(6,OUTPUT);
    pinMode(5,OUTPUT);
    hw.write(180);
    phoenix.write(0);
    digitalWrite(8,HIGH);
    delay(2500);
    hw.write(0);
    phoenix.write(180);
    digitalWrite(7,HIGH);
    delay(2500);
    hw.write(180);
    phoenix.write(0);
    digitalWrite(6,HIGH);
    delay(2000);
    hw.write(0);
    pinMode(ARDPIN,OUTPUT);
    digitalWrite(ARDPIN,HIGH);
    digitalWrite(5,HIGH);
}

void loop(){}
