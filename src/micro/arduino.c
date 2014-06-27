/* arduino.c --- Arduino side server control.
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

#include "arduinoh.h"
#include <Servo.h>

// The vector of servos as they must be statically declared.
rov_servo servov[SERVOC];

// Is the arduino waiting to process more data.
boolean is_waiting;

// Finds the servo object attached to pin p.
// return: the servo object on pin p or NULL if none is attached to pin p.
rov_servo *lookup_servo(int p){
    unsigned char n;
    for (n = 0;n < SERVOC;n++){
        if (p == servov[n].p){
            return &servov[n];
        }
    }
    return NULL;
}

// Initialization.
void setup(){
    unsigned char n;
    Serial.begin(9600);
    is_waiting = false;
    for (n = 0;n < SERVOC;n++){
        servov[n].p = -1;  // None of the servos are on a pin.
    }
}

// Main arduino loop.
void loop(){
    short      v;
    byte       t;
    int        p;
    rov_servo *s;
    if (!is_waiting && Serial.available() > 32){
        Serial.write(OP_SHOULDWAIT);
        is_waiting = true;
        return;
    }else if (is_waiting && Serial.available() < 16){
        Serial.write(OP_SHOULDSTART);
        is_waiting = false;
        return;
    }else if (Serial.available() > 0){
        switch(Serial.read()){
        case OP_SERVO_WRITE:
            while(!Serial.available());
            p = Serial.read();
            while(!Serial.available());
            v = Serial.read();
            if (s = lookup_servo(p)){  // Find the servo object on this pin.
                s->s.write(v);
            }
            break;
        case OP_DIGITAL_ON:
            while (!Serial.available());
            digitalWrite(Serial.read(),HIGH);
            break;
        case OP_DIGITAL_OFF:
            while (!Serial.available());
            digitalWrite(Serial.read(),LOW);
            break;
        case OP_DIGITAL_READ:
            while(!Serial.available());
            v = (digitalRead(Serial.read()) == HIGH) ? 1 : 0;
            Serial.write((uint8_t*) &v,2);
            break;
        case OP_ANALOG_WRITE:
            while (!Serial.available());
            p = Serial.read();
            while (!Serial.available());
            v = Serial.read();
            analogWrite(p,v);
            break;
        case OP_ANALOG_READ:
            while (!Serial.available());
            v = analogRead(Serial.read());
            Serial.write((uint8_t*) &v,2);
            break;
        case OP_SET_PINSTATE:
            while (!Serial.available());
            t = Serial.read();
            p = t & 0x3f;  // Get the pin number.
            if (s = lookup_servo(p)){  // Detach any servos on this pin.
                s->s.detach();
                s->p = -1;
            }
            t = (t & 0xc0) >> 6;
            if (t == ROV_SERVO){
                for (v = 0;v < SERVOC;v++){
                    if (servov[v].p == -1){  // Find a free servo.
                        s = &servov[v];
                        s->p = p;
                        s->s.attach(p,1000,2000);
                        s->s.write(90);
                        break;
                    }
                }
            }else if (t == INPUT_PULLUP){
                pinMode(p,INPUT_PULLUP);
            }else{
                pinMode(p,(t) ? OUTPUT : INPUT);
            }
            break;
        }
    }
}
