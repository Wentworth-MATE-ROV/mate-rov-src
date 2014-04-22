/* arduino.c --- Arduino side server control.
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

#define OP_SHOULDWAIT    0x50
#define OP_SHOULDSTART   0x60

#define OP_DIGITAL_ON    0x00
#define OP_DIGITAL_OFF   0x10
#define OP_ANALOG_WRITE  0x30
#define OP_ANALOG_READ   0x40

#define OP_SET_PINSTATE  0x70

bool is_waiting;

// Initialization.
void setup(){
    Serial.begin(9600);
    is_waiting = false;
}

// Main arduino loop.
void loop(){
    short v;
    byte *b = (byte*) &v;
    byte  t;
    int   p;
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
        case OP_DIGITAL_ON:
            while (!Serial.available());
            digitalWrite(Serial.read(),HIGH);
            break;
        case OP_DIGITAL_OFF:
            while (!Serial.available());
            digitalWrite(Serial.read(),LOW);
            break;
        case OP_ANALOG_WRITE:
            while (!Serial.available());
            b[1] = Serial.read();
            while (!Serial.available());
            t    = Serial.read();
            b[0] = t & 0xc0;
            p    = t & 0x3f;
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
            p = t & 127;
            pinMode(p,(t & 128) ? OUTPUT : INPUT);
            break;
        }
    }
}
