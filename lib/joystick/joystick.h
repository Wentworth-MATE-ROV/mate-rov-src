/* joystick.h --- Interface for librov_joystick.
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

#ifndef ROV_JOYSTICK_H
#define ROV_JOYSTICK_H

#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// Message type constants.
#define ROV_JSAXIS     0x02
#define ROV_JSBUTTON   0x01

// Button state constants.
#define ROV_JSPRESSED  0x01
#define ROV_JSRELEASED 0x00

// Joystick axis:
#define ROV_JS_X       0x00 // x axis
#define ROV_JS_Y       0x01 // y axis
#define ROV_JS_T       0x02 // Twist
#define ROV_JS_S       0x03 // Slider
#define ROV_JS_HX      0x04 // Hat X
#define ROV_JS_HY      0x05 // Hat y

// A joystick state.
typedef struct{
    int                fd;           // File descriptor to the joystick device.
    union{
        struct{
            short      x;            // The joystick's x pos.  (left < 0)
            short      y;            // The joystick's y pos.  (up   < 0)
            short      twist;        // The joystick's twist.  (left < 0)
            short      slider;       // The slider's position. (up   < 0)
            short      hat_x;        // The hat's x position.  (left < 0)
            short      hat_y;        // The hat's y position.  (up   < 0)
        };
        short          axes[6];      // Allows acces through axis number.
    };
    union{
        struct{
            bool       trigger  : 1; // Button 1 (Trigger)
            bool       button2  : 1; // Button 2
            bool       button3  : 1; // Button 3
            bool       button4  : 1; // Button 4
            bool       button5  : 1; // Button 5
            bool       button6  : 1; // Button 6
            bool       button7  : 1; // Button 7
            bool       button8  : 1; // Button 8
            bool       button9  : 1; // Button 9
            bool       button10 : 1; // Button 10
            bool       button11 : 1; // Button 11
            bool       button12 : 1; // Button 12
        };
        unsigned short buttons;      // The set of buttons.
    };
}rov_joystick;

// Initializes a new joystick structure that is linked with the given device.
// return: 0 on success, non-zero on failure.
int init_joystick(rov_joystick*,const char*);

// Destroys a joystick structure freeing all internal resources.
void destroy_joystick(rov_joystick*);

// read a joystick event of the joystick.
// return: 0 on success, non-zero on failure.
int read_jsevent(rov_joystick*);

// return: is button b being pressed (trigger = 1).
bool is_button(rov_joystick*,unsigned char);

#endif
