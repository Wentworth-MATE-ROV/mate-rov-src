// Joe Jevnik
// 2014.3.6
// Joystick controls.

#ifndef ROV_CONTROLS_H
#define ROV_CONTROLS_H

#include "../common.h"
#include "keybinds.h"
#include "../comm/comm.h"
#include "../ui/screen.h"

#include <unistd.h>
#include <string.h>
#include <stdbool.h>

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

// read a joystick event of the joystick.
// return: 0 on success, non-zero on failure.
int read_jsevent(rov_arduino*);

// return: is button b being pressed (trigger = 1).
bool is_button(rov_arduino*,unsigned char);

// return: true iff the all the bits in f are true in v.
bool checkbits(unsigned char,unsigned char);

// Sends the state of the axis to the arduino by setting the appropriate motors.
void send_axisstate(rov_arduino*);

// Process joystick input forever.
void *process_joystick(void*);

#endif
