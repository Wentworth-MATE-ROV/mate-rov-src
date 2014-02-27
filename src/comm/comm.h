// Joe Jevnik
// 2014.2.23
// Communication with the arduino.

#ifndef ROV_COMM_H
#define ROV_COMM_H

#include "../common.h"
#include "opcode.h"

#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>

// Initializes an arduino.
// Parameters: the arduino pointer
//             the device the arduino is connected to
//             the number of motors
//             the array of motors
//             the number of servos
//             the array of servos
//             the thermometer
//             the accelerometer
//             the laser
// return: 0 on success, non-zero on failure.
int init_arduino(rov_arduino*,char*,size_t,rov_motor**,size_t,rov_servo**,
                 rov_therm*,rov_accel*,rov_laser*);

// Writes a single byte to the arduino.
// return: 0 on success, non-zero on failure.
int write_char(rov_arduino*,unsigned char);

// Writes a string to the arduino.
// return: 0 on success, non-zero on failure.
int write_str(rov_arduino*,unsigned char*,size_t);

// Writes a short as a string of bytes.
// return: 0 on success, non-zero on failure.
int write_short(rov_arduino*,unsigned short);

// Sets a digital pin on or off.
// return: 0 on success, non-zero on failure.
int digital_write(rov_arduino*,rov_pin,bool);

// Sends a value to a pin in the range of [0,1023]
// Data is formatted as so: byte 1 = lsb of v.
//          first 2 bits of byte 2 = 2 last bits in v.
//           last 6 bits of byte 2 = the pin number.
// return: 0 on success, non-zero on failure.
int analog_write(rov_arduino*,rov_pin,unsigned short);

// Reads an analog value in the range of [0,1023] off of a pin.
// return: The value of the pin on success, negative value on failure.
int analog_read(rov_arduino*,rov_pin);

// Polls the arduino to check if it should stop sending messages.
// return: true iff you should stop sending messages.
bool poll_shouldstop(rov_arduino*);

// Polls the arduino to check if you should start sending messages again.
// return: true iff you should start sending messages again.
bool poll_shouldstart(rov_arduino*);

#endif
