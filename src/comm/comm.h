// Joe Jevnik
// 2014.2.23
// Communication with the arduino

#ifndef ROV_COMM_H
#define ROV_COMM_H

#include "../common.h"
#include "../motor/motor.h"
#include "opcode.h"

#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>

// The pin modes on the arduino:
typedef unsigned char rov_pinmode;
#define ROV_PININPUT  0
#define ROV_PINOUTPUT 1
#define ROV_PINANALOG 2
#define ROV_PINPWM    3
#define ROV_PINSERVO  4

// A complete arduino.
typedef struct{
    int         fd;          // A file descriptor to the /dev/tty_ channel.
    size_t      motorc;      // The number of motors connected.
    rov_motor **motorv;      // The array of motors.
    size_t      servoc;      // The number of servos.
    rov_servo **servov;      // The array of servos.
    rov_therm  *therm;       // The electrical cabinent temperature.
    rov_accel  *accel;       // The accelerometer on the robot.
    rov_laser  *laser;       // The laser mechanism.
} rov_arduino;

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
int init_arduino(rov_arduino*,const char*,size_t,const rov_motor**,
                 size_t,const rov_servo**,const rov_therm*,
                 const rov_accel*,const rov_laser*);

int write_byte(rov_arduino*,unsigned char);

// Writes an integer as 2 7bit values.
// return: 0 on success, non-zero on failure.
int write_int(rov_arduino*,int);

#endif
