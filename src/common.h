// Joe Jevnik
// 2014.2.24
// Types and macros common to all parts of the project.

#ifndef ROV_COMMON_H
#define ROV_COMMON_H

#include <stdlib.h>

typedef unsigned char rov_pin;

// PLACEHOLDER:
typedef int rov_laser;
typedef int rov_therm;
typedef int rov_accel;

// A struct representing a motor controller attached to the arduino.
typedef struct{
    rov_pin  pin;   // The pin this motor is connected to.
    char     power; // The power that is currently being supplied. [-100,100]
} rov_motor;

// A struct representing a servo attached to the arduino.
typedef struct{
    rov_pin pin;
    int     loc;
    int     max;
} rov_servo;

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

#endif
