// Joe Jevnik
// 2014.2.23
// Motor control interface

#ifndef ROV_MOTOR_H
#define ROV_MOTOR_H

#include "../common.h"

// A struct representing a motor controller attached to the arduino.
typedef struct{
    rov_pin  pin;   // The pin this motor is connected to.
    char     power; // The power that is currently being supplied. [-100,100]
} rov_motor;

// Sets up a motor on a given pin.
void init_motor(rov_motor*,rov_pin);

// Sets the power on a motor in the range of [-100,100]
// RETURN: 0 on success, non-zero on failure.
int m_setpower(rov_arduino*,unsigned char,char);

#endif
