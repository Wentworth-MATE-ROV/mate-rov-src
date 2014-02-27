// Joe Jevnik
// 2014.2.23
// Motor and servo control interface

#ifndef ROV_MOTOR_H
#define ROV_MOTOR_H

#include "../common.h"
#include "../comm/comm.h"

// Sets up a motor on a given pin.
void init_motor(rov_motor*,rov_pin);

// Sets up a servo on a given pin with a max degree of rotation.
void init_servo(rov_servo*,rov_pin,int);

// Sets the power on a motor in the range of [-100,100]
// RETURN: 0 on success, non-zero on failure.
int m_setpower(rov_arduino*,size_t,char);

int s_setangle(rov_arduino*,size_t,int);

#endif
