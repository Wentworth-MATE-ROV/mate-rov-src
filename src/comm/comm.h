// Joe Jevnik
// 2014.2.23
// Communication with the arduino

#ifndef ROV_COMM_H
#define ROV_COMM_H

#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>

typedef int rov_apin;
typedef int rov_dpin;

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
    int        *digital_out; // The digital data to write out.
    int        *digital_in;  // The digital data we have read in.
    int        *analog_in;   // The analog data we have read in.
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

// Writes an integer as 2 7bit values.
// return: 0 on success, non-zero on failure.
int write_int(rov_arduino*,int);

#endif
