/* common.h --- Types and macros common to the entire project.
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

#ifndef ROV_COMMON_H
#define ROV_COMMON_H

#include "librov/joystick.h"
#include "librov/screen.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <ncurses.h>

#define LEFT_THRUSTER  0
#define RIGHT_THRUSTER 1
#define FRONT_THRUSTER 2
#define BACK_TRHRUSTER 3

// The type of a pin on the arduino. Range: [0,63]
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

// A node in the queue.
struct rov_node{
    unsigned char   *msg;         // The message to send.
    size_t           len;         // The length of the message.
    volatile bool    is_blocking; // Is this node blocking the main thread.
    struct rov_node *tail;        // The next node in the list.
};
typedef struct rov_node rov_node;

// Forward declare the arduino.
struct rov_arduino;

// The message queue to handle mesage passing requests.
typedef struct rov_msgqueue{
    struct rov_arduino *arduino;     // The arduino that this queue works on.
    rov_node           *head;        // The first elem in the queue.
    rov_node           *last;        // The last elem in the queue.
    size_t              size;        // The number of messages in the queue.
    volatile bool       is_waiting;  // Is this queue waiting for the arduino.
    unsigned short      response;    // The response to grab for blockingcalls.
    size_t              miswrites;   // The number of miswrites.
    useconds_t          sleep_time;  // The number of microseconds to sleep for.
    pthread_mutex_t     mutex;       // The mutex for this structure.
    size_t              r_attempts;  // The number of times to try to resend a
                                     // message if it fails.
} rov_msgqueue;

// An axis structure that is either a pair of buttons or an actual axis.
typedef struct{
    bool is_pair;              // Is this axis created out of (pair a b).
    union{
        struct{
            unsigned char pos; // Button relating to the postive dir.
            unsigned char neg; // Button relating to the negative dir.
        };
        unsigned char axis;    // The axis that this relates to.
    };
}rov_jsaxis;

// A structure to hold all the keybindings in memory.
typedef struct{
    size_t        claw_openc;        // The amount of bindings to claw_open.
    unsigned char claw_openv[12];    // Buttons to open the claw.
    size_t        claw_closec;       // The amount of bindings to claw_close.
    unsigned char claw_closev[12];   // Buttons to close the claw.
    size_t        claw_xc;           // The amounf of bindings to claw_x;
    rov_jsaxis    claw_xv[6];        // Axes that move the claw along the x.
    size_t        claw_yc;           // The amount of bindings to claw_y.
    rov_jsaxis    claw_yv[6];        // Axes that move the claw along the y.
    size_t        rotate_zc;         // The amount of bindings to rotate_z.
    rov_jsaxis    rotate_zv[6];      // Axes to rotate about the z.
    size_t        rotate_yc;         // The amount of bindings to rotate_y.
    rov_jsaxis    rotate_yv[6];      // Axes to rotate about the y.
    size_t        transpose_xc;      // The amount of bindings to transpose_x.
    rov_jsaxis    transpose_xv[6];   // Axes to transpose the robot along the x.
    size_t        transpose_yc;      // The amount of bindings to transpose_y.
    rov_jsaxis    transpose_yv[6];   // Axes to transpose the robot along the y.
    size_t        turn_yc;           // The amount of bindings to turn around y.
    rov_jsaxis    turn_yv[6];        // Axes to turn the robot without rotating.
    size_t        thrust_modc;       // The amount of bindings to thrus_mod
    rov_jsaxis    thrust_modv[6];    // Axes to adjust the thrust power.
    size_t        laser_onc;         // The amount of bindings to laser_on.
    unsigned char laser_onv[12];     // Buttons that turn the lasers on.
    size_t        laser_offc;        // The amount of bindings to laser_off.
    unsigned char laser_offv[12];    // Buttons that turn the lasers off.
    size_t        laser_togglec;     // The amount of bindings to laser_toggle.
    unsigned char laser_togglev[12]; // Buttons that toggle the laser's state.
}rov_keybinds;

// A complete arduino.
typedef struct rov_arduino{
    int           fd;          // A file descriptor to the /dev/tty_ channel.
    rov_joystick  joystick;    // The joystick state.
    rov_keybinds  keybinds;    // The set of keybinds on the joystick.
    rov_msgqueue  queue;       // The message queue.
    pthread_t     qt;          // The queue thread.
    size_t        motorc;      // The number of motors connected.
    rov_motor   **motorv;      // The array of motors.
    size_t        servoc;      // The number of servos.
    rov_servo   **servov;      // The array of servos.
    rov_therm    *therm;       // The electrical cabinent temperature.
    rov_accel    *accel;       // The accelerometer on the robot.
    rov_laser    *laser;       // The laser mechanism.
} rov_arduino;

// A structure to pass to the process joystick thread.
typedef struct{
    rov_screen  *scr; // The screen structure.
    rov_arduino *a;   // The arduino that this joystick thread must act on.
    useconds_t   phz; // The Hz of the polling.
    useconds_t   shz; // The Hz of the sending.
}rov_pjs_param;

#endif
