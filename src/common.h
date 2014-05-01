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

// The type of a pin on the arduino. Range: [0,63]
typedef unsigned char rov_pin;

// PLACEHOLDER:
typedef int rov_therm;
typedef int rov_accel;

// The lights on the arduino.
typedef bool rov_light;

// A type representing a motor controller attached to the arduino.
typedef short rov_motor;

// The states the pins may be in.
typedef enum{
    ROV_INPUT  = 0x00,
    ROV_OUTPUT = 0x01,
    ROV_SERVO  = 0x02
}rov_pinstate;

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
    size_t              writes;      // The number of writes.
    useconds_t          sleep_time;  // The number of microseconds to sleep for.
    pthread_mutex_t     mutex;       // The mutex for this structure.
    size_t              r_attempts;  // The number of times to try to resend a
                                     // message if it fails.
}rov_msgqueue;

// An axis structure that is either a pair of buttons or an actual axis.
typedef struct{
    bool                  is_pair; // Is this axis created out of (pair a b).
    union{
        struct{
            unsigned char pos;     // Button relating to the postive dir.
            unsigned char neg;     // Button relating to the negative dir.
        };
        unsigned char     axis;    // The axis that this relates to.
    };
}rov_jsaxis;

// A structure to hold all the keybindings in memory.
typedef struct{
    size_t        claw_openc;            // The amount of bindings to claw_open.
    unsigned char claw_openv[12];        // Buttons to open the claw.
    size_t        claw_closec;           // The amount of bindings to claw_close.
    unsigned char claw_closev[12];       // Buttons to close the claw.
    size_t        claw_xc;               // The amounf of bindings to claw_x;
    rov_jsaxis    claw_xv[6];            // Axes that move the claw along the x.
    size_t        claw_yc;               // The amount of bindings to claw_y.
    rov_jsaxis    claw_yv[6];            // Axes that move the claw along the y.
    size_t        rotate_zc;             // The amount of bindings to rotate_z.
    rov_jsaxis    rotate_zv[6];          // Axes to rotate about the z.
    size_t        rotate_yc;             // The amount of bindings to rotate_y.
    rov_jsaxis    rotate_yv[6];          // Axes to rotate about the y.
    size_t        transpose_xc;          // The amount of bindings transpose_x.
    rov_jsaxis    transpose_xv[6];       // Axes to transpose along the x.
    size_t        transpose_yc;          // The amount of bindings transpose_y.
    rov_jsaxis    transpose_yv[6];       // Axes to transpose along the y.
    size_t        turn_yc;               // The bindings to turn about y.
    rov_jsaxis    turn_yv[6];            // Axes to turn without rotating.
    size_t        laser_togglec;         // The amount of bindings laser_toggle.
    unsigned char laser_togglev[12];     // Buttons that toggle the lasers.
    size_t        headlight_togglec;     // The amount of bindings headlights.
    unsigned char headlight_togglev[12]; // Buttons that toggle the headlights.
    size_t        sidelight_togglec;     // The amount of bindings sidelights.
    unsigned char sidelight_togglev[12]; // Buttons that toggle to sidelights.
}rov_keybinds;

// A structure to hold the pinlayout of the robot.
typedef struct{
    size_t        clawgripc;        // The number of pins for the claw grip.
    unsigned char clawgripv[54];    // The pins for the claw grip.
    size_t        laserc;           // The number of pins for the lasers.
    unsigned char laserv[54];       // The pins the lasers are on.
    size_t        headlightc;       // The number of pins for the headlights
    unsigned char headlightv[54];   // The pins the headlights are on.
    size_t        sidelightc;       // The number of pins for the sidelights.
    unsigned char sidelightv[54];   // The pins the sidelights are on.
    size_t        leftmotorc;       // The number of pins for the left motor.
    unsigned char leftmotorv[54];   // The pins the left motor is on.
    size_t        leftmotordc;      // The number of pins to the left direction.
    unsigned char leftmotordv[54];  // The pins the left motor direction is on.
    size_t        rightmotorc;      // The number of pins for the right motor.
    unsigned char rightmotorv[54];  // The pins the right motor is on.
    size_t        rightmotordc;     // The number of pins to the rightmotor d.
    unsigned char rightmotordv[54]; // The pins the right motor direction is on.
    size_t        frontmotorc;      // The number of pins for the front motor.
    unsigned char frontmotorv[54];  // The pins the front motor is on.
    size_t        frontmotordc;     // The number of pins to the frontmotor d.
    unsigned char frontmotordv[54]; // The pins the front motor direction is on.
    size_t        backmotorc;       // The number of pins for the back motor.
    unsigned char backmotorv[54];   // The pins the back motor is on.
    size_t        backmotordc;      // The number of pins for the backmotor d.
    unsigned char backmotordv[54];  // The pins the back motor direction is on.
}rov_pinlayout;

typedef struct{
    union{
        struct{
            rov_motor leftmotor;  // The left motor.
            rov_motor rightmotor; // The right motor.
            rov_motor frontmotor; // The front motor.
            rov_motor backmotor;  // The back motor.
        };
        rov_motor     motorv[4];  // The array of motors.
    };
    rov_light         headlights; // The headlights.
    rov_light         sidelights; // The lights on the side cameras.
    rov_light         lasers;     // The laser mechanism.
    bool              clawgrip;   // Is the claw closed?
}rov_ctrlstate;

// A complete arduino.
typedef struct rov_arduino{
    int               fd;         // A file descriptor to the /dev/tty_ channel.
    rov_joystick      joystick;   // The joystick state.
    rov_keybinds      keybinds;   // The set of keybinds on the joystick.
    rov_pinlayout     layout;     // The set of pin mappings on the arduino.
    rov_msgqueue      queue;      // The message queue.
    pthread_t         qt;         // The queue thread.
    rov_ctrlstate     ctrl;       // The entire control state.
}rov_arduino;

// A structure to pass to the process joystick thread.
typedef struct{
    rov_screen  *scr; // The screen structure.
    rov_arduino *a;   // The arduino that this joystick thread must act on.
    useconds_t   phz; // The Hz of the polling.
    useconds_t   shz; // The Hz of the sending.
}rov_pjs_param;

// A structure to pass to the process queue thread.
typedef struct{
    rov_msgqueue *q;   // The queue.
    rov_screen   *scr; // The screen.
}rov_pq_param;

#endif
