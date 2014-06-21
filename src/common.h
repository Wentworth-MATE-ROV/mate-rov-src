/* common.h --- Types and macros common to the entire project.
   Copyright (c) Joe Jevnik

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
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
#include <limits.h>

// The type of a pin on the arduino. Range: [0,63]
typedef unsigned char rov_pin;

// Pin limits.
#define ROV_PIN_MAX 63
#define ROV_PIN_MIN 0

// PLACEHOLDER:
typedef int rov_therm;
typedef int rov_accel;

// The lights on the arduino.
typedef bool rov_light;

// A type representing the values of power that can be sent to a motor.
typedef short rov_motor;

// Define the limits for this type.
#define ROV_MOTOR_MAX SHRT_MAX
#define ROV_MOTOR_MIN SHRT_MIN

// The states the pins may be in.
typedef enum{
    ROV_INPUT        = 0x00,
    ROV_OUTPUT       = 0x01,
    ROV_SERVO        = 0x02,
    ROV_INPUT_PULLUP = 0x03
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

// Keybinding length constants.
#define KEYCOUNT       11
#define KEYBUTTONCOUNT 5
#define KEYAXESCOUNT   6

// A structure to hold all the keybindings in memory.
typedef struct{
    union{
        struct{
            size_t        claw_openc;            // Claw open.
            size_t        claw_closec;           // Claw close.
            size_t        laser_togglec;         // Laser toggle.
            size_t        headlight_togglec;     // Headlights toggle.
            size_t        sidelight_togglec;     // Sidelights toggle.
            size_t        claw_xc;               // Claw x-axis.
            size_t        claw_yc;               // Claw y-axis.
            size_t        rotate_zc;             // Rotation about z.
            size_t        rotate_yc;             // Rotation about y.
            size_t        transpose_xc;          // Transposition over x.
            size_t        transpose_yc;          // Transposition over y.
        };
        size_t            keycounts[KEYCOUNT];
    };
    union{
        struct{
            unsigned char claw_openv[12];        // Claw open.
            unsigned char claw_closev[12];       // Claw close.
            unsigned char laser_togglev[12];     // Laser toggle.
            unsigned char headlight_togglev[12]; // Headlights toggle.
            unsigned char sidelight_togglev[12]; // Sidelights toggle.
        };
        unsigned char     buttonvalues[KEYBUTTONCOUNT][12];
    };
    union{
        struct{
            rov_jsaxis    claw_xv[6];            // Claw x-axis.
            rov_jsaxis    claw_yv[6];            // Claw y-axis.
            rov_jsaxis    rotate_zv[6];          // Rotation about z.
            rov_jsaxis    rotate_yv[6];          // Rotation about y.
            rov_jsaxis    transpose_xv[6];       // Transposition over x.
            rov_jsaxis    transpose_yv[6];       // Transposition over y.
        };
        rov_jsaxis        axesvalues[KEYAXESCOUNT][6];
    };
}rov_keybinds;

// Pin binding length constant.
#define PINCMDCOUNT 10

// A structure to hold the pinlayout of the robot.
typedef struct{
    union{
        struct{
            size_t        clawgripc;                  // Claw grip.
            size_t        laserc;                     // Lasers.
            size_t        headlightc;                 // Headlights.
            size_t        sidelightc;                 // Sidelights.
            size_t        leftmotorc;                 // Left motors.
            size_t        rightmotorc;                // Right motors.
            size_t        frontmotorc;                // Front motors.
            size_t        backmotorc;                 // Back motors.
            size_t        claw_90c;                   // Claw 90deg piston.
            size_t        claw_180c;                  // Claw 180deg piston.
        };
        size_t            pincounts[PINCMDCOUNT];     // The pin count vector.
    };
    union{
        struct{
            rov_pin clawgripv[54];                    // Claw grip.
            rov_pin laserv[54];                       // Lasers.
            rov_pin headlightv[54];                   // Headlights.
            rov_pin sidelightv[54];                   // Sidelights.
            rov_pin leftmotorv[54];                   // Left motors.
            rov_pin rightmotorv[54];                  // Right motors.
            rov_pin frontmotorv[54];                  // Front motors.
            rov_pin backmotorv[54];                   // Back motors.
            rov_pin claw_90v[54];                     // Claw 90deg piston.
            rov_pin claw_180v[54];                    // Claw 180deg piston.
        };
        rov_pin     pinvalues[PINCMDCOUNT][54];       // The pin value vector.
    };
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
    bool              claw_90;    // The 90deg piston.
    bool              claw_180;   // The 90deg piston.
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

// A structure to pass to the process logic thread.
typedef struct{
    rov_screen  *scr;         // The screen structure.
    rov_arduino *a;           // The arduino that thisthread must act on.
    const char  *logic_path;  // The path to the logic module (.scm).
    useconds_t   phz;         // The Hz of the polling.
    useconds_t   shz;         // The Hz of the sending.
    bool         always_step; // logic-step should be called even if input-state
                              // has not changed.
}rov_pl_param;

// A structure to pass to the process queue thread.
typedef struct{
    rov_msgqueue *q;   // The queue.
    rov_screen   *scr; // The screen.
}rov_pq_param;

#endif
