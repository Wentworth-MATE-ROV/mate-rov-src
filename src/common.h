// Joe Jevnik
// 2014.2.24
// Types and macros common to all parts of the project.

#ifndef ROV_COMMON_H
#define ROV_COMMON_H

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <ncurses.h>

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
    unsigned char   *msg;  // The message to send.
    size_t           len;  // The length of the message.
    volatile bool    is_blocking; // Is this node blocking the main thread.
    struct rov_node *tail; // The next node in the list.
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

// A complete arduino.
typedef struct rov_arduino{
    int           fd;          // A file descriptor to the /dev/tty_ channel.
    rov_msgqueue *queue;       // The message queue.
    size_t        motorc;      // The number of motors connected.
    rov_motor   **motorv;      // The array of motors.
    size_t        servoc;      // The number of servos.
    rov_servo   **servov;      // The array of servos.
    rov_therm    *therm;       // The electrical cabinent temperature.
    rov_accel    *accel;       // The accelerometer on the robot.
    rov_laser    *laser;       // The laser mechanism.
} rov_arduino;

// A message and its attribute.
typedef struct{
    char txt[81]; // The actual message (Up to 80 chars plus null terminator).
    int  attr;    // The message attributes as defined by ncurses or screen.h
} rov_logmsg;

// The screen structure that manages the entire UI.
typedef struct{
    rov_arduino    *arduino; // The arduino to pull stats from.
    int             mr;      // Max rows
    int             mc;      // Max columns
    int             lmc;     // Log max columns
    WINDOW         *logw;    // The message log window.
    WINDOW         *statw;   // The stat window.
    WINDOW         *ctlw;    // The control window.
    size_t          logc;    // Log count
    rov_logmsg     *logv;    // Log values
    FILE           *logf;    // Log File
    pthread_t       statt;   // Stat Thread
    pthread_mutex_t mutex;   // The mutex for screen writing
} rov_screen;

#endif
