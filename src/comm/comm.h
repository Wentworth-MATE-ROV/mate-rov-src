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
#include <string.h>
#include <assert.h>
#include <pthread.h>

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

// Closes the file descripotr to the arduino.
void destroy_arduino(rov_arduino*);

// Writes a single byte to the arduino.
// return: 0 on success, non-zero on failure.
int write_char(rov_arduino*,unsigned char);

// Writes a string to the arduino.
// return: 0 on success, non-zero on failure.
int write_str(rov_arduino*,unsigned char*,size_t);

// Writes a short as a string of bytes.
// return: 0 on success, non-zero on failure.
int write_short(rov_arduino*,unsigned short);

// Initializes a node with the given message.
void init_node(rov_node*,unsigned char*,size_t);

// Initializes an empty queue.
void init_queue(rov_msgqueue*,rov_arduino*,useconds_t,size_t);

// Frees the message queue's mesages and mutex.
void destroy_queue(rov_msgqueue*);

// Enqueues a message to be sent to the arduino when it is ready.
// return: The pointer to the allocated node.
rov_node *enqueue(rov_msgqueue*,unsigned char*,size_t);

// Enqueues a message and blocks the caller for a response.
// return: The response from the arduino.
unsigned short enqueue_blocking(rov_msgqueue*,unsigned char*,size_t);

// Dequeues the next message and sends it.
// return: the staus of the underlying write call.
int dequeue(rov_msgqueue*);

// Procedure to run in a pthread to manage the message queue.
// Accepts the rov_msgqueue* as a void*.
void *process_queue(void*);

// Enqueues a message that sets a digital pin on or off.
void digital_write(rov_arduino*,rov_pin,bool);

// Enqueues a message that sends a value to a pin in the range of [0,1023]
// Data is formatted as so: byte 1 = lsb of v.
//          first 2 bits of byte 2 = 2 last bits in v.
//           last 6 bits of byte 2 = the pin number.
void analog_write(rov_arduino*,rov_pin,unsigned short);

// Enqueues a message that reads an analog value in the range of [0,1023] off of
// a pin.
// warning: This call blocks the main thread until a respose is read.
unsigned short analog_read(rov_arduino*,rov_pin);

// Polls the arduino to check if it should stop sending messages.
// return: 0 if nothing is read, or the opcode.
unsigned char poll_wait(rov_arduino*);

#endif
