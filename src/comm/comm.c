/* comm.c --- Arduino communication functions.
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

#include "comm.h"

// Initializes an arduino.
// Parameters: the arduino pointer
//             the device the arduino is connected to
//             the device the joystick is connected to
// return: 0 on success, non-zero on failure.
int init_arduino(rov_arduino *a,const char *af,const char *jf){
    struct termios topts;
    if ((a->fd = open(af,O_RDWR | O_NONBLOCK)) == -1){
        perror("init_arduino: could not open af");
        return -1;
    }
    if (tcgetattr(a->fd,&topts) < 0){
        perror("init_arduino: could not get term attr");
        return -1;
    }
    cfsetispeed(&topts,B9600);
    cfsetospeed(&topts,B9600);
    topts.c_cflag     &= ~PARENB;
    topts.c_cflag     &= ~CSTOPB;
    topts.c_cflag     &= ~CSIZE;
    topts.c_cflag     |= CS8;
    topts.c_cflag     &= ~CRTSCTS;
    topts.c_cflag     |= CREAD | CLOCAL;
    topts.c_iflag     &= ~(IXON | IXOFF | IXANY);
    topts.c_lflag     &= ~(ICANON | ECHO | ECHOE | ISIG);
    topts.c_oflag     &= ~OPOST;
    topts.c_cc[VMIN]  =  0; // Minimum number of characters to read.
    topts.c_cc[VTIME] =  0; // VTIME * 0.1 = Time to wait for input.
    tcsetattr(a->fd,TCSANOW,&topts);
    if (tcsetattr(a->fd,TCSAFLUSH,&topts) < 0){
        perror("init_arduino: could not set term attributes");
        return -1;
    }
    if (!init_joystick(&a->joystick,jf)){
        perror("init_arduino: could not read joystick file");
        return -1;
    }
    memset(&a->ctrl,0,sizeof(rov_ctrlstate));
    init_pinlayout(&a->layout);
    init_queue(&a->queue,a,2500,100);
    return 0;
}

// Closes the file descripotr to the arduino.
void destroy_arduino(rov_arduino *a){
    pthread_cancel(a->qt);
    close(a->fd);
    destroy_joystick(&a->joystick);
}

// Writes a single byte to the arduino.
// return: 0 on success, non-zero on failure.
int write_char(rov_arduino *a,unsigned char b){
    return write(a->fd,&b,sizeof(unsigned char)) != sizeof(unsigned char);
}

// Writes a string to the arduino.
// return: 0 on success, non-zero on failure.
int write_str(rov_arduino *a,unsigned char *str,size_t s){
    unsigned int n;
    for (n = 0;n < s;n++){
        if (write_char(a,str[n])){
            break;
        }
    }
    return s - n;
}

// Writes a short as a string of bytes.
// return: 0 on success, non-zero on failure.
int write_short(rov_arduino *a,unsigned short v){
    return write_str(a,(unsigned char*) &v,sizeof(unsigned short));
}

// Set the state of the pin to output if true, or input if false.
void set_pinstate(rov_arduino *a,rov_pin p,rov_pinstate s){
    unsigned char msg[2] = { OP_SET_PINSTATE, (s << 6) | p };
    enqueue(&a->queue,msg,2 * sizeof(unsigned char));
}

// Sets a digital pin on or off.
void digital_write(rov_arduino *a,rov_pin p,bool v){
    unsigned char msg[] = { (v) ? OP_DIGITAL_ON : OP_DIGITAL_OFF,p };
    enqueue(&a->queue,msg,2 * sizeof(unsigned char));
}

// Reads the state of a pin on the arduino.
// return: true iff the pin is on.
bool digital_read(rov_arduino *a,rov_pin p){
    unsigned char msg[] = { OP_DIGITAL_READ,p };
    return enqueue_blocking(&a->queue,msg,2 * sizeof(unsigned char));
}

// Sends a value to a pin in the range of [0,256)
// Data is formatted as so: byte 0 = opcode
//                          byte 1 = pin number;p9
//                          byte 2 = value
void analog_write(rov_arduino *a,rov_pin p,unsigned char v){
    unsigned char msg[3] = { OP_ANALOG_WRITE,p,v };
    enqueue(&a->queue,msg,3 * sizeof(unsigned char));
}

// Sends a value to a pin in the range of [0,180]
// Data is formatted as so: byte 0 = opcode
//                          byte 1 = pin number
//                          byte 2 = value
void servo_write(rov_arduino *a,rov_pin p,unsigned char v){
    unsigned char msg[3] = { OP_SERVO_WRITE,p,v };
    enqueue(&a->queue,msg,3 * sizeof(unsigned char));
}

// Reads an analog value in the range of [0,1023] off of a pin.
// return: The value of the pin.
unsigned short analog_read(rov_arduino *a,rov_pin p){
    unsigned char msg[] = { OP_ANALOG_READ,p };
    return enqueue_blocking(&a->queue,msg,2 * sizeof(unsigned char));
}

// Polls the arduino to check if it should stop sending messages.
// return: 0 if nothing is read, out, or the opcode.
unsigned char poll_wait(rov_arduino *a){
    unsigned char c;
    if (read(a->fd,&c,sizeof(unsigned char) != 1)){
        return 0;
    }
    return c;
}
