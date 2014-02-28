// Joe Jevnik
// 2014.2.23
// Communication with the arduino.

#include "comm.h"

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
int init_arduino(rov_arduino *a,char *f,size_t motorc,rov_motor **motorv,
                 size_t servoc,rov_servo **servov,rov_therm *therm,
                 rov_accel *accel,rov_laser *laser){
    struct termios topts;
    if ((a->fd = open(f,O_RDWR | O_NONBLOCK)) == -1){
        perror("init_arduino: could not open f");
        return -1;
    }
    if (tcgetattr(a->fd,&topts) < 0){
        perror("init_arduino: could not get term attr");
        return -1;
    }
    cfsetispeed(&topts,B57600);
    cfsetospeed(&topts,B57600);
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
    a->motorc = motorc;
    a->motorv = motorv;
    a->servoc = servoc;
    a->servov = servov;
    a->therm  = therm;
    a->accel  = accel;
    a->laser  = laser;
    sleep(2);
    return 0;
}

// Closes the file descripotr to the arduino.
void destroy_arduino(rov_arduino *a){
    close(a->fd);
}

// Writes a single byte to the arduino.
// return: 0 on success, non-zero on failure.
int write_char(rov_arduino *a,unsigned char b){
    return write(a->fd,&b,sizeof(unsigned char)) != sizeof(unsigned char);
}

// Writes a string to the arduino.
// return: 0 on success, non-zero on failure.
int write_str(rov_arduino *a,unsigned char *str,size_t s){
    int n;
    for (n = 0;n < s;n++){
        if (write_char(a,str[n])){
            break;
        }
    }
    return !(s - n);
}

// Writes a short as a string of bytes.
// return: 0 on success, non-zero on failure.
int write_short(rov_arduino *a,unsigned short v){
    return write_str(a,(unsigned char*) &v,sizeof(unsigned short));
}

// Sets a digital pin on or off.
void digital_write(rov_arduino *a,rov_pin p,bool v){
    unsigned char msg[] = { (v) ? OP_DIGITAL_ON : OP_DIGITAL_OFF,p };
    enqueue(a->queue,msg,2 * sizeof(unsigned char));
}

// Sends a value to a pin in the range of [0,1023]
// Data is formatted as so: byte 0 = opcode
//                          byte 1 = lsb of v.
//          first 2 bits of byte 2 = 2 last bits in v.
//           last 6 bits of byte 2 = the pin number.
void analog_write(rov_arduino *a,rov_pin p,unsigned short v){
    unsigned char *b = (unsigned char*) &v;
    unsigned char msg[3];
    b[1] <<= 6;
    b[1] |=  p;
    msg[0] = OP_ANALOG_WRITE;
    msg[1] = b[0];
    msg[2] = b[1];
    enqueue(a->queue,msg,3 * sizeof(unsigned char));
}

// Reads an analog value in the range of [0,1023] off of a pin.
// return: The value of the pin.
unsigned short analog_read(rov_arduino *a,rov_pin p){
    unsigned char msg[] = { OP_ANALOG_READ,p };
    return enqueue_blocking(a->queue,msg,2 * sizeof(unsigned char));
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
