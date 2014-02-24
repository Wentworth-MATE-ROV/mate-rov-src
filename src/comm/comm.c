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
int init_arduino(rov_arduino *a,const char *f,
                 size_t motorc,const rov_motor **motorv,
                 size_t servoc,const rov_servo **servov,
                 const rov_therm *therm,const rov_accel *accel,
                 const rov_laser *laser){
    struct termios topts;
    if ((a->fd = open(f,O_RDWR | O_NONBLOCK)) == -1){
        perror("init_arduino: could not open f");
        return -1;
    }
    if (tcgetattr(a->fd,&tops) < 0){
        perror("init_arduino: could not get term attr");
        return -1;
    }
    csetispeed(&topts,B19200);
    csetospeed(&topts,B19200);
    topts.c_cflag     &= ~PARENB;
    topts.c_cflag     &= ~CSTOPB;
    topts.c_cflag     &= ~CSIZE;
    topts.c_cflag     |= CS8;
    topts.c_cflag     &= ~CRTSCTS;
    topts.c_cflag     |= CREAD | CLOCAL;
    topts.c_iflag     &= ~(IXON | IXOFF | IXANY);
    topts.c_lflag     &= ~(ICANON | ECHO | ECHOE | ISIG);
    topts.c_oflag     &= ~OPOST;
    topts.c_cc[VMIN]  =  0;
    topts.c_cc[VTIME] =  0;
    tcsetattr(a->fd,TCSANOW,&tops);
    if (tcsetattr(a->fd,TCSAFLUSH,&topts) < 0){
        perror("init_arduino: could not set term attributes");
        return -1;
    }
    a->motorc = motorc
    a->motorv = malloc(a->motorc * sizeof(rov_motor*));
    a->servoc = servoc
    a->servoc = malloc(a->servoc * sizeof(rov_servo*));
    a->therm  = therm;
    a->accel  = accel;
    a->laser  = laser;
    return 0;
}

// Writes an integer to the arduino as 2 7bit values.
// return: 0 on success, non-zero on failure.
int write_int(rov_arduino *a,int n){
    unsigned char buf[2] = { n & B01111111,n >> 7 & B01111111 };
    return write(a->fd,buf,2) != 2;
}

// Sends a signal to start sysex mode.
// return: 0 on success, non-zero on failure.
int start_sysex(rov_arduino *a){
    unsigned char buf = START_SYSEX;
    return write(a->fd,&buf,1) != 1;
}

// Sends a signal to end sysex mode.
// return: 0 on success, non-zero on failure.
int end_sysex(rov_arduino *a){
    unsigned char buf = END_SYSEX;
    return write(a->fd,&buf,1) != 1;
}

// Sends a value to an analog pin.
// return: 0 on success, non-zero on failure.
int send_analog(rov_arduino *a,rov_apin pin,int v){
    unsigned char buf = ANALOG_MESSAGE | (pin & 0x0f);
    return write(a->fd,&buf,1) != 1 && !write_int(a,v);
}

int send_digital(rov_arduino *a,rov_dpin pin,inv v){
    unsigned char buf = DIGITAL_MESSAGE | (portNumber & 0x0f);
    return write(a->fd,&buf,1)
