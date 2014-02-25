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
    int n;
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
    topts.c_cc[VMIN]  =  0;
    topts.c_cc[VTIME] =  0;
    tcsetattr(a->fd,TCSANOW,&topts);
    if (tcsetattr(a->fd,TCSAFLUSH,&topts) < 0){
        perror("init_arduino: could not set term attributes");
        return -1;
    }
    a->motorc = motorc;
    a->motorv = malloc(a->motorc * sizeof(rov_motor*));
    a->servoc = servoc;
    a->servov = malloc(a->servoc * sizeof(rov_servo*));
    a->therm  = therm;
    a->accel  = accel;
    a->laser  = laser;
    for (n = 0;n < 16;n++){
        write_byte(a,REPORT_DIGITAL | n);
        write_byte(a,1);
    }
    return 0;
}

// Writes a single byte to the arduino.
// return: 0 on success, non-zero on failure.
int write_byte(rov_arduino *a,unsigned char b){
    return write(a->fd,&b,sizeof(unsigned char)) != sizeof(unsigned char);
}

// Writes an integer to the arduino as 2 7bit values.
// return: 0 on success, non-zero on failure.
int write_int(rov_arduino *a,int n){
    unsigned char buf[2] = { n & 0x7f,n >> 7 };
    return write(a->fd,buf,sizeof(unsigned char)) != 2 * sizeof(unsigned char);
}

// Sets the mode of a pin on the arduino.
// return: 0 on success, non-zero on failure.
int set_pinmode(rov_arduino *a,rov_pin pin,rov_pinmode m){
    return !write_byte(a,SET_PIN_MODE)
        && !write_byte(a,pin)
        && !write_byte(a,m);
}

// Sends a signal to start sysex mode.
// return: 0 on success, non-zero on failure.
int start_sysex(rov_arduino *a){
    return write_byte(a,START_SYSEX);
}

// Sends a signal to end sysex mode.
// return: 0 on success, non-zero on failure.
int end_sysex(rov_arduino *a){
    return write_byte(a,END_SYSEX);
}

// Sends a value to an analog pin.
// return: 0 on success, non-zero on failure.
int send_analog(rov_arduino *a,rov_pin pin,int v){
    return !write_byte(a,ANALOG_MESSAGE | (pin & 0x0f))
        && !write_int(a,v);
}

// Sends data to a digital pin.
// return: 0 on success, non-zero on failure.
int send_digital(rov_arduino *a,rov_pin pin,int v){
    int port = (pin >> 3) & 0x0f;
    return !write_byte(a,DIGITAL_MESSAGE + pin)
        && !write_int(a,v);
}

// Sends servo data (just a thin wrapper over send_analog).
// return: 0 on success, non-zero on failure.
int send_servo(rov_arduino *a,rov_pin pin,int v){
    if (v > 180 || v < 0){
        return -1;
    }
    return !send_analog(a,pin,v);
}

int main(void){
    rov_arduino a;
    init_arduino(&a,"/dev/ttyACM0",0,NULL,0,NULL,NULL,NULL,NULL);
    set_pinmode(&a,8,ROV_PINOUTPUT);
    send_digital(&a,8,1);
}
