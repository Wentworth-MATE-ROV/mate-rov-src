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
    sleep(2);
    return 0;
}

// Writes a single byte to the arduino.
// return: 0 on success, non-zero on failure.
int write_char(rov_arduino *a,unsigned char b){
    return write(a->fd,&b,sizeof(unsigned char)) != sizeof(unsigned char);
}

// Writes a string to the arduino.
// return: the number of bytes written, -1 on failure.
ssize_t write_str(rov_arduino *a,unsigned char *s,size_t s){
    int n;
    for (n = 0;n < s;n++){
        if (!write_char(a,s[n])){
            break;
        }
    }
    if (!n){
        return -1;
    }
    return s - n;
}


// Writes a short as a string of bytes.
// return: 0 on success, non-zero on failure.
int write_short(rov_arduino *a,unsigned short v){
    return write_str(a,(unsigned char*) &v,sizeof (unsigned short))
        != sizeof(unsigned short);
}

// Sets a digital pin on or off.
// return: 0 on success, non-zero on failure.
int digital_write(rov_arduino *a,rov_pin p,bool v){
    return !write_byte(a->fd,(v) ? DIGITAL_ON : DIGITAL_OFF)
        && !write_byte(a->fd,p);
}

// Sends a value to a pin in the range of [0,1023]
// Data is formatted as so: byte 1 = lsb of v.
//          first 2 bits of byte 2 = 2 last bits in v.
//           last 6 bits of byte 2 = the pin number.
// return: 0 on success, non-zero on failure.
int analog_write(rov_arduino *a,rov_pin p,unsigned short v){
    unsigned char *b = (unsigned char*) &v;
    b[1] <<= 6;
    b[1] |=  p;
    return !write_byte(a,v[0])
        && !write_byte(a,v[1]);
}

int main(void){
    rov_arduino a;
    char buf[128];
    int n;
    init_arduino(&a,"/dev/ttyACM0",0,NULL,0,NULL,NULL,NULL,NULL);
    read(a.fd,buf,128);
    puts(buf);
    for (n = 0;n < 128;n++){
        buf[n] = 0;
    }
    write(a.fd,"test",4);
    sleep(1);
    read(a.fd,buf,4);
    puts(buf);
    return 0;
}
