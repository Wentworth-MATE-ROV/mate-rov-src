// Joe Jevnik
// 2014.2.23
// Motor control implementation.

// Sets up a motor on a given pin.
void init_motor(rov_motor *m,rov_apin pin){
    m->pin   = pin;
    m->power = 0;
}

// TODO:
int m_setpower(rov_motor *m,char p){
    if (p > 100 || p < -100){
        return -1;
    }
    m->power = p;
    return 0;
}
