/* pinlayout.c --- Contstruction of pinlayouts.
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

#include "pinlayout.h"

// The strings used when parsing the pins.
const char* const pin_laser_str        = "lasers";
const char* const pin_headlight_str    = "headlights";
const char* const pin_sidelight_str    = "sidelights";
const char* const pin_clawgrip_str     = "claw-grip";
const char* const pin_leftmotor_str    = "left-motor";
const char* const pin_leftmotor_d_str  = "left-motor-direction";
const char* const pin_leftmotor_s_str  = "left-motor-ssg";
const char* const pin_rightmotor_str   = "right-motor";
const char* const pin_rightmotor_d_str = "right-motor-direction";
const char* const pin_rightmotor_s_str = "right-motor-ssg";
const char* const pin_frontmotor_str   = "front-motor";
const char* const pin_frontmotor_d_str = "front-motor-direction";
const char* const pin_frontmotor_s_str = "front-motor-ssg";
const char* const pin_backmotor_str    = "back-motor";
const char* const pin_backmotor_d_str  = "back-motor-direction";
const char* const pin_backmotor_s_str  = "back-motor-ssg";

// Zeros the pinlayout.
void init_pinlayout(rov_pinlayout *l){
    memset(l->pincounts,0,PINCMDCOUNT * sizeof(size_t));
}

// Reads a line of output from the pin-parser.
// return: 0 on success, non-zero on failure.
int pin_read_scm_line(rov_pinlayout *l,char *str){
    int         n;
    size_t      m;
    char       *t;
    const char *cs[PINCMDCOUNT] = { pin_laser_str,
                                    pin_headlight_str,
                                    pin_sidelight_str,
                                    pin_clawgrip_str,
                                    pin_leftmotor_str,
                                    pin_leftmotor_d_str,
                                    pin_leftmotor_s_str,
                                    pin_rightmotor_str,
                                    pin_rightmotor_d_str,
                                    pin_rightmotor_s_str,
                                    pin_frontmotor_str,
                                    pin_frontmotor_d_str,
                                    pin_frontmotor_s_str,
                                    pin_backmotor_str,
                                    pin_backmotor_d_str,
                                    pin_backmotor_s_str };
    t = strtok(str," \n");
    if (t[0] != '('){
        return -1;
    }
    for (n = 0;n < PINCMDCOUNT;n++){
        if (!strcmp(&t[1],cs[n])){
            l->pincounts[n] = strtol(strtok(NULL," "),NULL,10);
            for (m = 0;m < l->pincounts[n];m++){
                if (!(t = strtok(NULL," )"))){
                    break;
                }
                if (t[0] == '('){
                    t += 1;
                }
                l->pinvalues[n][m] = (unsigned char) strtol(t,NULL,0);
            }
            return 0;
        }
    }
    return -1;
}

// Parses a pinlayout from a file.
// return: 0 on success, non-zero on failure.
int parse_pinlayout(rov_pinlayout *l,const char *pfl){
    char  cmd[128];
    FILE *scm;
    char  path[256];
    if (!pfl){
        return 0;
    }
    getcwd(path,256);
    strncat(path,pfl,256 - strlen(path));
    if (!access(path,F_OK)){
        return -1;
    }
    strcpy(cmd,"./pin-parser.scm");
    cmd[16] = ' ';
    cmd[17] = '\0';
    strncat(cmd,pfl,110);
    strncat(cmd," 2> /dev/null",128 - strlen(cmd));
    scm = popen(cmd,"r");
    while (fgets(cmd,128,scm) != NULL){
        if (pin_read_scm_line(l,cmd)){
            return -1;
        }
    }
    return 0;
}

// Macros to facilitate setting some pins.
#define SETPINSTATEIN(count,val)                    \
    for (n = 0;n < a->layout.count;n++){            \
        set_pinstate(a,a->layout.val[n],ROV_INPUT); \
    }
#define SETPINSTATEOUT(count,val)                       \
    for (n = 0;n < a->layout.count;n++){                \
        set_pinstate(a,a->layout.val[n],ROV_OUTPUT);    \
    }
#define SETPINSTATESERVO(count,val)                     \
    for (n = 0;n < a->layout.count;n++){                \
        set_pinstate(a,a->layout.val[n],ROV_SERVO);     \
    }

// Sets the pinmodes for the needed pins.
void pinmode_sync(rov_arduino *a){
    size_t n;
    SETPINSTATEOUT(laserc,laserv);
    SETPINSTATEOUT(headlightc,headlightv);
    SETPINSTATEOUT(sidelightc,sidelightv);
    SETPINSTATESERVO(leftmotorc,leftmotorv);
    SETPINSTATEOUT(leftmotordc,leftmotordv);
    SETPINSTATEIN(leftmotorsc,leftmotorsv);
    SETPINSTATESERVO(rightmotorc,rightmotorv);
    SETPINSTATEOUT(rightmotordc,rightmotordv);
    SETPINSTATEOUT(rightmotorsc,rightmotorsv);
    SETPINSTATESERVO(frontmotorc,frontmotorv);
    SETPINSTATEOUT(frontmotordc,frontmotordv);
    SETPINSTATEIN(frontmotorsc,frontmotorsv);
    SETPINSTATESERVO(backmotorc,backmotorv);
    SETPINSTATEOUT(backmotordc,backmotordv);
    SETPINSTATEIN(backmotorsc,backmotorsv);
}
