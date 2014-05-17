/* pinlayout.c --- Contstruction of pinlayouts.
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
int pin_read_scm_line(rov_pinlayout *l,SCM scm){
    unsigned int n,m;
    size_t       len;
    char        *op;
    const char  *cs[PINCMDCOUNT] = { pin_clawgrip_str,
                                     pin_laser_str,
                                     pin_headlight_str,
                                     pin_sidelight_str,
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
    op  = scm_to_locale_string(scm_car(scm));
    scm = scm_cdr(scm);
    len = scm_to_size_t(scm_length(scm));
    for (n = 0;n < PINCMDCOUNT;n++){
        if (!strcmp(op,cs[n])){
            break;
        }
    }
    free(op);
    if (n == PINCMDCOUNT){
        return -1;
    }
    l->pincounts[n] = len;
    for (m = 0;m < len;m++){
        l->pinvalues[n][m] = scm_to_uchar(scm_list_ref(scm,scm_from_uint(m)));
    }
    return 0;
}

// Parses a pinlayout from a file.
// return: 0 on success, non-zero on failure.
int parse_pinlayout(rov_pinlayout *l,const char *pfl){
    FILE *scm_fl = fopen("parsers/pin-parser.scm","r");
    FILE *l_fl;
    char  path[256];
    char  scm_fc[BUFSIZ];
    SCM   scm;
    if (!scm_fl){
        return -1;
    }
    if (!pfl){
        return 0;
    }
    memset(path,0,256);
    memset(scm_fc,0,BUFSIZ);
    getcwd(path,256);
    path[strlen(path)] = '/';
    strncat(path,pfl,256 - strlen(path));
    l_fl = fopen(path,"r");
    if (!l_fl){
        return -1;
    }
    scm_init_guile();
    fread(scm_fc,sizeof(char),BUFSIZ,scm_fl);
    fclose(scm_fl);
    scm_c_eval_string("(set! %load-path (cons (getcwd) %load-path))\n"
                      "(use-modules (parsers pin-parser))");
    scm_c_eval_string(scm_fc);
    while (fgets(scm_fc,BUFSIZ,l_fl)){
        scm = scm_c_eval_string(scm_fc);
        if (scm != SCM_UNSPECIFIED && scm_is_pair(scm)){
            pin_read_scm_line(l,scm);
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
