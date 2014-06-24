/* controls.c --- Where the controls of the robot are handled.
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

#include "controls.h"
#include "../comm/comm.h"
#include "../librov/screen.h"
#include "../ui/stats.h"

#include "init-logic.scm.ch"

// The scheme accesor functions.
static SCM scm_leftmotor;
static SCM scm_rightmotor;
static SCM scm_frontmotor;
static SCM scm_backmotor;
static SCM scm_headlights;
static SCM scm_sidelights;
static SCM scm_lasers;
static SCM scm_clawgrip;
static SCM scm_claw_90;
static SCM scm_claw_180;

// return: scales the values of the rov_motor to the range [0,255].
unsigned char scale_motorval(rov_motor v){
    return ((v + 100) / 200) * 255;
}

// return: scales v to the range of an rov_motor.
rov_motor scale_cleaned_val(int v){
    return (v > ROV_MOTOR_MAX) ? ROV_MOTOR_MAX
        : (v < ROV_MOTOR_MIN) ? ROV_MOTOR_MIN : v;
}

// Applies the keybinds to the joystick to clean it, storing the result in c.
void clean_joystick(rov_joystick *js,rov_keybinds *kbs,rov_clean_js *c){
    size_t n;
    int    trans_x,trans_y,rot_z,rot_y,claw_x,claw_y;
    memset(c,0,sizeof(rov_clean_js));
    for (n = 0;n < kbs->headlight_togglec;n++){
        if (is_button(js,kbs->headlight_togglev[n])){
            c->headlight_toggle = true;
            break;
        }
    }
    for (n = 0;n < kbs->sidelight_togglec;n++){
        if (is_button(js,kbs->sidelight_togglev[n])){
            c->sidelight_toggle = true;
            break;
        }
    }
    for (n = 0;n < kbs->laser_togglec;n++){
        if (is_button(js,kbs->laser_togglev[n])){
            c->laser_toggle = true;
            break;
        }
    }
    for (n = 0;n < kbs->claw_openc;n++){
        if (is_button(js,kbs->claw_openv[n])){
            c->claw_open = true;
            break;
        }
    }
    for (n = 0;n < kbs->claw_closec;n++){
        if (is_button(js,kbs->claw_closev[n])){
            c->claw_close = true;
            break;
        }
    }
    for (n = 0;n < kbs->claw_xc;n++){
        if (kbs->claw_xv[n].is_pair){
            if (is_button(js,kbs->claw_xv[n].pos)){
                claw_x += SHRT_MAX;
            }
            if (is_button(js,kbs->claw_xv[n].neg)){
                claw_x += SHRT_MIN;
            }
        }else{
            claw_x += js->axes[kbs->claw_xv[n].axis];
        }
    }
    c->claw_x = (short) (claw_x / kbs->claw_xc);
    for (n = 0;n < kbs->claw_yc;n++){
        if (kbs->claw_yv[n].is_pair){
            if (is_button(js,kbs->claw_yv[n].pos)){
                claw_y += SHRT_MAX;
            }
            if (is_button(js,kbs->claw_yv[n].neg)){
                claw_y += SHRT_MIN;
            }
        }else{
            claw_y+= js->axes[kbs->claw_yv[n].axis];
        }
    }
    c->claw_y = (short) (claw_y / kbs->claw_yc);
    for (n = 0;n < kbs->transpose_xc;n++){
        if (kbs->transpose_xv[n].is_pair){
            if (is_button(js,kbs->transpose_xv[n].pos)){
                trans_x += SHRT_MAX;
            }
            if (is_button(js,kbs->transpose_xv[n].neg)){
                trans_x += SHRT_MIN;
            }
        }else{
            trans_x += js->axes[kbs->transpose_xv[n].axis];
        }
    }
    c->transpose_x = (short) (trans_x / kbs->transpose_xc);
    for (n = 0;n < kbs->rotate_yc;n++){
        if (kbs->rotate_yv[n].is_pair){
            if (is_button(js,kbs->rotate_yv[n].pos)){
                rot_y += SHRT_MAX;
            }
            if (is_button(js,kbs->rotate_yv[n].neg)){
                rot_y += SHRT_MIN;
            }
        }else{
            rot_y += js->axes[kbs->rotate_yv[n].axis];
        }
    }
    c->rotate_y = (short) (rot_y / kbs->rotate_yc);
    for (n = 0;n < kbs->transpose_yc;n++){
        if (kbs->transpose_yv[n].is_pair){
            if (is_button(js,kbs->transpose_yv[n].pos)){
                trans_y += SHRT_MAX;
            }
            if (is_button(js,kbs->transpose_yv[n].neg)){
                trans_y += SHRT_MIN;
            }
        }else{
            trans_y += js->axes[kbs->transpose_yv[n].axis];
        }
    }
    c->transpose_y = (short) (trans_y / kbs->transpose_yc);
    for (n = 0;n < kbs->rotate_zc;n++){
        if (kbs->rotate_zv[n].is_pair){
            if (is_button(js,kbs->rotate_zv[n].pos)){
                rot_z += SHRT_MAX;
            }
            if (is_button(js,kbs->rotate_zv[n].neg)){
                rot_z += SHRT_MIN;
            }
        }else{
            rot_z += js->axes[kbs->rotate_zv[n].axis];
        }
    }
    c->rotate_z = (short) (rot_z / kbs->rotate_zc);
}


// Syncs the local control state back to the arduino if it has changed.
void sync_ctrlstate(rov_arduino *a,rov_ctrlstate *old){
    size_t n;
    short  v;
    if (old->lasers != a->ctrl.lasers){
        for (n = 0;n < a->layout.laserc;n++){
            digital_write(a,a->layout.laserv[n],a->ctrl.lasers);
        }
    }
    if (old->headlights != a->ctrl.headlights){
        for (n = 0;n < a->layout.headlightc;n++){
            digital_write(a,a->layout.headlightv[n],a->ctrl.headlights);
        }
    }
    if (old->sidelights != a->ctrl.sidelights){
        for (n = 0;n < a->layout.sidelightc;n++){
            digital_write(a,a->layout.sidelightv[n],a->ctrl.sidelights);
        }
    }
    if (old->clawgrip != a->ctrl.clawgrip){
        for (n = 0;n < a->layout.clawgripc;n++){
            digital_write(a,a->layout.clawgripv[n],a->ctrl.clawgrip);
        }
    }
    if (old->claw_180 != a->ctrl.claw_180){
        for (n = 0;n < a->layout.claw_180c;n++){
            digital_write(a,a->layout.claw_180v[n],a->ctrl.claw_180);
        }
    }
    if (old->claw_90 != a->ctrl.claw_90){
        for (n = 0;n < a->layout.claw_90c;n++){
            digital_write(a,a->layout.claw_90v[n],a->ctrl.claw_90);
        }
    }
    v = scale_motorval(a->ctrl.leftmotor);
    for (n = 0;n < a->layout.leftmotorc;n++){
        servo_write(a,a->layout.leftmotorv[n],v);
    }
    v = scale_motorval(a->ctrl.rightmotor);
    for (n = 0;n < a->layout.rightmotorc;n++){
        servo_write(a,a->layout.rightmotorv[n],v);
    }
    v = scale_motorval(a->ctrl.frontmotor);
    for (n = 0;n < a->layout.frontmotorc;n++){
        servo_write(a,a->layout.frontmotorv[n],v);
    }
    v = scale_motorval(a->ctrl.backmotor);
    for (n = 0;n < a->layout.backmotorc;n++){
        servo_write(a,a->layout.backmotorv[n],v);
    }
}

// Subtracts two timeval structures storing the result in the result struct.
// Returns 1 if result is negative.
// Source: http://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html
int timeval_subtract (struct timeval *result,
                      struct timeval *x,
                      struct timeval *y){
    int nsec;
    if (x->tv_usec < y->tv_usec){
        nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
        y->tv_usec -= 1000000 * nsec;
        y->tv_sec += nsec;
    }
    if (x->tv_usec - y->tv_usec > 1000000){
        nsec = (x->tv_usec - y->tv_usec) / 1000000;
        y->tv_usec += 1000000 * nsec;
        y->tv_sec -= nsec;
    }
    result->tv_sec = x->tv_sec - y->tv_sec;
    result->tv_usec = x->tv_usec - y->tv_usec;
    return x->tv_sec < y->tv_sec;
}

// return: the total number of microseonds in this timeval.
long long total_usec(struct timeval *t){
    return t->tv_sec * 1000 + t->tv_usec;
}

// Checks if it is safe to lookup a function and only looks it up if it safe to
// do so.
// return: 0 on success, non-zero on failure.
int safe_scm_c_lookup(const char *proc,SCM scm_proc_exists_p, SCM *scm_proc){
    if (scm_is_true(scm_call_1(scm_proc_exists_p,
                               scm_from_locale_string(proc)))){
        *scm_proc = scm_c_eval_string(proc);
        return 0;
    }
    return -1;
}

// Process robot logic forever.
void *process_logic(void *vps){
    rov_pl_param  *p           = vps;
    const char    *logic_path  = p->logic_path;
    rov_screen    *scr         = p->scr;
    rov_arduino   *a           = p->a;
    rov_ctrlstate  oldctrl     = a->ctrl;
    bool           always_step = p->always_step;
    useconds_t     sleep_time  = p->phz;
    FILE          *logic_fl    = fopen(logic_path,"r");
    rov_clean_js   cjs;
    rov_clean_js   oldinput;
    char           lb[BUFSIZ];
    SCM            scm_proc_exists_p;
    SCM            scm_logic_step;
    SCM            scm_sanatize;
    SCM            scm_ctrl_state;
    SCM            scm_initialize;
    SCM            scm_input_state;
    struct timeval d,before,after;
    long long      delta_t;
    memset(lb,0,BUFSIZ);
    fread(lb,sizeof(char),BUFSIZ,logic_fl);
    fclose(logic_fl);
    update_stats(scr,a);
    scm_init_guile();
    scm_c_eval_string(init_logic_str);
    scm_leftmotor     = scm_c_eval_string("left-motor");
    scm_rightmotor    = scm_c_eval_string("right-motor");
    scm_frontmotor    = scm_c_eval_string("front-motor");
    scm_backmotor     = scm_c_eval_string("back-motor");
    scm_headlights    = scm_c_eval_string("headlights");
    scm_sidelights    = scm_c_eval_string("sidelights");
    scm_lasers        = scm_c_eval_string("lasers");
    scm_clawgrip      = scm_c_eval_string("claw-grip");
    scm_claw_90       = scm_c_eval_string("claw-90");
    scm_claw_180      = scm_c_eval_string("claw-180");
    scm_proc_exists_p = scm_c_eval_string("proc-exists?");
    scm_sanatize      = scm_c_eval_string("sanatize-ctrl-state");
    scm_c_eval_string(lb);
    if (safe_scm_c_lookup("initialize",scm_proc_exists_p,&scm_initialize)){
        screen_printattr(scr,RED_PAIR,
                         "ERROR: Failed to find 'initialize' procedure");
        return NULL;
    }
    if (safe_scm_c_lookup("logic-step",scm_proc_exists_p,&scm_logic_step)){
        screen_printattr(scr,RED_PAIR,
                         "ERROR: Failed to find 'logic-step' procedure");
        return NULL;
    }
    scm_ctrl_state = scm_call_1(scm_initialize,
                                scm_c_eval_string("(mk-ctrl-state 0 0 0"
                                                  " 0 #f #f #f #f #f #f 'e)"));
    gettimeofday(&after,NULL);
    for (;;){
        read_jsevent(&a->joystick);
        oldctrl = a->ctrl;
        oldinput = cjs;
        clean_joystick(&a->joystick,&a->keybinds,&cjs);
        scm_input_state = scm_from_cjs(&cjs);
        if (always_step || memcmp(&oldinput,&cjs,sizeof(rov_clean_js))){
            gettimeofday(&before,NULL);
            timeval_subtract(&d,&after,&before);
            delta_t = total_usec(&d);
            screen_printf(scr,"* tx:%d ry:%d",
                          a->joystick.axes[a->keybinds.transpose_xv[0].axis],
                          a->joystick.axes[a->keybinds.rotate_yv[0].axis]);
            screen_printf(scr,"tx:%d ry:%d",cjs.transpose_x,cjs.rotate_y);
            screen_printf(scr,"c:%d",scale_cleaned_val(cjs.transpose_x));
            scm_ctrl_state = scm_call_1(scm_sanatize,
                                        scm_call_3(scm_logic_step,
                                                   scm_input_state,
                                                   scm_from_double(delta_t),
                                                   scm_ctrl_state));
            gettimeofday(&after,NULL);
            ctrl_from_scm(scm_ctrl_state,&a->ctrl);
            sync_ctrlstate(a,&oldctrl);
            diff_update_stats(scr,a,&oldctrl);
            usleep(sleep_time);
          }
          usleep(sleep_time); // Sleep the thread (quantizes the polling rate).
    }
    return NULL;
}

// Converts a (sanatized) ctrl-state to an rov_controlstate.
// WARNING: Does no type checking, call sanatize-ctrl-state first.
void ctrl_from_scm(SCM scm_ctrl,rov_ctrlstate *ctrl){
    ctrl->leftmotor  = scm_to_short(scm_call_1(scm_leftmotor,scm_ctrl));
    ctrl->rightmotor = scm_to_short(scm_call_1(scm_rightmotor,scm_ctrl));
    ctrl->frontmotor = scm_to_short(scm_call_1(scm_frontmotor,scm_ctrl));
    ctrl->backmotor  = scm_to_short(scm_call_1(scm_backmotor,scm_ctrl));
    ctrl->headlights = scm_to_bool(scm_call_1(scm_headlights,scm_ctrl));
    ctrl->sidelights = scm_to_bool(scm_call_1(scm_sidelights,scm_ctrl));
    ctrl->lasers     = scm_to_bool(scm_call_1(scm_lasers,scm_ctrl));
    ctrl->clawgrip   = scm_to_bool(scm_call_1(scm_clawgrip,scm_ctrl));
    ctrl->claw_90    = scm_to_bool(scm_call_1(scm_claw_90,scm_ctrl));
    ctrl->claw_180   = scm_to_bool(scm_call_1(scm_claw_180,scm_ctrl));
}


// Converts a boolean into a string representing the scheme type.
#define SCM_BOOL_STR(b) ((b) ? "#t" : "#f")

// return: A scheme input-state built from a clean joystick.
SCM scm_from_cjs(rov_clean_js *cjs){
    char b[BUFSIZ];
    snprintf(b,BUFSIZ,"(mk-input-state %s %s %s %s %s %d %d %d %d %d %d)",
             SCM_BOOL_STR(cjs->claw_open),
             SCM_BOOL_STR(cjs->claw_close),
             SCM_BOOL_STR(cjs->laser_toggle),
             SCM_BOOL_STR(cjs->headlight_toggle),
             SCM_BOOL_STR(cjs->sidelight_toggle),
             cjs->claw_x,
             cjs->claw_y,
             cjs->rotate_z,
             cjs->rotate_y,
             cjs->transpose_x,
             cjs->transpose_y);
    return scm_c_eval_string(b);
}
