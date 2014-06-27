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

// return: trucates v to the range of a normal joystick axis.
short trunc_cleaned_val(int v){
    return (v > SHRT_MAX) ? SHRT_MAX
        : (v < SHRT_MIN) ? SHRT_MIN : v;
}


// Applies the keybinds to the joystick to clean it, storing the result in c.
void clean_joystick(rov_joystick *js,rov_keybinds *kbs,rov_clean_js *c){
    size_t n;

// Finds if any of these buttons are true.
#define IS_BUTTON(bs)                                              \
    for (n = 0;n < kbs->bs ## c;n++){                              \
        if (is_button(js,kbs->bs ## v[n])){                        \
            c->bs = true;                                          \
            break;                                                 \
        }                                                          \
    }                                                              \

// Average the axes values.
#define AVG_AXES(as) \
    for (n = 0;n < kbs->as ## c;n++){                                   \
        if (kbs->as ## v[n].is_pair){                                   \
            if (is_button(js,kbs->as ## v[n].pos)){                     \
                c->as += SHRT_MAX;                                      \
            }                                                           \
            if (is_button(js,kbs->as ## v[n].neg)){                     \
                c->as += SHRT_MIN;                                      \
            }                                                           \
        }else{                                                          \
            c->as += js->axes[kbs->as ## v[n].axis];                    \
        }                                                               \
    }                                                                   \
    c->as = trunc_cleaned_val(c->as / kbs->as ##c);                     \

    memset(c,0,sizeof(rov_clean_js));  // Reset the clean_js.
    IS_BUTTON(headlight_toggle);
    IS_BUTTON(sidelight_toggle);
    IS_BUTTON(laser_toggle);
    IS_BUTTON(claw_open);
    IS_BUTTON(claw_close);
    AVG_AXES(transpose_x);
    AVG_AXES(transpose_y);
    AVG_AXES(rotate_y);
    AVG_AXES(rotate_z);
    AVG_AXES(claw_x);
    AVG_AXES(claw_y);

#undef IS_BUTTON
#undef AVG_AXES
}


// Syncs the local control state back to the arduino if it has changed.
void sync_ctrlstate(rov_arduino *a,rov_ctrlstate *old){
    size_t n;

// Syncs the state of b iff it updated.
#define DIFF_SYNC_BOOL(b)                                        \
    if (old->b != a->ctrl.b){                                    \
        for (n = 0;n < a->layout.b ## c;n++){                    \
            digital_write(a,a->layout.b ## v[n],a->ctrl.b);      \
        }                                                        \
    }                                                            \

// Syncs the state of ax iff it updated.
#define DIFF_SYNC_AXIS(ax)                                              \
    for (n = 0;n < a->layout.ax ## c;n++){                              \
        servo_write(a,a->layout.ax ## v[n],a->ctrl.ax);                 \
    }                                                                   \


    DIFF_SYNC_BOOL(laser);
    DIFF_SYNC_BOOL(headlight);
    DIFF_SYNC_BOOL(sidelight);
    DIFF_SYNC_BOOL(clawgrip);
    DIFF_SYNC_BOOL(claw_90);
    DIFF_SYNC_BOOL(claw_180);

    DIFF_SYNC_AXIS(leftmotor);
    DIFF_SYNC_AXIS(rightmotor);
    DIFF_SYNC_AXIS(frontmotor);
    DIFF_SYNC_AXIS(backmotor);

#undef DIFF_SYNC_BOOL
#undef DIFF_SYNC_AXIS
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
    useconds_t     sleep_time  = 1000000 / p->phz;
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
    fread(lb,sizeof(char),BUFSIZ,logic_fl);  // Load the logic file.
    fclose(logic_fl);
    update_stats(scr,a);  // Paint the stats.
    scm_init_guile();  // Start of the scheme vm.
    scm_c_eval_string(init_logic_str);  // Initialize the scheme vm.

    // Load up the accessor functions.
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

    // Evauluate the user's logic code.
    scm_c_eval_string(lb);

    // Lookup their initialize function, fail if it doesn't exist.
    if (safe_scm_c_lookup("initialize",scm_proc_exists_p,&scm_initialize)){
        screen_printattr(scr,RED_PAIR,
                         "ERROR: Failed to find 'initialize' procedure");
        return NULL;
    }

    // Lookup their logic-step function, fail if it doesn't exist.
    if (safe_scm_c_lookup("logic-step",scm_proc_exists_p,&scm_logic_step)){
        screen_printattr(scr,RED_PAIR,
                         "ERROR: Failed to find 'logic-step' procedure");
        return NULL;
    }

    // Make an empty <ctrl-state>.
    scm_ctrl_state = scm_call_1(scm_initialize,
                                scm_c_eval_string("(mk-ctrl-state 0 0 0"
                                                  " 0 #f #f #f #f #f #f 'e)"));
    gettimeofday(&after,NULL);  // The time since init starts now.
    for (;;){
        read_jsevent(&a->joystick);      // Load the js events.
        oldctrl = a->ctrl;               // Store the old control state.
        oldinput = cjs;                  // Store the old input state.
        clean_joystick(&a->joystick,&a->keybinds,&cjs);
        scm_input_state = scm_from_cjs(&cjs);
        if (always_step || memcmp(&oldinput,&cjs,sizeof(rov_clean_js))){
            gettimeofday(&before,NULL);  // Get the time for delta-t.
            timeval_subtract(&d,&after,&before);
            delta_t = total_usec(&d);
            // Call the users logic-step and sanatize the output.
            scm_ctrl_state = scm_call_1(scm_sanatize,
                                        scm_call_3(scm_logic_step,
                                                   scm_input_state,
                                                   scm_from_double(delta_t),
                                                   scm_ctrl_state));
            gettimeofday(&after,NULL);  // Get the time for delta-t round 2.
            ctrl_from_scm(scm_ctrl_state,&a->ctrl);
            sync_ctrlstate(a,&oldctrl); // Sync back to the robot.
            diff_update_stats(scr,a,&oldctrl);
          }
          usleep(sleep_time);           // Sleep the thread.
    }
    return NULL;
}

// Converts a (sanatized) ctrl-state to an rov_ctrlstate.
// WARNING: Does no type checking, call sanatize-ctrl-state first.
void ctrl_from_scm(SCM scm_ctrl,rov_ctrlstate *ctrl){
    ctrl->leftmotor  = scm_to_uchar(scm_call_1(scm_leftmotor,scm_ctrl));
    ctrl->rightmotor = scm_to_uchar(scm_call_1(scm_rightmotor,scm_ctrl));
    ctrl->frontmotor = scm_to_uchar(scm_call_1(scm_frontmotor,scm_ctrl));
    ctrl->backmotor  = scm_to_uchar(scm_call_1(scm_backmotor,scm_ctrl));
    ctrl->headlight  = scm_to_bool(scm_call_1(scm_headlights,scm_ctrl));
    ctrl->sidelight  = scm_to_bool(scm_call_1(scm_sidelights,scm_ctrl));
    ctrl->laser      = scm_to_bool(scm_call_1(scm_lasers,scm_ctrl));
    ctrl->clawgrip   = scm_to_bool(scm_call_1(scm_clawgrip,scm_ctrl));
    ctrl->claw_90    = scm_to_bool(scm_call_1(scm_claw_90,scm_ctrl));
    ctrl->claw_180   = scm_to_bool(scm_call_1(scm_claw_180,scm_ctrl));
}


// return: A scheme input-state built from a clean joystick.
SCM scm_from_cjs(rov_clean_js *cjs){
    char b[BUFSIZ];

// Converts a boolean into a string representing the scheme type.
#define SCM_BOOL_STR(b) ((b) ? "#t" : "#f")

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

#undef SCM_BOOL_STR
}
