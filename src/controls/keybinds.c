// Joe Jevnik
// 2014.3.13
// Keybinds parsing.

#include "keybinds.h"
#include "controls.h"

rov_jsaxis x_axis;            // Joystick's x axis.
rov_jsaxis y_axis;            // Joystick's y axis.
rov_jsaxis twist_axis;        // Joystick's twisting axis.
rov_jsaxis slider_axis;       // Joystick's slider.
rov_jsaxis hat_x_axis;        // Hat switch x axis.
rov_jsaxis hat_y_axis;        // Hat switch y axis.

rov_jsaxis trans_y_pair_axis; // Axis pair to translate y;
rov_jsaxis rot_z_pair_axis;   // Axis to rotate about the z.

// The default set of keybinds.
rov_keybinds default_keybinds;

const char* const claw_open_str        = "claw-open";
const char* const claw_close_str       = "claw-close";
const char* const claw_x_str           = "claw-x";
const char* const claw_y_str           = "claw-y";
const char* const rotate_z_str         = "rotate-z";
const char* const rotate_y_str         = "rotate-y";
const char* const transpose_x_str      = "transpose-x";
const char* const transpose_y_str      = "transpose-y";
const char* const turn_y_str           = "turn-y";
const char* const thrust_mod_str       = "thrust-mod";
const char* const laser_toggle_str     = "laser-toggle";
const char* const headlight_toggle_str = "headlight-toggle";
const char* const sidelight_toggle_str = "sidelight-toggle";

// Sets up the initial default keybind values, call before parsing!
void init_keybinds(){
    // The default 6 axis that exist on the joystick.
    x_axis.is_pair                     = false;
    x_axis.axis                        = ROV_JS_X;
    y_axis.is_pair                     = false;
    y_axis.axis                        = ROV_JS_Y;
    twist_axis.is_pair                 = false;
    twist_axis.axis                    = ROV_JS_T;
    slider_axis.is_pair                = false;
    slider_axis.axis                   = ROV_JS_S;
    hat_x_axis.is_pair                 = false;
    hat_x_axis.axis                    = ROV_JS_HX;
    hat_y_axis.is_pair                 = false;
    hat_y_axis.axis                    = ROV_JS_HY;

    trans_y_pair_axis.is_pair           = true;
    trans_y_pair_axis.pos               = 3;
    trans_y_pair_axis.neg               = 2;

    rot_z_pair_axis.is_pair             = true;
    rot_z_pair_axis.pos                 = 5;
    rot_z_pair_axis.neg                 = 4;

    // The default set of keybinds.
    default_keybinds.claw_openc         = 1;
    *default_keybinds.claw_openv        = 0;

    default_keybinds.claw_closec        = 1;
    *default_keybinds.claw_closev       = 1;

    default_keybinds.claw_xc            = 1;
    *default_keybinds.claw_xv           = hat_x_axis;

    default_keybinds.claw_yc            = 1;
    *default_keybinds.claw_yv           = hat_y_axis;

    default_keybinds.rotate_yc          = 1;
    *default_keybinds.rotate_yv         = twist_axis;

    default_keybinds.rotate_zc          = 1;
    *default_keybinds.rotate_zv         = rot_z_pair_axis;

    default_keybinds.transpose_xc       = 1;
    *default_keybinds.transpose_xv      = y_axis;

    default_keybinds.transpose_yc       = 1;
    *default_keybinds.transpose_yv      = trans_y_pair_axis;

    default_keybinds.turn_yc            = 1;
    *default_keybinds.turn_yv           = x_axis;

    default_keybinds.thrust_modc        = 1;
    *default_keybinds.thrust_modv       = slider_axis;

    default_keybinds.laser_togglec      = 1;
    *default_keybinds.laser_togglev     = 11;

    default_keybinds.headlight_togglec  = 1;
    *default_keybinds.headlight_togglev = 8;

    default_keybinds.sidelight_togglec  = 1;
    *default_keybinds.sidelight_togglev = 10;
}

// Length constants
#define OPCOUNT     13
#define BUTTONCOUNT 5
#define AXESCOUNT   8

// Reads sexpr, updating the keybinds as needed.
// return: 0 on success, non-zero on failure.
int keybinds_read_scm_line(rov_keybinds *kbs,char *str){
    size_t        len;
    char         *op;
    int           n;
    int           b = 0;
    const char   *ops[OPCOUNT]      = { claw_open_str,
                                        claw_close_str,
                                        laser_toggle_str,
                                        claw_x_str,
                                        claw_y_str,
                                        rotate_z_str,
                                        rotate_y_str,
                                        transpose_x_str,
                                        transpose_y_str,
                                        turn_y_str,
                                        thrust_mod_str,
                                        headlight_toggle_str,
                                        sidelight_toggle_str };
    unsigned char *bvs[BUTTONCOUNT] = { kbs->claw_openv,
                                        kbs->claw_closev,
                                        kbs->laser_togglev,
                                        kbs->headlight_togglev,
                                        kbs->sidelight_togglev };
    rov_jsaxis    *avs[AXESCOUNT]   = { kbs->claw_xv,
                                        kbs->claw_yv,
                                        kbs->rotate_zv,
                                        kbs->rotate_yv,
                                        kbs->transpose_xv,
                                        kbs->transpose_yv,
                                        kbs->turn_yv,
                                        kbs->thrust_modv };
    size_t        *cs[OPCOUNT]      = { &kbs->claw_openc,
                                        &kbs->claw_closec,
                                        &kbs->laser_togglec,
                                        &kbs->claw_xc,
                                        &kbs->claw_yc,
                                        &kbs->rotate_zc,
                                        &kbs->rotate_yc,
                                        &kbs->transpose_xc,
                                        &kbs->transpose_yc,
                                        &kbs->turn_yc,
                                        &kbs->thrust_modc,
                                        &kbs->headlight_togglec,
                                        &kbs->sidelight_togglec };
    if (str[0] == ';'){
        return 0;
    }
    if (str[0] != '('){
        return -1;
    }
    op = strtok(&str[1]," ");
    for (n = 0;n < OPCOUNT;n++){
        b += !strcmp(op,ops[n]);
    }
    if (!b){
        return -2;
    }
    len = strtol(strtok(NULL," "),NULL,10);
    for (n = 0;n < BUTTONCOUNT;n++){
        keybinds_parse_scm_params(op,ops[n],len,bvs[n],cs[n],true);
    }
    for (n = 0;n < AXESCOUNT;n++){
        keybinds_parse_scm_params(op,ops[n + BUTTONCOUNT],len,
                                  avs[n],cs[n + BUTTONCOUNT],false);
    }
    return 0;
}

// Parses the params from a sexpr.
// For param info, see: keybinds.h
void keybinds_parse_scm_params(char *op,const char *cstr,size_t len,
                               void *v,size_t *c,bool is_button){
    int n;
    unsigned char *bv = v;
    rov_jsaxis    *jv = v;
    char *a,*b;
    if (!strcmp(op,cstr)){ // Branch if it is a button param.
        *c = len;
        if (is_button){
            for (n = 0;n < len;n++){
                strtok(NULL," ");
                bv[n] = atoi(strtok(NULL,")"));
            }
        }else{ // Branch if it is an axis param.
            for (n = 0;n < len;n++){
                strtok(NULL," ");
                a = strtok(NULL," ");
                if (a[0] == '('){ // Checks if this is an axis-pair.
                    b = strtok(NULL,")");
                    jv[n].is_pair = true;
                    jv[n].pos     = atoi(&a[1]);
                    jv[n].neg     = atoi(b);
                    continue;
                }
                jv[n].is_pair = false;
                jv[n].axis    = atoi(a);
            }
        }
    }
}

// Parse a keybinds config out of a file (pass the path).
// If the file is NULL, or there is an error, returns a pointer to the default
// keybinds config.
// IO WARNING: Calls out to './keybinds-parser.scm' which requires guile.
// return: 0 on success, non-zero on failure.
int parse_keybinds(rov_keybinds *kbs,const char *kfl){
    char  cmd[128];
    FILE *scm;
    char  path[256];
    memcpy(kbs,&default_keybinds,sizeof(rov_keybinds));
    if (!kfl){
        return 0;
    }
    getcwd(path,256);
    strncat(path,kfl,256 - strlen(path));
    if (!access(path,F_OK)){
        return -1;
    }
    strcpy(cmd,"./keybinds-parser.scm");
    cmd[21] = ' ';
    cmd[22] = '\0';
    strncat(cmd,kfl,110);
    strncat(cmd," 2> /dev/null",128 - strlen(cmd));
    scm = popen(cmd,"r");
    while (fgets(cmd,128,scm) != NULL){
        if (keybinds_read_scm_line(kbs,cmd)){
            memcpy(kbs,&default_keybinds,sizeof(rov_keybinds));
            return -1;
        }
    }
    return 0;
}
