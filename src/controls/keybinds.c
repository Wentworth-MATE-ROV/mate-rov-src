// Joe Jevnik
// 2014.3.13
// Keybinds parsing.

#include "keybinds.h"
#include "controls.h"

rov_jsaxis x_axis;
rov_jsaxis y_axis;
rov_jsaxis twist_axis;
rov_jsaxis slider_axis;
rov_jsaxis hat_x_axis;
rov_jsaxis hat_y_axis;

rov_jsaxis test_pair_axis;

// The default set of keybinds.
rov_keybinds default_keybinds;

const char* const claw_open_str  = "claw-open";
const char* const claw_close_str = "claw-close";
const char* const rotate_x_str   = "rotate-x";
const char* const rotate_y_str   = "rotate-y";
const char* const transpose_str  = "transpose";
const char* const turn_str       = "turn";
const char* const thrust_mod_str = "thrust-mod";

// Sets up the initial default keybind values, call before parsing!
void init_keybinds(){
    // The default 6 axis that exist on the joystick.
    x_axis.is_pair              = false;
    x_axis.axis                 = ROV_JS_X;
    y_axis.is_pair              = false;
    y_axis.axis                 = ROV_JS_Y;
    twist_axis.is_pair          = false;
    twist_axis.axis             = ROV_JS_T;
    slider_axis.is_pair         = false;
    slider_axis.axis            = ROV_JS_S;
    hat_x_axis.is_pair          = false;
    hat_x_axis.axis             = ROV_JS_HX;
    hat_y_axis.is_pair          = false;
    hat_y_axis.axis             = ROV_JS_HY;

    test_pair_axis.is_pair      = true;
    test_pair_axis.pos          = 3;
    test_pair_axis.neg          = 4;

    // The default set of keybinds.
    default_keybinds.claw_openc   = 1;
    *default_keybinds.claw_openv  = 5;

    default_keybinds.claw_closec  = 1;
    *default_keybinds.claw_closev = 6;

    default_keybinds.rotate_xc    = 1;
    *default_keybinds.rotate_xv   = twist_axis;

    default_keybinds.rotate_yc    = 1;
    *default_keybinds.rotate_yv   = test_pair_axis;

    default_keybinds.transposec   = 1;
    *default_keybinds.transposev  = y_axis;

    default_keybinds.turnc        = 1;
    *default_keybinds.turnv       = x_axis;

    default_keybinds.thrust_modc  = 1;
    *default_keybinds.thrust_modv = slider_axis;
}

// Frees the key lists.
void destroy_keybinds(rov_keybinds *kbs){
}

// Reads sexpr, updating the keybinds as needed.
// return: 0 on success, non-zero on failure.
int read_scm_line(rov_keybinds *kbs,char *str){
    size_t        len;
    char         *op;
    int           n;
    int           b = 0;
    const char*   ops[7]  = { claw_open_str,claw_close_str,rotate_x_str,
                              rotate_y_str,transpose_str,turn_str,
                              thrust_mod_str };
    unsigned char* bvs[2] = { kbs->claw_openv,kbs->claw_closev };
    rov_jsaxis*    avs[5] = { kbs->rotate_xv,kbs->rotate_yv,kbs->transposev,
                              kbs->turnv,kbs->thrust_modv };
    size_t*        cs[7]  = { &kbs->claw_openc,&kbs->claw_closec,
                              &kbs->rotate_xc,&kbs->rotate_yc,
                              &kbs->transposec,&kbs->turnc,&kbs->thrust_modc };
    if (str[0] == ';'){
        return 0;
    }
    if (str[0] != '('){
        return -1;
    }
    op = strtok(&str[1]," ");
    for (n = 0;n < 7;n++){
        b += !strcmp(op,ops[n]);
    }
    if (!b){
        return -2;
    }
    len = strtol(strtok(NULL," "),NULL,10);
    for (n = 0;n < 2;n++){
        parse_scm_params(kbs,op,ops[n],len,bvs[n],cs[n],true);
    }
    for (n = 0;n < 5;n++){
        parse_scm_params(kbs,op,ops[n + 2],len,avs[n],cs[n + 2],false);
    }
    return 0;
}

// Parses the params from a sexpr.
// For param info, see: keybinds.h
void parse_scm_params(rov_keybinds *kbs,char *op,const char *cstr,size_t len,
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
                *bv = atoi(strtok(NULL,")"));
            }
        }else{ // Branch if it is an axis param.
            for (n = 0;n < len;n++){
                strtok(NULL," ");
                a = strtok(NULL," ");;
                if (a[0] == '('){ // Checks if this is an axis-pair.
                    b = strtok(NULL,")");
                    jv->is_pair = true;
                    jv->pos  = atoi(&a[1]);
                    jv->neg  = atoi(b);
                    continue;
                }
                jv->is_pair = false;
                jv->axis    = atoi(a);
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
    memcpy(kbs,&default_keybinds,sizeof(rov_keybinds));
    if (!kfl){
        return 0;
    }
    strcpy(cmd,"./keybinds-parser.scm");
    cmd[21] = ' ';
    cmd[22] = '\0';
    strncat(cmd,kfl,110);
    strncat(cmd," 2> /dev/null",128 - strlen(cmd));
    scm = popen(cmd,"r");
    while (fgets(cmd,128,scm) != NULL){
        if (read_scm_line(kbs,cmd)){
            kbs = &default_keybinds;
            return -1;
        }
    }
    return 0;
}
