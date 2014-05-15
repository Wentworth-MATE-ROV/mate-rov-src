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
const char* const laser_toggle_str     = "laser-toggle";
const char* const headlight_toggle_str = "headlight-toggle";
const char* const sidelight_toggle_str = "sidelight-toggle";

// Sets up the initial default keybind values, call before parsing!
void init_keybinds(){
    memset(default_keybinds.keycounts,0,KEYCOUNT * sizeof(size_t));
}

// Reads sexpr, updating the keybinds as needed.
// return: 0 on success, non-zero on failure.
int keybinds_read_scm_line(rov_keybinds *kbs,SCM scm){
    SCM          scm_len;
    size_t       len;
    SCM          scm_op;
    char        *op;
    unsigned int n,m;
    SCM          scm_val;
    const char  *ops[KEYCOUNT] = { claw_open_str,
                                   claw_close_str,
                                   laser_toggle_str,
                                   headlight_toggle_str,
                                   sidelight_toggle_str,
                                   claw_x_str,
                                   claw_y_str,
                                   rotate_z_str,
                                   rotate_y_str,
                                   transpose_x_str,
                                   transpose_y_str };
    scm_op  = scm_car(scm);
    op      = scm_to_locale_string(scm_op);
    scm     = scm_cdr(scm);
    scm_len = scm_length(scm);
    len     = scm_to_size_t(scm_len);
    for (n = 0;n < KEYCOUNT;n++){
        if (!strcmp(op,ops[n])){
            break;
        }
    }
    if (n == KEYCOUNT){
        free(op);
        return -1;
    }
    kbs->keycounts[n] = len;
    if (n < KEYBUTTONCOUNT){
        for (m = 0;m < len;m++){
            kbs->buttonvalues[n][m]
                = scm_to_char(scm_list_ref(scm,scm_from_uint(m)));
        }
    }else{
        n -= KEYBUTTONCOUNT;
        for (m = 0;m < len;m++){
            scm_val = scm_list_ref(scm,scm_from_uint(m));
            if (scm_is_pair(scm_val)){
                kbs->axesvalues[n][m].is_pair = true;
                kbs->axesvalues[n][m].pos
                    = scm_to_char(scm_car(scm_val));
                kbs->axesvalues[n][m].neg
                    = scm_to_char(scm_car(scm_val));
            }else{
                kbs->axesvalues[n][m].is_pair = false;
                kbs->axesvalues[n][m].axis
                    = scm_to_char(scm_val);
            }
        }
    }
    free(op);
    return 0;
}


// Parse a keybinds config out of a file (pass the path).
// If the file is NULL, or there is an error, returns a pointer to the default
// keybinds config.
// IO WARNING: Calls out to './keybinds-parser.scm' which requires guile.
// return: 0 on success, non-zero on failure.
int parse_keybinds(rov_keybinds *kbs,const char *kfl){
    FILE *scm_parser = fopen("keybinds-parser.scm","r");
    FILE *kbs_fl;
    char  path[256];
    char  scm_fc[BUFSIZ];
    SCM   scm;
    if (!scm_parser){
        return -1;
    }
    memset(path,0,256);
    memcpy(kbs,&default_keybinds,sizeof(rov_keybinds));
    if (!kfl){
        return 0;
    }
    getcwd(path,256);
    path[strlen(path)] = '/';
    strncat(path,kfl,256 - strlen(path));
    if (access(path,F_OK)){
        return -1;
    }
    kbs_fl = fopen(path,"r");
    if (!kbs_fl){
        return -1;
    }
    scm_init_guile();
    fread(scm_fc,sizeof(char),BUFSIZ,scm_parser);
    fclose(scm_parser);
    scm_c_eval_string(scm_fc);
    while (fgets(scm_fc,BUFSIZ,kbs_fl)){
        scm = scm_c_eval_string(scm_fc);
        if (scm != SCM_UNSPECIFIED && scm_is_pair(scm)){
            keybinds_read_scm_line(kbs,scm);
        }
    }
    return 0;
}
