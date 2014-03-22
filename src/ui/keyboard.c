// Joe Jevnik
// 2014.3.18
// Main screen operations.

#include "keyboard.h"

// Quick macro to faclitate printing a string to scr as boldgreen.
#define boldgreenprint(scr,str) screen_printattr(scr,GREEN_PAIR | A_BOLD,str)


// Test print for an axis.
static void print_jsaxis(rov_screen *scr,rov_jsaxis *j){
    if (j->is_pair){
        screen_printfattr(scr,GREEN_PAIR,"axis-pair: (%u %u) ",j->pos,j->neg);
    }else{
        screen_printfattr(scr,GREEN_PAIR,"axis: %u ",j->axis);
    }
}

// Quick macros to compare if diff'd.
#define diffcmpbutton(str,val,count)                                    \
    if (!diff || memcmp(&old.val,kbs->val,6)){                          \
        boldgreenprint(scr,str);                                        \
        for (n = 0;n < kbs->count;n++){                                 \
            screen_printfattr(scr,GREEN_PAIR,"%u ",kbs->val[n]);        \
        }                                                               \
        ++c;                                                            \
    }
#define diffcmpaxis(str,val,count)                                      \
    if (!diff || memcmp(&old.val,kbs->val,12 * sizeof(rov_jsaxis))){    \
        boldgreenprint(scr,str);                                        \
        for (n = 0;n < kbs->count;n++){                                 \
            print_jsaxis(scr,&kbs->val[n]);                             \
        }                                                               \
        ++c;                                                            \
    }

// Reloads the keybinds from the .keybinds file.
void screen_reload_keybinds(rov_screen *scr,rov_arduino *a,bool diff){
    int n,c = 0;
    rov_keybinds  old = a->keybinds;
    rov_keybinds *kbs = &a->keybinds;
    char line[81];
    if (parse_keybinds(kbs,".keybinds")){
        screen_printattr(scr,RED_PAIR,"Failed to reload keybinds!");
        return;
    }
    memset(line,'-',80);
    line[81] = '\0';
    screen_printattr(scr,GREEN_PAIR,line);
    diffcmpbutton("claw-open: ",claw_openv,claw_openc);
    diffcmpbutton("claw-close: ",claw_closev,claw_closec);
    diffcmpaxis("claw-x: ",claw_xv,claw_xc);
    diffcmpaxis("claw-y: ",claw_yv,claw_yc);
    diffcmpaxis("rotate-z: ",rotate_zv,rotate_zc);
    diffcmpaxis("rotate-y: ",rotate_yv,rotate_yc);
    diffcmpaxis("transpose-x: ",transpose_xv,transpose_xc);
    diffcmpaxis("transpose-y: ",transpose_yv,transpose_yc);
    diffcmpaxis("turn-y: ",turn_yv,turn_yc);
    diffcmpaxis("thrust-mod: ",thrust_modv,thrust_modc);
    diffcmpbutton("laser-on: ",laser_onv,laser_onc);
    diffcmpbutton("laser-off: ",laser_offv,laser_offc);
    diffcmpbutton("laser-toggle: ",laser_togglev,laser_togglec);
    if (!c){
        screen_printattr(scr,GREEN_PAIR | A_BOLD,"Nothing to reload!");
    }
}

// Handles all keyboard presses.
// Pass the screen.
void *process_keyboard(void *vp){
    rov_pkb_param *p   = vp;
    rov_screen    *scr = p->scr;
    rov_arduino   *a   = p->a;
    int c;
    while ((c = getch())){
        switch(c){
        case RELOAD_KEYBINDS:
            screen_reload_keybinds(scr,a,true);
            break;
        }
    }
    return NULL;
}
