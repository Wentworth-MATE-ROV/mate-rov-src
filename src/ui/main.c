/* main.c --- Testing the main function.
   Copyright (c) Joe Jevnik

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along with
   this program; if not, write to the Free Software Foundation, Inc., 51
   Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

#include "../common.h"
#include "../comm/comm.h"
#include "keyboard.h"
#include "stats.h"
#include "../controls/pinlayout.h"

#include <stdio.h>
#include <pthread.h>
#include <getopt.h>

char *statv[STATC];

// The help string
const char* const help_str =
    "Do you really need help with your own robot code?";

// The version string.
const char* const version_str =
    "mate-rov-src " VERSION_NUMBER " " BUILD_DATE "\n\
Copyright (C) 2014 Joe Jevnik.\n\
This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.";

int main(int argc,char **argv){
    rov_arduino          a;
    rov_screen           scr;
    pthread_t            jst;
    rov_pl_param         plp;
    rov_pq_param         pqp;
    int                  opt_ind;
    int                  c;
    const char          *logic_fl;
    const char          *keybinds_fl = ".keybinds";  // Default keybinds.
    const char          *pin_fl      = ".pins";      // Default pin config.
    const char          *log_fl      = "/dev/null";  // Default log file.
    static struct option long_ops[] =
        { { "help",     no_argument,       0, 'h' },
          { "version",  no_argument,       0, 'v' },
          { "pin",      required_argument, 0, 'p' },
          { "keybinds", required_argument, 0, 'k' },
          { "logic",    required_argument, 0, 'l' },
          { "log",      required_argument, 0, 'o' },
          { 0,          0,                 0,  0  } };
    if (argc == 1){
        puts("Usage: rov [-h --help|-v --version][-p PIN-CONFIG][-k KEYBINDS]"
             " -l LOGIC-FILE");
        return -1;
    }
    for (;;){
        opt_ind = 0;
        c = getopt_long(argc,argv,"hvp:k:l:o:",long_ops,&opt_ind);
        if (c == -1){
            break;
        }
        switch(c){
        case 'h':
            puts(help_str);
            return 0;
        case 'v':
            puts(version_str);
            return 0;
        case 'p':
            pin_fl = optarg;
            break;
        case 'k':
            keybinds_fl = optarg;
            break;
        case 'l':
            logic_fl = optarg;
            break;
        case 'o':
            log_fl = optarg;
            break;
        case '?':
            return -1;
        default:
            printf("Unknown argument: %c\n",c);
        }
    }
    // Loads the default keybinds.
    init_keybinds();
    if (init_arduino(&a,"/dev/ttyACM0","/dev/input/js0")){
        fputs("Could not initialize the arduino\n",stderr);
        return -1;
    }
    init_screen(&scr,fopen(log_fl,"w"),STATC,YELLOW_PAIR,statv);
    print_staticui(&scr);  // Sets up the frames and lines.
    screen_printattr(&scr,GREEN_PAIR,"Waiting 2s for Arduino...");
    sleep(2);
    pqp.scr         = &scr;
    pqp.q           = &a.queue;
    pthread_create(&a.qt,NULL,process_queue,&pqp);
    plp.a           = &a;
    plp.scr         = &scr;
    plp.phz         = 100;
    plp.shz         = 100;
    plp.logic_path  = logic_fl;
    plp.always_step = false;  // Call logic step every frame?
    screen_reload_keybinds(&scr,&a,keybinds_fl,false);
    screen_reload_pinlayout(&scr,&a,pin_fl,false);
    pinmode_sync(&a);  // Set the pinmodes to the appropriate values.
    pthread_create(&jst,NULL,process_logic,&plp);

    // Process keyboard; this blocks the main thread.
    process_keyboard(&scr,&a,keybinds_fl,pin_fl);

    // Cleanup
    destroy_screen(&scr);
    destroy_arduino(&a);
    return 0;
}
