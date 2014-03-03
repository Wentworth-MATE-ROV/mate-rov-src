// Joe Jevnik
// 2014.2.28
// Screen and UI declerations.

#ifndef ROV_SCREEN_H
#define ROV_SCREEN_H

#include "../common.h"

#include <ncurses.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdarg.h>

// The color pairs for printing and attributes.
#define DEFAULT_PAIR 0
#define GREEN_PAIR   COLOR_PAIR(1)
#define RED_PAIR     COLOR_PAIR(2)
#define YELLOW_PAIR  COLOR_PAIR(3)
#define MAGENTA_PAIR COLOR_PAIR(4)

// Initializes the screen structure and enters ncurses mode.
void init_screen(rov_screen*,rov_arduino*,FILE*);

void init_logmsg(rov_logmsg*,char*,int);

// Destroys (unitializes) the screen structure and leaves ncurses mode.
void destroy_screen(rov_screen*);

// Refreshes the screen elements.
void refresh_screen(rov_screen*);

// Updates the stats fields.
void update_stats(rov_screen*);

// Prints the static UI elements.
void print_staticui(rov_screen*);

// Writes a line to the screen with the default attributes.
void screen_print(rov_screen*,const char*);

// Writes a formatted line to the screen with the default attributes.
void screen_printf(rov_screen*,const char*,...);

// Writes a string to the screen with a set of attributes.
void screen_printattr(rov_screen*,int,const char*);

// Writes a formatted string to the screen with a set of attributes.
void screen_printfattr(rov_screen*,int,const char*,...);

// Handles the resizing of the terminal.
void handle_resize(rov_screen*);

#endif
