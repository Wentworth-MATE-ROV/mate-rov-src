/* screen.h --- Interface for librov_screen.
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

#ifndef ROV_SCREEN_H
#define ROV_SCREEN_H

#define _GNU_SOURCE
#include <ncurses.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <sched.h>
#include <unistd.h>

// The color pairs for printing and attributes.
#define DEFAULT_PAIR 0
#define GREEN_PAIR   COLOR_PAIR(1)
#define RED_PAIR     COLOR_PAIR(2)
#define YELLOW_PAIR  COLOR_PAIR(3)
#define MAGENTA_PAIR COLOR_PAIR(4)

// A message and its attribute.
typedef struct{
    char txt[81]; // The actual message (Up to 80 chars plus null terminator).
    int  attr;    // The message attributes as defined by ncurses or screen.h
} rov_logmsg;

// The screen structure that manages the entire UI.
typedef struct{
    int             mr;      // Max rows
    int             mc;      // Max columns
    int             lmc;     // Log max columns
    WINDOW         *logw;    // The message log window.
    WINDOW         *statw;   // The stat field window.
    size_t          statc;   // The stat field count.
    char          **statv;   // The stat field values.
    WINDOW         *ctlw;    // The control window.
    size_t          logc;    // Log count
    rov_logmsg     *logv;    // Log values
    FILE           *logf;    // Log File
    pthread_t       statt;   // Stat Thread
    pthread_mutex_t mutex;   // The mutex for screen writing
} rov_screen;

// Initializes the screen structure and enters ncurses mode.
void init_screen(rov_screen*,FILE*,char**,size_t);

// Destroys (unitializes) the screen structure and leaves ncurses mode.
void destroy_screen(rov_screen*);

// Initializes a log message.
void init_logmsg(rov_logmsg*,char*,int);

// Refreshes the screen elements.
void refresh_screen(rov_screen*);

// Updates the given stat field.
void update_stat(rov_screen*,size_t,const char*,...);

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
