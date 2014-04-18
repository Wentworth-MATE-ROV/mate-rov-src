/* screen.c --- Implementation for librov_screen.
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

#include "screen.h"

// Initializes a screen with a given arduino to pull from and a logfile.
void init_screen(rov_screen *scr,FILE *logf,char **statv,size_t statc){
    int n;
    initscr();
    getmaxyx(stdscr,scr->mr,scr->mc);
    start_color();
    use_default_colors();
    init_pair(1,COLOR_GREEN,-1);
    init_pair(2,COLOR_RED,-1);
    init_pair(3,COLOR_YELLOW,-1);
    init_pair(4,COLOR_MAGENTA,-1);
    cbreak();
    keypad(stdscr,TRUE);
    curs_set(0);
    noecho();
    scr->logf    = logf;
    scr->logc    = scr->mr - 2;
    scr->logv    = malloc(scr->logc * sizeof(rov_logmsg));
    scr->lmc     = 4 * scr->mc / 5;
    scr->statw   = newwin(12,6,3,10);
    scr->statc   = statc;
    scr->statv   = malloc(statc * sizeof(char*));
    for (n = 0;n < statc;n++){
        scr->statv[n] = strdup(statv[n]);
    }
    scr->ctlw    = newwin(scr->mr - 8,scr->mc / 5,scr->mr - 8,0);
    scr->logw    = newwin(scr->logc,scr->lmc,2,scr->mc / 5 + 1);
    pthread_mutex_init(&scr->mutex,NULL);
    for (n = 0;n < scr->logc;n++){
        memset(scr->logv[n].txt,0,81);
        scr->logv[n].attr = DEFAULT_PAIR;
    }
}

// Initializes a log message with the text and attribute sections.
void init_logmsg(rov_logmsg *msg,char *txt,int attr){
    memcpy(msg->txt,txt,81);
    msg->attr = attr;
}

// Exits ncurses mode and clears the message queue. Also closes the logfile.
void destroy_screen(rov_screen *scr){
    pthread_mutex_destroy(&scr->mutex);
    free(scr->logv);
    fclose(scr->logf);
    echo();
    endwin();
}

// Refreshes the screen.
void refresh_screen(rov_screen *scr){
    pthread_mutex_lock(&scr->mutex);
    wrefresh(scr->statw);
    wrefresh(scr->ctlw);
    wrefresh(scr->logw);
    refresh();
    pthread_mutex_unlock(&scr->mutex);
}

// Updates the stats panel.
void update_stat(rov_screen *scr,size_t n,const char *fmt,...){
    va_list ap;
    va_start(ap,fmt);
    pthread_mutex_lock(&scr->mutex);
    wmove(scr->statw,n,0);
    vwprintw(scr->statw,fmt,ap);
    pthread_mutex_unlock(&scr->mutex);
    va_end(ap);
    refresh_screen(scr);
}

// Writes a string to the log with the default attributes.
void screen_print(rov_screen *scr,const char *str){
    screen_printattr(scr,DEFAULT_PAIR,str);
}

void screen_printf(rov_screen *scr,const char *fmt,...){
    va_list ap;
    char buf[81];
    va_start(ap,fmt);
    vsnprintf(buf,81,fmt,ap);
    screen_printattr(scr,DEFAULT_PAIR,buf);
    va_end(ap);
    wrefresh(scr->statw);
}

// Writes a string to the console with a given attribute.
void screen_printattr(rov_screen *scr,int attr,const char *str){
    time_t t;
    struct tm ti;
    char buffer[81];
    int n;
    pthread_mutex_lock(&scr->mutex);
    for (n = scr->logc - 1;n > 0;n--){
        init_logmsg(&scr->logv[n],scr->logv[n - 1].txt,scr->logv[n - 1].attr);
    }
    time(&t);
    localtime_r(&t,&ti);
    memset(buffer,0,81);
    strftime(buffer,80,"[%H:%M:%S]:",&ti);
    strncat(buffer,str,(80 - strlen(buffer)) * sizeof(char));
    init_logmsg(&scr->logv[0],buffer,attr);
    fputs(scr->logv[0].txt,scr->logf);
    wclear(scr->logw);
    for (n = scr->logc;n >= 0;n--){
	wattron(scr->logw,scr->logv[n].attr);
	mvwprintw(scr->logw,scr->logc - n - 1,0,"%.*s",
                  (scr->lmc > 80) ? 80 : scr->lmc,scr->logv[n].txt);
	wattroff(scr->logw,scr->logv[n].attr);
    }

    pthread_mutex_unlock(&scr->mutex);
    wrefresh(scr->logw);
}
// Writes a formatted line to the screen with the default attributes.
void screen_printfattr(rov_screen *scr,int attr,const char *fmt,...){
    va_list ap;
    char buf[81];
    va_start(ap,fmt);
    vsnprintf(buf,81,fmt,ap);
    screen_printattr(scr,attr,buf);
    va_end(ap);
}

// Prints the basic UI features, they will be populated by the poll thread.
void print_staticui(rov_screen *scr){
    int c = scr->mc / 5;
    int n;
    pthread_mutex_lock(&scr->mutex);
    attron(YELLOW_PAIR | A_BOLD);
    mvprintw(0,scr->mc - 7,"Bot Six");
    for (n = 2;n < scr->mr;n++){
	mvprintw(n,c,"|");
    }
    for (n = 0;n < scr->mc;n++){
	mvprintw(1,n,"_");
    }
    attron(A_UNDERLINE);
    mvprintw(1,0,"ROV Status:");
    mvprintw(1,c + 1,"Log:");
    attroff(A_UNDERLINE | A_BOLD);
    attroff(YELLOW_PAIR);
    pthread_mutex_unlock(&scr->mutex);
    refresh_screen(scr);
}

// Resizes the screen to fit a newly resized window.
void handle_resize(rov_screen *scr){
    pthread_mutex_lock(&scr->mutex);
    // TODO
    pthread_mutex_unlock(&scr->mutex);
    refresh_screen(scr);
}
