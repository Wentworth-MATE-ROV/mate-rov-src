// Joe Jevnik
// 25.10.2013
// Implementation of Screen.

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <sched.h>
#include <unistd.h>

#include "scr.h"

void init_screen(rov_screen *scr,rov_arduino *a,FILE *logf){
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
    scr->arduino = a;
    scr->logf    = logf;
    scr->logc    = mr - 2;
    scr->logv    = malloc(logc * sizeof(rov_logmsg));
    scr->logq    = malloc(sizeof(rov_logqueue));
    pthread_mutex_init(&scr->mutex,NULL);
    for (n = 0;n < logc;n++){
        logv[n] = strdup("");
    }
}

void init_logmsg(rov_logmsg *msg,char *txt,int attr){
    msg->txt  = txt;
    msg->attr = attr;
}

void destroy_screen(rov_screen *scr){
    int n;
    for (n = 0;n < scr->logc;n++){
        destroy_logmsg(&logv[n]);
        free(logv[n]);
    }
    fclose(scr->logf);
    echo();
    endwin();
}

void destroy_logmsg(rov_logmsg *msg){
    free(msg->txt);
}

// Refreshes the screen.
void refresh_screen(rov_screen *scr){
    wrefresh(scr->statw);
    wrefresh(scr->ctlw);
    wrefresh(scr->logw);
    refresh();
}

// Updates the stats panel.
void update_stats(rov_screen *scr){
    // TODO
    refresh_screen(scr);
}

// Writes a string to the log with the default attributes.
void writeln(rov_screen *scr,const char *str){
    writeln_attr(src,str,DEFAULT_PAIR);
}

// Writes a string to the console with a given attribute.
void writeln_attr(rov_screen *scr,const char *str,int attr){
    time_t t;
    struct tm *ti;
    char *buffer;
    destroy_logmsg(scr->logv[scr->logc - 1]);
    free(scr->logv[scr->logc - 1]);
    for (int n = scr->logc - 1;n >= 0;n--){
	scr->logv[n] = scr->logv[n - 1];
    }
    time(&t);
    ti = localtime(&t);
    buffer = malloc(4 * scr->mc / 5 * sizeof(char));
    strftime(buffer,4 * scr->mc / 5,"[%H:%M:%S]:",ti);
    strcat(buffer,str);
    init_logmsg(&scr->logv[0],buffer,attr);
    fputs(scr->logv[0].txt,scr->logf);
    for (int n = 0;n < scr->logc;n++){
	wmove(scr->logw,scr->logc - (n + 1),0);
	wclrtoeol(scr->logw);
	wattron(scr->logw,scr->logv[n].attr);
	mvwprintw(scr->logw,scr->logc - (n + 1),0,"%s",scr->logv[n].txt);
	wattroff(scr->logw,scr->logv[n].attr);
    }
    refresh_screen(scr);
}

// Prints the basic UI features, they will be populated by the poll thread.
void print_staticui(rov_screen *scr){
    attron(YELLOW_PAIR | A_BOLD);
    mvprintw(0,scr->mc - 11,"NXT GROUP 9");
    mvprintw(3,0,"Battery:");
    mvprintw(5,0,"Motors:");
    attroff(A_BOLD);
    mvprintw(6,2,"Motor_1:");
    mvprintw(7,2,"Motor_2:");
    mvprintw(8,2,"Motor_3:");
    attron(A_BOLD);
    mvprintw(10,0,"Sensors:");
    attroff(A_BOLD);
    mvprintw(11,2,"Optical:");
    mvprintw(12,2,"Push_1:");
    mvprintw(13,2,"Push_2:");
    mvprintw(14,2,"RGB:");
    attron(A_BOLD);
    int c = scr->mc / 5;
    for (int n = 2;n < scr->mr;n++){
	mvprintw(n,c,"|");
    }
    for (int n = 0;n < scr->mc;n++){
	mvprintw(1,n,"_");
    }
    attron(A_UNDERLINE | A_BOLD);
    mvprintw(1,0,"ROV Status:");
    mvprintw(1,c + 1,"Log:");
    attroff(A_UNDERLINE | A_BOLD);
    attroff(YELLOW_PAIR);
}

// Resizes the screen to fit a newly resized window.
void handle_resize(rov_screen *scr){
    // TODO
}
