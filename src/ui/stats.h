/* stats.h --- Manages the printing of the robot's stats.
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

#ifndef ROV_STATS_H
#define ROV_STATS_H

// The length of the stat value array.
#define STATC           10

// Stat value indecies.
#define LEFTMOTOR_STAT  0
#define RIGHTMOTOR_STAT 1
#define FRONTMOTOR_STAT 2
#define BACKMOTOR_STAT  3
#define HEADLIGHTS_STAT 4
#define SIDELIGHTS_STAT 5
#define LASERS_STAT     6
#define CLAW_STAT       7
#define MISWRITES_STAT  9

// Stat name values.
extern char *statv[STATC];

#include "../common.h"
#include "../librov/screen.h"

// Prints all the stats.
void update_stats(rov_screen*,rov_arduino*);

// Updates the stats that have changed.
void diff_update_stats(rov_screen*,rov_arduino*,rov_ctrlstate*);

#endif
