#  Makefile --- Makefile for the libraries used in the rov project.
#  Copyright (c) Joe Jevnik

#  This program is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the Free
#  Software Foundation; either version 2 of the License, or (at your option)
#  any later version.

#  This program is distributed in the hope that it will be useful, but WITHOUT
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
#  more details.

#  You should have received a copy of the GNU General Public License along with
#  this program; if not, write to the Free Software Foundation, Inc., 51
#  Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

CC=gcc
CFLAGS=-Wall
SOURCES=joystick/joystick.c joystick/joystick.o screen/screen.c screen/screen.o

all: librov_joystick.a librov_screen.a

joystick.o: joystick/joystick.c
	$(CC) -c joystick/joystick.c -o joystick.o $(CFLAGS)

librov_joystick.a: joystick.o
	ar rcs librov_joystick.a joystick.o

screen.o: screen/screen.c
	$(CC) -c screen/screen.c -o screen.o $(CFLAGS)

librov_screen.a: screen.o
	ar rcs librov_screen.a screen.o

clean:
	rm *.o
	rm *.a
