mate-rov-src
============

Source code for The Anna Maria.
Licensed under the GPLv2.


Dependencies
------------

Format: pkg (version tested)

Libraries:
- glibc (2.19)
- guile (2.0.11)
- ncurses (5.9)
- pthread (2.19)

Tools:
- cmake (2.8.12.2)
- make (4.0)

Building
--------

Simply execute these commands:

    mkdir build
    cd lib
    make
    mv *.a ../build
    cd ../build
    cmake ..
    make

This should have fully built the executable `rov`.


Using
-----

![](https://raw.githubusercontent.com/Wentworth-MATE-ROV/mate-rov-src/master/ss.png)


On the left is the realtime statistics about the robot, while on the right is
the console log.

The console will be populated with various messages triggered from events on
both the robot or the computer. For example, when one reloads the keybinds, or
when a message fails to send. The console logs in multiple colors, ussually
depending on the type of message.

The realtime statistics panel is populated with the values being sent to each
motor, along with the state of the various toggle devices. The `miswrites` field
shows the value of write failueres / successful writes. This number is important
to watch. Remember, there are all kinds of things that can cause the write()
call to fail.
