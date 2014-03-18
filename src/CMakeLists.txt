# CMakeLists.txt --- CMake file for mate-rov-src

set(MATEROV_FILES comm/comm.c
                  comm/msgqueue.c
                  controls/controls.c
                  controls/keybinds.c
                  motor/motor.c
                  ui/screen.c
                  ui/keyboard.c
                  ui/main.c)

set(EXECUTABLE_OUTPUT_PATH ${MATEROV_BINARY_DIR})

add_executable(rov ${MATEROV_FILES})
