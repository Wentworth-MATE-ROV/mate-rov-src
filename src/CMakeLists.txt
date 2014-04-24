# CMakeLists.txt --- CMake file for mate-rov-src

set(MATEROV_FILES comm/comm.c
                  comm/msgqueue.c
                  controls/keybinds.c
                  controls/controls.c
                  controls/pinlayout.c
                  ui/keyboard.c
                  ui/main.c)

set(EXECUTABLE_OUTPUT_PATH ${MATEROV_BINARY_DIR})

add_executable(rov ${MATEROV_FILES})
target_link_libraries(rov ${MATEROV_BINARY_DIR}/../lib/librov_joystick.a ${MATEROV_BINARY_DIR}/../lib/librov_screen.a)
