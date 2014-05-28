# CMakeLists.txt --- CMake file for mate-rov-src

set(MATEROV_FILES
    comm/comm.c
    comm/msgqueue.c
    controls/keybinds.c
    controls/controls.c
    controls/pinlayout.c
    ui/keyboard.c
    ui/stats.c
    ui/main.c)

set(ROOT
    ${MATEROV_BINARY_DIR}/..)

SET(SRC
    ${ROOT}/src)


set(EXECUTABLE_OUTPUT_PATH ${MATEROV_BINARY_DIR})

add_custom_command(OUTPUT librov_joystick.a
                   COMMAND make
                   COMMAND mv librov_joystick.a ${MATEROV_BINARY_DIR}
                   WORKING_DIRECTORY ${ROOT}/lib)

add_custom_command(OUTPUT librov_screen.a
                   COMMAND mv librov_screen.a ${MATEROV_BINARY_DIR}
                   WORKING_DIRECTORY ${ROOT}/lib)

add_custom_command(
  PRE_BUILD
  OUTPUT controls/init-logic.scm.ch
  COMMAND fch -l c -m ROV_INIT_LOGIC_H ${SRC}/controls/init-logic.scm)

add_custom_command(
  PRE_BUILD
  OUTPUT controls/keybinds-parser.scm.ch
  COMMAND fch -l c -m ROV_KEYBINDS_PARSER_H ${SRC}/controls/keybinds-parser.scm)

include_directories(/usr/include/guile/2.0)

add_custom_target(
  gen_fch
  DEPENDS controls/init-logic.scm.ch controls/keybinds-parser.scm.ch)

add_executable(rov
               ${MATEROV_FILES})

add_dependencies(rov
                 gen_fch)

set_target_properties(rov PROPERTIES LINKER_LANGUAGE C)

target_link_libraries(rov
                      ${MATEROV_BINARY_DIR}/librov_joystick.a
                      ${MATEROV_BINARY_DIR}/librov_screen.a
                      pthread
                      ncurses
                      guile-2.0
                      gc)
