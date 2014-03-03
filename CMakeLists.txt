# CMakeLists.txt --- CMake file for mate-rov-src

cmake_minimum_required(VERSION 2.8)
project(MATEROV)
set(MATEROV_VERSION_MAJOR 0)
set(MATEROV_VERSION_MINOR 1)

set(CMAKE_C_FLAGS
"-std=gnu11 -Wall -lpthread -lncurses -g")

add_subdirectory(src)
