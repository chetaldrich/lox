cmake_minimum_required(VERSION 3.10)
project(clox)

# Collect all source files
file(GLOB SRC_FILES *.c)

SET(CMAKE_BUILD_TYPE Debug)

# Add executable target
add_executable(clox ${SRC_FILES})

