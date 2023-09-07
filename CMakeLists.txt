# minimum version based on Ubuntu 22.04 on 2022-09-16
cmake_minimum_required(VERSION 3.24)

# set the project name
project(spoon VERSION 0.0 LANGUAGES Fortran)

# executables will be built and placed in bin/ directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

# source code will be in src/ folder
add_subdirectory(src)
