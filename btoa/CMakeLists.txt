cmake_minimum_required(VERSION 3.25)
project(MyFortranProject LANGUAGES Fortran)

set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)
set(CMAKE_Fortran_EXTENSIONS OFF)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_subdirectory(src)
add_subdirectory(test)
