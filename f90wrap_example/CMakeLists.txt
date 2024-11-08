cmake_minimum_required (VERSION 3.12)
project (f90wrap_example Fortran C)

set(SOURCES
    src/field_base.f90
    src/field_impl1.f90
    src/field.f90
)

add_library (f90wrap_example SHARED ${SOURCES})
