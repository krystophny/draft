cmake_minimum_required(VERSION 3.14)
project(CGAL_STL_Intersection_Example LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
  CGAL
  URL https://github.com/CGAL/cgal/releases/download/v6.0.1/CGAL-6.0.1-library.tar.xz
  DOWNLOAD_EXTRACT_TIMESTAMP
  OVERRIDE_FIND_PACKAGE
)
Set(FETCHCONTENT_QUIET FALSE)
FetchContent_Populate(CGAL)

set(CGAL_SOURCE_DIR "${CMAKE_BINARY_DIR}_deps/cgal-src")

find_package(CGAL REQUIRED COMPONENTS Core PATHS ${CGAL_SOURCE_DIR})
set(CGAL_DO_NOT_WARN_ABOUT_CMAKE_BUILD_TYPE TRUE)

add_executable(stl_intersection example_cgal.cpp)
target_link_libraries(stl_intersection PRIVATE CGAL::CGAL_Core)
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  target_compile_options(stl_intersection PRIVATE -Wno-psabi)
endif()
