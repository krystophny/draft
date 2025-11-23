cmake_minimum_required(VERSION 3.18)
project(GPUBenchmarks LANGUAGES CXX C Fortran CUDA)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CUDA_STANDARD 17)
set(CMAKE_CUDA_STANDARD_REQUIRED ON)

# Find required packages
find_package(CUDAToolkit REQUIRED)
find_package(OpenMP REQUIRED)

# Kokkos
option(USE_KOKKOS "Enable Kokkos benchmarks" ON)
if(USE_KOKKOS)
    find_package(Kokkos QUIET)
    if(NOT Kokkos_FOUND)
        message(STATUS "Kokkos not found, will use FetchContent")
        include(FetchContent)
        FetchContent_Declare(
            Kokkos
            GIT_REPOSITORY https://github.com/kokkos/kokkos.git
            GIT_TAG master
        )
        set(Kokkos_ENABLE_CUDA ON CACHE BOOL "")
        set(Kokkos_ENABLE_OPENMP ON CACHE BOOL "")
        set(Kokkos_ARCH_AMPERE80 ON CACHE BOOL "")
        FetchContent_MakeAvailable(Kokkos)
    endif()
endif()

# Compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -march=native -Wall")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -march=native -Wall")
set(CMAKE_CUDA_FLAGS "${CMAKE_CUDA_FLAGS} -O3 --use_fast_math")

# OpenMP offload flags
# Note: GCC on this system is not configured with NVIDIA offload support
# GPU offloading with OpenMP requires either NVHPC or GCC with offload enabled
if(CMAKE_CXX_COMPILER_ID MATCHES "NVHPC|PGI")
    set(OPENMP_OFFLOAD_FLAGS "-mp=gpu" "-gpu=cc80,cc86")
    set(OPENMP_GPU_ENABLED TRUE)
    message(STATUS "OpenMP GPU offload enabled with NVHPC")
else()
    set(OPENMP_OFFLOAD_FLAGS "")
    set(OPENMP_GPU_ENABLED FALSE)
    message(STATUS "OpenMP GPU offload not available with current compiler")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC|PGI")
    set(OPENMP_FORTRAN_OFFLOAD_FLAGS "-mp=gpu" "-gpu=cc80,cc86" "-cuda")
else()
    set(OPENMP_FORTRAN_OFFLOAD_FLAGS "")
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Benchmark executables
add_subdirectory(src/cuda)
add_subdirectory(src/cuda_fortran)
add_subdirectory(src/openmp)
if(USE_KOKKOS)
    add_subdirectory(src/kokkos)
endif()
add_subdirectory(benchmarks)

# Enable testing
enable_testing()
