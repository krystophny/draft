if(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC")
    # NVHPC 25.9 does not bundle CUDA 13.0, skip CUDA Fortran for now
    message(STATUS "CUDA Fortran skipped: NVHPC 25.9 requires CUDA toolkit to be installed separately")
    message(STATUS "Install matching CUDA toolkit or use different NVHPC version")
else()
    message(STATUS "CUDA Fortran requires NVHPC compiler, skipping...")
endif()
