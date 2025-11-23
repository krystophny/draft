add_library(openmp_kernels
    particle_push_openmp.cpp
    fdm_laplacian_openmp.cpp
    many_body_openmp.cpp
)

target_include_directories(openmp_kernels PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(openmp_kernels PUBLIC OpenMP::OpenMP_CXX)

if(OPENMP_GPU_ENABLED)
    if(CMAKE_CXX_COMPILER_ID MATCHES "NVHPC")
        target_compile_options(openmp_kernels PRIVATE ${OPENMP_OFFLOAD_FLAGS})
        target_link_options(openmp_kernels PRIVATE ${OPENMP_OFFLOAD_FLAGS})
    endif()
endif()

# OpenMP Fortran disabled - NVHPC requires localrc for external CUDA
message(STATUS "OpenMP Fortran disabled (NVHPC requires localrc for external CUDA)")
