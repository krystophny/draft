if(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC|PGI")
    if(DEFINED ENV{CUDA_HOME})
        set(CUDA_PATH $ENV{CUDA_HOME})
    elseif(DEFINED CUDAToolkit_ROOT)
        set(CUDA_PATH ${CUDAToolkit_ROOT})
    else()
        set(CUDA_PATH "/opt/cuda")
    endif()

    add_library(cuda_fortran_kernels
        particle_push_cuda_fortran.cuf
        fdm_laplacian_cuda_fortran.cuf
        many_body_cuda_fortran.cuf
    )

    target_compile_options(cuda_fortran_kernels PRIVATE
        -cuda -gpu=cc80,cc86 -L${CUDA_PATH}/lib64
    )

    target_link_options(cuda_fortran_kernels PRIVATE
        -cuda -gpu=cc80,cc86 -L${CUDA_PATH}/lib64
    )

    target_link_libraries(cuda_fortran_kernels PUBLIC CUDA::cudart)
    message(STATUS "CUDA Fortran enabled using CUDA at ${CUDA_PATH}")
else()
    message(STATUS "CUDA Fortran requires NVHPC compiler, skipping...")
endif()
