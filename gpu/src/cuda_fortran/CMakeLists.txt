if(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC")
    add_library(cuda_fortran_kernels
        particle_push_cuda_fortran.cuf
        fdm_laplacian_cuda_fortran.cuf
        many_body_cuda_fortran.cuf
    )

    target_compile_options(cuda_fortran_kernels PRIVATE
        -cuda -gpu=cc80,cc86
    )

    target_link_options(cuda_fortran_kernels PRIVATE
        -cuda -gpu=cc80,cc86
    )
else()
    message(STATUS "CUDA Fortran requires NVHPC compiler, skipping...")
endif()
