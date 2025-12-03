set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cuda -gpu=cc80,cc86")

add_library(cuda_fortran_kernels STATIC
    particle_push_cuda_fortran.cuf
    fdm_laplacian_cuda_fortran.cuf
    many_body_cuda_fortran.cuf
)
