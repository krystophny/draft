add_library(cuda_kernels
    particle_push_cuda.cu
    fdm_laplacian_cuda.cu
    many_body_cuda.cu
)

target_include_directories(cuda_kernels PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

set_target_properties(cuda_kernels PROPERTIES
    CUDA_SEPARABLE_COMPILATION ON
    CUDA_ARCHITECTURES "80;86"
)
