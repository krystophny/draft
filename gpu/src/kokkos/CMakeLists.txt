add_library(kokkos_kernels
    particle_push_kokkos.cpp
    fdm_laplacian_kokkos.cpp
    many_body_kokkos.cpp
)

set_target_properties(kokkos_kernels PROPERTIES
    CUDA_SEPARABLE_COMPILATION OFF
    CUDA_RESOLVE_DEVICE_SYMBOLS OFF
)

target_include_directories(kokkos_kernels PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(kokkos_kernels PUBLIC Kokkos::kokkos)
