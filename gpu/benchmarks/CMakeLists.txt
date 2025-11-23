add_executable(run_all_benchmarks benchmark_runner.cpp)

target_include_directories(run_all_benchmarks PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
)

target_link_libraries(run_all_benchmarks PRIVATE
    cuda_kernels
    openmp_kernels
)

if(USE_KOKKOS)
    target_link_libraries(run_all_benchmarks PRIVATE kokkos_kernels)
endif()

target_link_libraries(run_all_benchmarks PRIVATE
    CUDA::cudart
    Kokkos::kokkos
)
