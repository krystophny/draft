add_executable(run_all_benchmarks benchmark_runner.cpp)

set_source_files_properties(benchmark_runner.cpp PROPERTIES LANGUAGE CXX)

set_target_properties(run_all_benchmarks PROPERTIES
    CUDA_SEPARABLE_COMPILATION OFF
    CUDA_RESOLVE_DEVICE_SYMBOLS OFF
    LINKER_LANGUAGE CXX
    CUDA_ARCHITECTURES OFF
)

target_include_directories(run_all_benchmarks PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
)

target_link_libraries(run_all_benchmarks PRIVATE
    cuda_kernels
    cuda_fortran_kernels
    openmp_kernels
    openmp_fortran_kernels
)

if(USE_KOKKOS)
    target_link_libraries(run_all_benchmarks PRIVATE kokkos_kernels)
endif()

target_link_libraries(run_all_benchmarks PRIVATE
    Kokkos::kokkos
)

if(CMAKE_CXX_COMPILER_ID MATCHES "NVHPC")
    target_link_options(run_all_benchmarks PRIVATE "-acclibs" "-cuda" ${OPENMP_OFFLOAD_FLAGS})
endif()
