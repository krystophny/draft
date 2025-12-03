#include <iostream>
#include <omp.h>
#include <Kokkos_Core.hpp>

int main(int argc, char* argv[]) {
    // Test OpenMP threads
    int num_threads_openmp = 0;
    #pragma omp parallel
    {
        #pragma omp single
        num_threads_openmp = omp_get_num_threads();
    }
    std::cout << "OpenMP threads: " << num_threads_openmp << std::endl;

    // Test Kokkos threads
    Kokkos::initialize(argc, argv);
    {
        int num_threads_kokkos = Kokkos::OpenMP::concurrency();
        std::cout << "Kokkos OpenMP threads: " << num_threads_kokkos << std::endl;
    }
    Kokkos::finalize();

    return 0;
}
