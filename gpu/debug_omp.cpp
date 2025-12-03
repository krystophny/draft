#include <iostream>
#include <vector>
#include <chrono>
#include <omp.h>

void test_openmp(double* x, double dt, size_t n) {
    int num_threads = 0;
    #pragma omp parallel
    {
        #pragma omp single
        num_threads = omp_get_num_threads();
    }
    std::cout << "OpenMP using " << num_threads << " threads" << std::endl;

    #pragma omp parallel for
    for (size_t i = 0; i < n; ++i) {
        x[i] += dt * i;
    }
}

int main() {
    size_t n = 1000000;
    std::vector<double> x(n, 0.0);
    double dt = 0.01;

    auto start = std::chrono::high_resolution_clock::now();
    for (int iter = 0; iter < 10; ++iter) {
        test_openmp(x.data(), dt, n);
    }
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> elapsed = end - start;

    std::cout << "Time: " << elapsed.count() / 10.0 << " ms" << std::endl;

    return 0;
}
