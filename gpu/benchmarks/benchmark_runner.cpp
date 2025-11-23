#include <iostream>
#include <fstream>
#include <vector>
#include <iomanip>
#include <Kokkos_Core.hpp>
#include "benchmark_common.h"

BenchmarkResult benchmark_particle_push_cuda(size_t n, int num_iterations);
BenchmarkResult benchmark_fdm_laplacian_cuda(int n, int num_iterations);
BenchmarkResult benchmark_many_body_cuda(size_t n, int num_iterations);

BenchmarkResult benchmark_particle_push_openmp(size_t n, int num_iterations,
                                                bool use_gpu);
BenchmarkResult benchmark_fdm_laplacian_openmp(int n, int num_iterations,
                                                bool use_gpu);
BenchmarkResult benchmark_many_body_openmp(size_t n, int num_iterations,
                                            bool use_gpu);

BenchmarkResult benchmark_particle_push_kokkos(size_t n, int num_iterations,
                                                bool use_gpu);
BenchmarkResult benchmark_fdm_laplacian_kokkos(int n, int num_iterations,
                                                bool use_gpu);
BenchmarkResult benchmark_many_body_kokkos(size_t n, int num_iterations,
                                            bool use_gpu);

void print_header() {
    std::cout << std::string(120, '=') << std::endl;
    std::cout << "GPU Benchmark Suite for Plasma Physics Kernels" << std::endl;
    std::cout << std::string(120, '=') << std::endl;
    std::cout << std::setw(25) << std::left << "Kernel"
              << std::setw(15) << "Implementation"
              << std::setw(10) << "Device"
              << std::setw(12) << "Size"
              << std::setw(12) << "Time (ms)"
              << std::setw(12) << "GFLOPS"
              << std::setw(12) << "BW (GB/s)"
              << std::setw(10) << "Valid"
              << std::endl;
    std::cout << std::string(120, '-') << std::endl;
}

void save_results_csv(const std::vector<BenchmarkResult>& results,
                     const std::string& filename) {
    std::ofstream file(filename);
    file << "Kernel,Implementation,Device,Size,Time_ms,GFLOPS,Bandwidth_GBs,Validated\n";
    for (const auto& r : results) {
        file << r.kernel_name << ","
             << r.implementation << ","
             << r.device << ","
             << r.problem_size << ","
             << std::fixed << std::setprecision(3) << r.time_ms << ","
             << std::fixed << std::setprecision(2) << r.gflops << ","
             << std::fixed << std::setprecision(2) << r.bandwidth_gb_s << ","
             << (r.validated ? "PASS" : "FAIL") << "\n";
    }
    file.close();
}

int main(int argc, char* argv[]) {
    Kokkos::initialize(argc, argv);
    {
        std::vector<BenchmarkResult> results;

        print_header();

        size_t particle_n = 1000000;
        int fdm_n = 128;
        size_t many_body_n = 4096;
        int num_iterations = 10;

        std::cout << "Running CUDA benchmarks..." << std::endl;
        try {
            auto r1 = benchmark_particle_push_cuda(particle_n, num_iterations);
            print_result(r1);
            results.push_back(r1);

            auto r2 = benchmark_fdm_laplacian_cuda(fdm_n, num_iterations);
            print_result(r2);
            results.push_back(r2);

            auto r3 = benchmark_many_body_cuda(many_body_n, num_iterations);
            print_result(r3);
            results.push_back(r3);
        } catch (const std::exception& e) {
            std::cerr << "CUDA benchmarks failed: " << e.what() << std::endl;
        }

        std::cout << "\nRunning OpenMP CPU benchmarks..." << std::endl;
        try {
            auto r4 = benchmark_particle_push_openmp(particle_n, num_iterations, false);
            print_result(r4);
            results.push_back(r4);

            auto r5 = benchmark_fdm_laplacian_openmp(fdm_n, num_iterations, false);
            print_result(r5);
            results.push_back(r5);

            auto r6 = benchmark_many_body_openmp(many_body_n, num_iterations, false);
            print_result(r6);
            results.push_back(r6);
        } catch (const std::exception& e) {
            std::cerr << "OpenMP CPU benchmarks failed: " << e.what() << std::endl;
        }

        std::cout << "\nRunning OpenMP GPU offload benchmarks..." << std::endl;
        try {
            auto r7 = benchmark_particle_push_openmp(particle_n, num_iterations, true);
            print_result(r7);
            results.push_back(r7);

            auto r8 = benchmark_fdm_laplacian_openmp(fdm_n, num_iterations, true);
            print_result(r8);
            results.push_back(r8);

            auto r9 = benchmark_many_body_openmp(many_body_n, num_iterations, true);
            print_result(r9);
            results.push_back(r9);
        } catch (const std::exception& e) {
            std::cerr << "OpenMP GPU benchmarks failed: " << e.what() << std::endl;
        }

        std::cout << "\nRunning Kokkos CPU benchmarks..." << std::endl;
        try {
            auto r10 = benchmark_particle_push_kokkos(particle_n, num_iterations, false);
            print_result(r10);
            results.push_back(r10);

            auto r11 = benchmark_fdm_laplacian_kokkos(fdm_n, num_iterations, false);
            print_result(r11);
            results.push_back(r11);

            auto r12 = benchmark_many_body_kokkos(many_body_n, num_iterations, false);
            print_result(r12);
            results.push_back(r12);
        } catch (const std::exception& e) {
            std::cerr << "Kokkos CPU benchmarks failed: " << e.what() << std::endl;
        }

        std::cout << "\nRunning Kokkos GPU benchmarks..." << std::endl;
        try {
            auto r13 = benchmark_particle_push_kokkos(particle_n, num_iterations, true);
            print_result(r13);
            results.push_back(r13);

            auto r14 = benchmark_fdm_laplacian_kokkos(fdm_n, num_iterations, true);
            print_result(r14);
            results.push_back(r14);

            auto r15 = benchmark_many_body_kokkos(many_body_n, num_iterations, true);
            print_result(r15);
            results.push_back(r15);
        } catch (const std::exception& e) {
            std::cerr << "Kokkos GPU benchmarks failed: " << e.what() << std::endl;
        }

        std::cout << std::string(120, '=') << std::endl;
        std::cout << "Benchmark complete. Saving results..." << std::endl;

        save_results_csv(results, "../results/benchmark_results.csv");
        std::cout << "Results saved to results/benchmark_results.csv" << std::endl;
    }
    Kokkos::finalize();

    return 0;
}
