#ifndef BENCHMARK_COMMON_H
#define BENCHMARK_COMMON_H

#include <chrono>
#include <string>
#include <vector>
#include <cmath>
#include <iostream>
#include <iomanip>

struct BenchmarkResult {
    std::string kernel_name;
    std::string implementation;
    std::string device;
    size_t problem_size;
    double time_ms;
    double gflops;
    double bandwidth_gb_s;
    bool validated;
};

class Timer {
public:
    void start() {
        start_time = std::chrono::high_resolution_clock::now();
    }

    double stop() {
        auto end_time = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> elapsed = end_time - start_time;
        return elapsed.count();
    }

private:
    std::chrono::time_point<std::chrono::high_resolution_clock> start_time;
};

inline double relative_error(const double* a, const double* b, size_t n) {
    double norm_diff = 0.0;
    double norm_ref = 0.0;
    for (size_t i = 0; i < n; ++i) {
        double diff = a[i] - b[i];
        norm_diff += diff * diff;
        norm_ref += b[i] * b[i];
    }
    return std::sqrt(norm_diff / (norm_ref + 1e-30));
}

inline bool validate_result(const double* result, const double* reference,
                           size_t n, double tolerance = 1e-6) {
    double err = relative_error(result, reference, n);
    return err < tolerance;
}

inline void print_result(const BenchmarkResult& result) {
    std::cout << std::setw(25) << std::left << result.kernel_name
              << std::setw(15) << result.implementation
              << std::setw(10) << result.device
              << std::setw(12) << result.problem_size
              << std::setw(12) << std::fixed << std::setprecision(3) << result.time_ms
              << std::setw(12) << std::fixed << std::setprecision(2) << result.gflops
              << std::setw(12) << std::fixed << std::setprecision(2) << result.bandwidth_gb_s
              << std::setw(10) << (result.validated ? "PASS" : "FAIL")
              << std::endl;
}

#endif
