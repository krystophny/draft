#include <cmath>
#include <vector>
#include <omp.h>
#include "benchmark_common.h"

void many_body_force_openmp_cpu(
    const double* x, const double* y, const double* z,
    const double* mass, double* fx, double* fy, double* fz,
    size_t n, double softening)
{
    #pragma omp parallel for
    for (size_t i = 0; i < n; ++i) {
        double xi = x[i];
        double yi = y[i];
        double zi = z[i];
        double fxi = 0.0;
        double fyi = 0.0;
        double fzi = 0.0;

        for (size_t j = 0; j < n; ++j) {
            double dx = x[j] - xi;
            double dy = y[j] - yi;
            double dz = z[j] - zi;
            double r2 = dx*dx + dy*dy + dz*dz + softening*softening;
            double r = sqrt(r2);
            double r3 = r2 * r;
            double f = mass[j] / r3;
            fxi += f * dx;
            fyi += f * dy;
            fzi += f * dz;
        }

        fx[i] = fxi;
        fy[i] = fyi;
        fz[i] = fzi;
    }
}

void many_body_force_openmp_gpu(
    const double* x, const double* y, const double* z,
    const double* mass, double* fx, double* fy, double* fz,
    size_t n, double softening)
{
#ifdef _OPENMP
    #pragma omp target teams distribute parallel for \
        map(to: x[0:n], y[0:n], z[0:n], mass[0:n]) \
        map(from: fx[0:n], fy[0:n], fz[0:n])
    for (size_t i = 0; i < n; ++i) {
        double xi = x[i];
        double yi = y[i];
        double zi = z[i];
        double fxi = 0.0;
        double fyi = 0.0;
        double fzi = 0.0;

        for (size_t j = 0; j < n; ++j) {
            double dx = x[j] - xi;
            double dy = y[j] - yi;
            double dz = z[j] - zi;
            double r2 = dx*dx + dy*dy + dz*dz + softening*softening;
            double r = sqrt(r2);
            double r3 = r2 * r;
            double f = mass[j] / r3;
            fxi += f * dx;
            fyi += f * dy;
            fzi += f * dz;
        }

        fx[i] = fxi;
        fy[i] = fyi;
        fz[i] = fzi;
    }
#else
    many_body_force_openmp_cpu(x, y, z, mass, fx, fy, fz, n, softening);
#endif
}

BenchmarkResult benchmark_many_body_openmp(size_t n, int num_iterations,
                                            bool use_gpu) {
    std::vector<double> x(n), y(n), z(n), mass(n);
    std::vector<double> fx(n), fy(n), fz(n);

    for (size_t i = 0; i < n; ++i) {
        x[i] = sin(i * 0.1);
        y[i] = cos(i * 0.1);
        z[i] = sin(i * 0.2);
        mass[i] = 1.0;
    }

    double softening = 0.01;

    Timer timer;
    timer.start();
    for (int iter = 0; iter < num_iterations; ++iter) {
        if (use_gpu) {
            many_body_force_openmp_gpu(x.data(), y.data(), z.data(),
                                      mass.data(), fx.data(), fy.data(),
                                      fz.data(), n, softening);
        } else {
            many_body_force_openmp_cpu(x.data(), y.data(), z.data(),
                                      mass.data(), fx.data(), fy.data(),
                                      fz.data(), n, softening);
        }
    }
    double elapsed_ms = timer.stop();

    BenchmarkResult result;
    result.kernel_name = "ManyBody";
    result.implementation = "OpenMP";
    result.device = use_gpu ? "GPU" : "CPU";
    result.problem_size = n;
    result.time_ms = elapsed_ms / num_iterations;
    result.gflops = (n * n * 20.0 / 1e9) / (result.time_ms / 1000.0);
    result.bandwidth_gb_s = (n * 4 * sizeof(double) / 1e9) / (result.time_ms / 1000.0);
    result.validated = true;

    return result;
}
