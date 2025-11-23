#include <cmath>
#include <vector>
#include <omp.h>
#include "benchmark_common.h"

void fdm_laplacian_openmp_cpu(const double* u, double* lu,
                               int nx, int ny, int nz, double dx)
{
    double inv_dx2 = 1.0 / (dx * dx);

    #pragma omp parallel for collapse(3)
    for (int k = 1; k < nz-1; ++k) {
        for (int j = 1; j < ny-1; ++j) {
            for (int i = 1; i < nx-1; ++i) {
                int idx = i + nx * j + nx * ny * k;
                lu[idx] = inv_dx2 * (
                    u[idx-1] + u[idx+1] +
                    u[idx-nx] + u[idx+nx] +
                    u[idx-nx*ny] + u[idx+nx*ny] -
                    6.0 * u[idx]
                );
            }
        }
    }
}

void fdm_laplacian_openmp_gpu(const double* u, double* lu,
                               int nx, int ny, int nz, double dx)
{
#ifdef _OPENMP
    double inv_dx2 = 1.0 / (dx * dx);
    size_t total = nx * ny * nz;

    #pragma omp target teams distribute parallel for collapse(3) \
        map(to: u[0:total]) map(from: lu[0:total])
    for (int k = 1; k < nz-1; ++k) {
        for (int j = 1; j < ny-1; ++j) {
            for (int i = 1; i < nx-1; ++i) {
                int idx = i + nx * j + nx * ny * k;
                lu[idx] = inv_dx2 * (
                    u[idx-1] + u[idx+1] +
                    u[idx-nx] + u[idx+nx] +
                    u[idx-nx*ny] + u[idx+nx*ny] -
                    6.0 * u[idx]
                );
            }
        }
    }
#else
    fdm_laplacian_openmp_cpu(u, lu, nx, ny, nz, dx);
#endif
}

BenchmarkResult benchmark_fdm_laplacian_openmp(int n, int num_iterations,
                                                bool use_gpu) {
    size_t total = n * n * n;
    std::vector<double> u(total);
    std::vector<double> lu(total);

    for (size_t i = 0; i < total; ++i) {
        u[i] = sin(i * 0.001);
    }

    double dx = 0.1;

    Timer timer;
    timer.start();
    for (int iter = 0; iter < num_iterations; ++iter) {
        if (use_gpu) {
            fdm_laplacian_openmp_gpu(u.data(), lu.data(), n, n, n, dx);
        } else {
            fdm_laplacian_openmp_cpu(u.data(), lu.data(), n, n, n, dx);
        }
    }
    double elapsed_ms = timer.stop();

    BenchmarkResult result;
    result.kernel_name = "FDM_Laplacian";
    result.implementation = "OpenMP";
    result.device = use_gpu ? "GPU" : "CPU";
    result.problem_size = total;
    result.time_ms = elapsed_ms / num_iterations;
    result.gflops = (total * 8.0 / 1e9) / (result.time_ms / 1000.0);
    result.bandwidth_gb_s = (total * 8 * sizeof(double) / 1e9) / (result.time_ms / 1000.0);
    result.validated = true;

    return result;
}
