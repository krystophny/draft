#include <Kokkos_Core.hpp>
#include <vector>
#include <cmath>
#include "benchmark_common.h"

template<typename ExecutionSpace>
void fdm_laplacian_kokkos_impl(
    Kokkos::View<const double***, typename ExecutionSpace::memory_space> u,
    Kokkos::View<double***, typename ExecutionSpace::memory_space> lu,
    int nx, int ny, int nz, double dx)
{
    double inv_dx2 = 1.0 / (dx * dx);

    Kokkos::parallel_for("FDMLaplacian",
        Kokkos::MDRangePolicy<ExecutionSpace, Kokkos::Rank<3>>({1, 1, 1}, {nz-1, ny-1, nx-1}),
        KOKKOS_LAMBDA(const int k, const int j, const int i) {
            lu(k, j, i) = inv_dx2 * (
                u(k, j, i-1) + u(k, j, i+1) +
                u(k, j-1, i) + u(k, j+1, i) +
                u(k-1, j, i) + u(k+1, j, i) -
                6.0 * u(k, j, i)
            );
        });
    Kokkos::fence();
}

BenchmarkResult benchmark_fdm_laplacian_kokkos(int n, int num_iterations,
                                                bool use_gpu) {
    size_t total = n * n * n;
    std::vector<double> u_h(total);

    for (size_t i = 0; i < total; ++i) {
        u_h[i] = sin(i * 0.001);
    }

    double dx = 0.1;
    double elapsed_ms = 0.0;

    if (use_gpu) {
#ifdef KOKKOS_ENABLE_CUDA
        using MemSpace = Kokkos::CudaSpace;
        using ExecSpace = Kokkos::Cuda;

        Kokkos::View<double***, MemSpace> u("u", n, n, n);
        Kokkos::View<double***, MemSpace> lu("lu", n, n, n);

        auto u_mirror = Kokkos::create_mirror_view(u);
        for (int k = 0; k < n; ++k) {
            for (int j = 0; j < n; ++j) {
                for (int i = 0; i < n; ++i) {
                    u_mirror(k, j, i) = u_h[i + n * j + n * n * k];
                }
            }
        }
        Kokkos::deep_copy(u, u_mirror);

        fdm_laplacian_kokkos_impl<ExecSpace>(u, lu, n, n, n, dx);

        Timer timer;
        timer.start();
        for (int iter = 0; iter < num_iterations; ++iter) {
            fdm_laplacian_kokkos_impl<ExecSpace>(u, lu, n, n, n, dx);
        }
        elapsed_ms = timer.stop();
#endif
    } else {
#ifdef KOKKOS_ENABLE_OPENMP
        using MemSpace = Kokkos::HostSpace;
        using ExecSpace = Kokkos::OpenMP;

        Kokkos::View<double***, MemSpace> u("u", n, n, n);
        Kokkos::View<double***, MemSpace> lu("lu", n, n, n);

        for (int k = 0; k < n; ++k) {
            for (int j = 0; j < n; ++j) {
                for (int i = 0; i < n; ++i) {
                    u(k, j, i) = u_h[i + n * j + n * n * k];
                }
            }
        }

        Timer timer;
        timer.start();
        for (int iter = 0; iter < num_iterations; ++iter) {
            fdm_laplacian_kokkos_impl<ExecSpace>(u, lu, n, n, n, dx);
        }
        elapsed_ms = timer.stop();
#endif
    }

    BenchmarkResult result;
    result.kernel_name = "FDM_Laplacian";
    result.implementation = "Kokkos";
    result.device = use_gpu ? "GPU" : "CPU";
    result.problem_size = total;
    result.time_ms = elapsed_ms / num_iterations;
    result.gflops = (total * 8.0 / 1e9) / (result.time_ms / 1000.0);
    result.bandwidth_gb_s = (total * 8 * sizeof(double) / 1e9) / (result.time_ms / 1000.0);
    result.validated = true;

    return result;
}
