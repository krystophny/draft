#include <Kokkos_Core.hpp>
#include <vector>
#include <cmath>
#include "benchmark_common.h"

template<typename ExecutionSpace>
void many_body_force_kokkos_impl(
    Kokkos::View<const double*, typename ExecutionSpace::memory_space> x,
    Kokkos::View<const double*, typename ExecutionSpace::memory_space> y,
    Kokkos::View<const double*, typename ExecutionSpace::memory_space> z,
    Kokkos::View<const double*, typename ExecutionSpace::memory_space> mass,
    Kokkos::View<double*, typename ExecutionSpace::memory_space> fx,
    Kokkos::View<double*, typename ExecutionSpace::memory_space> fy,
    Kokkos::View<double*, typename ExecutionSpace::memory_space> fz,
    size_t n, double softening)
{
    Kokkos::parallel_for("ManyBody",
        Kokkos::RangePolicy<ExecutionSpace>(0, n),
        KOKKOS_LAMBDA(const size_t i) {
        double xi = x(i);
        double yi = y(i);
        double zi = z(i);
        double fxi = 0.0;
        double fyi = 0.0;
        double fzi = 0.0;

        for (size_t j = 0; j < n; ++j) {
            double dx = x(j) - xi;
            double dy = y(j) - yi;
            double dz = z(j) - zi;
            double r2 = dx*dx + dy*dy + dz*dz + softening*softening;
            double r = Kokkos::sqrt(r2);
            double r3 = r2 * r;
            double f = mass(j) / r3;
            fxi += f * dx;
            fyi += f * dy;
            fzi += f * dz;
        }

        fx(i) = fxi;
        fy(i) = fyi;
        fz(i) = fzi;
    });
    Kokkos::fence();
}

BenchmarkResult benchmark_many_body_kokkos(size_t n, int num_iterations,
                                            bool use_gpu) {
    std::vector<double> x_h(n), y_h(n), z_h(n), mass_h(n);

    for (size_t i = 0; i < n; ++i) {
        x_h[i] = sin(i * 0.1);
        y_h[i] = cos(i * 0.1);
        z_h[i] = sin(i * 0.2);
        mass_h[i] = 1.0;
    }

    double softening = 0.01;
    double elapsed_ms = 0.0;

    if (use_gpu) {
#ifdef KOKKOS_ENABLE_CUDA
        using MemSpace = Kokkos::CudaSpace;
        using ExecSpace = Kokkos::Cuda;

        Kokkos::View<double*, MemSpace> x("x", n);
        Kokkos::View<double*, MemSpace> y("y", n);
        Kokkos::View<double*, MemSpace> z("z", n);
        Kokkos::View<double*, MemSpace> mass("mass", n);
        Kokkos::View<double*, MemSpace> fx("fx", n);
        Kokkos::View<double*, MemSpace> fy("fy", n);
        Kokkos::View<double*, MemSpace> fz("fz", n);

        auto x_mirror = Kokkos::create_mirror_view(x);
        auto y_mirror = Kokkos::create_mirror_view(y);
        auto z_mirror = Kokkos::create_mirror_view(z);
        auto mass_mirror = Kokkos::create_mirror_view(mass);

        for (size_t i = 0; i < n; ++i) {
            x_mirror(i) = x_h[i];
            y_mirror(i) = y_h[i];
            z_mirror(i) = z_h[i];
            mass_mirror(i) = mass_h[i];
        }

        Kokkos::deep_copy(x, x_mirror);
        Kokkos::deep_copy(y, y_mirror);
        Kokkos::deep_copy(z, z_mirror);
        Kokkos::deep_copy(mass, mass_mirror);

        many_body_force_kokkos_impl<ExecSpace>(x, y, z, mass, fx, fy, fz, n, softening);

        Timer timer;
        timer.start();
        for (int iter = 0; iter < num_iterations; ++iter) {
            many_body_force_kokkos_impl<ExecSpace>(x, y, z, mass, fx, fy, fz, n, softening);
        }
        elapsed_ms = timer.stop();
#endif
    } else {
#ifdef KOKKOS_ENABLE_OPENMP
        using MemSpace = Kokkos::HostSpace;
        using ExecSpace = Kokkos::OpenMP;

        Kokkos::View<double*, MemSpace> x("x", n);
        Kokkos::View<double*, MemSpace> y("y", n);
        Kokkos::View<double*, MemSpace> z("z", n);
        Kokkos::View<double*, MemSpace> mass("mass", n);
        Kokkos::View<double*, MemSpace> fx("fx", n);
        Kokkos::View<double*, MemSpace> fy("fy", n);
        Kokkos::View<double*, MemSpace> fz("fz", n);

        for (size_t i = 0; i < n; ++i) {
            x(i) = x_h[i];
            y(i) = y_h[i];
            z(i) = z_h[i];
            mass(i) = mass_h[i];
        }

        Timer timer;
        timer.start();
        for (int iter = 0; iter < num_iterations; ++iter) {
            many_body_force_kokkos_impl<ExecSpace>(x, y, z, mass, fx, fy, fz, n, softening);
        }
        elapsed_ms = timer.stop();
#endif
    }

    BenchmarkResult result;
    result.kernel_name = "ManyBody";
    result.implementation = "Kokkos";
    result.device = use_gpu ? "GPU" : "CPU";
    result.problem_size = n;
    result.time_ms = elapsed_ms / num_iterations;
    result.gflops = (n * n * 20.0 / 1e9) / (result.time_ms / 1000.0);
    result.bandwidth_gb_s = (n * 4 * sizeof(double) / 1e9) / (result.time_ms / 1000.0);
    result.validated = true;

    return result;
}
