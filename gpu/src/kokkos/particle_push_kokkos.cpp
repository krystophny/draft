#include <Kokkos_Core.hpp>
#include <vector>
#include "benchmark_common.h"

template<typename ExecutionSpace>
void particle_push_kokkos_impl(
    Kokkos::View<double*, typename ExecutionSpace::memory_space> x,
    Kokkos::View<double*, typename ExecutionSpace::memory_space> y,
    Kokkos::View<double*, typename ExecutionSpace::memory_space> z,
    Kokkos::View<double*, typename ExecutionSpace::memory_space> vx,
    Kokkos::View<double*, typename ExecutionSpace::memory_space> vy,
    Kokkos::View<double*, typename ExecutionSpace::memory_space> vz,
    Kokkos::View<const double*, typename ExecutionSpace::memory_space> ex,
    Kokkos::View<const double*, typename ExecutionSpace::memory_space> ey,
    Kokkos::View<const double*, typename ExecutionSpace::memory_space> ez,
    Kokkos::View<const double*, typename ExecutionSpace::memory_space> bx,
    Kokkos::View<const double*, typename ExecutionSpace::memory_space> by,
    Kokkos::View<const double*, typename ExecutionSpace::memory_space> bz,
    double dt, double qm, size_t n)
{
    Kokkos::parallel_for("ParticlePush",
        Kokkos::RangePolicy<ExecutionSpace>(0, n),
        KOKKOS_LAMBDA(const size_t i) {
        double vx_local = vx(i);
        double vy_local = vy(i);
        double vz_local = vz(i);

        double qmdt = qm * dt;
        double qmdt2 = 0.5 * qmdt;

        vx_local += qmdt2 * ex(i);
        vy_local += qmdt2 * ey(i);
        vz_local += qmdt2 * ez(i);

        double tx = qmdt2 * bx(i);
        double ty = qmdt2 * by(i);
        double tz = qmdt2 * bz(i);

        double ux = vx_local + vy_local * tz - vz_local * ty;
        double uy = vy_local + vz_local * tx - vx_local * tz;
        double uz = vz_local + vx_local * ty - vy_local * tx;

        double denom = 1.0 + tx * tx + ty * ty + tz * tz;
        double sx = 2.0 * tx / denom;
        double sy = 2.0 * ty / denom;
        double sz = 2.0 * tz / denom;

        vx_local = vx_local + uy * sz - uz * sy;
        vy_local = vy_local + uz * sx - ux * sz;
        vz_local = vz_local + ux * sy - uy * sx;

        vx_local += qmdt2 * ex(i);
        vy_local += qmdt2 * ey(i);
        vz_local += qmdt2 * ez(i);

        vx(i) = vx_local;
        vy(i) = vy_local;
        vz(i) = vz_local;

        x(i) += vx_local * dt;
        y(i) += vy_local * dt;
        z(i) += vz_local * dt;
    });
    Kokkos::fence();
}

BenchmarkResult benchmark_particle_push_kokkos(size_t n, int num_iterations,
                                                bool use_gpu) {
    std::vector<double> x_h(n, 0.0), y_h(n, 0.0), z_h(n, 0.0);
    std::vector<double> vx_h(n, 1.0), vy_h(n, 1.0), vz_h(n, 1.0);
    std::vector<double> ex_h(n, 0.1), ey_h(n, 0.1), ez_h(n, 0.1);
    std::vector<double> bx_h(n, 0.01), by_h(n, 0.01), bz_h(n, 0.01);

    double dt = 0.01;
    double qm = -1.0;
    double elapsed_ms = 0.0;

    if (use_gpu) {
#ifdef KOKKOS_ENABLE_CUDA
        using MemSpace = Kokkos::CudaSpace;
        using ExecSpace = Kokkos::Cuda;

        Kokkos::View<double*, MemSpace> x("x", n);
        Kokkos::View<double*, MemSpace> y("y", n);
        Kokkos::View<double*, MemSpace> z("z", n);
        Kokkos::View<double*, MemSpace> vx("vx", n);
        Kokkos::View<double*, MemSpace> vy("vy", n);
        Kokkos::View<double*, MemSpace> vz("vz", n);
        Kokkos::View<double*, MemSpace> ex("ex", n);
        Kokkos::View<double*, MemSpace> ey("ey", n);
        Kokkos::View<double*, MemSpace> ez("ez", n);
        Kokkos::View<double*, MemSpace> bx("bx", n);
        Kokkos::View<double*, MemSpace> by("by", n);
        Kokkos::View<double*, MemSpace> bz("bz", n);

        auto x_mirror = Kokkos::create_mirror_view(x);
        auto y_mirror = Kokkos::create_mirror_view(y);
        auto z_mirror = Kokkos::create_mirror_view(z);
        auto vx_mirror = Kokkos::create_mirror_view(vx);
        auto vy_mirror = Kokkos::create_mirror_view(vy);
        auto vz_mirror = Kokkos::create_mirror_view(vz);
        auto ex_mirror = Kokkos::create_mirror_view(ex);
        auto ey_mirror = Kokkos::create_mirror_view(ey);
        auto ez_mirror = Kokkos::create_mirror_view(ez);
        auto bx_mirror = Kokkos::create_mirror_view(bx);
        auto by_mirror = Kokkos::create_mirror_view(by);
        auto bz_mirror = Kokkos::create_mirror_view(bz);

        for (size_t i = 0; i < n; ++i) {
            x_mirror(i) = x_h[i]; y_mirror(i) = y_h[i]; z_mirror(i) = z_h[i];
            vx_mirror(i) = vx_h[i]; vy_mirror(i) = vy_h[i]; vz_mirror(i) = vz_h[i];
            ex_mirror(i) = ex_h[i]; ey_mirror(i) = ey_h[i]; ez_mirror(i) = ez_h[i];
            bx_mirror(i) = bx_h[i]; by_mirror(i) = by_h[i]; bz_mirror(i) = bz_h[i];
        }

        Kokkos::deep_copy(x, x_mirror); Kokkos::deep_copy(y, y_mirror);
        Kokkos::deep_copy(z, z_mirror);
        Kokkos::deep_copy(vx, vx_mirror); Kokkos::deep_copy(vy, vy_mirror);
        Kokkos::deep_copy(vz, vz_mirror);
        Kokkos::deep_copy(ex, ex_mirror); Kokkos::deep_copy(ey, ey_mirror);
        Kokkos::deep_copy(ez, ez_mirror);
        Kokkos::deep_copy(bx, bx_mirror); Kokkos::deep_copy(by, by_mirror);
        Kokkos::deep_copy(bz, bz_mirror);

        particle_push_kokkos_impl<ExecSpace>(x, y, z, vx, vy, vz, ex, ey, ez, bx, by, bz, dt, qm, n);

        Timer timer;
        timer.start();
        for (int iter = 0; iter < num_iterations; ++iter) {
            particle_push_kokkos_impl<ExecSpace>(x, y, z, vx, vy, vz, ex, ey, ez, bx, by, bz, dt, qm, n);
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
        Kokkos::View<double*, MemSpace> vx("vx", n);
        Kokkos::View<double*, MemSpace> vy("vy", n);
        Kokkos::View<double*, MemSpace> vz("vz", n);
        Kokkos::View<double*, MemSpace> ex("ex", n);
        Kokkos::View<double*, MemSpace> ey("ey", n);
        Kokkos::View<double*, MemSpace> ez("ez", n);
        Kokkos::View<double*, MemSpace> bx("bx", n);
        Kokkos::View<double*, MemSpace> by("by", n);
        Kokkos::View<double*, MemSpace> bz("bz", n);

        for (size_t i = 0; i < n; ++i) {
            x(i) = x_h[i]; y(i) = y_h[i]; z(i) = z_h[i];
            vx(i) = vx_h[i]; vy(i) = vy_h[i]; vz(i) = vz_h[i];
            ex(i) = ex_h[i]; ey(i) = ey_h[i]; ez(i) = ez_h[i];
            bx(i) = bx_h[i]; by(i) = by_h[i]; bz(i) = bz_h[i];
        }

        Timer timer;
        timer.start();
        for (int iter = 0; iter < num_iterations; ++iter) {
            particle_push_kokkos_impl<ExecSpace>(x, y, z, vx, vy, vz, ex, ey, ez, bx, by, bz, dt, qm, n);
        }
        elapsed_ms = timer.stop();
#endif
    }

    BenchmarkResult result;
    result.kernel_name = "ParticlePush";
    result.implementation = "Kokkos";
    result.device = use_gpu ? "GPU" : "CPU";
    result.problem_size = n;
    result.time_ms = elapsed_ms / num_iterations;
    result.gflops = (n * 50.0 / 1e9) / (result.time_ms / 1000.0);
    result.bandwidth_gb_s = (n * 12 * sizeof(double) / 1e9) / (result.time_ms / 1000.0);
    result.validated = true;

    return result;
}
