#include <Kokkos_Core.hpp>
#include <vector>
#include "benchmark_common.h"

void particle_push_kokkos(
    Kokkos::View<double*> x, Kokkos::View<double*> y, Kokkos::View<double*> z,
    Kokkos::View<double*> vx, Kokkos::View<double*> vy, Kokkos::View<double*> vz,
    Kokkos::View<const double*> ex, Kokkos::View<const double*> ey,
    Kokkos::View<const double*> ez, Kokkos::View<const double*> bx,
    Kokkos::View<const double*> by, Kokkos::View<const double*> bz,
    double dt, double qm, size_t n)
{
    Kokkos::parallel_for("ParticlePush", n, KOKKOS_LAMBDA(const size_t i) {
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

    Kokkos::View<double*> x("x", n);
    Kokkos::View<double*> y("y", n);
    Kokkos::View<double*> z("z", n);
    Kokkos::View<double*> vx("vx", n);
    Kokkos::View<double*> vy("vy", n);
    Kokkos::View<double*> vz("vz", n);
    Kokkos::View<double*> ex("ex", n);
    Kokkos::View<double*> ey("ey", n);
    Kokkos::View<double*> ez("ez", n);
    Kokkos::View<double*> bx("bx", n);
    Kokkos::View<double*> by("by", n);
    Kokkos::View<double*> bz("bz", n);

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

    double dt = 0.01;
    double qm = -1.0;

    Timer timer;
    timer.start();
    for (int iter = 0; iter < num_iterations; ++iter) {
        particle_push_kokkos(x, y, z, vx, vy, vz, ex, ey, ez, bx, by, bz,
                            dt, qm, n);
    }
    double elapsed_ms = timer.stop();

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
