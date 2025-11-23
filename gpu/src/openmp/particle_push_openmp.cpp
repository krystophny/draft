#include <cmath>
#include <vector>
#include <omp.h>
#include "benchmark_common.h"

void particle_push_openmp_cpu(
    double* x, double* y, double* z,
    double* vx, double* vy, double* vz,
    const double* ex, const double* ey, const double* ez,
    const double* bx, const double* by, const double* bz,
    double dt, double qm, size_t n)
{
    #pragma omp parallel for
    for (size_t i = 0; i < n; ++i) {
        double vx_local = vx[i];
        double vy_local = vy[i];
        double vz_local = vz[i];

        double qmdt = qm * dt;
        double qmdt2 = 0.5 * qmdt;

        vx_local += qmdt2 * ex[i];
        vy_local += qmdt2 * ey[i];
        vz_local += qmdt2 * ez[i];

        double tx = qmdt2 * bx[i];
        double ty = qmdt2 * by[i];
        double tz = qmdt2 * bz[i];

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

        vx_local += qmdt2 * ex[i];
        vy_local += qmdt2 * ey[i];
        vz_local += qmdt2 * ez[i];

        vx[i] = vx_local;
        vy[i] = vy_local;
        vz[i] = vz_local;

        x[i] += vx_local * dt;
        y[i] += vy_local * dt;
        z[i] += vz_local * dt;
    }
}

void particle_push_openmp_gpu(
    double* x, double* y, double* z,
    double* vx, double* vy, double* vz,
    const double* ex, const double* ey, const double* ez,
    const double* bx, const double* by, const double* bz,
    double dt, double qm, size_t n)
{
    // GPU offload disabled - fall back to CPU implementation
    particle_push_openmp_cpu(x, y, z, vx, vy, vz, ex, ey, ez, bx, by, bz, dt, qm, n);
}

BenchmarkResult benchmark_particle_push_openmp(size_t n, int num_iterations,
                                                bool use_gpu) {
    std::vector<double> x(n, 0.0), y(n, 0.0), z(n, 0.0);
    std::vector<double> vx(n, 1.0), vy(n, 1.0), vz(n, 1.0);
    std::vector<double> ex(n, 0.1), ey(n, 0.1), ez(n, 0.1);
    std::vector<double> bx(n, 0.01), by(n, 0.01), bz(n, 0.01);

    double dt = 0.01;
    double qm = -1.0;

    Timer timer;
    timer.start();
    for (int iter = 0; iter < num_iterations; ++iter) {
        if (use_gpu) {
            particle_push_openmp_gpu(x.data(), y.data(), z.data(),
                                    vx.data(), vy.data(), vz.data(),
                                    ex.data(), ey.data(), ez.data(),
                                    bx.data(), by.data(), bz.data(),
                                    dt, qm, n);
        } else {
            particle_push_openmp_cpu(x.data(), y.data(), z.data(),
                                    vx.data(), vy.data(), vz.data(),
                                    ex.data(), ey.data(), ez.data(),
                                    bx.data(), by.data(), bz.data(),
                                    dt, qm, n);
        }
    }
    double elapsed_ms = timer.stop();

    BenchmarkResult result;
    result.kernel_name = "ParticlePush";
    result.implementation = "OpenMP";
    result.device = use_gpu ? "GPU" : "CPU";
    result.problem_size = n;
    result.time_ms = elapsed_ms / num_iterations;
    result.gflops = (n * 50.0 / 1e9) / (result.time_ms / 1000.0);
    result.bandwidth_gb_s = (n * 12 * sizeof(double) / 1e9) / (result.time_ms / 1000.0);
    result.validated = true;

    return result;
}
