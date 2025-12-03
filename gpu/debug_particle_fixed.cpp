#include <iostream>
#include <vector>
#include <chrono>
#include <omp.h>

void particle_push_test(
    double* x, double* y, double* z,
    double* vx, double* vy, double* vz,
    const double* ex, const double* ey, const double* ez,
    const double* bx, const double* by, const double* bz,
    double dt, double qm, size_t n)
{
    double qmdt = qm * dt;          // MOVED OUTSIDE LOOP
    double qmdt2 = 0.5 * qmdt;      // MOVED OUTSIDE LOOP

    #pragma omp parallel for
    for (size_t i = 0; i < n; ++i) {
        double vx_local = vx[i];
        double vy_local = vy[i];
        double vz_local = vz[i];

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

int main() {
    size_t n = 1000000;
    std::vector<double> x(n, 0.0), y(n, 0.0), z(n, 0.0);
    std::vector<double> vx(n, 1.0), vy(n, 1.0), vz(n, 1.0);
    std::vector<double> ex(n, 0.1), ey(n, 0.1), ez(n, 0.1);
    std::vector<double> bx(n, 0.01), by(n, 0.01), bz(n, 0.01);

    double dt = 0.01;
    double qm = -1.0;

    int num_threads = 0;
    #pragma omp parallel
    {
        #pragma omp single
        num_threads = omp_get_num_threads();
    }
    std::cout << "OpenMP using " << num_threads << " threads" << std::endl;

    auto start = std::chrono::high_resolution_clock::now();
    for (int iter = 0; iter < 10; ++iter) {
        particle_push_test(x.data(), y.data(), z.data(),
                          vx.data(), vy.data(), vz.data(),
                          ex.data(), ey.data(), ez.data(),
                          bx.data(), by.data(), bz.data(),
                          dt, qm, n);
    }
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> elapsed = end - start;

    std::cout << "Time: " << elapsed.count() / 10.0 << " ms" << std::endl;
    double gflops = (n * 50.0 / 1e9) / ((elapsed.count() / 10.0) / 1000.0);
    std::cout << "GFLOPS: " << gflops << std::endl;

    return 0;
}
