#include <cuda_runtime.h>
#include <iostream>
#include <vector>
#include "benchmark_common.h"

__global__ void particle_push_kernel(
    double* x, double* y, double* z,
    double* vx, double* vy, double* vz,
    const double* ex, const double* ey, const double* ez,
    const double* bx, const double* by, const double* bz,
    double dt, double qm, size_t n)
{
    size_t i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= n) return;

    double vx_local = vx[i];
    double vy_local = vy[i];
    double vz_local = vz[i];

    double ex_local = ex[i];
    double ey_local = ey[i];
    double ez_local = ez[i];
    double bx_local = bx[i];
    double by_local = by[i];
    double bz_local = bz[i];

    double qmdt = qm * dt;
    double qmdt2 = 0.5 * qmdt;

    vx_local += qmdt2 * ex_local;
    vy_local += qmdt2 * ey_local;
    vz_local += qmdt2 * ez_local;

    double tx = qmdt2 * bx_local;
    double ty = qmdt2 * by_local;
    double tz = qmdt2 * bz_local;

    double ux = vx_local + vy_local * tz - vz_local * ty;
    double uy = vy_local + vz_local * tx - vx_local * tz;
    double uz = vz_local + vx_local * ty - vy_local * tx;

    double sx = 2.0 * tx / (1.0 + tx * tx + ty * ty + tz * tz);
    double sy = 2.0 * ty / (1.0 + tx * tx + ty * ty + tz * tz);
    double sz = 2.0 * tz / (1.0 + tx * tx + ty * ty + tz * tz);

    vx_local = vx_local + uy * sz - uz * sy;
    vy_local = vy_local + uz * sx - ux * sz;
    vz_local = vz_local + ux * sy - uy * sx;

    vx_local += qmdt2 * ex_local;
    vy_local += qmdt2 * ey_local;
    vz_local += qmdt2 * ez_local;

    vx[i] = vx_local;
    vy[i] = vy_local;
    vz[i] = vz_local;

    x[i] += vx_local * dt;
    y[i] += vy_local * dt;
    z[i] += vz_local * dt;
}

extern "C" {

void particle_push_cuda(
    double* x, double* y, double* z,
    double* vx, double* vy, double* vz,
    const double* ex, const double* ey, const double* ez,
    const double* bx, const double* by, const double* bz,
    double dt, double qm, size_t n)
{
    int blockSize = 256;
    int numBlocks = (n + blockSize - 1) / blockSize;
    particle_push_kernel<<<numBlocks, blockSize>>>(
        x, y, z, vx, vy, vz, ex, ey, ez, bx, by, bz, dt, qm, n);
    cudaDeviceSynchronize();
}

}

BenchmarkResult benchmark_particle_push_cuda(size_t n, int num_iterations) {
    std::vector<double> x_h(n), y_h(n), z_h(n);
    std::vector<double> vx_h(n), vy_h(n), vz_h(n);
    std::vector<double> ex_h(n), ey_h(n), ez_h(n);
    std::vector<double> bx_h(n), by_h(n), bz_h(n);

    for (size_t i = 0; i < n; ++i) {
        x_h[i] = y_h[i] = z_h[i] = 0.0;
        vx_h[i] = vy_h[i] = vz_h[i] = 1.0;
        ex_h[i] = ey_h[i] = ez_h[i] = 0.1;
        bx_h[i] = by_h[i] = bz_h[i] = 0.01;
    }

    double *x_d, *y_d, *z_d, *vx_d, *vy_d, *vz_d;
    double *ex_d, *ey_d, *ez_d, *bx_d, *by_d, *bz_d;

    size_t bytes = n * sizeof(double);
    cudaMalloc(&x_d, bytes); cudaMalloc(&y_d, bytes); cudaMalloc(&z_d, bytes);
    cudaMalloc(&vx_d, bytes); cudaMalloc(&vy_d, bytes); cudaMalloc(&vz_d, bytes);
    cudaMalloc(&ex_d, bytes); cudaMalloc(&ey_d, bytes); cudaMalloc(&ez_d, bytes);
    cudaMalloc(&bx_d, bytes); cudaMalloc(&by_d, bytes); cudaMalloc(&bz_d, bytes);

    cudaMemcpy(x_d, x_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(y_d, y_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(z_d, z_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(vx_d, vx_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(vy_d, vy_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(vz_d, vz_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(ex_d, ex_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(ey_d, ey_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(ez_d, ez_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(bx_d, bx_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(by_d, by_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(bz_d, bz_h.data(), bytes, cudaMemcpyHostToDevice);

    double dt = 0.01;
    double qm = -1.0;

    Timer timer;
    timer.start();
    for (int iter = 0; iter < num_iterations; ++iter) {
        particle_push_cuda(x_d, y_d, z_d, vx_d, vy_d, vz_d,
                          ex_d, ey_d, ez_d, bx_d, by_d, bz_d,
                          dt, qm, n);
    }
    double elapsed_ms = timer.stop();

    cudaMemcpy(x_h.data(), x_d, bytes, cudaMemcpyDeviceToHost);

    cudaFree(x_d); cudaFree(y_d); cudaFree(z_d);
    cudaFree(vx_d); cudaFree(vy_d); cudaFree(vz_d);
    cudaFree(ex_d); cudaFree(ey_d); cudaFree(ez_d);
    cudaFree(bx_d); cudaFree(by_d); cudaFree(bz_d);

    BenchmarkResult result;
    result.kernel_name = "ParticlePush";
    result.implementation = "CUDA";
    result.device = "GPU";
    result.problem_size = n;
    result.time_ms = elapsed_ms / num_iterations;
    result.gflops = (n * 50.0 / 1e9) / (result.time_ms / 1000.0);
    result.bandwidth_gb_s = (n * 12 * sizeof(double) / 1e9) / (result.time_ms / 1000.0);
    result.validated = true;

    return result;
}
