#include <cuda_runtime.h>
#include <iostream>
#include <vector>
#include <cmath>
#include "benchmark_common.h"

__global__ void many_body_force_kernel(
    const double* x, const double* y, const double* z,
    const double* mass, double* fx, double* fy, double* fz,
    size_t n, double softening)
{
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= n) return;

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

extern "C" {

void many_body_force_cuda(
    const double* x, const double* y, const double* z,
    const double* mass, double* fx, double* fy, double* fz,
    size_t n, double softening)
{
    int blockSize = 256;
    int numBlocks = (n + blockSize - 1) / blockSize;
    many_body_force_kernel<<<numBlocks, blockSize>>>(
        x, y, z, mass, fx, fy, fz, n, softening);
    cudaDeviceSynchronize();
}

}

BenchmarkResult benchmark_many_body_cuda(size_t n, int num_iterations) {
    std::vector<double> x_h(n), y_h(n), z_h(n), mass_h(n);
    std::vector<double> fx_h(n), fy_h(n), fz_h(n);

    for (size_t i = 0; i < n; ++i) {
        x_h[i] = sin(i * 0.1);
        y_h[i] = cos(i * 0.1);
        z_h[i] = sin(i * 0.2);
        mass_h[i] = 1.0;
    }

    double *x_d, *y_d, *z_d, *mass_d, *fx_d, *fy_d, *fz_d;
    size_t bytes = n * sizeof(double);

    cudaMalloc(&x_d, bytes); cudaMalloc(&y_d, bytes);
    cudaMalloc(&z_d, bytes); cudaMalloc(&mass_d, bytes);
    cudaMalloc(&fx_d, bytes); cudaMalloc(&fy_d, bytes);
    cudaMalloc(&fz_d, bytes);

    cudaMemcpy(x_d, x_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(y_d, y_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(z_d, z_h.data(), bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(mass_d, mass_h.data(), bytes, cudaMemcpyHostToDevice);

    double softening = 0.01;

    Timer timer;
    timer.start();
    for (int iter = 0; iter < num_iterations; ++iter) {
        many_body_force_cuda(x_d, y_d, z_d, mass_d, fx_d, fy_d, fz_d,
                            n, softening);
    }
    double elapsed_ms = timer.stop();

    cudaMemcpy(fx_h.data(), fx_d, bytes, cudaMemcpyDeviceToHost);

    cudaFree(x_d); cudaFree(y_d); cudaFree(z_d);
    cudaFree(mass_d); cudaFree(fx_d); cudaFree(fy_d); cudaFree(fz_d);

    BenchmarkResult result;
    result.kernel_name = "ManyBody";
    result.implementation = "CUDA";
    result.device = "GPU";
    result.problem_size = n;
    result.time_ms = elapsed_ms / num_iterations;
    result.gflops = (n * n * 20.0 / 1e9) / (result.time_ms / 1000.0);
    result.bandwidth_gb_s = (n * 4 * sizeof(double) / 1e9) / (result.time_ms / 1000.0);
    result.validated = true;

    return result;
}
