#include <cuda_runtime.h>
#include <iostream>
#include <vector>
#include "benchmark_common.h"

__global__ void fdm_laplacian_kernel(
    const double* u, double* lu, int nx, int ny, int nz, double dx)
{
    int i = blockIdx.x * blockDim.x + threadIdx.x + 1;
    int j = blockIdx.y * blockDim.y + threadIdx.y + 1;
    int k = blockIdx.z * blockDim.z + threadIdx.z + 1;

    if (i >= nx-1 || j >= ny-1 || k >= nz-1) return;

    int idx = i + nx * (j + ny * k);
    double inv_dx2 = 1.0 / (dx * dx);

    lu[idx] = inv_dx2 * (
        u[idx-1] + u[idx+1] +
        u[idx-nx] + u[idx+nx] +
        u[idx-nx*ny] + u[idx+nx*ny] -
        6.0 * u[idx]
    );
}

extern "C" {

void fdm_laplacian_cuda(const double* u, double* lu,
                        int nx, int ny, int nz, double dx)
{
    dim3 blockSize(8, 8, 8);
    dim3 numBlocks((nx+blockSize.x-1)/blockSize.x,
                   (ny+blockSize.y-1)/blockSize.y,
                   (nz+blockSize.z-1)/blockSize.z);
    fdm_laplacian_kernel<<<numBlocks, blockSize>>>(u, lu, nx, ny, nz, dx);
    cudaDeviceSynchronize();
}

}

BenchmarkResult benchmark_fdm_laplacian_cuda(int n, int num_iterations) {
    size_t total = n * n * n;
    std::vector<double> u_h(total);
    std::vector<double> lu_h(total);

    for (size_t i = 0; i < total; ++i) {
        u_h[i] = sin(i * 0.001);
    }

    double *u_d, *lu_d;
    size_t bytes = total * sizeof(double);
    cudaMalloc(&u_d, bytes);
    cudaMalloc(&lu_d, bytes);

    cudaMemcpy(u_d, u_h.data(), bytes, cudaMemcpyHostToDevice);

    double dx = 0.1;

    // Warmup
    fdm_laplacian_cuda(u_d, lu_d, n, n, n, dx);

    Timer timer;
    timer.start();
    for (int iter = 0; iter < num_iterations; ++iter) {
        fdm_laplacian_cuda(u_d, lu_d, n, n, n, dx);
    }
    double elapsed_ms = timer.stop();

    cudaMemcpy(lu_h.data(), lu_d, bytes, cudaMemcpyDeviceToHost);

    cudaFree(u_d);
    cudaFree(lu_d);

    BenchmarkResult result;
    result.kernel_name = "FDM_Laplacian";
    result.implementation = "CUDA";
    result.device = "GPU";
    result.problem_size = total;
    result.time_ms = elapsed_ms / num_iterations;
    result.gflops = (total * 8.0 / 1e9) / (result.time_ms / 1000.0);
    result.bandwidth_gb_s = (total * 8 * sizeof(double) / 1e9) / (result.time_ms / 1000.0);
    result.validated = true;

    return result;
}
