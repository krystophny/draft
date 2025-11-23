# GPU Benchmark Suite for Plasma Physics Kernels

Comprehensive benchmarking of GPU programming models for plasma physics applications.

## Implementations Compared

- **CUDA**: Native NVIDIA CUDA (C++)
- **CUDA Fortran**: NVIDIA CUDA Fortran (nvfortran)
- **Kokkos**: Portable C++ performance portability library
- **OpenMP**: CPU threading and GPU offload (gcc/gfortran/nvfortran)

## Kernels Benchmarked

1. **Particle Push**: Boris pusher for charged particle motion in electromagnetic fields
2. **FDM Laplacian**: Finite difference method 7-point stencil for Laplacian operator
3. **Many-Body**: N-body gravitational/electrostatic force calculation

## Requirements

- CUDA Toolkit 11.0+
- CMake 3.18+
- Ninja build system
- C++ compiler with C++17 support
- Fortran compiler (gfortran or nvfortran)
- OpenMP support
- Python 3.6+ with matplotlib and pandas (for visualization)

## Building

```bash
make build
```

This configures and builds all benchmark variants using CMake with Ninja.

## Running Benchmarks

```bash
make benchmark
```

This runs all benchmarks and saves results to `results/benchmark_results.csv`.

## Visualization

```bash
make visualize
```

Generates performance comparison plots and a detailed report:
- `results/performance_comparison.png`: Time comparison CPU vs GPU
- `results/gflops_comparison.png`: Compute performance
- `results/bandwidth_comparison.png`: Memory bandwidth
- `results/benchmark_report.txt`: Detailed analysis

## Clean

```bash
make clean
```

## Directory Structure

```
gpu/
├── src/
│   ├── cuda/              # Native CUDA implementations
│   ├── cuda_fortran/      # CUDA Fortran implementations
│   ├── kokkos/            # Kokkos implementations
│   └── openmp/            # OpenMP CPU/GPU implementations
├── include/               # Common headers
├── benchmarks/            # Benchmark harness
├── scripts/               # Visualization scripts
└── results/               # Output directory
```

## Customization

Edit problem sizes and iteration counts in `benchmarks/benchmark_runner.cpp`:

```cpp
size_t particle_n = 1000000;      // Number of particles
int fdm_n = 128;                  // Grid size per dimension (128^3)
size_t many_body_n = 4096;        // Number of bodies
int num_iterations = 10;          // Benchmark iterations
```
