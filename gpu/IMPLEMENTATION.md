# GPU Benchmark Suite - Implementation Summary

## Overview

Comprehensive benchmarking framework for comparing GPU programming models on realistic plasma physics kernels.

## Implementations

### 1. Native CUDA (C++)
- **Location**: `src/cuda/`
- **Compiler**: nvcc (CUDA 13.0.88)
- **Architecture**: sm_80, sm_86 (Ampere)
- **Status**: ✅ Built successfully

Kernels implemented:
- Particle push (Boris pusher)
- FDM Laplacian (7-point stencil)
- Many-body force calculation

### 2. CUDA Fortran
- **Location**: `src/cuda_fortran/`
- **Compiler**: nvfortran (requires NVHPC)
- **Architecture**: cc80, cc86
- **Status**: ⚠️ Skipped (NVHPC compiler not available)

Note: Build system configured to compile with NVHPC when available.

### 3. Kokkos (C++)
- **Location**: `src/kokkos/`
- **Version**: 4.7.1 (master branch)
- **Backends**: CUDA + OpenMP
- **Architecture**: Ampere80
- **Status**: ✅ Built successfully

Provides performance portability across CPU (OpenMP) and GPU (CUDA).

### 4. OpenMP
- **Location**: `src/openmp/`
- **Compiler**: GCC 15.2.1 with OpenMP 4.5
- **Status**: ✅ CPU-only (GPU offload not available)

Note: GCC on this system is not configured with NVIDIA offload support.
GPU offload requires either NVHPC or GCC built with --enable-offload-targets=nvptx-none.

## Kernels Benchmarked

### Particle Push (Boris Method)
- **Problem size**: 1,000,000 particles
- **FLOPs**: ~50 per particle
- **Physics**: Charged particle motion in electromagnetic fields
- **Applications**: PIC simulations, plasma physics

### FDM Laplacian
- **Problem size**: 128³ grid points
- **Stencil**: 7-point (central differences)
- **FLOPs**: 8 per grid point
- **Applications**: Elliptic PDEs, diffusion, electromagnetics

### Many-Body Forces
- **Problem size**: 4,096 bodies
- **Complexity**: O(N²)
- **FLOPs**: 20 per interaction
- **Applications**: Gravitational/electrostatic forces, molecular dynamics

## Build System

### CMake Structure
```
CMakeLists.txt              # Top-level configuration
├── src/cuda/               # CUDA kernels
├── src/cuda_fortran/       # CUDA Fortran (conditional)
├── src/kokkos/             # Kokkos kernels
├── src/openmp/             # OpenMP kernels
└── benchmarks/             # Benchmark harness
```

### Makefile Targets
- `make build`: Configure and build all benchmarks
- `make benchmark`: Run benchmarks and save results
- `make visualize`: Generate performance plots
- `make clean`: Remove build artifacts

## Results and Visualization

### Generated Files
- `results/benchmark_results.csv`: Raw timing data
- `results/performance_comparison.png`: CPU vs GPU time comparison
- `results/gflops_comparison.png`: Compute performance
- `results/bandwidth_comparison.png`: Memory bandwidth
- `results/benchmark_report.txt`: Detailed analysis and recommendations

### Visualization Script
Python script (`scripts/visualize_results.py`) generates:
- Bar charts comparing implementations
- Speedup analysis (CPU vs GPU)
- Performance metrics (GFLOPS, bandwidth)

## Compiler and Tool Versions

- **CMake**: 3.31.5
- **Ninja**: 1.12.1
- **GCC**: 15.2.1
- **gfortran**: 15.2.1
- **CUDA**: 13.0.88
- **Kokkos**: 4.7.1
- **OpenMP**: 4.5

## GPU Architecture Targeted

- **SM 80**: NVIDIA Ampere (A100)
- **SM 86**: NVIDIA Ampere (RTX 30xx series)

## Performance Considerations

### Memory Access Patterns
- Coalesced global memory access
- Structure-of-arrays layout for particles
- Cache-friendly stencil operations

### Optimization Flags
- CUDA: `-O3 --use_fast_math`
- C++: `-O3 -march=native`
- Fortran: `-O3 -march=native`

### Kernel Launch Configuration
- Particle push: 256 threads/block
- FDM Laplacian: 8×8×8 thread blocks
- Many-body: 256 threads/block

## Limitations and Notes

1. **OpenMP GPU Offload**: Not available with current GCC build
   - Requires GCC configured with `--enable-offload-targets=nvptx-none`
   - Alternative: Use NVHPC compiler

2. **CUDA Fortran**: Requires NVHPC compiler
   - Build system ready when NVHPC available
   - All `.cuf` files implemented

3. **Kokkos Version**: Using master branch for CUDA 13 compatibility
   - Released versions had API incompatibilities with CUDA 13
   - Master branch resolves these issues

## Future Enhancements

1. Add GPU-resident data management for multi-iteration benchmarks
2. Implement CUDA Fortran variants when NVHPC available
3. Add OpenMP GPU offload when appropriate compiler available
4. Include power/energy measurements
5. Add roofline analysis
6. Extend to multi-GPU benchmarks
7. Add AMD GPU support (HIP, OpenMP)

## References

- CUDA Programming Guide: https://docs.nvidia.com/cuda/
- Kokkos Documentation: https://kokkos.org/kokkos-core-wiki/
- OpenMP Specification: https://www.openmp.org/specifications/
- Boris Pusher: Birdsall & Langdon, Plasma Physics via Computer Simulation
