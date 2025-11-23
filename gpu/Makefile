.PHONY: all build clean benchmark visualize help

BUILD_DIR := build
NINJA := ninja
NVHPC_ROOT := /opt/nvidia/hpc_sdk/Linux_x86_64/25.9/compilers
CUDA_ROOT := /opt/cuda

all: build

help:
	@echo "GPU Benchmark Suite"
	@echo "==================="
	@echo "Targets:"
	@echo "  build       - Configure and build all benchmarks"
	@echo "  benchmark   - Run all benchmarks and collect results"
	@echo "  visualize   - Generate performance visualization plots"
	@echo "  clean       - Remove build directory"
	@echo "  help        - Show this help message"

build:
	env PATH=$(NVHPC_ROOT)/bin:/opt/cuda/bin:$$PATH \
		CUDA_HOME=$(CUDA_ROOT) \
		cmake -S . -B $(BUILD_DIR) -G Ninja \
			-DCMAKE_C_COMPILER=$(NVHPC_ROOT)/bin/nvc \
			-DCMAKE_CXX_COMPILER=$(NVHPC_ROOT)/bin/nvc++ \
			-DCMAKE_Fortran_COMPILER=$(NVHPC_ROOT)/bin/nvfortran \
			-DCMAKE_CUDA_COMPILER=/opt/cuda/bin/nvcc \
			-DCMAKE_BUILD_TYPE=Release \
			-DCMAKE_CUDA_ARCHITECTURES="80;86"
	env PATH=$(NVHPC_ROOT)/bin:/opt/cuda/bin:$$PATH \
		CUDA_HOME=$(CUDA_ROOT) \
		cmake --build $(BUILD_DIR) -j

benchmark: build
	@mkdir -p results
	@echo "Running GPU benchmarks..."
	OMP_NUM_THREADS=$$(nproc) OMP_PROC_BIND=spread OMP_PLACES=threads \
		cd $(BUILD_DIR) && ./benchmarks/run_all_benchmarks

visualize:
	@echo "Generating visualizations..."
	python3 scripts/visualize_results.py

clean:
	rm -rf $(BUILD_DIR)
	rm -rf results/*.csv results/*.png
