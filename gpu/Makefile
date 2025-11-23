.PHONY: all build clean benchmark visualize help

BUILD_DIR := build
NINJA := ninja

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
	cmake -S . -B $(BUILD_DIR) -G Ninja \
		-DCMAKE_BUILD_TYPE=Release \
		-DCMAKE_CUDA_ARCHITECTURES="80;86"
	cmake --build $(BUILD_DIR) -j

benchmark: build
	@mkdir -p results
	@echo "Running GPU benchmarks..."
	cd $(BUILD_DIR) && ./benchmarks/run_all_benchmarks

visualize:
	@echo "Generating visualizations..."
	python3 scripts/visualize_results.py

clean:
	rm -rf $(BUILD_DIR)
	rm -rf results/*.csv results/*.png
