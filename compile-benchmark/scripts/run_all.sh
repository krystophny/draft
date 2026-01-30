#!/bin/bash
set -uo pipefail

# Master script to run the complete compile time benchmark
# Note: Continues on individual build failures

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="$(dirname "$SCRIPT_DIR")"

echo "============================================================"
echo "Compile Time Benchmark Suite"
echo "============================================================"
echo ""
echo "Start time: $(date)"
echo "Base directory: $BASE_DIR"
echo ""

# Record hardware info
echo "Recording hardware info..."
{
    echo "# Hardware Information"
    echo ""
    echo "Date: $(date -Iseconds)"
    echo "CPU: $(sysctl -n machdep.cpu.brand_string 2>/dev/null || echo 'Unknown')"
    echo "Cores: $(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 'Unknown')"
    echo "RAM: $(sysctl -n hw.memsize 2>/dev/null | awk '{printf "%.1f GB", $0/1024/1024/1024}' || echo 'Unknown')"
    echo "OS: $(sw_vers -productName 2>/dev/null || uname -s) $(sw_vers -productVersion 2>/dev/null || uname -r)"
    echo ""
    echo "# Compiler Versions"
    echo ""
    echo "gfortran: $(gfortran --version 2>/dev/null | head -1 || echo 'Not installed')"
    echo "gcc: $(gcc --version 2>/dev/null | head -1 || echo 'Not installed')"
    echo "g++: $(g++ --version 2>/dev/null | head -1 || echo 'Not installed')"
    echo "zig: $(zig version 2>/dev/null || echo 'Not installed')"
    echo "go: $(go version 2>/dev/null | head -1 || echo 'Not installed')"
    echo "cmake: $(cmake --version 2>/dev/null | head -1 || echo 'Not installed')"
    echo "ninja: $(ninja --version 2>/dev/null || echo 'Not installed')"
} > "$BASE_DIR/results/hardware_info.txt"

# Step 1: Clone repositories
echo ""
echo "============================================================"
echo "Step 1: Cloning repositories..."
echo "============================================================"
bash "$SCRIPT_DIR/clone_repos.sh" || echo "Some clones failed, continuing..."

# Step 2: Measure LOC
echo ""
echo "============================================================"
echo "Step 2: Measuring lines of code..."
echo "============================================================"
bash "$SCRIPT_DIR/measure_loc.sh" || echo "Some LOC measurements failed, continuing..."

# Step 3: Build all languages
echo ""
echo "============================================================"
echo "Step 3: Running build benchmarks..."
echo "============================================================"

echo ""
echo "--- Fortran builds ---"
bash "$SCRIPT_DIR/build_fortran.sh" || echo "Some Fortran builds failed, continuing..."

echo ""
echo "--- C builds ---"
bash "$SCRIPT_DIR/build_c.sh" || echo "Some C builds failed, continuing..."

echo ""
echo "--- C++ builds ---"
bash "$SCRIPT_DIR/build_cpp.sh" || echo "Some C++ builds failed, continuing..."

echo ""
echo "--- Zig builds ---"
bash "$SCRIPT_DIR/build_zig.sh" || echo "Some Zig builds failed, continuing..."

echo ""
echo "--- Go builds ---"
bash "$SCRIPT_DIR/build_go.sh" || echo "Some Go builds failed, continuing..."

echo ""
echo "--- Rust builds ---"
bash "$SCRIPT_DIR/build_rust.sh" || echo "Some Rust builds failed, continuing..."

# Step 4: Analyze results
echo ""
echo "============================================================"
echo "Step 4: Analyzing results..."
echo "============================================================"
python3 "$SCRIPT_DIR/analyze.py"

echo ""
echo "============================================================"
echo "Benchmark Complete!"
echo "============================================================"
echo ""
echo "End time: $(date)"
echo ""
echo "Results:"
echo "  - LOC stats: $BASE_DIR/results/loc_stats.csv"
echo "  - Compile times: $BASE_DIR/results/compile_times.csv"
echo "  - Summary report: $BASE_DIR/results/summary.md"
echo "  - Hardware info: $BASE_DIR/results/hardware_info.txt"
