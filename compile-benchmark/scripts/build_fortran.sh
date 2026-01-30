#!/bin/bash
set -uo pipefail

# Build Fortran projects and measure compile time

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="$(dirname "$SCRIPT_DIR")"
REPOS_DIR="$BASE_DIR/repos/fortran"
RESULTS_DIR="$BASE_DIR/results/timings"

source "$SCRIPT_DIR/projects.sh"

mkdir -p "$RESULTS_DIR"

# Number of parallel compile jobs (all cores)
NPROC=$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 8)
echo "Using $NPROC parallel compile jobs"

# Output file
TIMING_FILE="$RESULTS_DIR/fortran_times.csv"
if [[ ! -f "$TIMING_FILE" ]]; then
    echo "project,size,mode,run,time_sec,success" > "$TIMING_FILE"
fi

purge_cache() {
    if command -v purge &> /dev/null; then
        sudo purge 2>/dev/null || true
    fi
    sync
    sleep 1
}

get_size_for_project() {
    local name="$1"
    for entry in "${FORTRAN_PROJECTS[@]}"; do
        IFS='|' read -r url size build notes <<< "$entry"
        local proj_name
        proj_name=$(basename "$url" .git)
        if [[ "$proj_name" == "$name" ]]; then
            echo "$size"
            return
        fi
    done
    echo "unknown"
}

get_build_system() {
    local name="$1"
    for entry in "${FORTRAN_PROJECTS[@]}"; do
        IFS='|' read -r url size build notes <<< "$entry"
        local proj_name
        proj_name=$(basename "$url" .git)
        if [[ "$proj_name" == "$name" ]]; then
            echo "$build"
            return
        fi
    done
    echo "unknown"
}

build_project() {
    local name="$1"
    local dir="$REPOS_DIR/$name"

    if [[ ! -d "$dir" ]]; then
        echo "SKIP: $name (not found)"
        return
    fi

    local size build_sys
    size=$(get_size_for_project "$name")
    build_sys=$(get_build_system "$name")

    echo ""
    echo "Building: $name (size: $size, build: $build_sys)"

    for mode in debug release; do
        for run in 1 2 3; do
            echo "  $mode run $run..."
            purge_cache

            cd "$dir"
            rm -rf build/ .fpm/ zig-cache/ zig-out/ 2>/dev/null || true

            local start end elapsed success

            # Record start time
            start=$(gdate +%s.%N)

            # Determine build command and run it
            if [[ -f "$dir/fpm.toml" ]]; then
                if [[ "$mode" == "release" ]]; then
                    fpm build --flag '-O3 -march=native' > /tmp/build_$$.log 2>&1 && success=1 || success=0
                else
                    fpm build --flag '-g -O0' > /tmp/build_$$.log 2>&1 && success=1 || success=0
                fi
            elif [[ -f "$dir/CMakeLists.txt" ]]; then
                local build_type
                if [[ "$mode" == "release" ]]; then
                    build_type="Release"
                else
                    build_type="Debug"
                fi
                mkdir -p build
                if cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE="$build_type" > /tmp/build_$$.log 2>&1 && \
                   cmake --build build -j"$NPROC" >> /tmp/build_$$.log 2>&1; then
                    success=1
                else
                    success=0
                fi
            else
                echo "    SKIP: No fpm.toml or CMakeLists.txt"
                continue
            fi

            # Record end time
            end=$(gdate +%s.%N)
            elapsed=$(echo "$end - $start" | bc)

            # Log result
            echo "$name,$size,$mode,$run,$elapsed,$success" >> "$TIMING_FILE"
            echo "    Time: ${elapsed}s (success: $success)"

            rm -f /tmp/build_$$.log
        done
    done
}

echo "=========================================="
echo "Fortran Build Benchmark"
echo "=========================================="

for dir in "$REPOS_DIR"/*/; do
    if [[ -d "$dir" ]]; then
        name=$(basename "$dir")
        build_project "$name"
    fi
done

echo ""
echo "=========================================="
echo "Fortran builds complete!"
echo "=========================================="
echo "Results saved to: $TIMING_FILE"
