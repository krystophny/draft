#!/bin/bash
set -uo pipefail

# Build Rust projects and measure compile time

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="$(dirname "$SCRIPT_DIR")"
REPOS_DIR="$BASE_DIR/repos/rust"
RESULTS_DIR="$BASE_DIR/results/timings"

source "$SCRIPT_DIR/projects.sh"

mkdir -p "$RESULTS_DIR"

NPROC=$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 8)
echo "Using $NPROC parallel compile jobs"

TIMING_FILE="$RESULTS_DIR/rust_times.csv"
if [[ ! -f "$TIMING_FILE" ]]; then
    echo "project,size,mode,run,time_sec,success" > "$TIMING_FILE"
fi

purge_cache() {
    if command -v purge &> /dev/null; then
        sudo purge 2>/dev/null || true
    fi
    # Clear cargo cache for this project
    rm -rf target/ 2>/dev/null || true
    sync
    sleep 1
}

get_size_for_project() {
    local name="$1"
    for entry in "${RUST_PROJECTS[@]}"; do
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

build_project() {
    local name="$1"
    local dir="$REPOS_DIR/$name"

    if [[ ! -d "$dir" ]]; then
        echo "SKIP: $name (not found)"
        return
    fi

    local size
    size=$(get_size_for_project "$name")

    echo ""
    echo "Building: $name (size: $size)"

    for mode in debug release; do
        for run in 1 2 3; do
            echo "  $mode run $run..."
            purge_cache

            cd "$dir"
            rm -rf target/ 2>/dev/null || true

            local start end elapsed success
            local cargo_flags

            if [[ "$mode" == "release" ]]; then
                cargo_flags="--release"
            else
                cargo_flags=""
            fi

            start=$(gdate +%s.%N)

            # Build with cargo
            if cargo build $cargo_flags -j "$NPROC" > /tmp/build_$$.log 2>&1; then
                success=1
            else
                success=0
            fi

            end=$(gdate +%s.%N)
            elapsed=$(echo "$end - $start" | bc)

            echo "$name,$size,$mode,$run,$elapsed,$success" >> "$TIMING_FILE"
            echo "    Time: ${elapsed}s (success: $success)"

            rm -f /tmp/build_$$.log
        done
    done
}

echo "=========================================="
echo "Rust Build Benchmark"
echo "=========================================="

for dir in "$REPOS_DIR"/*/; do
    if [[ -d "$dir" ]]; then
        name=$(basename "$dir")
        build_project "$name"
    fi
done

echo ""
echo "=========================================="
echo "Rust builds complete!"
echo "=========================================="
echo "Results saved to: $TIMING_FILE"
