#!/bin/bash
set -uo pipefail

# Build Zig projects and measure compile time

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="$(dirname "$SCRIPT_DIR")"
REPOS_DIR="$BASE_DIR/repos/zig"
RESULTS_DIR="$BASE_DIR/results/timings"

source "$SCRIPT_DIR/projects.sh"

mkdir -p "$RESULTS_DIR"

NPROC=$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 8)
echo "Using $NPROC parallel compile jobs"

TIMING_FILE="$RESULTS_DIR/zig_times.csv"
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
    for entry in "${ZIG_PROJECTS[@]}"; do
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
            rm -rf zig-cache/ zig-out/ .zig-cache/ 2>/dev/null || true

            local start end elapsed success
            local optimize_flag zig_mode

            if [[ "$mode" == "release" ]]; then
                optimize_flag="-Doptimize=ReleaseFast"
                zig_mode="ReleaseFast"
            else
                optimize_flag="-Doptimize=Debug"
                zig_mode="Debug"
            fi

            start=$(gdate +%s.%N)

            if [[ -f "$dir/build.zig" ]]; then
                if zig build "$optimize_flag" -j"$NPROC" > /tmp/build_$$.log 2>&1; then
                    success=1
                else
                    success=0
                fi
            elif [[ -f "$dir/src/main.zig" ]]; then
                if zig build-exe src/main.zig -O "$zig_mode" > /tmp/build_$$.log 2>&1; then
                    success=1
                else
                    success=0
                fi
            elif [[ -f "$dir/src/lib.zig" ]]; then
                if zig build-lib src/lib.zig -O "$zig_mode" > /tmp/build_$$.log 2>&1; then
                    success=1
                else
                    success=0
                fi
            else
                echo "    SKIP: No build.zig or src/main.zig"
                continue
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
echo "Zig Build Benchmark"
echo "=========================================="

for dir in "$REPOS_DIR"/*/; do
    if [[ -d "$dir" ]]; then
        name=$(basename "$dir")
        build_project "$name"
    fi
done

echo ""
echo "=========================================="
echo "Zig builds complete!"
echo "=========================================="
echo "Results saved to: $TIMING_FILE"
