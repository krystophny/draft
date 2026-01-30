#!/bin/bash
set -uo pipefail

# Build C projects and measure compile time

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="$(dirname "$SCRIPT_DIR")"
REPOS_DIR="$BASE_DIR/repos/c"
RESULTS_DIR="$BASE_DIR/results/timings"

source "$SCRIPT_DIR/projects.sh"

mkdir -p "$RESULTS_DIR"

NPROC=$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 8)
echo "Using $NPROC parallel compile jobs"

TIMING_FILE="$RESULTS_DIR/c_times.csv"
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
    for entry in "${C_PROJECTS[@]}"; do
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
            rm -rf build/ 2>/dev/null || true
            make clean > /dev/null 2>&1 || true
            rm -f *.o *.a 2>/dev/null || true

            local start end elapsed success
            local cflags build_type

            if [[ "$mode" == "release" ]]; then
                cflags="-O3 -march=native"
                build_type="Release"
            else
                cflags="-g -O0"
                build_type="Debug"
            fi

            start=$(gdate +%s.%N)

            if [[ -f "$dir/CMakeLists.txt" ]]; then
                mkdir -p build
                if cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE="$build_type" > /tmp/build_$$.log 2>&1 && \
                   cmake --build build -j"$NPROC" >> /tmp/build_$$.log 2>&1; then
                    success=1
                else
                    success=0
                fi
            elif [[ -f "$dir/Makefile" ]] || [[ -f "$dir/makefile" ]]; then
                if make -j"$NPROC" CFLAGS="$cflags" > /tmp/build_$$.log 2>&1; then
                    success=1
                else
                    # Try without CFLAGS
                    if make -j"$NPROC" > /tmp/build_$$.log 2>&1; then
                        success=1
                    else
                        success=0
                    fi
                fi
            else
                # Header-only: compile a test
                local main_header
                main_header=$(find . -maxdepth 2 -name "*.h" | head -1)
                if [[ -n "$main_header" ]]; then
                    echo "#include \"$main_header\"" > /tmp/test_$$.c
                    echo "int main(void) { return 0; }" >> /tmp/test_$$.c
                    if gcc $cflags -I. -c /tmp/test_$$.c -o /tmp/test_$$.o > /tmp/build_$$.log 2>&1; then
                        success=1
                    else
                        success=0
                    fi
                    rm -f /tmp/test_$$.c /tmp/test_$$.o
                else
                    echo "    SKIP: No build system found"
                    continue
                fi
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
echo "C Build Benchmark"
echo "=========================================="

for dir in "$REPOS_DIR"/*/; do
    if [[ -d "$dir" ]]; then
        name=$(basename "$dir")
        build_project "$name"
    fi
done

echo ""
echo "=========================================="
echo "C builds complete!"
echo "=========================================="
echo "Results saved to: $TIMING_FILE"
