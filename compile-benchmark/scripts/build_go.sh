#!/bin/bash
set -uo pipefail

# Build Go projects and measure compile time

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="$(dirname "$SCRIPT_DIR")"
REPOS_DIR="$BASE_DIR/repos/go"
RESULTS_DIR="$BASE_DIR/results/timings"

source "$SCRIPT_DIR/projects.sh"

mkdir -p "$RESULTS_DIR"

NPROC=$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 8)
echo "Using $NPROC parallel compile jobs (GOMAXPROCS)"

TIMING_FILE="$RESULTS_DIR/go_times.csv"
if [[ ! -f "$TIMING_FILE" ]]; then
    echo "project,size,mode,run,time_sec,success" > "$TIMING_FILE"
fi

purge_cache() {
    if command -v purge &> /dev/null; then
        sudo purge 2>/dev/null || true
    fi
    go clean -cache 2>/dev/null || true
    sync
    sleep 1
}

get_size_for_project() {
    local name="$1"
    for entry in "${GO_PROJECTS[@]}"; do
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

            export GOMAXPROCS="$NPROC"

            local start end elapsed success
            local gcflags ldflags

            if [[ "$mode" == "release" ]]; then
                gcflags=""
                ldflags="-s -w"
            else
                gcflags="-N -l"
                ldflags=""
            fi

            start=$(gdate +%s.%N)

            # Initialize module if needed
            if [[ ! -f "go.mod" ]]; then
                go mod init "benchmark/$(basename "$dir")" > /tmp/build_$$.log 2>&1 || true
            fi

            # Download deps
            go mod download >> /tmp/build_$$.log 2>&1 || true
            go mod tidy >> /tmp/build_$$.log 2>&1 || true

            # Build
            if [[ -n "$gcflags" ]]; then
                if go build -gcflags="$gcflags" -ldflags="$ldflags" -p "$NPROC" ./... >> /tmp/build_$$.log 2>&1; then
                    success=1
                else
                    success=0
                fi
            else
                if go build -ldflags="$ldflags" -p "$NPROC" ./... >> /tmp/build_$$.log 2>&1; then
                    success=1
                else
                    success=0
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
echo "Go Build Benchmark"
echo "=========================================="

for dir in "$REPOS_DIR"/*/; do
    if [[ -d "$dir" ]]; then
        name=$(basename "$dir")
        build_project "$name"
    fi
done

echo ""
echo "=========================================="
echo "Go builds complete!"
echo "=========================================="
echo "Results saved to: $TIMING_FILE"
