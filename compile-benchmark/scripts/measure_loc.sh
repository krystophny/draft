#!/bin/bash
set -uo pipefail

# Measure lines of code for all repositories using scc

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="$(dirname "$SCRIPT_DIR")"
REPOS_DIR="$BASE_DIR/repos"
RESULTS_DIR="$BASE_DIR/results"
SCC_DIR="$RESULTS_DIR/scc"

source "$SCRIPT_DIR/projects.sh"

mkdir -p "$SCC_DIR"

# CSV header
LOC_CSV="$RESULTS_DIR/loc_stats.csv"
echo "language,project,size_category,files,lines,code,comments,blanks" > "$LOC_CSV"

measure_project() {
    local lang="$1"
    local name="$2"
    local size="$3"
    local dir="$REPOS_DIR/$lang/$name"

    if [[ ! -d "$dir" ]]; then
        echo "SKIP: $lang/$name (not found)"
        return 0
    fi

    echo "MEASURE: $lang/$name"

    # Run scc and save JSON
    local json_file="$SCC_DIR/${lang}_${name}.json"
    scc --format json --no-complexity "$dir" > "$json_file" 2>/dev/null || true

    # Extract totals and append to CSV
    if [[ -f "$json_file" ]] && [[ -s "$json_file" ]]; then
        # Parse JSON to get totals (sum all languages in the project)
        local stats
        stats=$(python3 -c "
import json
import sys
try:
    with open('$json_file') as f:
        data = json.load(f)
    files = sum(lang.get('Count', 0) for lang in data)
    lines = sum(lang.get('Lines', 0) for lang in data)
    code = sum(lang.get('Code', 0) for lang in data)
    comments = sum(lang.get('Comment', 0) for lang in data)
    blanks = sum(lang.get('Blank', 0) for lang in data)
    print(f'{files},{lines},{code},{comments},{blanks}')
except Exception as e:
    print('0,0,0,0,0', file=sys.stderr)
    sys.exit(1)
" 2>/dev/null)

        if [[ -n "$stats" ]] && [[ "$stats" != "0,0,0,0,0" ]]; then
            echo "$lang,$name,$size,$stats" >> "$LOC_CSV"
        else
            echo "WARN: No stats for $lang/$name"
        fi
    fi
}

get_size_for_project() {
    local lang="$1"
    local name="$2"

    case "$lang" in
        fortran) projects=("${FORTRAN_PROJECTS[@]}") ;;
        c) projects=("${C_PROJECTS[@]}") ;;
        cpp) projects=("${CPP_PROJECTS[@]}") ;;
        zig) projects=("${ZIG_PROJECTS[@]}") ;;
        go) projects=("${GO_PROJECTS[@]}") ;;
    esac

    for entry in "${projects[@]}"; do
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

echo "Measuring lines of code..."
echo "Results will be saved to: $LOC_CSV"
echo ""

for lang in fortran c cpp zig go rust; do
    echo ""
    echo "=========================================="
    echo "Processing $lang..."
    echo "=========================================="

    if [[ -d "$REPOS_DIR/$lang" ]]; then
        for dir in "$REPOS_DIR/$lang"/*/; do
            if [[ -d "$dir" ]]; then
                name=$(basename "$dir")
                size=$(get_size_for_project "$lang" "$name")
                measure_project "$lang" "$name" "$size"
            fi
        done
    fi
done

echo ""
echo "=========================================="
echo "LOC measurement complete!"
echo "=========================================="
echo "Results saved to: $LOC_CSV"
echo ""

# Quick summary
echo "Summary by language:"
for lang in fortran c cpp zig go rust; do
    total_code=$(grep "^$lang," "$LOC_CSV" | awk -F',' '{sum+=$6} END {print sum+0}')
    count=$(grep -c "^$lang," "$LOC_CSV" || echo 0)
    echo "  $lang: $count projects, $total_code total LOC"
done
