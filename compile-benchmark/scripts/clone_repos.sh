#!/bin/bash
set -uo pipefail

# Clone all repositories for compile time benchmark
# Uses shallow clones for speed
# Note: Continues on clone failures (some repos may not exist)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="$(dirname "$SCRIPT_DIR")"
REPOS_DIR="$BASE_DIR/repos"

source "$SCRIPT_DIR/projects.sh"

# Number of parallel clone jobs
PARALLEL_JOBS=8

clone_project() {
    local url="$1"
    local lang="$2"
    local size="$3"

    local name
    name=$(basename "$url" .git)
    local dest="$REPOS_DIR/$lang/$name"

    if [[ -d "$dest" ]]; then
        echo "SKIP: $lang/$name (already exists)"
        return 0
    fi

    echo "CLONE: $lang/$name from $url"
    if git clone --depth=1 --recurse-submodules --shallow-submodules "$url" "$dest" 2>/dev/null; then
        echo "OK: $lang/$name"
    else
        echo "FAIL: $lang/$name"
        # Continue despite failure
    fi
    return 0
}

export -f clone_project
export REPOS_DIR

clone_language() {
    local lang="$1"
    shift
    local projects=("$@")

    echo ""
    echo "=========================================="
    echo "Cloning $lang projects..."
    echo "=========================================="

    mkdir -p "$REPOS_DIR/$lang"

    for entry in "${projects[@]}"; do
        IFS='|' read -r url size build notes <<< "$entry"
        echo "$url|$lang|$size"
    done | xargs -P "$PARALLEL_JOBS" -I {} bash -c '
        IFS="|" read -r url lang size <<< "{}"
        clone_project "$url" "$lang" "$size"
    '
}

echo "Starting repository cloning..."
echo "Base directory: $BASE_DIR"
echo "Parallel jobs: $PARALLEL_JOBS"

clone_language "fortran" "${FORTRAN_PROJECTS[@]}"
clone_language "c" "${C_PROJECTS[@]}"
clone_language "cpp" "${CPP_PROJECTS[@]}"
clone_language "zig" "${ZIG_PROJECTS[@]}"
clone_language "go" "${GO_PROJECTS[@]}"
clone_language "rust" "${RUST_PROJECTS[@]}"

echo ""
echo "=========================================="
echo "Cloning complete!"
echo "=========================================="
echo ""

# Summary
for lang in fortran c cpp zig go; do
    count=$(find "$REPOS_DIR/$lang" -maxdepth 1 -type d | wc -l)
    count=$((count - 1))  # Subtract the directory itself
    echo "$lang: $count repositories"
done
