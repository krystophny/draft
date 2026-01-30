#!/bin/bash
set -euo pipefail

# Install dependencies for compile time benchmark
# Requires Homebrew on macOS

echo "Installing benchmark dependencies..."

# Check if Homebrew is available
if ! command -v brew &> /dev/null; then
    echo "Error: Homebrew is required but not installed"
    exit 1
fi

# Install tools
brew install go zig scc coreutils

# Verify installations
echo ""
echo "Verifying installations:"
echo "  go:       $(go version 2>/dev/null | head -1 || echo 'NOT FOUND')"
echo "  zig:      $(zig version 2>/dev/null || echo 'NOT FOUND')"
echo "  scc:      $(scc --version 2>/dev/null | head -1 || echo 'NOT FOUND')"
echo "  gdate:    $(gdate --version 2>/dev/null | head -1 || echo 'NOT FOUND')"
echo "  gfortran: $(gfortran --version 2>/dev/null | head -1 || echo 'NOT FOUND')"
echo "  gcc:      $(gcc --version 2>/dev/null | head -1 || echo 'NOT FOUND')"
echo "  g++:      $(g++ --version 2>/dev/null | head -1 || echo 'NOT FOUND')"
echo "  cmake:    $(cmake --version 2>/dev/null | head -1 || echo 'NOT FOUND')"

echo ""
echo "Dependencies installed successfully"
