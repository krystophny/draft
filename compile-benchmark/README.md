# Compile Time Benchmark

Measure compile times, file counts, and lines of code for 150 representative open-source projects across 5 languages: Fortran, C, C++, Zig, and Go.

## Quick Start

```bash
# Install dependencies
./scripts/install_deps.sh

# Run the complete benchmark (8-12 hours)
./scripts/run_all.sh
```

## Project Structure

```
compile-benchmark/
├── repos/                  # Cloned repositories (gitignored)
│   ├── fortran/
│   ├── c/
│   ├── cpp/
│   ├── zig/
│   └── go/
├── scripts/
│   ├── install_deps.sh     # Install go, zig, scc
│   ├── projects.sh         # Project URL definitions
│   ├── clone_repos.sh      # Clone all 150 repos
│   ├── measure_loc.sh      # Run scc on each repo
│   ├── build_fortran.sh    # fpm/cmake builds
│   ├── build_c.sh          # cmake/make builds
│   ├── build_cpp.sh        # cmake builds
│   ├── build_zig.sh        # zig build
│   ├── build_go.sh         # go build
│   ├── run_all.sh          # Master script
│   └── analyze.py          # Generate statistics
└── results/
    ├── loc_stats.csv       # scc output per project
    ├── compile_times.csv   # Timing results
    ├── hardware_info.txt   # System info
    └── summary.md          # Final report
```

## Size Categories

| Size   | LOC Range        |
|--------|------------------|
| Small  | < 2,000          |
| Medium | 2,000 - 20,000   |
| Large  | 20,000 - 100,000 |

## Methodology

### LOC Measurement
Uses [scc](https://github.com/boyter/scc) for accurate line counting with language detection.

### Compile Time Measurement
- 3 runs per project per mode (debug and release)
- Cold cache between runs (macOS `purge` or Linux `drop_caches`)
- All builds use maximum parallelism (`-j$(nproc)`)
- Mean and standard deviation computed

### Build Systems
| Language | Build System              |
|----------|---------------------------|
| Fortran  | fpm or cmake              |
| C        | cmake, make, or header-only compile |
| C++      | cmake or header-only compile |
| Zig      | zig build                 |
| Go       | go build                  |

## Running Individual Steps

```bash
# Clone only
./scripts/clone_repos.sh

# Measure LOC only
./scripts/measure_loc.sh

# Build specific language
./scripts/build_fortran.sh
./scripts/build_c.sh
./scripts/build_cpp.sh
./scripts/build_zig.sh
./scripts/build_go.sh

# Analyze results
python3 ./scripts/analyze.py
```

## Output Format

### loc_stats.csv
```csv
language,project,size_category,files,lines,code,comments,blanks
fortran,stdlib,medium,245,18432,15234,2100,1098
```

### compile_times.csv
```csv
language,project,size,files,code_lines,mode,mean_time,std_time,runs,loc_per_sec
fortran,stdlib,medium,245,15234,release,4.32,0.035,3,3526
```

## Requirements

- macOS or Linux
- Homebrew (macOS)
- gfortran, gcc, g++
- cmake, ninja
- go, zig, scc (installed by install_deps.sh)
- python3

## Constraints

- Only projects < 100k LOC
- No heavy dependencies (CUDA, MPI, HDF5)
- Target build time < 5 minutes per project
