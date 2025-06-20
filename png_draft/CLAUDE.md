# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Fortran project that generates PNG images from scratch using the PNG file format specification. The project implements a pure Fortran PNG generator that creates binary PNG files with proper chunk structure, CRC32 checksums, and zlib compression.

## Development Commands

### Build and Run
- `fpm build` - Compile the project
- `fpm run` - Build and execute the main program (generates output.png)
- `fpm build --profile release` - Build with optimizations
- `fpm clean` - Clean build artifacts

### Testing
- `fpm test` - Run tests (currently minimal test in test/check.f90)

## Architecture

### Core Components
- **app/main.f90**: Main PNG generator program containing:
  - PNG signature and chunk writing functions
  - IHDR, IDAT, and IEND chunk implementations
  - zlib compression interface via C bindings
  - CRC32 calculation interface via C bindings
  - Big-endian byte order conversion utilities

### Key Features
- **C Interoperability**: Uses `iso_c_binding` to interface with system zlib library
- **Binary File I/O**: Uses stream access for writing binary PNG data
- **Chunk-based Architecture**: Implements PNG chunk structure with proper length, type, data, and CRC fields
- **Memory Management**: Proper allocation/deallocation of image and compression buffers

### Dependencies
- **zlib**: System zlib library (linked via `link = ["z"]` in fpm.toml)
- **Fortran 2008**: Uses modern Fortran features including C interoperability

### Data Handling
- Uses `integer(1)` for byte-level data manipulation
- Handles signed/unsigned byte conversion for PNG format compatibility
- Implements big-endian byte order conversion for PNG compliance

### Build Configuration
- **fpm.toml**: Standard Fortran Package Manager configuration
- **Auto-detection**: Automatically detects executables, tests, and examples
- **Library linking**: Links against system zlib library