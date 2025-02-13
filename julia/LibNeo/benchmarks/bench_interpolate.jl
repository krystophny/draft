#!/usr/bin/env julia

using BenchmarkTools
using LibNeo
using Printf

# Constants matching the Fortran tests
const X_MIN = 1.23
const X_MAX = 2π + 1.23

"""
    benchmark_spline(order, periodic)

Constructs a spline of the specified order and periodicity from test data,
and then benchmarks the evaluation at a chosen point.
"""
function benchmark_spline(order::Int, periodic::Bool)
    # Use a larger number of points for a robust benchmark.
    N_POINTS = 1000
    x = range(X_MIN, X_MAX, length=N_POINTS)
    y = cos.(x)

    @printf("\nBenchmarking spline order %d, periodic = %s\n", order, string(periodic))
    # Construct the spline (this is not timed here)
    spl = construct_splines_1d(X_MIN, X_MAX, y, order, periodic)

    # Choose an evaluation point (for example, the midpoint)
    x_eval = (X_MIN + X_MAX) / 2
    @printf("Evaluating spline at x = %g\n", x_eval)

    # Benchmark the evaluation function.
    bench_result = @benchmark evaluate_splines_1d($spl, $x_eval)
    println("Benchmark results:")
    display(bench_result)
end

function main()
    # Loop over different spline orders and periodic settings.
    for order in [3, 5]
        for periodic in (false, true)
            benchmark_spline(order, periodic)
        end
    end
end

main()
