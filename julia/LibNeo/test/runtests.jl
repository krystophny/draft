using Test
using LibNeo.Interpolate

@testset "LibNeo Tests" begin
    @test 1 + 1 == 2
end

# Constants matching the Fortran code
const TOL = 1.0e-6
const TOL_EXACT = 1.0e-11
const X_MIN = 1.23
const X_MAX = 2π + 1.23

"""
Test 1D spline interpolation with different orders and periodicity settings.
Similar to test_spline_1d in the Fortran code.
"""
function test_splines_1d(spline_order::Int, periodic::Bool)
    N_POINTS = 100

    # Create test data points
    x = range(X_MIN, X_MAX, length=N_POINTS)
    y = cos.(x)

    # Construct splines
    spl = construct_splines_1d(X_MIN, X_MAX, y, spline_order, periodic)

    # Test point between grid points
    x_eval = 0.5 * (x[10] + x[11])

    # Expected values
    expected = cos(x_eval)

    # Test function value
    actual = evaluate_splines_1d(spl, x_eval)
    @test abs(expected - actual) < TOL

end

@testset "1D Splines" begin
    # Test different spline orders and periodicity combinations
    @testset "Order $order, Periodic=$periodic" for order in [3,5], periodic in [false, true]
        test_splines_1d(order, periodic)
    end
end
