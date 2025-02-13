include("splines.jl")

struct SplineData1D
    order::Int
    num_points::Int
    periodic::Bool
    x_min::Float64
    h_step::Float64
    coeff::Matrix{Float64}
end

function construct_splines_1d(x_min::Float64, x_max::Float64, y::Vector{Float64}, order::Int, periodic::Bool)
    num_points = length(y)
    h_step = (x_max - x_min) / (num_points - 1)
    coeff = zeros(Float64, order+1, num_points)
    coeff[1, :] = y

    if periodic
        spl_per!(order, num_points, h_step, coeff)
    else
        spl_reg!(order, num_points, h_step, coeff)
    end

    return SplineData1D(order, num_points, periodic, x_min, h_step, coeff)
end

function destroy_splines_1d(spl::SplineData1D)
    spl.coeff = nothing
end

function evaluate_splines_1d(spl::SplineData1D, x::Float64)
    if spl.periodic
        xj = mod(x, spl.h_step * (spl.num_points - 1))
    else
        xj = x
    end
    x_norm = (xj - spl.x_min) / spl.h_step
    interval_index = max(0, min(spl.num_points-1, Int(floor(x_norm))))
    x_local = (x_norm - interval_index) * spl.h_step

    coeff_local = spl.coeff[:, interval_index+1]
    y = coeff_local[end]
    for k_power in spl.order-1:-1:0
        y = coeff_local[k_power+1] + x_local * y
    end
    return y
end
