import numpy as np
from numba import njit

@njit
def periodic(fun, x, knots, degree, i):
    n = len(knots) - 2*degree - 1
    dx = 1.0/(n + degree)
    if i >= n:
        y = np.empty_like(x)
        y[:] = x
        y[x < (i-n+1)*dx] += 1.0
        ret = fun(y, knots, degree, i)
        ret[x < 0.0] = 0.0
        ret[x >= 1.0] = 0.0
        return ret
    else:
        return fun(x, knots, degree, i)


@njit
def bspline_hat(x, knots, degree, i):
    """Generates a single B-spline basis function."""
    n_knots = len(knots)
    if degree == 0:
        return np.where((x >= knots[i]) & (x < knots[i + 1]), 1.0, 0.0)
    else:
        left = (x - knots[i]) / (knots[i + degree] - knots[i])
        right = (knots[i + degree + 1] - x) / (knots[i + degree + 1] - knots[i + 1])
        return left * bspline_hat(x, knots, degree - 1, i) + \
               right * bspline_hat(x, knots, degree - 1, i + 1)


@njit
def bspline_hat_derivative(x, knots, degree, i):
    """Generates the derivative of a single B-spline basis function."""
    n_knots = len(knots)
    if degree == 0:
        return np.where((x >= knots[i]) & (x < knots[i + 1]), 1.0, 0.0)
    else:
        left = 1.0 / (knots[i + degree] - knots[i])
        right = -1.0 / (knots[i + degree + 1] - knots[i + 1])
        return left * bspline_hat(x, knots, degree - 1, i) + \
               right * bspline_hat(x, knots, degree - 1, i + 1)


@njit
def generate(fun, n, degree, x):
    n_basis = n + degree
    basis = np.zeros((len(x), n_basis))

    for i in range(n_basis):
        evaluate(fun, n, degree, i, x, basis[:, i])

    return basis


@njit
def init_knots(n, degree):
    dx = 1.0/(n + degree)
    return np.linspace(0.0, 1.0 + degree*dx, n + 2*degree + 1)


@njit
def evaluate(fun, n, degree, i, x, result):
    """Evaluates the i-th B-spline basis function at the points x."""
    dx = 1.0/(n + degree)
    knots = init_knots(n, degree)
    n_basis = len(knots) - degree - 1
    if i < 0 or i >= n_basis:
        result[:] = 0.0
    result[:] = periodic(fun, x, knots, degree, i)


@njit
def gauss_legendre_quadrature():
    points = np.array([-0.7745966692414834, 0.0, 0.7745966692414834])
    weights = np.array([0.2777777777777778, 0.4444444444444444, 0.2777777777777778])
    return points, weights

import numpy as np
from numba import njit


@njit
def assemble_matrix(fun, knots, degree):
    n_basis = len(knots) - degree - 1
    quad_points, quad_weights = gauss_legendre_quadrature()
    band_width = degree + 1
    matrix = np.zeros((n_basis, n_basis))

    for i in range(band_width):
        start = knots[i]
        end = knots[i + degree + 1]

        mapped_points = start + (end - start) * 0.5 * (quad_points + 1)
        integrand_values = np.zeros_like(mapped_points)

        b_i = fun(mapped_points, knots, degree, 0)
        b_j = fun(mapped_points, knots, degree, i)

        integrand_values = b_i * b_j
        print(i, start, end, mapped_points, b_i, b_j, integrand_values)

        value = np.sum(integrand_values * quad_weights) * (end - start) * 0.5

        for j in range(n_basis):
            matrix[j, np.mod(j + i, n_basis)] = value
            matrix[np.mod(j + i, n_basis), j] = value

    return matrix
