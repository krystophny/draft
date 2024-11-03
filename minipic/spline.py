import numpy as np
from numba import njit

@njit
def periodic(fun, x, knots, degree, i):
    n = len(knots) - 2*(1 + degree)
    dx = 1.0/(n + 1 + degree)
    if i > n:
        y = x.copy()
        y[x < (i-n)*dx] += 1.0
        return fun(y, knots, degree, i)
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
    n_basis = n + 1 + degree
    basis = np.zeros((len(x), n_basis))

    for i in range(n_basis):
        evaluate(fun, n, degree, i, x, basis[:, i])

    return basis


@njit
def init_knots(n, degree):
    dx = 1.0/(n + 1 + degree)
    return np.linspace(0.0, 1.0 + degree*dx, n + 2*(1 + degree))


@njit
def evaluate(fun, n, degree, i, x, result):
    """Evaluates the i-th B-spline basis function at the points x."""
    dx = 1.0/(n + 1 + degree)
    knots = init_knots(n, degree)
    n_basis = len(knots) - degree - 1
    if i < 0 or i >= n_basis:
        result[:] = 0.0
    result[:] = periodic(fun, x, knots, degree, i)
