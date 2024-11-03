#%%
import numpy as np
from numba import njit

@njit
def single_bspline_hat(x, knots, degree, i):
    """Generates a single B-spline basis function."""
    n_knots = len(knots)
    if degree == 0:
        return np.where((x >= knots[i]) & (x < knots[i + 1]), 1.0, 0.0)
    else:
        left = (x - knots[i]) / (knots[i + degree] - knots[i] + 1e-10)
        right = (knots[i + degree + 1] - x) / (knots[i + degree + 1] - knots[i + 1] + 1e-10)
        return left * single_bspline_hat(x, knots, degree - 1, i) + \
               right * single_bspline_hat(x, knots, degree - 1, i + 1)

@njit
def generate_periodic_bspline_basis(n, degree, x):
    n_basis = n + 1 + degree
    basis = np.zeros((len(x), n_basis))

    for i in range(n_basis):
        evaluate_basis(n, degree, i, x, basis[:, i])

    return basis

@njit
def evaluate_basis(n, degree, i, x, result):
    """Evaluates the i-th B-spline basis function at the points x."""
    dx = 1.0/(n + 1 + degree)
    knots = np.linspace(0.0, 1.0 + degree*dx, n + 2*(1 + degree))
    n_basis = len(knots) - degree - 1
    if i < 0 or i >= n_basis:
        result[:] = 0.0

    if i > n:
        y = x.copy()
        y[x < (i-n)*dx] += 1.0
        result[:] = single_bspline_hat(y, knots, degree, i)
    else:
        result[:] = single_bspline_hat(x, knots, degree, i)
