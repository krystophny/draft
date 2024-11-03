#%%
import numpy as np
from numba import jit

@jit(nopython=True)
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

def generate_periodic_bspline_basis(n, degree, x):
    """Generates the set of periodic B-spline basis functions."""
    dx = 1.0/(n + 1 + degree)
    knots = np.arange(0, 1.0+3*dx, step=dx)
    n_basis = len(knots) - degree - 1

    basis = np.zeros((len(x), n_basis))

    for i in range(n_basis):
        basis[:, i] = single_bspline_hat(x, knots, degree, i)

    return basis

# Example usage:
degree = 3
n = 11  # Number of basis functions
x = np.linspace(0, 1.5, 1000)
basis = generate_periodic_bspline_basis(n, degree, x)

import matplotlib.pyplot as plt
plt.plot(x, basis)
# %%
