import numpy as np
from numba import njit

@njit
def lagrange_basis(x, order):
