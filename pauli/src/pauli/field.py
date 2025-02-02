import numpy as np
from numba import njit

mu0 = 4e-7 * np.pi

# Fields are normalized to 1A*mu0/2pi

@njit
def afield_wire_cyl(R, phi, Z, A):
    A[0] = 0.0
    A[1] = 0.0
    A[2] = - np.log(R)

@njit
def bfield_wire_cyl(R, phi, Z, B):
    B[0] = 0.0
    B[1] = phi / R
    B[2] = 0.0

# @njit
# def bfield_circle_cyl(R, phi, Z, B):
#     LMAX = 100
#     B[:] = 0.0
#     for l in np.arange(1, LMAX, 2):
#         B[0] +=

#     B[:] = -np.pi*B[:]
