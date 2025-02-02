import numpy as np
from numba import njit
from pauli.util import legendre_p, assoc_legendre_p

mu0 = 4e-7 * np.pi

# Fields are normalized to 1A*mu0/2pi


@njit
def afield_wire_cyl(R, phi, Z, A):
    A[0] = 0.0
    A[1] = 0.0
    A[2] = -np.log(R)


@njit
def bfield_wire_cyl(R, phi, Z, B):
    B[0] = 0.0
    B[1] = phi / R
    B[2] = 0.0


@njit
def bfield_circle_cyl(R, phi, Z, B):
    """
    Calculate the magnetic field of a circular loop in cylindrical coordinates.
    See https://farside.ph.utexas.edu/teaching/jk1/Electromagnetism/node52.html
    """
    LMAX = 32
    B[:] = 0.0

    r = np.sqrt(R**2 + Z**2)
    costh = Z / r
    sinth = R / r
    for l in np.arange(1, LMAX, 2):
        Pl10 = assoc_legendre_p(l, 1, 0.0)
        Br = Pl10 * legendre_p(l, costh)
        Bth = Pl10 * assoc_legendre_p(l, 1, costh) / l
        if r < 1:
            Br *= r ** (l - 1.0)
            Bth *= r ** (l - 1.0) / l
        else:
            Br *= r ** (-l - 2.0)
            Bth *= r ** (-l - 2.0) / (l + 1.0)

        B[0] += Br * sinth + Bth * costh
        B[2] += Br * costh - Bth * sinth

    B[:] = -np.pi * B[:]
