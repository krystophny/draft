import numpy as np
from scipy.special import ellipk, ellipe
from numba import jit, njit
from pauli.util import legendre_p, assoc_legendre_p

EPS = 1e-13
mu0 = 4e-7 * np.pi

# Fields are normalized to 1A*mu0/2pi


def bfield_wire(X, Y, Z, B):
    Bcyl = np.zeros(3)
    cyl_to_cart(bfield_wire_cyl, X, Y, Z, B)


def bfield_circle(X, Y, Z, B):
    Bcyl = np.zeros(3)
    cyl_to_cart(bfield_circle_cyl, X, Y, Z, B)


@njit
def afield_wire_cyl(R, phi, Z, A):
    A[0] = 0.0
    A[1] = 0.0
    A[2] = -np.log(max(R, EPS))


@njit
def bfield_wire_cyl(R, phi, Z, B):
    B[0] = 0.0
    B[1] = 1.0 / max(R, EPS)
    B[2] = 0.0


def bfield_circle_cyl(R, phi, Z, B):
    """
    Calculate the magnetic field of a circular loop in cylindrical coordinates.
    http://plasmalab.pbworks.com/w/file/fetch/109941181/Bfield-calcs.pdf
    https://tiggerntatie.github.io/emagnet-py/offaxis/off_axis_loop.html
    """

    a = 1.0
    r = max(np.sqrt(R**2 + Z**2), EPS)

    alpha = r / a
    beta = Z / a
    gamma = Z / r
    Q = (1 + alpha) ** 2 + beta**2
    k2 = 4 * alpha / Q

    K = ellipk(k2)
    E = ellipe(k2)

    B[0] = (
        gamma
        / (np.pi * np.sqrt(Q))
        * (E * (1 + alpha**2 + beta**2) / (Q - 4 * alpha) - K)
    )
    B[2] = (
        1.0
        / (np.pi * np.sqrt(Q))
        * (E * (1 - alpha**2 - beta**2) / (Q - 4 * alpha) + K)
    )


@njit
def bfield_circle_cyl2(R, phi, Z, B):
    """
    Calculate the magnetic field of a circular loop in cylindrical coordinates.
    See https://farside.ph.utexas.edu/teaching/jk1/Electromagnetism/node52.html
    Unstable at r=a
    """
    LMAX = 128
    B[:] = 0.0

    r = max(np.sqrt(R**2 + Z**2), EPS)
    costh = Z / r
    sinth = R / r
    for l in np.arange(1, LMAX, 2):
        Pl10 = assoc_legendre_p(l, 1, 0.0)
        Br = Pl10 * legendre_p(l, costh)
        Bth = Pl10 * assoc_legendre_p(l, 1, costh)
        if r < 1:
            Br *= r ** (l - 1.0)
            Bth *= r ** (l - 1.0) / l
        else:
            Br *= r ** (-l - 2.0)
            Bth *= -(r ** (-l - 2.0)) / (l + 1.0)

        B[0] += Br * sinth + Bth * costh
        B[2] += Br * costh - Bth * sinth

    B[:] = -np.pi * B[:]


def cyl_to_cart(fun, X, Y, Z, B):
    Bcyl = np.zeros(3)
    R = np.sqrt(X**2 + Y**2)
    phi = np.arctan2(Y, X)
    fun(R, phi, Z, Bcyl)

    cosphi = X / max(R, EPS)
    sinphi = Y / max(R, EPS)
    B[0] = Bcyl[0] * cosphi - Bcyl[1] * sinphi
    B[1] = Bcyl[0] * sinphi + Bcyl[1] * cosphi
    B[2] = Bcyl[2]
