from numba import njit
from pauli.field import afield_simple, bfield_simple, bmod_simple
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D


@njit
def velo(t, z):
    A = np.empty(3)
    B = np.empty(3)
    Bmod = np.empty(1)
    dBmod = np.empty(3)
    afield_simple(z[0], z[1], z[2], A)
    bfield_simple(z[0], z[1], z[2], B)
    bmod_simple(z[0], z[1], z[2], Bmod, dBmod)

    zdot = np.empty(6)
    zdot[0] = z[3]
    zdot[1] = z[4]
    zdot[2] = z[5]
    zdot[3] = z[4] * B[2] - z[5] * B[1]
    zdot[4] = z[5] * B[0] - z[3] * B[2]
    zdot[5] = z[3] * B[1] - z[4] * B[0]

    return zdot


# %%
z0 = np.array([1.0, 2.0, 3.0, 0.1, 0.1, 0.1])

sol = solve_ivp(velo, [0, 10], z0, method="RK45", rtol=1e-6, atol=1e-6)

# %%
plt.figure()
plt.plot(sol.y[0], sol.y[1])

# %%
plt.figure()
plt.plot(sol.y[1], sol.y[2])

# %%
