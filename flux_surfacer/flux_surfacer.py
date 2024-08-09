#%%
import numpy as np
from numpy.linalg import lstsq
from numba import njit

np.random.seed(42)

def fourier_coefficients(x, M):
    num_dim = x.shape[0]
    num_samples = x.shape[1]

    theta = np.arctan2(x[1, :], x[0, :])
    A = design_matrix(theta, M)

    x_c = np.zeros((num_dim, M + 1))
    x_s = np.zeros((num_dim, M + 1))

    for d in range(num_dim):
        coeffs, _, _, _ = lstsq(A, x[d, :], rcond=1e-4)
        x_c[d, :] = coeffs[::2]
        x_s[d, :] = coeffs[1::2]

    return x_c, x_s

@njit
def design_matrix(theta, M):
    A = np.zeros((len(theta), 2 * (M + 1)))
    for m in range(M + 1):
        A[:, 2 * m] = np.cos(m * theta)
        A[:, 2 * m + 1] = np.sin(m * theta)
    return A


@njit
def evaluate_fourier_series(x_c, x_s, theta):
    M = x_c.shape[1] - 1
    N = len(theta)
    x = np.zeros((2, N))
    for n in range(N):
        for m in range(M + 1):
            x[:,n] += x_c[:,m] * np.cos(m * theta[n]) + x_s[:,m] * np.sin(m * theta[n])
    return x


@njit
def generate_circle_points(theta, radius=1.0, center=(1, 0), noise_level=0.1):
    n_points = len(theta)
    x = np.zeros((2, n_points))
    x[0,:] = center[0] + radius * np.cos(theta) + noise_level * np.random.normal(0, 1, n_points)
    x[1,:] = center[1] + radius * np.sin(theta) + noise_level * np.random.normal(0, 1, n_points)
    return x
