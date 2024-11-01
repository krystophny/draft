import numpy as np
from numba import njit

@njit
def main_loop(x, p, phi_h, x_plot, p_plot, n_steps, plot_every, dt):
    for i in np.arange(1, n_steps-1):
        timestep(x, p, phi_h, dt)
        if i % plot_every == 0:
            x_plot[i//plot_every, :] = x
            p_plot[i//plot_every, :] = p

@njit
def timestep(x, p, phi_h, dt):
    E = np.empty_like(x)
    x[:] = (x + p*dt) % 1.0
    compute_E_at(x, phi_h, E)
    p[:] = p + E*dt

@njit
def compute_E_at(x, phi_h, E):
    return compute_multiple(compute_E_at_single, x, phi_h, E)

@njit
def compute_phi_at(x, phi_h, phi):
    return compute_multiple(compute_phi_at_single, x, phi_h, phi)

@njit
def compute_E_at_single(x, phi_h):
    N = len(phi_h)
    dx = 1.0 / N
    x = x % 1.0
    i = int(x // dx)
    return (phi_h[(i + 1) % N] - phi_h[i]) / dx

@njit
def compute_phi_at_single(x, phi_h):
    N = len(phi_h)
    dx = 1.0 / N
    x = x % 1.0
    i = int(x // dx)
    x_i = i * dx
    x_ip1 = (i + 1) % N * dx
    return linear_interpolation(x, x_i, x_ip1, phi_h[i], phi_h[(i + 1) % N])

@njit
def compute_multiple(func, x, phi_h, result):
    N = len(x)
    for i in range(N):
        result[i] = func(x[i], phi_h)

def project_nodal(x, phi_func):
    N = len(x)
    phi_h = np.zeros(N)
    for i in range(N):
        phi_h[i] = phi_func(x[i])
    return phi_h

@njit
def linear_interpolation(x, x0, x1, y0, y1):
    return y0 + (y1 - y0) / (x1 - x0) * (x - x0)
