import numpy as np
from numba import njit

eps = 1e-2

@njit
def discretize(f, n):
    knots = np.linspace(0.0, 1.0, 2*n)[:-1]
    values = f(knots)
    f_h = np.empty(n, dtype=np.complex128)
    for k in range(n):
        f_h[k] = np.sum(values*np.exp(-1.0j*2*np.pi*k*knots))/(0.5*(2*n-1))
    return f_h


@njit
def evaluate(f_h, x):
    n = len(f_h)
    f = np.empty_like(x)
    for i in range(len(x)):
        f[i] = np.real(np.sum(f_h*np.exp(1.0j*2*np.pi*np.arange(n)*x[i])))
    return f


@njit
def evaluate_derivative(f_h, x):
    n = len(f_h)
    f = np.empty_like(x)
    for i in range(len(x)):
        k = np.arange(n)
        f[i] = np.real(1.0j*2*np.pi*np.sum(k*f_h*np.exp(1.0j*2*np.pi*k*x[i])))
    return f


@njit
def main_loop(x, p, phi_h, x_plot, p_plot, n_steps, plot_every, dt):
    for i in np.arange(1, n_steps-1):
        timestep(x, p, phi_h, dt)
        if i % plot_every == 0:
            x_plot[i//plot_every, :] = x
            p_plot[i//plot_every, :] = p


@njit
def timestep(x, p, phi_h, dt):
    timestep_particles(x, p, phi_h, dt)
    timestep_field(x, p, phi_h, dt)


@njit
def timestep_particles(x, p, phi_h, dt):
    E = np.empty_like(x)
    x[:] = (x + p*dt) % 1.0
    compute_E_at(x, phi_h, E)
    p[:] = p + E*dt


@njit
def timestep_field(x, p, phi_h, dt):
    pass
    # n = len(phi_h)
    # k = np.arange(n)
    # rho_h = rho_h_from_particles(x, n)
    # phi_h[1:] = eps*rho_h[1:]/k[1:]**2


@njit
def compute_E_at(x, phi_h, E):
    E[:] = evaluate_derivative(phi_h, x)


@njit
def compute_phi_at(x, phi_h, phi):
    phi[:] = evaluate(phi_h, x)


@njit
def rho_h_from_particles(x, n):
    rho_h = np.zeros(n, dtype=np.complex128)
    k = np.arange(n)
    for i in range(len(x)):
        rho_h += np.exp(-1.0j*2*np.pi*k*x[i])
    return rho_h


# generate normal distribution
xtest = np.random.randn(1000)/20.0 + 0.5
rho_h = rho_h_from_particles(xtest, 16)
x = np.linspace(0.0, 1.0, 1000)
rho = evaluate(rho_h, x)

import matplotlib.pyplot as plt
plt.figure()
plt.plot(x, rho)
