import numpy as np
from numba import njit

from spline import *

@njit
def main_loop(x, p, solver, phi_h, x_plot, p_plot, n_steps, plot_every, dt):
    for i in np.arange(1, n_steps-1):
        timestep(x, p, solver, phi_h, dt)
        if i % plot_every == 0:
            x_plot[i//plot_every, :] = x
            p_plot[i//plot_every, :] = p

@njit
def timestep(x, p, solver, phi_h, dt):
    timestep_particles(x, p, solver, phi_h, dt)
    timestep_field(x, p, solver, phi_h, dt)

@njit
def timestep_particles(x, p, phi_h, dt):
    E = np.empty_like(x)
    x[:] = (x + p*dt) % 1.0
    compute_E_at(x, solver, phi_h, E)
    p[:] = p + E*dt

@njit
def timestep_field(x, p, phi_h, dt):
    pass

@njit
def compute_E_at(x, solver, phi_h, E):
    evaluate_derivative(x, solver, phi_h, E)

@njit
def compute_phi_at(x, solver, phi_h, phi):
    phi[:] = solver.evaluate(phi_h, x)
