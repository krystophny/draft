#%%
import numpy as np
from numba import njit

from spline import *
from minipic import *
from plotting import *

np.random.seed(42)

n_particles = 1000
n_grid = 3
order = 5
n_steps = 10000
n_plot = 1000
plot_every = n_steps // n_plot
dt = 0.01

x = np.random.rand(n_particles)
p = np.random.rand(n_particles) - 0.5
x_grid = np.linspace(0.0, 1.0, n_grid)

@njit
def phi0(x):
    return np.cos(2*np.pi*(x-0.5))

@njit
def dphi0dx(x):
    return -2*np.pi*np.sin(2*np.pi*(x-0.5))

solver = PoissonSolver(n_grid, order)

x_plot = np.linspace(0.0, 1.0, 1000)
phi_h = solver.discretize(phi0)
phi_num = solver.evaluate(phi_h, np.mod(x_plot, 1.0))
phi_ref = phi0(x_plot)

plt.figure()
plt.plot(x_plot, phi_num)
plt.plot(x_plot, phi_ref, '--')

phi_der_num = solver.evaluate_derivative(phi_h, x_plot)
phi_der_ref = dphi0dx(x_plot)

plt.figure()
plt.plot(x_plot, phi_der_num)
plt.plot(x_plot, phi_der_ref, '--')


#plot_phi(x_grid, phi_h)

#%%

x_plot = np.empty((n_steps, n_particles))
p_plot = np.empty((n_steps, n_particles))

x_plot[0, :] = x
p_plot[0, :] = p

main_loop(x, p, solver, phi_h, x_plot, p_plot, n_steps, plot_every, dt)

#%%
plot_particles(x, p)

#%%
plot_orbit(x_plot[:,:3], p_plot[:,:3])

# %%
