#%%
import numpy as np
from minipic import *
from plotting import *

np.random.seed(42)

n_particles = 1000
n_grid = 1000
n_steps = 100000
n_plot = 10000
plot_every = n_steps // n_plot
dt = 0.01

x = np.random.rand(n_particles)
p = np.random.rand(n_particles) - 0.5
x_grid = np.linspace(0, 1, n_grid)
phi_h = project_nodal(x_grid, lambda x: np.cos(2*np.pi*(x-0.5)))

x_plot = np.empty((n_steps, n_particles))
p_plot = np.empty((n_steps, n_particles))

x_plot[0, :] = x
p_plot[0, :] = p

main_loop(x, p, phi_h, x_plot, p_plot, n_steps, plot_every, dt)

#%%
plot_phi(x_grid, phi_h)

#%%
plot_particles(x, p)

#%%
plot_orbit(x_plot[:,:3], p_plot[:,:3])

# %%
