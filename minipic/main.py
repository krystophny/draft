#%%
import numpy as np
from numba import njit

from minifem import *
from minipic import *
from plotting import *

np.random.seed(42)

n_particles = 1000
n_grid = num_elements
order = element_order
n_steps = 100000
n_plot = 10000
plot_every = n_steps // n_plot
dt = 0.01

x = np.random.rand(n_particles)
p = np.random.rand(n_particles) - 0.5
x_grid = np.linspace(0, 1, n_grid)

@njit
def phi0(x):
    return np.cos(2*np.pi*(x-0.5))
phi_h = project(phi0)
test = evaluate(np.linspace(0.1,0.9,100), phi_h)

#plt.plot(test)

plot_phi(x_grid, phi_h)

#%%


x_plot = np.empty((n_steps, n_particles))
p_plot = np.empty((n_steps, n_particles))

x_plot[0, :] = x
p_plot[0, :] = p

main_loop(x, p, phi_h, x_plot, p_plot, n_steps, plot_every, dt)


#%%
plot_particles(x, p)

#%%
plot_orbit(x_plot[:,:3], p_plot[:,:3])

# %%
