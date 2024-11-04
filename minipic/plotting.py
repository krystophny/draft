import matplotlib.pyplot as plt
from minipic import *

def plot_phi(x, phi_h):
    phi = np.empty_like(x)
    compute_phi_at(x, phi_h, phi)
    plt.plot(x, phi, 'o-')

def plot_particles(x, p):
    plt.plot(x, p, '.')
    plt.xlim(0.0, 1.0)

def plot_orbit(x, p):
    plt.plot(x, p, ',')
    plt.xlim(0.0, 1.0)
    plt.ylim(-3.0, 3.0)
