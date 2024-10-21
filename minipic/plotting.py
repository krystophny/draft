import matplotlib.pyplot as plt
from minipic import *

def plot_phi(x, phi_h):
    phi = compute_phi_at(x, phi_h)
    plt.plot(x, phi_h, 'o-')

def plot_particles(x, p):
    plt.plot(x, p, 'o')
    plt.xlim(0.0, 1.0)

def plot_orbit(x, p):
    plt.plot(x, p, '.')
    plt.xlim(0.0, 1.0)
    plt.ylim(-2.0, 2.0)
