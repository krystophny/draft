import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from scipy.integrate import solve_ivp
from scipy.fft import fft2, fftshift

# Parameters
dx = dy = 1.0  # Grid spacing
Lx, Ly = 200, 100  # Domain size
Nx, Ny = int(Lx/dx), int(Ly/dy)  # Number of grid points
nt = 500  # Number of time steps

# Constants
beta = 1.0e-3  # Beta parameter (Rossby parameter)
U0 = 10.0  # Jet stream velocity scale
V0 = 2.0  # Meridional flow scale
dt = 0.25 * dx / U0  # CFL condition for stability
robert_alpha = 0.05  # Adjusted Robert filter coefficient

# Create grid
x = np.linspace(0, Lx, Nx, endpoint=False)
y = np.linspace(0, Ly, Ny, endpoint=False)
X, Y = np.meshgrid(x, y)

# Background flow (fully 2D flow)
U = U0 * np.exp(-((Y - Ly/2) / (Ly/10))**2)  # Zonal jet centered at Ly/2
V = V0 * np.sin(2 * np.pi * X / Lx) * np.exp(-((Y - Ly/2) / (Ly/15))**2)  # Meridional flow component

def U_background(x, y):
    return U0 * np.exp(-((y - Ly/2) / (Ly/10))**2)

def V_background(x, y):
    return V0 * np.sin(2 * np.pi * x / Lx) * np.exp(-((y - Ly/2) / (Ly/15))**2)

# Initial condition: Gaussian wave packet with wavenumber in x and y directions
def gaussian_packet(X, Y, x0=Lx/2, y0=Ly/2, sigma=10):
    return np.exp(-((X-x0)**2 + (Y-y0)**2) / (2*sigma**2)) * np.cos(4*np.pi*X/Lx + 4*np.pi*Y/Ly)

psi = gaussian_packet(X, Y)
psi_new = np.copy(psi)
psi_old = np.copy(psi)

# Compute dominant wavenumber using Fourier Transform
psi_fft = fft2(psi)
psi_fft_shifted = fftshift(np.abs(psi_fft))
kx_vals = np.fft.fftfreq(Nx, d=dx) * 2 * np.pi
ky_vals = np.fft.fftfreq(Ny, d=dy) * 2 * np.pi
KX, KY = np.meshgrid(kx_vals, ky_vals)
kx, ky = KX[np.unravel_index(np.argmax(psi_fft_shifted), psi_fft_shifted.shape)], KY[np.unravel_index(np.argmax(psi_fft_shifted), psi_fft_shifted.shape)]

# Track the wave packet center of mass
def wave_packet_center(psi):
    total_mass = np.sum(np.abs(psi))
    x_cm = np.sum(X * np.abs(psi)) / total_mass
    y_cm = np.sum(Y * np.abs(psi)) / total_mass
    return x_cm, y_cm

# Time stepping loop
def update(frame):
    global psi, psi_new, psi_old

    # Compute Laplacian using improved scheme
    laplacian_term = (np.roll(psi, 1, axis=0) + np.roll(psi, -1, axis=0) +
                      np.roll(psi, 1, axis=1) + np.roll(psi, -1, axis=1) - 4*psi) / dx**2

    # Background advection term (consistent with ray-tracing equations)
    dpsi_dx = (np.roll(psi, -1, axis=1) - np.roll(psi, 1, axis=1)) / (2*dx)
    dpsi_dy = (np.roll(psi, -1, axis=0) - np.roll(psi, 1, axis=0)) / (2*dy)
    advection = -U * dpsi_dx - V * dpsi_dy

    # Beta effect term
    beta_term = beta * dpsi_dx

    # Time stepping (semi-implicit with Robert filter)
    psi_new = (1 - robert_alpha) * (2 * psi - psi_old + dt**2 * (laplacian_term + advection + beta_term)) + robert_alpha * psi_old

    # Update fields
    psi_old = np.copy(psi)
    psi = np.copy(psi_new)

    # Update wave packet center
    x_cm, y_cm = wave_packet_center(psi)
    ray_point.set_data([x_cm], [y_cm])
    im.set_array(psi)
    return [im, ray_point]

# Set up animation
fig, ax = plt.subplots()
im = ax.imshow(psi, extent=[0, Lx, 0, Ly], origin='lower', cmap='RdBu_r')
ax.set_title("Rossby Wave Evolution with Corrected Center Tracking")
ax.set_xlabel("x")
ax.set_ylabel("y")
ray_point, = ax.plot([], [], 'ko', markersize=5, label='Wave Packet Center')
ax.legend()

ani = animation.FuncAnimation(fig, update, frames=nt, interval=1)
plt.show()
