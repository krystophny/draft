import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# Parameters
dx = dy = 1.0  # Grid spacing
Lx, Ly = 200, 100  # Domain size
Nx, Ny = int(Lx/dx), int(Ly/dy)  # Number of grid points
nt = 500  # Number of time steps

# Constants
beta = 1.0e-3  # Beta parameter (Rossby parameter)
U0 = 10.0  # Jet stream velocity scale
dt = 0.5 * dx / U0  # CFL condition for stability
robert_alpha = 0.1  # Robert filter coefficient

# Create grid
x = np.linspace(0, Lx, Nx, endpoint=False)
y = np.linspace(0, Ly, Ny, endpoint=False)
X, Y = np.meshgrid(x, y)

# Background flow (zonal jet)
U = U0 * np.exp(-((Y - Ly/2) / (Ly/10))**2)  # Gaussian jet centered at Ly/2

# Initial condition: Gaussian wave packet with wavenumber in x and y directions
kx, ky = 4*np.pi/Lx, 4*np.pi/Ly  # Increased wavenumbers for dispersion
def gaussian_packet(X, Y, x0=Lx/2, y0=Ly/2, sigma=10):
    return np.exp(-((X-x0)**2 + (Y-y0)**2) / (2*sigma**2)) * np.cos(kx*X + ky*Y)

psi = gaussian_packet(X, Y)
psi_new = np.copy(psi)
psi_old = np.copy(psi)

# Time stepping loop
def update(frame):
    global psi, psi_new, psi_old

    # Finite difference for Laplacian
    laplacian = (
        np.roll(psi, 1, axis=0) + np.roll(psi, -1, axis=0) +
        np.roll(psi, 1, axis=1) + np.roll(psi, -1, axis=1) - 4*psi) / dx**2

    # Background advection term
    dpsi_dx = (np.roll(psi, -1, axis=1) - np.roll(psi, 1, axis=1)) / (2*dx)
    advection = -U * dpsi_dx

    # Beta effect term
    beta_term = beta * dpsi_dx

    # Time stepping (leapfrog with Robert filter)
    psi_new = 2 * psi - psi_old + dt**2 * (laplacian + advection + beta_term)
    psi_new = (1 - robert_alpha) * psi_new + robert_alpha * psi_old  # Robert filter

    # Update fields
    psi_old = np.copy(psi)
    psi = np.copy(psi_new)

    # Update plot
    im.set_array(psi)
    return [im]

# Set up animation
fig, ax = plt.subplots()
im = ax.imshow(psi, extent=[0, Lx, 0, Ly], origin='lower', cmap='RdBu_r')
ax.set_title("Rossby Wave Evolution")
ax.set_xlabel("x")
ax.set_ylabel("y")

ani = animation.FuncAnimation(fig, update, frames=nt, interval=50)
plt.show()
