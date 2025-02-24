import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# Define parameters
Nx, Ny = 50, 50   # Grid points
Lx, Ly = 10, 10   # Domain size
U = 1.0           # Mean zonal flow
beta = 0.2        # Rossby parameter
dx, dy = Lx/Nx, Ly/Ny   # Grid spacing
T = 10            # Total time
Nt = 200          # Number of time steps
dt = T / Nt       # Time step size

# Initialize streamfunction psi
psi = np.zeros((Nx, Ny))

# Initial condition: Gaussian perturbation
x = np.linspace(0, Lx, Nx)
y = np.linspace(0, Ly, Ny)
X, Y = np.meshgrid(x, y, indexing='ij')
psi = np.exp(-((X - Lx/2)**2 + (Y - Ly/2)**2))

# Background jetstream profile
jetstream = np.sin(2 * np.pi * Y / Ly)  # Simplified jetstream profile
psi += 0.1 * jetstream  # Add jetstream effect

# Temperature field (simplified linear gradient)
temperature = 300 - 10 * (Y / Ly)  # Decreasing with latitude

# Crank-Nicholson matrices
alpha_x = U * dt / (4 * dx)
alpha_y = beta * dt / (4 * dy)

# Construct tridiagonal matrices
Ax = np.eye(Nx) - alpha_x * np.eye(Nx, k=1) + alpha_x * np.eye(Nx, k=-1)
Ay = np.eye(Ny) - alpha_y * np.eye(Ny, k=1) + alpha_y * np.eye(Ny, k=-1)
Ax_inv = np.linalg.inv(Ax)
Ay_inv = np.linalg.inv(Ay)

# Apply periodic boundary conditions
def apply_periodic_bc(field):
    field[0, :] = field[-2, :]
    field[-1, :] = field[1, :]
    field[:, 0] = field[:, -2]
    field[:, -1] = field[:, 1]
    return field

# Set up separate figures for animations
fig1, ax1 = plt.subplots()
fig2, ax2 = plt.subplots()
cbar1, cbar2 = None, None

def update_crank_nicholson(n):
    global psi, cbar1
    psi_new = Ax_inv @ psi @ Ay_inv  # Apply Crank-Nicholson step
    psi = apply_periodic_bc(psi_new)  # Update solution with periodic BC
    ax1.clear()
    contour = ax1.contourf(X, Y, psi, levels=20, cmap='RdBu_r')
    if cbar1 is None:
        cbar1 = fig1.colorbar(contour)
    ax1.set_title(f'Time step {n} (Crank-Nicholson)')

i_crank = animation.FuncAnimation(fig1, update_crank_nicholson, frames=Nt, interval=50)
plt.show(block=False)  # Show Crank-Nicholson animation in a separate window

# Ray tracing approach
num_rays = 10
kx, ky = np.linspace(-np.pi/Lx, np.pi/Lx, num_rays), np.linspace(-np.pi/Ly, np.pi/Ly, num_rays)
kx, ky = np.meshgrid(kx, ky)

x_rays, y_rays = np.full_like(kx, Lx/2), np.full_like(ky, Ly/2)  # Initial position

def update_ray_tracing(n):
    global x_rays, y_rays, cbar2
    omega = U * kx - beta * kx / (kx**2 + ky**2 + 1e-6)  # Avoid division by zero
    dx_dt = np.gradient(omega, axis=0) / (kx[1, 0] - kx[0, 0])
    dy_dt = np.gradient(omega, axis=1) / (ky[0, 1] - ky[0, 0])

    x_rays += dx_dt * dt
    y_rays += dy_dt * dt

    x_rays = np.mod(x_rays, Lx)  # Apply periodic boundary conditions
    y_rays = np.mod(y_rays, Ly)

    #ax2.clear()
    ax2.scatter(x_rays, y_rays, color='black', marker='o', label='Ray tracing')
    #contour = ax2.contourf(X, Y, psi, levels=20, cmap='RdBu_r', alpha=0.6)
    #if cbar2 is None:
    #    cbar2 = fig2.colorbar(contour)
    #ax2.legend()
    ax2.set_title(f'Time step {n} (Ray Tracing)')

i_rays = animation.FuncAnimation(fig2, update_ray_tracing, frames=Nt, interval=50)
plt.show(block=False)  # Show Ray Tracing animation in a separate window

# Keep both animations running
while plt.fignum_exists(fig1.number) and plt.fignum_exists(fig2.number):
    plt.pause(0.1)
