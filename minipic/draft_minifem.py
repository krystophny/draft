#%%
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import lagrange
from scipy.integrate import quad

# Step 1: Define the 1D domain
num_elements = 5          # Number of finite elements
element_order = 3          # Order of the Lagrange polynomial (cubic)
num_nodes = num_elements * element_order + 1  # Total number of nodes (with periodicity)

# Generate nodes across the domain with periodic boundary conditions
x_nodes = np.linspace(0, 1, num_nodes, endpoint=False)

# Step 2: Define basis functions (Lagrange polynomials) on the reference element
def lagrange_basis(order):
    nodes = np.linspace(0, 1, order + 1)
    return [lagrange(nodes, [1 if i == j else 0 for j in range(order + 1)]) for i in range(order + 1)]

basis = lagrange_basis(element_order)

# Step 3: Assemble the mass matrix with periodic boundary condition
mass_matrix = np.zeros((num_nodes, num_nodes))

for element in range(num_elements):
    for i in range(element_order + 1):
        for j in range(element_order + 1):
            # Basis functions for current nodes i, j within each element
            integrand = lambda x: basis[i](x) * basis[j](x)
            M_ij = quad(integrand, 0, 1)[0]
            global_i = (element * element_order + i) % num_nodes
            global_j = (element * element_order + j) % num_nodes
            mass_matrix[global_i, global_j] += M_ij

# Step 4: Project sin(2*pi*x) onto the finite element space
rhs = np.zeros(num_nodes)
f = lambda x: np.sin(2 * np.pi * x)

for element in range(num_elements):
    for i in range(element_order + 1):
        integrand = lambda x: f(element / num_elements + x / num_elements) * basis[i](x)
        b_i = quad(integrand, 0, 1)[0]
        global_i = (element * element_order + i) % num_nodes
        rhs[global_i] += b_i

# Solve for the finite element coefficients
coefficients = np.linalg.solve(mass_matrix, rhs)

# Step 5: Evaluate the projection over a finer grid
x_fine = np.linspace(0, 1, 500)
projection = np.zeros_like(x_fine)

# Loop through each element and sum contributions from each basis function
for element in range(num_elements):
    # Map fine points within each element
    local_x_fine = x_fine[(x_fine >= element / num_elements) & (x_fine < (element + 1) / num_elements)]
    local_projection = np.zeros_like(local_x_fine)

    for i in range(element_order + 1):
        global_i = (element * element_order + i) % num_nodes
        local_x_mapped = (local_x_fine - element / num_elements) * num_elements
        local_projection += coefficients[global_i] * basis[i](local_x_mapped)

    projection[(x_fine >= element / num_elements) & (x_fine < (element + 1) / num_elements)] = local_projection

# Plot the original function and the projection
y_exact = np.sin(2 * np.pi * x_fine)
plt.plot(x_fine, y_exact, label="Exact sin(2πx)")
plt.plot(x_fine, projection, 'r--', label="Projection (Order 3 Lagrange)")
plt.legend()
plt.xlabel("x")
plt.ylabel("Function Value")
plt.title("Projection of sin(2πx) onto Order 3 Lagrange Finite Element Space")
plt.show()

# %%
