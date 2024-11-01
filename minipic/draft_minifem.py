#%%
import numpy as np
import matplotlib.pyplot as plt
import numba



@numba.njit
def evaluate_projection(x_fine, coefficients, num_elements, element_order, num_nodes):
    projection = np.zeros_like(x_fine)
    nodes = lagrange_basis_nodes(element_order)

    for element in range(num_elements):
        element_start = element * element_order
        for i in range(element_order + 1):
            global_i = element_start + i
            if global_i >= num_nodes:
                global_i -= num_nodes
            for k in range(len(x_fine)):
                x = x_fine[k]
                if element / num_elements <= x < (element + 1) / num_elements:
                    x_mapped = (x - element / num_elements) * num_elements
                    projection[k] += coefficients[global_i] * lagrange_basis(x_mapped, element_order, i, nodes)
    return projection

# Main function to run the finite element projection
def run_projection(num_elements=10, element_order=3):
    num_nodes = num_elements * element_order  # Define number of global nodes with periodicity
    x_nodes = np.linspace(0, 1, num_nodes, endpoint=False)

    # Step 3: Assemble the mass matrix and right-hand side
    mass_matrix = assemble_mass_matrix(num_elements, element_order, num_nodes)
    rhs = assemble_rhs(num_elements, element_order, num_nodes, f)

    # Solve for the coefficients in the finite element basis
    coefficients = np.linalg.solve(mass_matrix, rhs)

    # Evaluate the projection on a fine grid for visualization
    x_fine = np.linspace(0, 1, 500)
    projection = evaluate_projection(x_fine, coefficients, num_elements, element_order, num_nodes)

    # Plot the original function and the projection
    y_exact = np.sin(2 * np.pi * x_fine)
    plt.plot(x_fine, y_exact, label="Exact sin(2πx)")
    plt.plot(x_fine, projection, 'r--', label="Projection (Order 3 Lagrange)")
    plt.legend()
    plt.xlabel("x")
    plt.ylabel("Function Value")
    plt.title("Projection of sin(2πx) onto Order 3 Lagrange Finite Element Space")
    plt.show()

# Run the projection
run_projection(num_elements=5, element_order=3)

# %%
