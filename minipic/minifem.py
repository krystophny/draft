import numpy as np
from numba import njit

num_elements = 5
element_order = 3
num_nodes = num_elements * element_order

@njit
def evaluate(x, coefficients):
    projection = np.zeros_like(x)
    nodes = lagrange_basis_nodes(element_order)

    for element in range(num_elements):
        element_start = element * element_order
        for i in range(element_order + 1):
            global_i = element_start + i
            if global_i >= num_nodes:
                global_i -= num_nodes
            for k in range(len(x)):
                y = x[k]
                if element / num_elements <= y < (element + 1) / num_elements:
                    x_mapped = (y - element / num_elements) * num_elements
                    projection[k] += coefficients[global_i] * lagrange_basis(x_mapped, element_order, i, nodes)
    return projection

@njit
def project(target_function):
    rhs = assemble_rhs(target_function)
    mass_matrix = assemble_mass_matrix()
    projected_values = np.linalg.solve(mass_matrix, rhs)
    return projected_values

@njit
def assemble_mass_matrix():
    mass_matrix = np.zeros((num_nodes, num_nodes))
    nodes = lagrange_basis_nodes(element_order)
    quad_points, quad_weights = gauss_legendre_quadrature()

    for element in range(num_elements):
        for i in range(element_order + 1):
            for j in range(element_order + 1):
                M_ij = 0.0
                for q in range(len(quad_points)):
                    x_q = quad_points[q]
                    weight = quad_weights[q]
                    M_ij += weight * lagrange_basis(x_q, element_order, i, nodes) * lagrange_basis(x_q, element_order, j, nodes)

                global_i = (element * element_order + i) % num_nodes
                global_j = (element * element_order + j) % num_nodes
                mass_matrix[global_i, global_j] += M_ij
    return mass_matrix


@njit
def assemble_rhs(target_function):
    rhs = np.zeros(num_nodes)
    nodes = lagrange_basis_nodes(element_order)
    quad_points, quad_weights = gauss_legendre_quadrature()

    for element in range(num_elements):
        for i in range(element_order + 1):
            b_i = 0.0
            for q in range(len(quad_points)):
                x_q = quad_points[q]
                weight = quad_weights[q]
                x_mapped = element / num_elements + x_q / num_elements
                b_i += weight * target_function(x_mapped) * lagrange_basis(x_q, element_order, i, nodes)

            global_i = (element * element_order + i) % num_nodes
            rhs[global_i] += b_i

    return rhs

@njit
def lagrange_basis_nodes(order):
    return np.linspace(0, 1, order + 1)

@njit
def lagrange_basis(x, order, i, nodes):
    value = 1.0
    for j in range(order + 1):
        if i != j:
            value *= (x - nodes[j]) / (nodes[i] - nodes[j])
    return value

@njit
def gauss_legendre_quadrature():
    points = np.array([0.1127016653792583, 0.5, 0.8872983346207417])
    weights = np.array([0.2777777777777778, 0.4444444444444444, 0.2777777777777778])
    return points, weights
