import numpy as np
from numba import njit

num_elements = 9
order = 3
num_nodes = num_elements * order
nodes = np.linspace(0, 1, order + 1)

def solve_poisson(rho, phi_h):
    stiffness_matrix = assemble_stiffness_matrix()
    rhs = assemble_rhs(rho, basis)
    phi_h[:] = np.linalg.solve(stiffness_matrix, rhs)

@njit
def project(target_function):
    rhs = assemble_rhs(target_function, basis)
    mass_matrix = assemble_mass_matrix()
    projected_values = np.linalg.solve(mass_matrix, rhs)
    return projected_values


@njit
def assemble_stiffness_matrix():
    return assemble_matrix(basis_derivative, basis_derivative)


@njit
def assemble_mass_matrix():
    return assemble_matrix(basis, basis)


@njit
def evaluate(x, basis, coefficients, result):
    result[:] = 0.0
    for element in range(num_elements):
        element_start = element * order
        for i in range(order + 1):
            global_i = element_start + i
            if global_i >= num_nodes:
                global_i -= num_nodes
            for k in range(len(x)):
                y = x[k]
                if element / num_elements <= y < (element + 1) / num_elements:
                    x_mapped = (y - element / num_elements) * num_elements
                    result[k] += coefficients[global_i] * basis(x_mapped, i)


@njit
def assemble_matrix(basis1, basis2):
    matrix = np.zeros((num_nodes, num_nodes))
    quad_points, quad_weights = gauss_legendre_quadrature()

    for element in range(num_elements):
        for i in range(order + 1):
            for j in range(order + 1):
                M_ij = 0.0
                for q in range(len(quad_points)):
                    x_q = quad_points[q]
                    weight = quad_weights[q]
                    M_ij += weight * basis1(x_q, i) * basis2(x_q, j)

                global_i = (element * order + i) % num_nodes
                global_j = (element * order + j) % num_nodes
                matrix[global_i, global_j] += M_ij

    return matrix


@njit
def assemble_rhs(target_function, basis):
    rhs = np.zeros(num_nodes)
    quad_points, quad_weights = gauss_legendre_quadrature()

    for element in range(num_elements):
        for i in range(order + 1):
            b_i = 0.0
            for q in range(len(quad_points)):
                x_q = quad_points[q]
                weight = quad_weights[q]
                x_mapped = element / num_elements + x_q / num_elements
                b_i += weight * target_function(x_mapped) * basis(x_q, i)

            global_i = (element * order + i) % num_nodes
            rhs[global_i] += b_i

    return rhs


@njit
def gauss_legendre_quadrature():
    points = np.array([0.1127016653792583, 0.5, 0.8872983346207417])
    weights = np.array([0.2777777777777778, 0.4444444444444444, 0.2777777777777778])
    return points, weights
