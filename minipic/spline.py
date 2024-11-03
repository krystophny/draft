#%%
import numpy as np
import matplotlib.pyplot as plt
from numba import njit

@njit
def bspline_basis(x, knots, order, i):
    """
    Evaluate the B-spline basis function of given order at position x.

    Parameters:
        x (float): Point at which to evaluate.
        knots (numpy.ndarray): Knot vector.
        order (int): Order of the B-spline (degree + 1).
        i (int): Index of the B-spline basis function.

    Returns:
        float: Value of the B-spline basis function at x.
    """
    if order == 1:
        return 1.0 if knots[i] <= x < knots[i + 1] else 0.0

    left_denom = knots[i + order - 1] - knots[i]
    right_denom = knots[i + order] - knots[i + 1]

    left_term = ((x - knots[i]) / left_denom) * bspline_basis(x, knots, order - 1, i) if left_denom != 0 else 0
    right_term = ((knots[i + order] - x) / right_denom) * bspline_basis(x, knots, order - 1, i + 1) if right_denom != 0 else 0

    return left_term + right_term

def generate_bspline_basis(order, L, num_points):
    """
    Generate B-spline basis functions of a given order on [0, L] with periodic boundary conditions.

    Parameters:
        order (int): The order of the B-spline (degree + 1).
        L (float): The length of the interval [0, L].
        num_points (int): Number of equidistant points between 0 and L.

    Returns:
        list of function: A list of B-spline basis functions.
    """
    knots = np.linspace(-L*1/num_points, L*(num_points + 1)/num_points,
        num_points + order + 2)

    basis_functions = []
    for i in range(len(knots) - order):
        basis_functions.append(lambda x, i=i: bspline_basis(x % L, knots, order, i))

    return basis_functions

def gauss_quadrature(func, a, b, n=5):
    """
    Perform Gaussian quadrature over interval [a, b].

    Parameters:
        func (function): Function to integrate.
        a (float): Lower limit of integration.
        b (float): Upper limit of integration.
        n (int): Number of Gauss points.

    Returns:
        float: Approximate integral of func over [a, b].
    """
    gauss_weights = np.array([0.236926885, 0.478628670, 0.568888889, 0.478628670, 0.236926885])
    gauss_nodes = np.array([-0.906179845, -0.538469310, 0.000000000, 0.538469310, 0.906179845])

    integral = 0.0
    for i in range(n):
        xi = 0.5 * (b - a) * gauss_nodes[i] + 0.5 * (b + a)
        integral += gauss_weights[i] * func(xi)
    return 0.5 * (b - a) * integral

def compute_mass_matrix(basis_functions, L, num_points):
    """
    Compute the mass matrix for the B-spline basis.

    Parameters:
        basis_functions (list of function): B-spline basis functions.
        L (float): The length of the interval [0, L].
        num_points (int): Number of intervals for Gaussian quadrature.

    Returns:
        numpy.ndarray: The mass matrix.
    """
    num_basis = len(basis_functions)
    mass_matrix = np.zeros((num_basis, num_basis))
    dx = L / num_points

    for i in range(num_basis):
        for j in range(num_basis):
            integrand = lambda x: basis_functions[i](x) * basis_functions[j](x)
            mass_matrix[i, j] = sum(gauss_quadrature(integrand, k * dx, (k + 1) * dx) for k in range(num_points))

    return mass_matrix

def project_function_non_orthogonal(func, basis_functions, L, num_points):
    """
    Project a function onto a non-orthogonal B-spline basis.

    Parameters:
        func (function): Function to project.
        basis_functions (list of function): B-spline basis functions.
        L (float): The length of the interval [0, L].
        num_points (int): Number of intervals for Gaussian quadrature.

    Returns:
        numpy.ndarray: Coefficients of the projection.
    """
    num_basis = len(basis_functions)
    dx = L / num_points
    f_vec = np.zeros(num_basis)

    for i in range(num_basis):
        integrand = lambda x: func(x) * basis_functions[i](x)
        f_vec[i] = sum(gauss_quadrature(integrand, k * dx, (k + 1) * dx) for k in range(num_points))

    mass_matrix = compute_mass_matrix(basis_functions, L, num_points)

    # Solve M * c = f
    coefficients = np.linalg.solve(mass_matrix, f_vec)
    return coefficients

def evaluate_bspline(coefficients, basis_functions, x_values):
    """
    Evaluate the B-spline interpolation at given x_values.

    Parameters:
        coefficients (numpy.ndarray): Coefficients of the B-spline basis functions.
        basis_functions (list of function): B-spline basis functions.
        x_values (numpy.ndarray): Points at which to evaluate the interpolation.

    Returns:
        numpy.ndarray: Interpolated values at x_values.
    """
    interpolated_values = np.zeros_like(x_values)
    for i, basis in enumerate(basis_functions):
        interpolated_values += coefficients[i] * np.array([basis(x) for x in x_values])
    return interpolated_values


# Parameters
L = 1            # Period of the function and the interval length
order = 5        # Order of the B-spline
num_points = 8   # Number of equidistant points

# Define the function to project
func = lambda x: np.cos(2 * np.pi * x)

# Generate B-spline basis
basis = generate_bspline_basis(order, L, num_points)

# plot basis functions
plt.figure(figsize=(10, 6))
x_values = np.linspace(0, L, 500)
for i, basis_function in enumerate(basis):
    plt.plot(x_values, [basis_function(x) for x in x_values], label=f'Basis {i}')

#%%
# Project function onto the B-spline basis
coefficients = project_function_non_orthogonal(func, basis, L, num_points)

# Evaluate the interpolated function
interpolated_values = evaluate_bspline(coefficients, basis, x_values)

# Plot the reference and interpolated function
plt.figure(figsize=(10, 6))
plt.plot(x_values, func(x_values), label='Reference $\\cos(2\\pi x)$', linestyle='dashed')
plt.plot(x_values, interpolated_values, label='Interpolated Function')
plt.legend()
plt.xlabel('x')
plt.ylabel('Function Value')
plt.title('Reference vs Interpolated Function')
plt.grid(True)
plt.show()
