import numpy as np
from numba import njit


@njit
def periodic(fun, x, knots, degree, i):
    n = len(knots) - 2 * degree - 1
    dx = 1.0 / (n + degree)
    if i >= n:
        y = np.empty_like(x)
        y[:] = x
        y[x < (i - n + 1) * dx] += 1.0
        ret = fun(y, knots, degree, i)
        ret[x < 0.0] = 0.0
        ret[x > 1.0] = 0.0
        return ret
    else:
        return fun(x, knots, degree, i)


@njit
def bspline_hat(x, knots, degree, i):
    """Generates a single B-spline basis function."""
    n_knots = len(knots)
    if degree == 0:
        return np.where((x >= knots[i]) & (x < knots[i + 1]), 1.0, 0.0)
    else:
        left = (x - knots[i]) / (knots[i + degree] - knots[i])
        right = (knots[i + degree + 1] - x) / (
            knots[i + degree + 1] - knots[i + 1]
        )
        return left * bspline_hat(
            x, knots, degree - 1, i
        ) + right * bspline_hat(x, knots, degree - 1, i + 1)


@njit
def bspline_hat_derivative(x, knots, degree, i):
    """Generates the derivative of a single B-spline basis function."""
    n_knots = len(knots)
    if degree == 0:
        return np.where((x >= knots[i]) & (x < knots[i + 1]), 1.0, 0.0)
    else:
        left = 1.0 / (knots[i + degree] - knots[i])
        right = -1.0 / (knots[i + degree + 1] - knots[i + 1])
        return left * bspline_hat(
            x, knots, degree - 1, i
        ) + right * bspline_hat(x, knots, degree - 1, i + 1)


@njit
def generate(basis, n, degree, x):
    n_basis = n + degree
    result = np.zeros((len(x), n_basis))

    for i in range(n_basis):
        evaluate_basis(basis, n, degree, i, x, result[:, i])

    return result


@njit
def init_knots(n, degree):
    dx = 1.0 / (n + degree)
    return np.linspace(0.0, 1.0 + degree * dx, n + 2 * degree + 1)


@njit
def evaluate_basis(basis, n, degree, i, x, result):
    dx = 1.0 / (n + degree)
    knots = init_knots(n, degree)
    n_basis = len(knots) - degree - 1
    if i < 0 or i >= n_basis:
        result[:] = 0.0
    result[:] = periodic(basis, x, knots, degree, i)


@njit
def gauss_legendre_quadrature():
    points = np.array([-0.7745966692414834, 0.0, 0.7745966692414834])
    weights = np.array(
        [0.2777777777777778, 0.4444444444444444, 0.2777777777777778]
    )
    return points, weights


@njit
def assemble_matrix(basis, knots, degree):
    n_basis = len(knots) - degree - 1
    quad_points, quad_weights = gauss_legendre_quadrature()
    band_width = degree + 1
    matrix = np.zeros((n_basis, n_basis))

    for i in range(band_width):
        start = knots[i]
        end = knots[i + degree + 1]

        mapped_points = start + (end - start) * 0.5 * (quad_points + 1)
        integrand_values = np.zeros_like(mapped_points)

        b_i = basis(mapped_points, knots, degree, 0)
        b_j = basis(mapped_points, knots, degree, i)

        integrand_values = b_i * b_j

        value = np.sum(integrand_values * quad_weights) * (end - start) * 0.5

        for j in range(n_basis):
            matrix[j, np.mod(j + i, n_basis)] = value
            matrix[np.mod(j + i, n_basis), j] = value

    return matrix


@njit
def assemble_vector(basis, f, knots, degree):
    n_basis = len(knots) - degree - 1
    quad_points, quad_weights = gauss_legendre_quadrature()
    vector = np.zeros(n_basis)

    for i in range(n_basis):
        start = knots[i]
        end = knots[i + degree + 1]

        mapped_points = start + (end - start) * 0.5 * (quad_points + 1)
        integrand_values = np.zeros_like(mapped_points)

        b_i = basis(mapped_points, knots, degree, i)
        integrand_values = f(mapped_points) * b_i

        value = np.sum(integrand_values * quad_weights) * (end - start) * 0.5
        vector[i] = value

    return vector


class PoissonSolver:
    def __init__(self, n, degree):
        self.n = n
        self.degree = degree
        self.knots = init_knots(n, degree)
        self.mass_matrix = assemble_matrix(bspline_hat, self.knots, degree)
        self.stiffness_matrix = assemble_matrix(
            bspline_hat_derivative, self.knots, degree
        )*self.degree**2

    def discretize(self, f):
        M_times_b = assemble_vector(bspline_hat, f, self.knots, self.degree)
        return np.linalg.solve(self.mass_matrix, M_times_b)

    def evaluate(self, f_h, x):
        return np.sum(
            f_h
            * generate(bspline_hat, self.n, self.degree, x),
            axis=1,
        )

    def evaluate_derivative(self, f_h, x):
        return np.sum(
            f_h
            * generate(bspline_hat_derivative, self.n, self.degree, x),
            axis=1,
        )*self.degree
