#%%
import numpy as np
from minifem import *

@njit
def phi(x):
    return np.cos(2*np.pi*(x-0.5))

@njit
def dphidx(x):
    return -2*np.pi*np.sin(2*np.pi*(x-0.5))

@njit
def d2phidx2(x):
    return -(2*np.pi)**2*phi(x)

@njit
def rho(x):
    return -d2phidx2(x)

def test_project_eval():
    f_h = project(phi)
    x_eval = np.linspace(0.01, 0.99, 100)
    f_eval = np.empty_like(x_eval)
    evaluate(x_eval, f_h, f_eval)
    f_ref = phi(x_eval)
    f_err = f_ref - f_eval

    assert np.allclose(f_err, 0.0, atol=2e-3)


def test_derivative():
    f_h = project(phi)
    x_eval = np.linspace(0.01, 0.99, 100)
    dfdx_eval = np.empty_like(x_eval)
    evaluate_derivative(x_eval, f_h, dfdx_eval)
    dfdx_ref = dphidx(x_eval)
    assert(np.mean(abs(dfdx_ref - dfdx_eval)) < 0.005)


def test_lagrange_derivative_accuracy():
    order = 3
    nodes = np.array([0.0, 0.33, 0.66, 1.0])
    x = 0.5
    h = 1e-5
    tolerance = 1e-6

    print("Testing Lagrange basis function derivatives:")

    for i in range(order + 1):
        analytical_derivative = lagrange_basis_derivative(x, i)

        f_x_plus_h = lagrange_basis(x + h, i)
        f_x_minus_h = lagrange_basis(x - h, i)
        numerical_derivative = (f_x_plus_h - f_x_minus_h) / (2 * h)

        difference = abs(analytical_derivative - numerical_derivative)
        assert difference < tolerance


phi_h = np.empty(num_nodes)
solve_poisson(rho, phi_h)

import matplotlib.pyplot as plt
x_fine = np.linspace(0, 1, 500)
phi_fine = np.empty_like(x_fine)
evaluate(x_fine, phi_h, phi_fine)
plt.plot(x_fine, phi_fine, label="Numerical")
plt.plot(x_fine, phi(x_fine), label="Exact")
plt.legend()


# %%
