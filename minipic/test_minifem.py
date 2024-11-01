#%%
import numpy as np
from minifem import *

@njit
def f(x):
    return np.cos(2*np.pi*(x-0.5))

@njit
def dfdx(x):
    return -2*np.pi*np.sin(2*np.pi*(x-0.5))

def test_project_eval():
    f_h = project(f)
    x_eval = np.linspace(0.01, 0.99, 100)
    f_eval = np.empty_like(x_eval)
    evaluate(x_eval, f_h, f_eval)
    f_ref = f(x_eval)
    f_err = f_ref - f_eval

    assert np.allclose(f_err, 0.0, atol=2e-3)


def test_derivative():
    f_h = project(f)
    x_eval = np.linspace(0.01, 0.99, 100)
    dfdx_eval = np.empty_like(x_eval)
    evaluate_derivative(x_eval, f_h, dfdx_eval)
    dfdx_ref = dfdx(x_eval)
    assert(np.mean(abs(dfdx_ref - dfdx_eval)) < 0.005)


def test_lagrange_derivative_accuracy():
    order = 3
    nodes = np.array([0.0, 0.33, 0.66, 1.0])
    x = 0.5
    h = 1e-5
    tolerance = 1e-6

    print("Testing Lagrange basis function derivatives:")

    for i in range(order + 1):
        analytical_derivative = lagrange_basis_derivative(x, order, i, nodes)

        f_x_plus_h = lagrange_basis(x + h, order, i, nodes)
        f_x_minus_h = lagrange_basis(x - h, order, i, nodes)
        numerical_derivative = (f_x_plus_h - f_x_minus_h) / (2 * h)

        difference = abs(analytical_derivative - numerical_derivative)
        assert difference < tolerance

# %%
