from spline import (
    generate,
    bspline_hat,
    bspline_hat_derivative,
    init_knots,
    assemble_matrix
)

n = 20
degree = 3
x = np.linspace(1e-13, 1.0-1e-13, 100)


def test_partition_unity():
    x = np.linspace(1e-13, 1.0-1e-13, 100)
    for degree in range(1, 7):
        for n in range(1, 30):
            basis = generate(bspline_hat, n, degree, x)
            assert np.allclose(basis.sum(axis=1), 1.0)


def test_derivatives():
    x = np.linspace(1e-13, 1.0-1e-13, 100)
    for degree in range(1, 7):
        for n in range(1, 30):
            basis_der = generate(bspline_hat_derivative, n, degree, x)
            assert np.allclose(basis_der.sum(axis=1), 0.0, atol=1e-11)


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

def test_solver():
    solver = PoissonSolver(n, degree)
    phi_h = solver.discretize(phi)
    phi_ref = phi(x)
    phi_num = solver.evaluate(phi_h, x)
    assert np.allclose(phi_num, phi_ref, atol=1e-2, rtol=1e-2)

    dphi_ref = dphidx(x)
    dphi_num = solver.evaluate_derivative(phi_h, x)
    assert np.allclose(dphi_num, dphi_ref, atol=1e-2, rtol=1e-2)

    b = solver.assemble_rhs(rho)
    plt.figure()
    plt.plot(b)
    plt.figure()
    plt.plot(x, rho(x), '--')

    print(b)
    plt.figure()
    plt.plot(solver.stiffness_matrix[:, 0])
    phi_sol = np.linalg.solve(solver.stiffness_matrix, b)
    phi_sol_num = solver.evaluate(phi_sol, x)
    plt.figure()
    plt.plot(x, phi_sol_num)
    plt.plot(x, phi(x), '--')


if __name__ == "__main__":
    test_partition_unity()
    test_derivatives()
    test_solver()
