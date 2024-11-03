from spline import (
    generate,
    bspline_hat,
    bspline_hat_derivative,
)


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


if __name__ == "__main__":
    test_partition_unity()
    test_derivatives()
