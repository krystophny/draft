from spline import generate_periodic_bspline_basis

def test_partition_unity():
    x = np.linspace(0, 1, 100)
    for degree in range(1, 7):
        for n in range(1, 30):
            basis = generate_periodic_bspline_basis(n, degree, x)
            assert(np.allclose(basis.sum(axis=1), 1.0))

if __name__ == "__main__":
    test_partition_unity()
