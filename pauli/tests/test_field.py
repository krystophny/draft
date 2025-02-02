import pytest

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from numba import njit

from pauli.field import bfield_wire


def test_plot(tmp_path):
    plt.plot([1, 2, 3], [4, 5, 6])
    plot_path = tmp_path / "test_plot.png"
    plt.savefig(plot_path)
    plt.close()
    print("Plot saved to", plot_path)
    assert plot_path.exists()


def test_bfield_wire(tmp_path):
    nx = 10
    ny = 10
    nz = 2

    x = np.linspace(-2, 2, nx)
    y = np.linspace(-2, 2, ny)
    z = np.linspace(-2, 2, nz)

    X, Y, Z = np.meshgrid(x, y, z, indexing="ij")

    B = np.zeros((3, nx, ny, nz))
    apply_3d(bfield_wire, X, Y, Z, B)
    b = B / np.sqrt(np.sum(B**2, axis=0))

    fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
    ax.quiver(X, Y, Z, b[0], b[1], b[2])
    plot_path = tmp_path / "test_bfield_wire.png"
    plt.savefig(plot_path)
    plt.close()
    print("Plot saved to", plot_path)



@njit
def apply_3d(fun, X, Y, Z, B):
    nx, ny, nz = X.shape
    for i in range(nx):
        for j in range(ny):
            for k in range(nz):
                fun(X[i, j, k], Y[i, j, k], Z[i, j, k], B[:, i, j, k])


if __name__ == "__main__":
    pytest.main(["-sv", "--basetemp=tests/output", __file__])
