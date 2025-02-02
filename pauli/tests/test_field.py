import pytest

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from numba import jit

from pauli.field import bfield_wire, bfield_circle


def test_plot(tmp_path):
    plt.plot([1, 2, 3], [4, 5, 6])
    plot_path = tmp_path / "test_plot.png"
    plt.savefig(plot_path)
    plt.close()
    print("Plot saved to", plot_path)
    assert plot_path.exists()


def test_bfield_wire(tmp_path):
    plot_field_3d(bfield_wire, tmp_path)


def test_bfield_circle(tmp_path):
    plot_field_xz(bfield_circle, tmp_path)

def test_bfield_sum(tmp_path):
    def bfield(x, y, z, B):
        B1 = np.zeros(3)
        B2 = np.zeros(3)
        bfield_wire(x, y, z, B1)
        bfield_circle(x, y, z, B2)
        B[:] = B1 + 10.0*B2
    plot_field_3d(bfield, tmp_path)


def plot_field_xz(fun, tmp_path):
    nx = 50
    nz = 50

    x = np.linspace(-1.5, 1.5, nx)
    z = np.linspace(-.5, .5, nz)

    X, Z = np.meshgrid(x, z)

    B = np.zeros((3, nx, nz))
    apply_xz(fun, X, Z, B)
    b = B / np.sqrt(np.sum(B**2, axis=0))

    plt.streamplot(X, Z, B[0], B[2])
    plot_path = tmp_path / f"test_{fun.__name__}.png"
    plt.savefig(plot_path)
    plt.close()
    print("Plot saved to", plot_path)


def plot_field_3d(fun, tmp_path):
    nx = 10
    ny = 10
    nz = 10

    x = np.linspace(-2, 2, nx)
    y = np.linspace(-2, 2, ny)
    z = np.linspace(-2, 2, nz)

    X, Y, Z = np.meshgrid(x, y, z, indexing="ij")

    B = np.zeros((3, nx, ny, nz))
    apply_3d(fun, X, Y, Z, B)
    b = B / np.sqrt(np.sum(B**2, axis=0))

    fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
    ax.quiver(X, Y, Z, b[0], b[1], b[2])
    plot_path = tmp_path / f"test_{fun.__name__}.png"
    plt.savefig(plot_path)
    plt.close()
    print("Plot saved to", plot_path)


def apply_xz(fun, X, Z, B):
    nx, nz = X.shape
    for i in range(nx):
        for k in range(nz):
            fun(X[i, k], 0.0, Z[i, k], B[:, i, k])


def apply_3d(fun, X, Y, Z, B):
    nx, ny, nz = X.shape
    for i in range(nx):
        for j in range(ny):
            for k in range(nz):
                fun(X[i, j, k], Y[i, j, k], Z[i, j, k], B[:, i, j, k])


if __name__ == "__main__":
    pytest.main(["-sv", "--basetemp=tests/output", __file__])
