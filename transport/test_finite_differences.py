import pytest
import numpy as np

from finite_differences import FiniteDifferenceFunction, \
    grad, div, diffusive_flux

def test_grad():
    xh = np.linspace(0.0, 1.0, 100)
    yh = 1.0 - xh
    nh = FiniteDifferenceFunction(xh, yh)
    gh = grad(nh)
    assert np.allclose(gh.y, -np.ones_like(xh))

def test_div():
    xh = np.linspace(0.0, 1.0, 100)
    yh = 1.0 - xh
    gh = FiniteDifferenceFunction(xh, yh)
    fh = div(gh)
    assert np.allclose(fh.y, -np.ones_like(xh))
