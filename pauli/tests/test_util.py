import pytest

import numpy as np
import scipy.special as sps
from pauli.util import legendre_p, assoc_legendre_p


@pytest.mark.parametrize(
    "n",
    [
        0,
        1,
        2,
        3,
        4,
        5,
    ],
)
def test_legendre_p(n):
    x0 = 0.53
    our_val = legendre_p(n, x0)
    scipy_val = sps.legendre_p(n, x0)
    assert np.isclose(our_val, scipy_val, rtol=1e-10)


@pytest.mark.parametrize(
    "n, m",
    [
        (0, 0),
        (1, 0),
        (1, 1),
        (1, 0),
        (2, 1),
        (2, 2),
        (3, 0),
        (3, 1),
        (3, 2),
        (3, 3),
    ],
)
def test_assoc_legendre_p(n, m):
    x0 = 0.53
    our_val = assoc_legendre_p(n, m, x0)
    scipy_val = sps.assoc_legendre_p(n, m, x0)
    assert np.isclose(our_val, scipy_val, rtol=1e-10)


if __name__ == "__main__":
    pytest.main(["-sv", __file__])
