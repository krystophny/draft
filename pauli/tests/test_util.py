import pytest

import numpy as np
from pauli.util import associated_legendre_p
from scipy.special import lpmv


def test_basic_values():
    assert np.isclose(associated_legendre_p(1, 1, 0), -1.0)




if __name__ == "__main__":
    pytest.main(["-v", __file__])
