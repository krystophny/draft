#%%
import numpy as np
from minifem import *

@njit
def f(x):
    return np.cos(2*np.pi*(x-0.5))

def test_project_eval():
    f_h = project(f)
    x_eval = np.linspace(0.01, 0.99, 100)
    f_eval = evaluate(x_eval, f_h)
    f_ref = f(x_eval)
    f_err = f_ref - f_eval

    assert np.allclose(f_err, 0.0, atol=2e-3)
# %%
