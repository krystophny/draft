#%%
import numpy as np

def n(r):
    return 1.0 - r

xmin = 0.0
xmax = 1.0
nx = 100

xh = np.linspace(xmin, xmax, nx)
nh = FiniteDifferenceFunction(xh, n(xh))

# %%
