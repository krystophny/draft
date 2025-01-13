import numpy as np
from netCDF4 import Dataset

x = np.linspace(0, 2 * np.pi, 100)
y = np.sin(x)

with Dataset('data.nc', 'w') as nc:
    nc.createDimension('x', len(x))

    x_var = nc.createVariable('x', 'f4', ('x',))
    x_var[:] = x
    x_var.units = 'radians'
    x_var.description = 'Independent variable (x-axis)'

    y_var = nc.createVariable('y', 'f4', ('x',))
    y_var[:] = y
    y_var.units = 'unitless'
    y_var.description = 'Dependent variable (y = sin(x))'
