import numpy as np
import h5py

x = np.linspace(0, 2 * np.pi, 100)
y = np.sin(x)

with h5py.File('data.h5', 'w') as f:
    x_data = f.create_dataset('x', data=x)
    y_data = f.create_dataset('y', data=y)
