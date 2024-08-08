import numpy as np

def standard_map(x, x_next, K=0.5):
    x_next[1] = np.mod(x[1] + K * np.sin(x[0]), 2*np.pi)
    x_next[0] = np.mod(x[0] + x_next[1], 2*np.pi)
    return x_next
