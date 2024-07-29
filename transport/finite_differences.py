from jax import jit
import jax.numpy as np

def create_finite_difference_function(xmin, xmax, y):
    return {
        "xmin": xmin,
        "xmax": xmax,
        "dx": (xmax - xmin)/(len(y) - 1),
        "y": y
    }

@jit
def diffusive_flux(f, diffusivity):
    return {
        "xmin": f["xmin"],
        "xmax": f["xmax"],
        "dx": f["dx"],
        "y": -np.gradient(diffusivity*np.gradient(f["y"])/f["dx"])/f["dx"]
    }

@jit
def evaluate(f):
    return f["y"]
