import numpy as np

class FiniteDifferenceFunction:
    def __init__(self, x, y):
        self.x = x
        self.y = y

def diffusive_flux(nh, diffusivity):
    return FiniteDifferenceFunction(nh.x,
        -diffusivity * grad(nh)
    )

def grad(nh):
    return FiniteDifferenceFunction(nh.x,
        np.gradient(nh.y, nh.x)
    )

def div(gh):
    return FiniteDifferenceFunction(gh.x,
        np.gradient(gh.y, gh.x)
    )
