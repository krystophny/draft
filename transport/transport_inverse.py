#%%
import jax.numpy as np
from jax import grad
from finite_differences import create_finite_difference_function, diffusive_flux, \
    evaluate

def n(x):
    return -0.5*x**2 + 1.0 - x

def q(x):
    return np.ones_like(x)

def cost(nh, qh, source_strength, diffusivity):
    return np.sum(
        (
            evaluate(diffusive_flux(nh, diffusivity)) -
            source_strength*evaluate(qh)
        )**2
    )

xmin = 0.0
xmax = 1.0
nx = 100

xh = np.linspace(xmin, xmax, nx)
nh = create_finite_difference_function(xmin, xmax, n(xh))
qh = create_finite_difference_function(xmin, xmax, q(xh))

print(cost(nh, qh, 1.0, 1.0))

grad_cost = grad(cost, argnums=[2,3])
print(grad_cost(nh, qh, 1.0, 1.0))

# %% Plot cost
import matplotlib.pyplot as plt
from jax import vmap

diffusivities = np.linspace(-4.0, 4.0, 100)
costs = vmap(lambda diffusivity: cost(nh, qh, 1.0, diffusivity))(diffusivities)

plt.plot(diffusivities, costs)

# %% Now plot cost over both, source_strength and diffusivity
source_strengths = np.linspace(-2.0, 2.0, 100)
diffusivities = np.linspace(-2.0, 2.0, 100)

XX, YY = np.meshgrid(source_strengths, diffusivities)
XX = XX.flatten()
YY = YY.flatten()
ZZ = vmap(lambda source_strength, diffusivity: cost(nh, qh, source_strength, diffusivity))(XX, YY)

# Exact solution is diffusivity == source_strength here
# See Mathematica DSolve[-D[a*D[n[x],x],x] == q, n[x], x]
x_exact = np.linspace(-2.0, 2.0, 100)
y_exact = np.linspace(-2.0, 2.0, 100)

plt.figure()
plt.contour(
    XX.reshape((100, 100)), YY.reshape((100, 100)), np.log(ZZ.reshape((100, 100)))
)
plt.plot(x_exact, y_exact, "k", label="Exact solution")
plt.colorbar()
plt.xlabel("Source strength")
plt.ylabel("Diffusivity")
plt.legend()

# %%
