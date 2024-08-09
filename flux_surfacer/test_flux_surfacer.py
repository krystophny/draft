#%%
import numpy as np
import matplotlib.pyplot as plt
from flux_surfacer import fourier_coefficients, evaluate_fourier_series, \
    generate_circle_points
from standard_map import standard_map

nmap = 10000

x = np.zeros((2, nmap))
x[:,0] = [-0.5*np.pi, 0.84*2*np.pi]

for i in range(1, nmap):
    standard_map(x[:,i-1], x[:,i])

x[1,:] = np.mod(x[1,:] + np.pi, 2*np.pi) - np.pi

plt.figure()
plt.plot(x[0,:], x[1,:], ',')
plt.xlim([0, 2*np.pi])
plt.ylim([-np.pi, np.pi])

xtrain = np.zeros((2, nmap))

M = 64
x0 = 0.0
r0 = 2.0

def rscale(th):
    return 1.0 - 0.4*np.sin(2*th + 0.3*np.pi)

xtrain[0,:] = -(x[1,:] + r0)*rscale(x[0,:])*np.cos(x[0,:]) - x0
xtrain[1,:] = (x[1,:] + r0)*rscale(x[0,:])*np.sin(x[0,:])

x_c, x_s = fourier_coefficients(xtrain, M)
theta = np.linspace(0, 2 * np.pi, 100)
xtest = evaluate_fourier_series(x_c, x_s, theta)

plt.figure()
plt.plot(xtrain[0,:], xtrain[1,:], ',')
plt.plot(xtest[0,:], xtest[1,:], '-')


# %%

# Example usage
M = 64  # Number of Fourier terms
num_samples = 10000  # Number of Monte Carlo samples

# random theta
theta = np.random.uniform(0, 2 * np.pi, num_samples)
xtrain = generate_circle_points(theta, radius=rscale, center=(0.0, 0.0), noise_level=0.01)

#x_c, x_s = monte_carlo_coefficients(xtrain, M)
x_c, x_s = fourier_coefficients(xtrain, M)

print("Cosine coefficients:", x_c)
print("Sine coefficients:", x_s)

theta = np.linspace(0, 2 * np.pi, 100)
xeval = evaluate_fourier_series(x_c, x_s, theta)

plt.figure()
plt.plot(xtrain[0,:], xtrain[1,:], ',')
plt.plot(xeval[0,:], xeval[1,:], '-')
plt.axis('equal')
plt.xlabel('x')
plt.ylabel('y')
# %%
