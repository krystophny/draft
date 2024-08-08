#%%
import numpy as np
import matplotlib.pyplot as plt
from standard_map import standard_map

nmap = 1000

x = np.zeros((2, nmap))
x[:,0] = [-0.5*np.pi, 0.92*2*np.pi]

for i in range(1, nmap):
    standard_map(x[:,i-1], x[:,i])

x[1,:] = np.mod(x[1,:] + np.pi, 2*np.pi) - np.pi

plt.figure()
plt.plot(x[0,:], x[1,:], ',')
plt.xlim([0, 2*np.pi])
plt.ylim([-np.pi, np.pi])


R0 = 10.0
def r0(x):
    return 5.0

R = R0 + (r0(x) + x[1,:])*np.cos(x[0,:])
Z = (r0(x) + x[1,:])*np.sin(x[0,:])

plt.figure()
plt.plot(R, Z, ',')
plt.xlim([R0-2*np.pi, R0+2*np.pi])
plt.ylim([-2*np.pi, 2*np.pi])

RZmid0 = np.zeros(2)
RZmid0[0] = np.mean(R)
RZmid0[1] = np.mean(Z)
plt.plot(RZmid0[0], RZmid0[1], 'x')



RZtest = np.zeros((2, 100))
RZtest[0,:] = np.linspace(R0-2*np.pi, R0+2*np.pi, 100)
plt.plot(RZtest[0,:], RZtest[1,:], '-')

RZdist = np.zeros(100)
for i in range(100):
    RZdist[i] = np.sum((RZtest[0,i] - R)**2 + (RZtest[1,i] - Z)**2)

# %%
plt.figure()
plt.plot(RZtest[0,:], RZdist, '-')

# %%
