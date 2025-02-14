#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
import scipy.interpolate as spi
import os
import re
#from exportfig import exportfig
#from noexportfig import exportfig

plt.close('all')

datadirs = {}
s = {}
smin = 0.0
smax = 1.0


datadir = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/160407_rmp90_torfreq'


pattern = re.compile(r'driftorbit([0-9]+)_torfreq\.out')
files = os.listdir(datadir)
files = [f for f in files if pattern.match(f)] # new format (> 01/2016)
infiles = [f.replace('_torfreq.out','.in') for f in files]
s = []
for f in infiles:
    fp = open(os.path.join(datadir, f))
    lines = fp.readlines()
    s.append(float(lines[3].split()[0]))
    fp.close()
s = np.array(s)

data = []
for f in files:
    print(f)
    data.append(np.loadtxt(os.path.join(datadir,f),comments='!',))
data=np.array(data)   

#condi = s>=0.1
#s = s[condi]
#data = data[condi,:]
    
order = np.argsort(s)
s = s[order]
data = data[order,:]
d1 = data[:,20,:]
d2 = data[:,94,:]
d3 = data[:,99,:]

# Bugfix until bug is eliminated in driftorbit
goodind = [-9,-8,-7,-6,-2,-1]
badind = [-5,-4,-3]
x = s[goodind]
y = d3[goodind,7]
d3i = spi.interp1d(x,y,kind='cubic')
d3[badind,7] = d3i(s[badind])

plt.figure(1)
plt.clf()
plt.title('eta values')
plt.plot(s,d1[:,0],'d-',markevery=5)
plt.plot(s,d2[:,0],'d-',markevery=5)
plt.plot(s,d3[:,0],'d-',markevery=5)
plt.plot(s,d1[:,1])
plt.plot(s,d1[:,2])
plt.plot(s,(d1[:,1]+d1[:,2])/2e0)
plt.xlabel('s')
plt.ylabel('eta')
plt.xlim([0,1])
plt.ylim([0,1.1*np.max(d1[:,2])])
plt.grid(True)
plt.legend(['tp','mid','dt'],loc='best')

plt.figure(2)
plt.clf()
plt.title(r'$\omega_b$')
plt.plot(s,d1[:,7])
plt.plot(s,d2[:,7])
plt.plot(s,d3[:,7])
plt.plot(s,d1[:,3])
plt.xlabel('s')
plt.ylabel(r'$\omega$')
plt.xlim([0,1])
#plt.ylim([0,1.1*np.max(d1[:,2])])
plt.grid(True)
plt.legend(['eta_tp','eta_mid','eta_dt'],loc='best')

plt.figure(3)
plt.clf()
plt.title(r'$<\Omega_{tB}>_b$')
plt.plot(s,d1[:,4])
plt.plot(s,d2[:,4])
plt.plot(s,d3[:,4])
plt.plot(s,d1[:,3])
plt.xlabel('s')
plt.ylabel(r'$\omega$')
plt.xlim([0,1])
plt.ylim([-5000,5000])
plt.grid(True)
plt.legend(['eta_tp','eta_mid','eta_dt'],loc='best')

plt.figure(4)
plt.clf()
plt.title(r'$\Omega_{tE}$')
plt.plot(s,d1[:,3])
plt.xlabel('s')
plt.ylabel(r'$\omega$')
plt.xlim([0,1])
#plt.ylim([0,1.1*np.max(d1[:,2])])
plt.grid(True)
plt.legend(['eta_tp','eta_mid','eta_dt'],loc='best')

header = b'%1:s                    '+\
         ' 2:eta                   '+\
         ' 3:etatp                 '+\
         ' 4:etadt                 '+\
         ' 5:Om_tE                 '+\
         ' 6:OmtB                  '+\
         ' 7:dOmtbdv               '+\
         ' 8:dOmtbdeta             '+\
         ' 9:Omth                  '+\
         '10:dOmthdv               '+\
         '11:dOmthdeta             '+\
         '12:Omph                  '+\
         '13:dOmphdv               '+\
         '14:dOmphdeta             '
         
with open('rmp90_torfreq_tp.txt', 'wb') as f:
  f.write(header)
  np.savetxt(f, np.c_[s,d1])
  
with open('rmp90_torfreq_mid.txt', 'wb') as f:
  f.write(header)
  np.savetxt(f, np.c_[s,d2])
  
with open('rmp90_torfreq_dt.txt', 'wb') as f:
  f.write(header)
  np.savetxt(f, np.c_[s,d3])

sind1 = 44  # s=0.2
sind2 = 138 # s=0.7
sind3 = 155 # s=0.8

col = np.ones([data.shape[1],1])
  
with open('rmp90_torfreq_s0p2.txt', 'wb') as f:
  f.write(header)
  sk = s[sind1]*col
  np.savetxt(f, np.c_[sk,data[sind1,:,:]])
  
with open('rmp90_torfreq_s0p7.txt', 'wb') as f:
  f.write(header)
  sk = s[sind2]*col
  np.savetxt(f, np.c_[sk,data[sind2,:,:]])
  
with open('rmp90_torfreq_s0p8.txt', 'wb') as f:
  f.write(header)
  sk = s[sind3]*col
  np.savetxt(f, np.c_[sk,data[sind3,:,:]])
  