#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
import os
import re

plt.close('all')

qi = 4.8e-10
mi = 3.3366e-24
c  = 3e10

keys = ['R0', 'a', 'eps', 'A', 'psi_pr', 'B0', 'Bthcov',
          'Bphcov', 'dBthcovds', 'dBphcovds', 'q', 'iota', 'M_t',
          'Om_tE', 'vth', 'T', 'm0', 'n0', 'Drp']

datadir = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-03-11_n1_shear'

profname = os.path.join('/proj/plasma/RMP/DOC/Diss_Albert/code/driftorbit/Mtprofile_Hamiltonian.dat')
profdata = np.loadtxt(profname)

pattern = re.compile(r'driftorbit([0-9]+)\.out')
files = os.listdir(datadir)
files = [f for f in files if pattern.match(f)] # new format (> 01/2016)
infiles = [f.replace('out','in') for f in files]
s = []
vth = []
Mt = []
for f in infiles:
    fp = open(os.path.join(datadir, f))
    lines = fp.readlines()
    s.append(float(lines[3].split()[0]))
    Mt.append(float(lines[4].split()[0]))
    vth.append(float(lines[8].split()[0]))
    fp.close()
s = np.array(s)
vth = np.array(vth)
Mt = np.array(Mt)

magfie = dict()
for key in keys:
    magfie[key] = []
for f in files:
    datafile = os.path.join(datadir,f.replace('.out','_magfie_param.out'))
    fp = open(datafile)
    lines = fp.readlines()
    for l in range(len(lines)):
        if (l>0 and l<len(keys)+1):
            magfie[keys[l-1]].append(lines[l].split()[-1])
    fp.close()
        
order = np.argsort(s)
s = s[order]
vth = vth[order]
Mt = Mt[order]
for key in keys:
    magfie[key] = np.array(magfie[key],dtype='float64')
    magfie[key] = magfie[key][order]

ds = np.diff(s)
s2 = s[0:-1]+ds/2 # mid points

dBds2   = np.diff(magfie['B0'])/ds
dBds    = np.append(dBds2, dBds2[-1])
dBthds  = magfie['dBthcovds']
dBthds2 = np.diff(magfie['Bthcov'])/ds
dBphds  = magfie['dBphcovds']
dBphds2 = np.diff(magfie['Bphcov'])/ds
Bph     = magfie['Bphcov']
q       = magfie['q']
dqds2   = np.diff(magfie['q'])/ds
dqds    = np.append(dqds2, dqds2[-1])       # hack
omc     = qi*magfie['B0']/(mi*c)
dpsipds = magfie['psi_pr']/magfie['q']
OmtBbar = vth**2/(2.0*omc*dpsipds)
OmtE    = Mt*vth/magfie['R0']

plt.figure(1)
plt.semilogy(s, OmtBbar*dBds)
plt.semilogy(s, OmtBbar*dBthds)
#plt.plot(s2, dBthds2, '--')
plt.semilogy(s, np.abs(OmtBbar*q*dBphds))
#plt.semilogy(s, -OmtBbar*q*dBphds, 'b--')
plt.semilogy(s, OmtBbar*dqds*Bph)
plt.semilogy(s, np.abs(OmtE))
#plt.semilogy(s, -OmtE, 'r--')
#plt.plot(s2, dBphds2, '--')
#plt.plot(s, Bph)
plt.title('drift terms')
plt.legend([r'$\bar{\Omega}_{tB}\,dB/ds$',
            r'$\bar{\Omega}_{tB}\,dB_{\vartheta}/ds$',
            r'$\bar{\Omega}_{tB}\,q\,dB_{\varphi}/ds$',
            r'$\bar{\Omega}_{tB}\,dq/ds\,B_{\varphi}$',
            r'$\Omega_{tE}$'],ncol=2,loc='lower right')
