#!/usr/bin/python

import numpy as np
import scipy.interpolate as spi
import matplotlib.pyplot as plt
import os
import re
#from exportfig import exportfig
from noexportfig import exportfig

plt.close('all')

datadirs = {}
s = {}
data = {}
sbdata = {}
D11do = {}
D12do = {}
D12D11do = {}
D11sb = {}
D12sb = {}
D11i = {}
D12i = {}
D12D11i = {}
smin = 0.0
smax = 1.0

#neodata = np.loadtxt('ntv_out_asdex30835_rmp_n2_HatFun14.dat')
neodata = np.loadtxt('ntv_out_nonlocal.dat')

neos = neodata[:,0]
neoorder = np.argsort(neos)
avnabs = neodata[:,14]
avnabs_interp = spi.interpolate.interp1d(neos, avnabs, bounds_error=False)
neos = neos[neoorder]
neodata = neodata[neoorder,:]
condi2 = (neos<smax)*(neos>smin)
neos = neos[condi2]
neodata = neodata[condi2,:]
D11neo = neodata[:,7]
D12neo = neodata[:,8]
D12D11neo = D12neo/D11neo

datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-08-24_n1_shear_newmagfie'
#datadirs[1] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-03-11_n1_shear'
#datadirs[1] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-08-24_n1_shear_newmagfie'
datadirs[1] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-08-24_n1_shear_nonlin'

profname1 = os.path.join('/proj/plasma/RMP/DOC/Diss_Albert.orig/code/driftorbit/Mtprofile_Hamiltonian.dat')
profname2 = os.path.join('/proj/plasma/RMP/DOC/Diss_Albert.orig/code/driftorbit/ASDEX/profiles/Mtprofile190.dat')
profdata1 = np.loadtxt(profname1)
profdata2 = np.loadtxt(profname2)

for k in datadirs.keys():
    datadir = datadirs[k]
    pattern1 = re.compile(r'driftorbit([0-9]+)\.out')
    pattern2 = re.compile(r'driftorbit_*([0-9]+\.[0-9]+)\.out')
    files = os.listdir(datadir)
    files1 = [f for f in files if pattern1.match(f)] # new format (> 01/2016)
    files2 = [f for f in files if pattern2.match(f)] # old format

    if len(files1) > 0:
        files = files1
        infiles = [f.replace('out','in') for f in files]
        s[k] = []
        for f in infiles:
            fp = open(os.path.join(datadirs[k], f))
            lines = fp.readlines()
            s[k].append(float(lines[3].split()[0]))
            fp.close()
        s[k] = np.array(s[k])
    else:
        files = files2
        s[k] = np.array([float(pattern2.match(f).groups(1)[0]) for f in files])

    sbdata[k] = []
    data[k] = []
    for f in files:
        print(f)
        data[k].append(np.loadtxt(os.path.join(datadir,f)))
        sbdata[k].append(np.loadtxt(os.path.join(datadir,f.replace('.out','_integral.out'))))

    data[k] = np.array(data[k])
    sbdata[k] = np.concatenate(sbdata[k])
    sbdata[k] = sbdata[k][np.abs(sbdata[k][:,1])<0.5]

    order = np.argsort(s[k])
    s[k] = s[k][order]
    data[k] = data[k][order,:]
    sbdata[k] = sbdata[k][order,:]
    condi = (s[k]<smax)*(s[k]>smin)

    data[k] = data[k][condi,:]
    sbdata[k] = sbdata[k][condi,:]
    s[k] = s[k][condi]

    # correction of avnabs = dsdreff = <|nabla s|>
    a = 46.0
    avnabs_old = 2.0/a*np.sqrt(s[k])
    for l in range(1, data[k][1,:].size):
        data[k][:,l] = data[k][:,l]*(avnabs_old/avnabs_interp(s[k]))**2
    for l in range(2,9):
        sbdata[k][:,l] = sbdata[k][:,l]*(avnabs_old/avnabs_interp(s[k]))**2

    if len(files1) > 0: # new file format
        D11do[k] = data[k][:,4]
        D12do[k] = data[k][:,8]
        D12D11do[k] = data[k][:,8]/data[k][:,4]
        D11sb[k] = sbdata[k][:,4]
        D12sb[k] = sbdata[k][:,8]
    else:
        D11do[k] = data[k][:,3]
        D12do[k] = data[k][:,6]
        D12D11do[k] = data[k][:,6]/data[k][:,3]

    D11i[k] = spi.interpolate.interp1d(s[k], D11do[k], bounds_error=False)
    D12i[k] = spi.interpolate.interp1d(s[k], D12do[k], bounds_error=False)
    D12D11i[k] = spi.interpolate.interp1d(s[k], D12D11do[k], bounds_error=False)
    
sMt1  = profdata1[:,0]
Mt1  = profdata1[:,1]
sMt2  = profdata2[:,0]
Mt2  = profdata2[:,1]

sa = np.concatenate(s.values())
D11 = np.concatenate(D11do.values())
D12 = np.concatenate(D12do.values())
D12D11 = np.concatenate(D12D11do.values())
D11sb = np.concatenate(D11sb.values())
D12sb = np.concatenate(D12sb.values())
order = np.argsort(sa)
sa = sa[order]
D11 = D11[order]
D12 = D12[order]
D12D11 = D12D11[order]
D11sb = D11sb[order]
D12sb = D12sb[order]

condi = (~np.isinf(D11))*(~np.isnan(D11))*(~np.isinf(D12))*(~np.isnan(D12))*\
        (~np.isinf(D12D11))*(~np.isnan(D12D11))
sa = sa[condi]
D11 = D11[condi]
D12 = D12[condi]
D12D11 = D12D11[condi]
D11sb = D11sb[condi]
D12sb = D12sb[condi]

facnew = 46./49.41
#facnew = 6816./6845.*46./49.41
#facnew = 0.866

plt.figure(2)
plt.title('D11 ASDEX-U #30835 (RMP) n=2')
plt.hold(1)
plt.semilogy(s[0], D11do[0]*facnew, '-o', linewidth=2, mew=1, ms=5)
plt.semilogy(s[1], D11do[1]*facnew, '-s', linewidth=2, mew=1, ms=5)
plt.semilogy(neos, D11neo, '-', linewidth=2)

#plt.semilogy(sa, D11sb, '-')
#plt.ylim([0,0.4])
plt.ylim([1e-3*max(D11neo),1.1*max(D11neo)])
plt.grid(True, which="both")
plt.xlabel('s')
plt.ylabel('D11/Dp')
plt.legend(['Hamiltonian QL', 'Hamiltonian NL', 'Neo2'], 'best')
#plt.ylim([1e-4,0.05])
#plt.ylim([0,0.002])
ax2 = plt.gca().twinx()
ax2.plot(sMt1, Mt1, 'b')
ax2.plot(sMt2, Mt2, 'r--')
ax2.set_ylabel('M_t', color='b')
for tl in ax2.get_yticklabels():
    tl.set_color('b')
ax2.set_ylim([-0.3, 0.3])
exportfig('asdex_rmp_D11')
