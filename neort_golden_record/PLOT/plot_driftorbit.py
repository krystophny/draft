#!/usr/bin/python

import numpy as np
import scipy.interpolate as spi
import matplotlib.pyplot as plt
import os
import re
import sys
import f90nml
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

neodata = np.loadtxt(sys.argv[1], comments="%")

#neodata = np.loadtxt('ntv_out_asdex30835_rmp_n2_HatFun14.dat')
#neodata = np.loadtxt('ntv_out_nonlocal.dat')

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

datadirs = sys.argv[2:]
datadirs = {i:datadirs[i] for i in range(len(datadirs))}

#datadirs[0] = '/temp/ert/CONDOR/driftorbit/n1_supban_0.6'
#datadirs[0] = '/proj/plasma/RMP/DOC/Diss_Albert/code/driftorbit/ASDEX/30835_rmp/n1_driftorbit'
#datadirs[1] = '/proj/plasma/RMP/DOC/Diss_Albert/code/driftorbit/ASDEX/30835_rmp/n1_supban'
#datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-01-28_n1_nomag'
#datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-01-27_n1_magdrift'
#datadirs[1] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-02-22_n1_0.20'
#datadirs[2] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-01-28_n1_0.45'
#datadirs[3] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-01-28_n1_0.6'
#datadirs[4] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-02-22_n1_0.70'
#datadirs[5] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-01-28_n1_0.75'
#datadirs[6] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-01-28_n1_0.85'
#datadirs[7] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-02-22_n1_0.90'

#datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-02-22_n1_nomag'
#datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-02-22_n3_nomag'
#datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-03-29_rmp_m150_shear'
#datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-03-31_rmp_90_n3_noshear'
#datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-03-xx_shear'
#datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-03-11_n1_shear'
#datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-08-24_n1_shear_newmagfie'
#datadirs[0] = '/temp/ert/CONDOR/driftorbit/RUNS_CIRC/160328_supban_s_shear'
#datadirs[1] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-03-18_shear_0.6'
#datadirs[2] = '/temp/ert/CONDOR/driftorbit/RUNS_ASDEX/2016-03-18_shear_0.8'


#datadirs[0] = '/temp/ert/CONDOR/driftorbit/n1_magdrift_0.85'
#datadirs[1] = '/temp/ert/CONDOR/driftorbit/n1_supban.2'
#datadirs[1] = '/temp/ert/CONDOR/driftorbit/n1_magdrift_0.6'
#datadirs[1] = '/temp/ert/CONDOR/driftorbit/n1.2'
#profname = os.path.join(datadir,'Mtprofile.in')
profname1 = os.path.join('Mtprofile_Hamiltonian.dat')
profname2 = os.path.join('Mtprofile190.dat')
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
            try:
                nml = f90nml.read(os.path.join(datadirs[k], f))
                s[k].append(nml['params']['s'])
            except:
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

# ASDEX with shear
#condi1 = np.logical_or(s[0] < 0.6, s[0] > 0.75)
# only for ASDEX runs from 2016-01-27_n1_magdrift plus finer grid
#condi1 = np.logical_or(s[0] < 0.45, np.logical_and(s[0] > 0.46, s[0] < 0.6))
#s[0] = s[0][condi1]
#D11do[0] = D11do[0][condi1]
#D12do[0] = D12do[0][condi1]
#D11sb[0] = D11sb[0][condi1]
#D12sb[0] = D12sb[0][condi1]
#D12D11do[0] = D12D11do[0][condi1]

sMt1  = profdata1[:,0]
Mt1  = profdata1[:,1]
sMt2  = profdata2[:,0]
Mt2  = profdata2[:,1]

sa = np.concatenate(list(s.values()))
D11 = np.concatenate(list(D11do.values()))
D12 = np.concatenate(list(D12do.values()))
D12D11 = np.concatenate(list(D12D11do.values()))
D11sb = np.concatenate(list(D11sb.values()))
D12sb = np.concatenate(list(D12sb.values()))
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


#plt.figure(1)
#plt.plot(s[1], avnabs_old, 'x-')
#
#plt.plot(neos, avnabs[condi2])
#plt.plot(s[1], avnabs_interp(s[1]))
#plt.grid(1)
#exportfig('asdex_rmp_sprofile')
#plt.show()

#plt.figure(1)
#plt.plot(neos, 1/neodata[:,10], 'x-')
#plt.title('q profile')
#exportfig('asdex_rmp_sprofile')
#plt.show()

plt.figure(2)
plt.title('D11 ASDEX-U #30835 (RMP) n=2')
#plt.semilogy(s, data[:,1], '.')
#plt.semilogy(s, data[:,2], 'd')
#plt.semilogy(np.sqrt(s[0]), D11do[0], '-x')
plt.semilogy(sa, D11, '-x')

#plt.semilogy(s[0], D11do[0]+D11i[1](s[0]), '-', linewidth=3)
plt.semilogy(neos, D11neo, '-', linewidth=2)
plt.semilogy(sa, D11sb, '-')
#plt.ylim([0,0.4])
plt.ylim([1e-3*max(D11neo),1.1*max(D11neo)])
plt.grid(True, which="both")
plt.xlabel('s')
plt.ylabel('D11/Dp')
plt.legend(['Hamiltonian', 'Neo2', 'Superbanana'])
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

plt.figure(4)
plt.title('D12 ASDEX-U #30835 (RMP) n=2')
#plt.semilogy(s, data[:,1], '.')

#plt.semilogy(s, data[:,2], 'd')
#plt.semilogy(np.sqrt(s[0]), D12do[0], '-o')
plt.semilogy(sa, D12, '-x')
plt.semilogy(neos, D12neo, '-')
plt.semilogy(sa, D12sb, '-')
#plt.ylim([0,0.4])
plt.grid(True, which="both")
plt.xlabel('s')
plt.ylabel('D12/Dp')
plt.legend(['Hamiltonian', 'Neo2', 'Superbanana'])
#plt.ylim([3e-4,0.1])
plt.ylim([1e-3*max(D12neo),1.1*max(D12neo)])
ax2 = plt.gca().twinx()
ax2.plot(sMt1, Mt1, 'b')
ax2.plot(sMt2, Mt2, 'r--')
ax2.set_ylabel('M_t', color='b')
for tl in ax2.get_yticklabels():
    tl.set_color('b')
ax2.set_ylim([-0.3, 0.3])
exportfig('asdex_rmp_D12')

plt.figure(3)
plt.title('ASDEX-U #30835 D12/D11 (RMP) n=2')
#plt.semilogy(s, data[:,1], '.')

#plt.semilogy(s, data[:,2], 'd')
plt.plot(sa, D12D11, '-x')
#plt.plot(np.sqrt(s[0]), (D12do[0]+D12i[1](s[0]))/(D11do[0]+D11i[1](s[0])), '--x')
plt.plot(neos, D12D11neo, '-')
#plt.ylim([1e-2,2e-1])
#plt.ylim([1e-6,0.1])
plt.grid(True)
plt.xlabel('s')
plt.ylabel('D12/D11')
plt.legend(['Hamiltonian', 'Neo2'])
ax2 = plt.gca().twinx()
ax2.plot(sMt1, Mt1, 'b')
ax2.plot(sMt2, Mt2, 'r--')
ax2.set_ylabel('M_t', color='b')
for tl in ax2.get_yticklabels():
    tl.set_color('b')
ax2.set_ylim([-0.3, 0.3])
exportfig('asdex_rmp_D12D11')

## plt.subplot(2,2,4)
## plt.plot(s, data[:,4]/data[:,1], '.')
##
## plt.plot(s, data[:,5]/data[:,2], 'x')
## plt.plot(s, data[:,6]/data[:,3], 'o')
#plt.show()

np.savetxt('tot_rmp_n3_noshear.txt', np.transpose([sa,D11,D12]))
np.savetxt('sb_rmp_n3_noshear.txt', np.transpose([sa,D11sb,D12sb]))
np.savetxt('do_rmp_n3_noshear.txt', np.transpose([sa,D11-D11sb,D12-D12sb]))

plt.show()
