import booz_xform as bx
import os
import numpy as np
import matplotlib.pyplot as plt
from libneo import boozer
import scipy.constants as const

b = bx.Booz_xform()
b.read_wout("/afs/itp.tugraz.at/user/zenzphil/convertor_comparison/AUG/booz_xform/g30835.32000/wout_g30835.3200_EQH.nc")
print(b)
b.mboz = 13
b.nboz = 0
b.run()
"""print(b.mboz) # max poloidal mode number
print(b.nboz) # max toroidal mode number
print(b.ns_b) # number of surfaces on which the Boozer coordinates are calculated
#missing equivalent to nper
print(b.phi[-1]) # toroidal flux at the edge
#missing minor radius
#missing major radius

print(b.s_b) # Values of normalized toroidal flux for which the output data is stored
print(b.iota) # rotational transform on input grid! might be problematic if not calculating on all surfaces
#missing j_pol/nper
#missing current
#missing pprime
# Boozer metric tensor?

#missing m
#missing n
print(b.rmnc_b) # Fourier harmonics of R in boozer
print(b.rmns_b) # Fourier harmonics of R in boozer
print(b.zmnc_b) # Fourier harmonics of Z in boozer
print(b.zmns_b) # Fourier harmonics of Z in boozer
print(b.numnc_b) # Fourier harmonics of nu in boozer
print(b.numns_b) # Fourier harmonics of nu in boozer
print(b.bmnc_b) # Fourier harmonics of B in boozer
print(b.bmns_b) # Fourier harmonics of B in boozer"""


b.write_boozmn("boozmn_30835.3200_EQH.nc")

boozer_libneo=boozer.BoozerFile("")
boozer_libneo.initialize()

def print_boozer_file_properties(boozer_file):
    print(f"Comments: {boozer_file.comments}")
    print(f"m0b: {boozer_file.m0b}")
    print(f"n0b: {boozer_file.n0b}")
    print(f"nsurf: {boozer_file.nsurf}")
    print(f"nper: {boozer_file.nper}")
    print(f"Flux: {boozer_file.flux}")
    print(f"a: {boozer_file.a}")
    print(f"R: {boozer_file.R}")
    print(f"s: {boozer_file.s}")
    print(f"iota: {boozer_file.iota}")
    print(f"Jpol_divided_by_nper: {boozer_file.Jpol_divided_by_nper}")
    print(f"Itor: {boozer_file.Itor}")
    print(f"pprime: {boozer_file.pprime}")
    print(f"sqrt_g_00: {boozer_file.sqrt_g_00}")
    print(f"m: {boozer_file.m}")
    print(f"n: {boozer_file.n}")
    print(f"rmnc: {boozer_file.rmnc}")
    print(f"rmns: {boozer_file.rmns}")
    print(f"zmnc: {boozer_file.zmnc}")
    print(f"zmns: {boozer_file.zmns}")
    print(f"vmnc: {boozer_file.vmnc}")
    print(f"vmns: {boozer_file.vmns}")
    print(f"bmnc: {boozer_file.bmnc}")
    print(f"bmns: {boozer_file.bmns}")
print("xm_b")
print(b.xm_b)
print("xn_b")
print(b.xn_b)
print("rmnc_b")
print(b.rmnc_b.shape)



boozer_libneo.comments = []
boozer_libneo.m0b = b.mboz
boozer_libneo.n0b = b.nboz
boozer_libneo.nsurf = b.ns_b
boozer_libneo.nper = b.nfp
boozer_libneo.flux = b.phi[-1] #[Tm^2]
boozer_libneo.a = b.rmnc_b[1,-1]
boozer_libneo.R = b.rmnc_b[0,0] # [m]
boozer_libneo.s = b.s_b
boozer_libneo.iota = b.iota
boozer_libneo.Jpol_divided_by_nper = - 2*np.pi/const.mu_0*b.Boozer_G/boozer_libneo.nper#np.zeros(len(b.s_b))
boozer_libneo.Itor = - 2*np.pi/const.mu_0*b.Boozer_I#np.zeros(len(b.s_b))
boozer_libneo.pprime = np.zeros(len(b.s_b))
boozer_libneo.sqrt_g_00 = np.zeros(len(b.s_b))
boozer_libneo.m = np.tile(b.xm_b,(b.s_b.size,1))
boozer_libneo.n = np.tile(b.xn_b,(b.s_b.size,1))
boozer_libneo.rmnc = b.rmnc_b.T # [m]
boozer_libneo.rmns = b.rmns_b.T # [m]
boozer_libneo.zmnc = b.zmnc_b.T # [m]
boozer_libneo.zmns = b.zmns_b.T # [m]
boozer_libneo.vmnc = b.numnc_b.T
boozer_libneo.vmns = b.numns_b.T
boozer_libneo.bmnc = b.bmnc_b.T # [T]
boozer_libneo.bmns = b.bmns_b.T # [T]

boozer_libneo.write("test_boozer.bc")

print_boozer_file_properties(boozer_libneo)

bx.modeplot(b)
plt.show()
