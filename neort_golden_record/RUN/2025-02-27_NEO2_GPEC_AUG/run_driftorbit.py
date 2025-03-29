#!/usr/bin/python

import numpy as np
import os
import re
import sys

tempname = 'driftorbit.in.template'
profname = 'profile.in'
profdata = np.loadtxt(profname)

exename = sys.argv[0]
fsnum = int(sys.argv[1])

s      = profdata[fsnum,0]
M_t    = profdata[fsnum,1]
vth    = profdata[fsnum,2]

dic = {
'<S_TOKEN>': s,
'<M_T_TOKEN>': M_t,
'<VTH_TOKEN>': vth
}

pattern = re.compile('|'.join(dic.keys()))

runname = 'driftorbit{}'.format(fsnum)

with open(tempname,'r') as tempf, open(runname+'.in','w') as outf:
    for line in tempf:
        result = pattern.sub(lambda x: str(dic[x.group()]), line)
        outf.write(result)

os.system('time ./neo_rt.x {}'.format(runname))
