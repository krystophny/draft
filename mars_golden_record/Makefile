all: MacConvBoozer.m MacReadRMZM.m MacGetRZ.m MacGetUnitVec.m MacReadVPLASMA.m

MacConvBoozer.m:
	sed "s|SDIR = '/cscratch/liuy/WorkReKink/';|SDIR = '/var/tmp/ert/data/DEMO/MARS/MARSQ_OUTPUTS_100kAt_dBkinetic_NTVkinetic_NEO2profs_KEYTORQ_1/';|g" /var/tmp/ert/code/external/MARS/RZplot/MacConvBoozer.m > MacConvBoozer.m

MacReadRMZM.m:
	cp /var/tmp/ert/code/external/MARS/RZplot/MacReadRMZM.m .

MacGetRZ.m:
	cp /var/tmp/ert/code/external/MARS/RZplot/MacGetRZ.m .

MacGetUnitVec.m:
	cp /var/tmp/ert/code/external/MARS/RZplot/MacGetUnitVec.m .

MacReadVPLASMA.m:
	cp /var/tmp/ert/code/external/MARS/RZplot/MacReadVPLASMA.m .

clean:
	rm -f MacConvBoozer.m MacReadRMZM.m MacGetRZ.m MacGetUnitVec.m MacReadVPLASMA.m
	rm -f GRADSM0.OUT MacDataS MacRmid.txt MacSurfS probe.points.rz.in RLFS.txt
