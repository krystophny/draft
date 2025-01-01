all: MacConvBoozerCustom.m

MacConvBoozerCustom.m:
	sed -e "1i\\addpath('/var/tmp/ert/code/external/MARS/RZplot');" \
		-e "1i\\Mac.mm_plot = [];" \
		-e "1i\\Mac.edge = 1.0;" \
		-e "1i\\Mac.core = 0.0;" \
		-e "1i\\Mac.plot_VM = 0;" \
		-e "1i\\Mac.plot_VS = 0;" \
		-e "s|SDIR = '/cscratch/liuy/WorkReKink/';|SDIR = '/var/tmp/ert/data/DEMO/MARS/MARSQ_OUTPUTS_100kAt_dBkinetic_NTVkinetic_NEO2profs_KEYTORQ_1/';|g" \
		/var/tmp/ert/code/external/MARS/RZplot/MacConvBoozer.m > MacConvBoozerCustom.m

clean:
	rm -f MacConvBoozerCustom.m MacReadRMZM.m MacGetRZ.m MacGetUnitVec.m MacReadVPLASMA.m
	rm -f GRADSM0.OUT MacDataS MacRmid.txt MacSurfS probe.points.rz.in RLFS.txt
	rm -f amp.dat mode_imag.dat mode_real.dat r_orbit
