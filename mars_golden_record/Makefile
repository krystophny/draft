MARS_SCRIPTS = $(CODE)/external/MARS/RZplot

all: run

run: main.m MacConvBoozerCustom.m
	matlab -nodisplay -batch main


MacConvBoozerCustom.m: Makefile
	sed -e "s|SDIR = '/cscratch/liuy/WorkReKink/';|% &|g" \
		-e "s|Mac.Norm   = 2.4229e-3;  % normalization factor|% &|g" \
		$(MARS_SCRIPTS)/MacConvBoozer.m > MacConvBoozerCustom.m

clean:
	rm -f MacConvBoozerCustom.m  \
	GRADSM0.OUT MacDataS MacRmid.txt MacSurfS probe.points.rz.in RLFS.txt \
	amp.dat mode_imag.dat mode_real.dat r_orbit
