REFCODE ?= ../CODE/NEO-RT.old/v2016.01.27
REFDATA ?= ../DATA
RUNS = 2016-01-27_n1_magdrift 2016-01-28_n1_nomag 2016-03-11_n1_noshear 2016-03-11_n1_shear
FILES = neo.in neo_pert.in in_file in_file_pert Mtprofile.in driftorbit.in.template

all: $(RUNS)

$(RUNS):
	mkdir -p $@
	for file in $(FILES); do cp $(REFDATA)/$@/$$file $@; done
	ln -s ../$(REFCODE)/build/driftorbit_test $@/driftorbit_test
	ln -s ../run_driftorbit.py $@/run_driftorbit.py

clean:
	rm -rf $(RUNS)

.PHONY: all clean $(RUNS)
