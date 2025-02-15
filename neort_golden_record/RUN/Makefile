NEWCODE ?= ../CODE/NEO-RT
REFDATA ?= ../DATA
RUNS = 2016-01-27_n1_magdrift 2016-01-28_n1_nomag 2016-03-11_n1_noshear 2016-03-11_n1_shear 170118_n1_ql 170118_n1_nonlin 170118_n1_noshear_ql 170118_n1_noshear_nonlin
FILES = neo.in neo_pert.in in_file in_file_pert Mtprofile.in

all: $(RUNS)

$(RUNS):
	mkdir -p $@
	for file in $(FILES); do cp $(REFDATA)/$@/$$file $@; done
	cp $(REFDATA)/$@/thetafun_inp.dat $@/thetafun_inp.dat | true
	cp $(REFDATA)/$@/plasma.in $@/plasma.in | true
	ln -s Mtprofile.in $@/profile.in
	python convert_input_old_to_new.py \
		$(REFDATA)/$@/driftorbit.in.template > $@/driftorbit.in.template
	ln -s ../$(NEWCODE)/build/neo_rt.x $@/driftorbit_test
	ln -s ../run_driftorbit.py $@/run_driftorbit.py
	ln -s ../Makefile.template $@/Makefile

run:
	for run in $(RUNS); do \
		cd $$run; \
		make -j8; \
		cd ..; \
	done

clean:
	rm -rf $(RUNS)

.PHONY: all clean run $(RUNS)
