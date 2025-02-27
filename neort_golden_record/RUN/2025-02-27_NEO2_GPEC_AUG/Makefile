RUNS := $(shell seq 0 99)
NAMES := $(addprefix driftorbit, $(RUNS))
OUTPUTS := $(addsuffix .out, $(NAMES))

all: $(OUTPUTS)

$(OUTPUTS): plasma.in profile.in
	python run_driftorbit.py $(subst driftorbit,,$(basename $@)) > $@.log 2>&1

plasma.in profile.in:
	python $(CODE)/NEO-RT/python/neo_2_profile_to_neo_rt_profile.py

.PHONY: all $(OUTPUTS) clean

clean:
	rm -f *.out $(addsuffix .log, $(OUTPUTS)) $(addsuffix .in, $(NAMES))
