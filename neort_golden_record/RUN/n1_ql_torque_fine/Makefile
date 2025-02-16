RUNS := $(shell seq 0 99)
NAMES := $(addprefix driftorbit, $(RUNS))
OUTPUTS := $(addsuffix .out, $(NAMES))

all: $(OUTPUTS)

$(OUTPUTS):
	python run_driftorbit.py $(subst driftorbit,,$(basename $@)) > $@.log 2>&1

.PHONY: all $(OUTPUTS)
