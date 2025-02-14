# Define the origin URL of the repository
ORIGIN ?= https://github.com/itpplasma/NEO-RT

# List of tags to clone
TAGS = v2016.01.27 v2016.03.31

# Default target
all: $(TAGS)

# Rule to clone each tag into a directory named after the tag
$(TAGS): deps
	@if [ ! -d "$@" ]; then \
		echo "Cloning tag $@..."; \
		git clone --branch $@ $(ORIGIN) $@; \
	else \
		echo "Directory $@ already exists. Skipping clone."; \
	fi
	cd $@ && \
	touch neo.in && \
	touch tok-synch2-n0.bc && \
	ln -s ../BOOZER_MAGFIE . && \
	ln -s ../spline . && \
	ln -s ../contrib . && \
	mkdir -p build && \
	cd build && \
	cmake -GNinja .. && \
	ninja


deps:
	if [ ! -d BOOZER_MAGFIE ]; then \
		git clone https://github.com/itpplasma/BOOZER_MAGFIE; \
	fi
	if [ ! -d spline ]; then \
		git clone https://github.com/itpplasma/spline; \
	fi

	mkdir -p contrib
	cd contrib  && \
	if [ ! -d quadpack ]; then \
		git clone https://github.com/itpplasma/quadpack; \
	fi ;\
	if [ ! -d vode ]; then \
		git clone https://github.com/itpplasma/vode; \
	fi


# Clean up cloned directories
clean:
	rm -rf $(TAGS) BOOZER_MAGFIE spline contrib

.PHONY: all deps clean
