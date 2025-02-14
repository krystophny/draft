all: CODE/NEO-RT/build/neo_rt.x

CODE/NEO-RT/build/neo_rt.x: CODE/NEO-RT
	cd CODE/NEO-RT && make

CODE/NEO-RT:
	if [ ! -d CODE/NEO-RT ]; then \
		git clone https://github.com/itpplasma/NEO-RT CODE/NEO-RT; \
	fi

clean:
	make -C CODE/NEO-RT clean

.PHONY: all clean
