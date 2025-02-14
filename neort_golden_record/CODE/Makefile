all: NEO-RT NEO-RT.old

NEO-RT:
	if [ ! -d NEO-RT ]; then \
		git clone https://github.com/itpplasma/NEO-RT CODE/NEO-RT; \
	fi
	make -C NEO-RT

NEO-RT.old: NEO-RT
	if [ ! -d NEO-RT.old ]; then \
		mkdir -p NEO-RT.old; \
	fi
	ORIGIN="../NEO-RT" make -C NEO-RT.old

clean:
	make -C NEO-RT clean
	make -C NEO-RT.old clean

.PHONY: all clean NEO-RT NEO-RT.old
