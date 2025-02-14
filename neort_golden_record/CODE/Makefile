all: NEO-RT

NEO-RT:
	if [ ! -d NEO-RT ]; then \
		git clone https://github.com/itpplasma/NEO-RT CODE/NEO-RT; \
	fi
	make -C NEO-RT

clean:
	make -C NEO-RT clean

.PHONY: all clean NEO-RT
