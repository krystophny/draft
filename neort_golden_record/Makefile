all: CODE DATA

CODE:
	make -C CODE

DATA:
	make -C DATA

clean:
	make -C CODE clean

distclean: clean
	make -C DATA clean

.PHONY: all clean distclean CODE DATA
