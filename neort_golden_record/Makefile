all: CODE DATA

CODE:
	make -C CODE

DATA:
	make -C DATA

RUN:
	make -C RUN

clean:
	make -C CODE clean

distclean: clean
	make -C DATA clean
	make -C RUN clean

.PHONY: all clean distclean CODE DATA RUN
