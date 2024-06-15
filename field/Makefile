FC = gfortran
FFLAGS = -fPIC -Wall -Wno-unused-dummy-argument -g -Og

SOURCES = field_abstract.f90

all: libfield.so

libfield.so: $(SOURCES)
	$(FC) -shared -o $@ $^ $(FFLAGS)

clean:
	rm -f *.o *.so *.x *.mod
