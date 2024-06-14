FC = gfortran
FFLAGS = -fPIC -Wall -Wno-unused-dummy-argument -g -O2
FFLAGS += -I$(CODE)/libneo/build -L$(CODE)/libneo/build -lneo

SOURCES = field3d_abstract.f90 field3d_analytical.f90 field3d.f90

all: test_field3d_analytical.x

test_field3d_analytical.x: $(CODE)/draft/unittest/unittest.f90 \
	test_field3d_analytical.f90 libfield3d.so
	$(FC) -o $@ $^ $(FFLAGS)

libfield3d.so: $(SOURCES)
	$(FC) -shared -o $@ $^ $(FFLAGS)

clean:
	rm -f *.o *.so *.x *.mod
