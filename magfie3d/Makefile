FC = gfortran
FFLAGS = -fPIC -Wall -Wno-unused-dummy-argument -g -Og
FFLAGS += -I$(CODE)/libneo/build -L$(CODE)/libneo/build -lneo

SOURCES = magfie3d_abstract.f90 magfie3d_analytical.f90 magfie3d.f90

all: test_magfie3d_analytical.x

test_magfie3d_analytical.x: $(CODE)/draft/unittest/unittest.f90 \
	test_magfie3d_analytical.f90 libmagfie3d.so
	$(FC) -o $@ $^ $(FFLAGS)

libmagfie3d.so: $(SOURCES)
	$(FC) -shared -o $@ $^ $(FFLAGS)

clean:
	rm -f *.o *.so *.x *.mod
