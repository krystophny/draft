FC = gfortran
CXX = g++
FFLAGS = -fPIC -Wall -Wno-unused-dummy-argument -g -Og
CXXFLAGS = -fPIC -Wall -g -Og \
	-I$(CODE)/external/mfem-4.7/build -I/usr/include/suitesparse \
	-L$(CODE)/external/mfem-4.7/build -lmfem \
	-lumfpack -lamd -lcholmod -lcolamd -lklu

SOURCES = field_abstract.f90 field_mfem.f90 field.f90

all: libfield.so test_mfem.x aug.msh

libfield.so: $(SOURCES)
	$(FC) -shared -o $@ $^ $(FFLAGS)

test_mfem.x: field_mfem_interface.cpp
	$(CXX) -o $@ $^ $(CXXFLAGS)

aug.msh: convexwall_to_mesh.py convexwall_aug_inside_bcoils.dat
	python $^ $@

clean:
	rm -f *.o *.so *.x *.mod *.msh
