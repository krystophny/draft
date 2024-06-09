FC = gfortran
FFLAGS = -fPIC -Wall -Wno-unused-dummy-argument -g -O2
FFLAGS += -I$(CODE)/libneo/build -L$(CODE)/libneo/build -lneo

SOURCES = biotsavart.f90

all: test_biotsavart.x

test_biotsavart.x: unittest.f90 test_biotsavart.f90 biotsavart.so
	$(FC) -o $@ $^ $(FFLAGS)

biotsavart.so: $(SOURCES)
	$(FC) -shared -o $@ $^ $(FFLAGS)

clean:
	rm -f *.o *.so *.x *.mod test_biotsavart
