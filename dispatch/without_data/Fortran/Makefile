FC = gfortran-14
FFLAGS = -O3 -march=native

all: dispatch_switch.x

dispatch_switch.x: dispatch_switch.f90 dispatch_switch_module.f90
	$(FC) $(FFLAGS) -o dispatch_switch.x dispatch_switch_module.f90 dispatch_switch.f90

clean:
	rm -f *.x *.o
