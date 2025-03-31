FC ?= gfortran-14
FFLAGS ?= -O3 -march=native

all: dispatch_class.x dispatch_switch.x dispatch_container.x

dispatch_switch.x: dispatch_switch.f90 dispatch_switch_module.f90
	$(FC) $(FFLAGS) -o dispatch_switch.x dispatch_switch_module.f90 dispatch_switch.f90

dispatch_class.x: dispatch_class.f90 dispatch_class_module.o
	$(FC) $(FFLAGS) -o dispatch_class.x dispatch_class_module.o dispatch_class.f90

dispatch_class_module.o: dispatch_class_module.f90
	$(FC) $(FFLAGS) -c dispatch_class_module.f90

dispatch_container.x: dispatch_container.f90
	$(FC) $(FFLAGS) -std=f95 -o dispatch_container.x dispatch_container.f90

f90wrap_class:
	f90wrap -m dispatch_class_module dispatch_class_module.f90 -v
	f2py-f90wrap --build-dir . -c -m _dispatch_class_module f90wrap_dispatch_class_module.f90 dispatch_class_module.o

clean:
	rm -f *.x *.o

.PHONY: all f90wrap clean
