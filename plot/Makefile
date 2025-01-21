all: draft_pyplot.x

draft_pyplot.x: draft_pyplot.f90 pyplot_module.o
	gfortran -o draft_pyplot.x draft_pyplot.f90 pyplot_module.o

pyplot_module.o: pyplot_module.F90
	gfortran -cpp -c pyplot_module.F90
