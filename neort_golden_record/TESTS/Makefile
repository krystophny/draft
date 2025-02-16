FC ?= gfortran
FFLAGS = -O3 -J OBJS
NEORT_PATH ?= ../CODE/NEO-RT

all: compare_flux_surface_area.x

compare_flux_surface_area.x: compare_flux_surface_area.f90
	$(FC) $(FFLAGS) -o $@ \
		$(NEORT_PATH)/build/OBJS/_deps/spline-src/spline.f90 \
		$(NEORT_PATH)/SRC/common.f90 \
		$(NEORT_PATH)/SRC/do_magfie_standalone.f90 \
		-lblas -llapack $<

clean:
	rm -f compare_flux_surface_area.x

.PHONY: all clean
