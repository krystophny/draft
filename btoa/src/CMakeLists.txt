add_library(bspline SHARED
    bspline.f90
)

install(TARGETS bspline DESTINATION lib)
