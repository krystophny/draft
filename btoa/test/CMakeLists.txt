enable_testing()

add_executable(test_bspline.x
    test_bspline.f90
    util_for_test.f90
)
target_link_libraries(test_bspline.x PRIVATE bspline)

add_test(NAME test_bspline COMMAND test_bspline.x)
