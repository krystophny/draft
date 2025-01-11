program test_bspline
    use iso_fortran_env, only: dp => real64
    use util_for_test, only: print_test, pass_test, fail_test, skip_test, status
    implicit none

    integer, parameter :: NUM_ITERATIONS = 100000
    real(dp), parameter :: DX = 1.0d-3, DXINV = 1.0_dp/DX

    call test_bspline_basis()
    call test_bspline_basis_order5()
    call test_evaluate_bspline()
    call exit(status)

contains

    subroutine test_bspline_basis()
        use bspline_mod, only: bspline_basis
        integer :: i
        real(dp) :: x(NUM_ITERATIONS), start_time, end_time
        real(dp), volatile :: y  ! Prevent optimizing away the function call
        call print_test("test_bspline_basis")

        call random_number(x)

        call cpu_time(start_time)
        do i = 1, NUM_ITERATIONS
            y = bspline_basis(5, DXINV, 100, x(i))
        end do
        call cpu_time(end_time)

        print *, "Elapsed time: ", end_time - start_time, "s"
        call pass_test
    end subroutine test_bspline_basis


    subroutine test_bspline_basis_order5()
        use bspline_mod, only: bspline_basis_order5
        integer :: i
        real(dp) :: x(NUM_ITERATIONS), start_time, end_time
        real(dp), volatile :: y  ! Prevent optimizing away the function call
        call print_test("test_bspline_basis_order5")

        call random_number(x)

        call cpu_time(start_time)
        do i = 1, NUM_ITERATIONS
            y = bspline_basis_order5(DXINV, 100, x(i))
        end do
        call cpu_time(end_time)

        print *, "Elapsed time: ", end_time - start_time, "s"
        call pass_test
    end subroutine test_bspline_basis_order5

    subroutine test_evaluate_bspline
        call print_test("test_evaluate_bspline")

        call pass_test
    end subroutine test_evaluate_bspline


end program test_bspline
