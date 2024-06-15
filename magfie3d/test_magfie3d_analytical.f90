program test_biotsavart
    use unittest
    use magfie3d_analytical
    implicit none

    real(dp), parameter :: TOL = 1.0d-10

    call test_afield_wires_straight

    contains

    subroutine test_afield_wires_straight
        character(len=*), parameter :: test_name = &
            "test_afield_wires_straight"

        integer, parameter :: NPOINTS_WIRE = 1000
        integer, parameter :: NPOINTS_EVAL = 21
        real(dp), parameter :: WIRE_LENGTH = 2.0d1

        real(dp), dimension(3, NPOINTS_EVAL) :: x_eval, A_ref, A
        real(dp), dimension(3, NPOINTS_WIRE) :: x_wires

        type(magfie3d_wires_t) :: field

        call get_points_on_line(1.0d0, 0.0d0, WIRE_LENGTH, x_eval)
        call get_points_on_line(0.0d0, 0.0d0, WIRE_LENGTH, x_wires)

        call field%init(x_wires)

        call afield_wire_zaxis(-0.5d0*WIRE_LENGTH, 0.5d0*WIRE_LENGTH, x_eval, A_ref)

        call field%afield(x_eval, A)

        if (maxval(abs(A - A_ref)) < TOL) then
            call pass(test_name)
        else
            call fail(test_name)
        end if

    end subroutine test_afield_wires_straight


    subroutine get_points_on_line(x1, x2, length, x)
        real(dp), intent(in) :: x1, x2, length
        real(dp), intent(out) :: x(:, :)

        integer :: i

        x(1, :) = x1
        x(2, :) = x2

        do i = 1, size(x, 2)
            x(3, i) = (i-1)*length/(size(x, 2)-1) - 0.5d0*length
        end do
    end subroutine get_points_on_line

end program test_biotsavart
