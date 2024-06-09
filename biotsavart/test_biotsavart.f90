program test_biotsavart
    use unittest
    use biotsavart
    implicit none

    real(dp), parameter :: TOL = 1.0d-10

    call test_vector_potential_straight_wire

    contains

    subroutine test_vector_potential_straight_wire
        character(len=*), parameter :: test_name = &
            "test_vector_potential_straight_wire"

        integer, parameter :: NPOINTS_WIRE = 1000
        integer, parameter :: NPOINTS_EVAL = 21
        real(dp), parameter :: WIRE_LENGTH = 2.0d1

        real(dp), dimension(3, NPOINTS_EVAL) :: x_eval, A_ref, A
        real(dp), dimension(3, NPOINTS_WIRE) :: x_wire

        call get_points_on_line(1.0d0, 0.0d0, WIRE_LENGTH, x_eval)
        call get_points_on_line(0.0d0, 0.0d0, WIRE_LENGTH, x_wire)

        call calc_ref_vector_potential_straight_wire( &
            -0.5d0*WIRE_LENGTH, 0.5d0*WIRE_LENGTH, x_eval, A_ref)

        call calc_vector_potential(x_wire, x_eval, A)

        if (maxval(abs(A - A_ref)) < TOL) then
            call pass(test_name)
        else
            call fail(test_name)
        end if

    end subroutine test_vector_potential_straight_wire

    subroutine calc_ref_vector_potential_straight_wire(z1, z2, x, A)
        ! https://physicspages.com/pdf/Electrodynamics/
        ! Magnetic%20vector%20potential.pdf
        real(dp), intent(in) :: z1, z2
        real(dp), intent(in) :: x(:, :)
        real(dp), intent(out) :: A(:, :)

        real(dp) :: rho
        integer :: i

        A(1, :) = 0.0d0
        A(2, :) = 0.0d0

        do i = 1, size(x, 2)
            rho = sqrt(x(1, i)**2 + x(2, i)**2)
            A(3, i) = MU0_OVER_FOURPI*( &
                  asinh((z2 - x(3, i))/rho) - asinh((z1 - x(3, i))/rho))
        end do
    end subroutine calc_ref_vector_potential_straight_wire

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
