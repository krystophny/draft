module bspline_mod
    use iso_fortran_env, only: dp => real64
    implicit none

contains

    pure function construct_bspline(order, dxinv, y, periodic) result(coeffs)
        real(dp), intent(in) :: dxinv, y(:)
        logical, intent(in) :: periodic
        real(dp), allocatable :: coeffs(:)

        allocate(coeffs(size(y)+2*order))
    end function construct_bspline

    pure recursive function bspline_basis(order, dxinv, i, x) result(y)
        integer, intent(in) :: order
        real(dp), intent(in) :: dxinv  ! Inversed grid spacing
        integer, intent(in) :: i
        real(dp), intent(in) :: x
        real(dp) :: y

        real(dp) :: weight

        select case (order)
            ! Unrolled for performance.
            ! For the last 10% speed, call the inner functions directly
            case (0)
                y = bspline_basis_order0(dxinv, i, x)
            case (1)
                y = bspline_basis_order1(dxinv, i, x)
            case (2)
                y = bspline_basis_order2(dxinv, i, x)
            case (3)
                y = bspline_basis_order3(dxinv, i, x)
            case (4)
                y = bspline_basis_order4(dxinv, i, x)
            case (5)
                y = bspline_basis_order5(dxinv, i, x)
            case default
                weight = x*dxinv - i
                y = weight*bspline_basis(order - 1, dxinv, i, x) &
                    + (1.0_dp - weight)*bspline_basis(order - 1, dxinv, i + 1, x)
        end select
    end function bspline_basis

    pure function bspline_basis_order0(dxinv, i, x) result(y)
        real(dp), intent(in) :: dxinv
        integer, intent(in) :: i
        real(dp), intent(in) :: x
        real(dp) :: x_times_dxinv
        real(dp) :: y

        x_times_dxinv = x*dxinv
        if (i <= x_times_dxinv .and. x_times_dxinv < (i + 1)) then
            y = 1.0_dp
            return
        end if
        y = 0.0_dp
    end function bspline_basis_order0

    pure function bspline_basis_order1(dxinv, i, x) result(y)
        real(dp), intent(in) :: dxinv
        integer, intent(in) :: i
        real(dp), intent(in) :: x
        real(dp) :: y

        real(dp) :: weight

        weight = x*dxinv - i
        y = weight*bspline_basis_order0(dxinv, i, x) &
            + (1.0_dp - weight)*bspline_basis_order0(dxinv, i + 1, x)
    end function bspline_basis_order1

    pure function bspline_basis_order2(dxinv, i, x) result(y)
        real(dp), intent(in) :: dxinv
        integer, intent(in) :: i
        real(dp), intent(in) :: x
        real(dp) :: y

        real(dp) :: weight

        weight = x*dxinv - i
        y = weight*bspline_basis_order1(dxinv, i, x) &
            + (1.0_dp - weight)*bspline_basis_order1(dxinv, i + 1, x)
    end function bspline_basis_order2

    pure function bspline_basis_order3(dxinv, i, x) result(y)
        real(dp), intent(in) :: dxinv
        integer, intent(in) :: i
        real(dp), intent(in) :: x
        real(dp) :: y

        real(dp) :: weight

        weight = x*dxinv - i
        y = weight*bspline_basis_order2(dxinv, i, x) &
            + (1.0_dp - weight)*bspline_basis_order2(dxinv, i + 1, x)
    end function bspline_basis_order3

    pure function bspline_basis_order4(dxinv, i, x) result(y)
        real(dp), intent(in) :: dxinv
        integer, intent(in) :: i
        real(dp), intent(in) :: x
        real(dp) :: y

        real(dp) :: weight

        weight = x*dxinv - i
        y = weight*bspline_basis_order3(dxinv, i, x) &
            + (1.0_dp - weight)*bspline_basis_order3(dxinv, i + 1, x)
    end function bspline_basis_order4

    pure function bspline_basis_order5(dxinv, i, x) result(y)
        real(dp), intent(in) :: dxinv
        integer, intent(in) :: i
        real(dp), intent(in) :: x
        real(dp) :: y

        real(dp) :: weight

        weight = x*dxinv - i
        y = weight*bspline_basis_order4(dxinv, i, x) &
            + (1.0_dp - weight)*bspline_basis_order4(dxinv, i + 1, x)
    end function bspline_basis_order5

end module bspline_mod
