module biotsavart
    use libneo_kinds
    use math_constants

    implicit none

    real(dp), parameter :: MU0_OVER_FOURPI = 1.0d-7
    real(dp), parameter :: MU0_OVER_TWOPI = 2.0d-7

    contains

    pure subroutine calc_vector_potential(x_wire, x_eval, A)
        real(dp), intent(in) :: x_wire(:, :), x_eval(:, :)
        real(dp), intent(out) :: A(:, :)

        integer :: i

        do i = 1, size(x_eval, 2)
            call calc_vector_potential_single(x_wire, x_eval(:, i), A(:, i))
        end do
    end subroutine calc_vector_potential

    pure subroutine calc_vector_potential_single(x_wire, x_eval, A)
        real(dp), intent(in) :: x_wire(:, :), x_eval(:)
        real(dp), intent(out) :: A(:)

        real(dp) :: dl(3), l2, l, e_l(3), delta_r(3), rho(3), &
            rho2, z, R_i, R_f, eps
        integer :: j

        A = 0.0d0
        do j = 1, size(x_wire, 2) - 1
            dl = x_wire(:, j + 1) - x_wire(:, j)
            l2 = dl(1)**2 + dl(2)**2 + dl(3)**2
            l = sqrt(l2)
            e_l = dl / l
            delta_r = x_eval - x_wire(:, j)
            z = delta_r(1)*e_l(1) + delta_r(2)*e_l(2) + delta_r(3)*e_l(3)
            rho = delta_r - z*e_l
            rho2 = rho(1)**2 + rho(2)**2 + rho(3)**2
            R_i = sqrt(rho2 + z**2)
            R_f = sqrt(rho2 + (l - z)**2)
            eps = l / (R_i + R_f)

            A = A + MU0_OVER_TWOPI * e_l * atanh(eps)
        end do

    end subroutine calc_vector_potential_single

end module biotsavart
