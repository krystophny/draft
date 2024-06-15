module magfie3d_analytical
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use magfie3d_abstract

    implicit none

    real(dp), parameter :: MU0_OVER_FOURPI = 1.0d-7
    real(dp), parameter :: MU0_OVER_TWOPI = 2.0d-7


    type, extends(magfie3d_t) :: magfie3d_wires_t
        real(dp), allocatable :: x_wires(:, :)
        contains
        procedure :: init => magfie3d_wires_init
        procedure :: afield => magfie3d_wires_afield
    end type magfie3d_wires_t

    contains

    subroutine magfie3d_wires_init(self, x_wires)
        class(magfie3d_wires_t), intent(inout) :: self
        real(dp), intent(in) :: x_wires(:, :)

        self%x_wires = x_wires
    end subroutine magfie3d_wires_init

    subroutine magfie3d_wires_afield(self, x, A)
        class(magfie3d_wires_t), intent(inout) :: self
        real(dp), intent(in) :: x(:, :)
        real(dp), intent(inout) :: A(:, :)

        call afield_wires(self%x_wires, x, A)
    end subroutine magfie3d_wires_afield


    pure subroutine afield_wires(x_wires, x_eval, A)
        ! https://doi.org/10.1016/j.cpc.2023.108692
        real(dp), intent(in) :: x_wires(:, :), x_eval(:, :)
        real(dp), intent(inout) :: A(:, :)

        real(dp) :: dl(3), l2, l, e_l(3), delta_r(3), rho(3), &
            rho2, z, R_i, R_f, eps

        integer :: i, j

        A = 0.0d0

        do i = 1, size(x_eval, 2)
            do j = 1, size(x_wires, 2) - 1
                dl = x_wires(:, j + 1) - x_wires(:, j)
                l2 = dl(1)**2 + dl(2)**2 + dl(3)**2
                l = sqrt(l2)
                e_l = dl / l
                delta_r = x_eval(:, i) - x_wires(:, j)
                z = delta_r(1)*e_l(1) + delta_r(2)*e_l(2) + delta_r(3)*e_l(3)
                rho = delta_r - z*e_l
                rho2 = rho(1)**2 + rho(2)**2 + rho(3)**2
                R_i = sqrt(rho2 + z**2)
                R_f = sqrt(rho2 + (l - z)**2)
                eps = l / (R_i + R_f)

                A(:, i) = A(:, i) + MU0_OVER_TWOPI * e_l * atanh(eps)
            end do
        end do
    end subroutine afield_wires


    subroutine afield_wire_zaxis(z1, z2, x, A)
        ! https://physicspages.com/pdf/Electrodynamics/Magnetic%20vector%20potential.pdf
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
    end subroutine afield_wire_zaxis
end module magfie3d_analytical
