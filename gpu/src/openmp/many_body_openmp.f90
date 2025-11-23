module many_body_openmp_m
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use omp_lib
    implicit none

contains

    subroutine many_body_force_openmp_cpu_f( &
        x, y, z, mass, fx, fy, fz, n, softening) &
        bind(C, name="many_body_force_openmp_cpu_f")
        integer, intent(in), value :: n
        real(dp), intent(in) :: x(n), y(n), z(n), mass(n)
        real(dp), intent(out) :: fx(n), fy(n), fz(n)
        real(dp), intent(in), value :: softening
        integer :: i, j
        real(dp) :: xi, yi, zi, fxi, fyi, fzi
        real(dp) :: dx_val, dy_val, dz_val, r2, r, r3, f

        !$omp parallel do private(i, j, xi, yi, zi, fxi, fyi, fzi, &
        !$omp dx_val, dy_val, dz_val, r2, r, r3, f)
        do i = 1, n
            xi = x(i)
            yi = y(i)
            zi = z(i)
            fxi = 0.0d0
            fyi = 0.0d0
            fzi = 0.0d0

            do j = 1, n
                dx_val = x(j) - xi
                dy_val = y(j) - yi
                dz_val = z(j) - zi
                r2 = dx_val*dx_val + dy_val*dy_val + dz_val*dz_val + &
                     softening*softening
                r = sqrt(r2)
                r3 = r2 * r
                f = mass(j) / r3
                fxi = fxi + f * dx_val
                fyi = fyi + f * dy_val
                fzi = fzi + f * dz_val
            end do

            fx(i) = fxi
            fy(i) = fyi
            fz(i) = fzi
        end do
        !$omp end parallel do
    end subroutine many_body_force_openmp_cpu_f

    subroutine many_body_force_openmp_gpu_f( &
        x, y, z, mass, fx, fy, fz, n, softening) &
        bind(C, name="many_body_force_openmp_gpu_f")
        integer, intent(in), value :: n
        real(dp), intent(in) :: x(n), y(n), z(n), mass(n)
        real(dp), intent(out) :: fx(n), fy(n), fz(n)
        real(dp), intent(in), value :: softening
        integer :: i, j
        real(dp) :: xi, yi, zi, fxi, fyi, fzi
        real(dp) :: dx_val, dy_val, dz_val, r2, r, r3, f

        !$omp target teams distribute parallel do &
        !$omp map(to: x, y, z, mass) map(from: fx, fy, fz) &
        !$omp private(i, j, xi, yi, zi, fxi, fyi, fzi, &
        !$omp dx_val, dy_val, dz_val, r2, r, r3, f)
        do i = 1, n
            xi = x(i)
            yi = y(i)
            zi = z(i)
            fxi = 0.0d0
            fyi = 0.0d0
            fzi = 0.0d0

            do j = 1, n
                dx_val = x(j) - xi
                dy_val = y(j) - yi
                dz_val = z(j) - zi
                r2 = dx_val*dx_val + dy_val*dy_val + dz_val*dz_val + &
                     softening*softening
                r = sqrt(r2)
                r3 = r2 * r
                f = mass(j) / r3
                fxi = fxi + f * dx_val
                fyi = fyi + f * dy_val
                fzi = fzi + f * dz_val
            end do

            fx(i) = fxi
            fy(i) = fyi
            fz(i) = fzi
        end do
        !$omp end target teams distribute parallel do
    end subroutine many_body_force_openmp_gpu_f

end module many_body_openmp_m
