module particle_push_openmp_m
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use omp_lib
    implicit none

contains

    subroutine particle_push_openmp_cpu_f( &
        x, y, z, vx, vy, vz, ex, ey, ez, bx, by, bz, dt, qm, n) &
        bind(C, name="particle_push_openmp_cpu_f")
        integer, intent(in), value :: n
        real(dp), intent(inout) :: x(n), y(n), z(n)
        real(dp), intent(inout) :: vx(n), vy(n), vz(n)
        real(dp), intent(in) :: ex(n), ey(n), ez(n)
        real(dp), intent(in) :: bx(n), by(n), bz(n)
        real(dp), intent(in), value :: dt, qm
        integer :: i
        real(dp) :: vx_local, vy_local, vz_local
        real(dp) :: qmdt, qmdt2, tx, ty, tz
        real(dp) :: ux, uy, uz, sx, sy, sz, denom

        !$omp parallel do private(i, vx_local, vy_local, vz_local, &
        !$omp qmdt, qmdt2, tx, ty, tz, ux, uy, uz, sx, sy, sz, denom)
        do i = 1, n
            vx_local = vx(i)
            vy_local = vy(i)
            vz_local = vz(i)

            qmdt = qm * dt
            qmdt2 = 0.5d0 * qmdt

            vx_local = vx_local + qmdt2 * ex(i)
            vy_local = vy_local + qmdt2 * ey(i)
            vz_local = vz_local + qmdt2 * ez(i)

            tx = qmdt2 * bx(i)
            ty = qmdt2 * by(i)
            tz = qmdt2 * bz(i)

            ux = vx_local + vy_local * tz - vz_local * ty
            uy = vy_local + vz_local * tx - vx_local * tz
            uz = vz_local + vx_local * ty - vy_local * tx

            denom = 1.0d0 + tx * tx + ty * ty + tz * tz
            sx = 2.0d0 * tx / denom
            sy = 2.0d0 * ty / denom
            sz = 2.0d0 * tz / denom

            vx_local = vx_local + uy * sz - uz * sy
            vy_local = vy_local + uz * sx - ux * sz
            vz_local = vz_local + ux * sy - uy * sx

            vx_local = vx_local + qmdt2 * ex(i)
            vy_local = vy_local + qmdt2 * ey(i)
            vz_local = vz_local + qmdt2 * ez(i)

            vx(i) = vx_local
            vy(i) = vy_local
            vz(i) = vz_local

            x(i) = x(i) + vx_local * dt
            y(i) = y(i) + vy_local * dt
            z(i) = z(i) + vz_local * dt
        end do
        !$omp end parallel do
    end subroutine particle_push_openmp_cpu_f

    subroutine particle_push_openmp_gpu_f( &
        x, y, z, vx, vy, vz, ex, ey, ez, bx, by, bz, dt, qm, n) &
        bind(C, name="particle_push_openmp_gpu_f")
        integer, intent(in), value :: n
        real(dp), intent(inout) :: x(n), y(n), z(n)
        real(dp), intent(inout) :: vx(n), vy(n), vz(n)
        real(dp), intent(in) :: ex(n), ey(n), ez(n)
        real(dp), intent(in) :: bx(n), by(n), bz(n)
        real(dp), intent(in), value :: dt, qm
        integer :: i
        real(dp) :: vx_local, vy_local, vz_local
        real(dp) :: qmdt, qmdt2, tx, ty, tz
        real(dp) :: ux, uy, uz, sx, sy, sz, denom

        !$omp target teams distribute parallel do &
        !$omp map(tofrom: x, y, z, vx, vy, vz) &
        !$omp map(to: ex, ey, ez, bx, by, bz) &
        !$omp private(i, vx_local, vy_local, vz_local, &
        !$omp qmdt, qmdt2, tx, ty, tz, ux, uy, uz, sx, sy, sz, denom)
        do i = 1, n
            vx_local = vx(i)
            vy_local = vy(i)
            vz_local = vz(i)

            qmdt = qm * dt
            qmdt2 = 0.5d0 * qmdt

            vx_local = vx_local + qmdt2 * ex(i)
            vy_local = vy_local + qmdt2 * ey(i)
            vz_local = vz_local + qmdt2 * ez(i)

            tx = qmdt2 * bx(i)
            ty = qmdt2 * by(i)
            tz = qmdt2 * bz(i)

            ux = vx_local + vy_local * tz - vz_local * ty
            uy = vy_local + vz_local * tx - vx_local * tz
            uz = vz_local + vx_local * ty - vy_local * tx

            denom = 1.0d0 + tx * tx + ty * ty + tz * tz
            sx = 2.0d0 * tx / denom
            sy = 2.0d0 * ty / denom
            sz = 2.0d0 * tz / denom

            vx_local = vx_local + uy * sz - uz * sy
            vy_local = vy_local + uz * sx - ux * sz
            vz_local = vz_local + ux * sy - uy * sx

            vx_local = vx_local + qmdt2 * ex(i)
            vy_local = vy_local + qmdt2 * ey(i)
            vz_local = vz_local + qmdt2 * ez(i)

            vx(i) = vx_local
            vy(i) = vy_local
            vz(i) = vz_local

            x(i) = x(i) + vx_local * dt
            y(i) = y(i) + vy_local * dt
            z(i) = z(i) + vz_local * dt
        end do
        !$omp end target teams distribute parallel do
    end subroutine particle_push_openmp_gpu_f

end module particle_push_openmp_m
