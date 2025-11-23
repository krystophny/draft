module fdm_laplacian_openmp_m
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use omp_lib
    implicit none

contains

    subroutine fdm_laplacian_openmp_cpu_f(u, lu, nx, ny, nz, dx) bind(C, name="fdm_laplacian_openmp_cpu_f")
        integer, intent(in), value :: nx, ny, nz
        real(dp), intent(in) :: u(nx, ny, nz)
        real(dp), intent(out) :: lu(nx, ny, nz)
        real(dp), intent(in), value :: dx
        integer :: i, j, k
        real(dp) :: inv_dx2

        inv_dx2 = 1.0d0 / (dx * dx)

        !$omp parallel do collapse(3) private(i, j, k)
        do k = 2, nz-1
            do j = 2, ny-1
                do i = 2, nx-1
                    lu(i, j, k) = inv_dx2 * ( &
                        u(i-1, j, k) + u(i+1, j, k) + &
                        u(i, j-1, k) + u(i, j+1, k) + &
                        u(i, j, k-1) + u(i, j, k+1) - &
                        6.0d0 * u(i, j, k) &
                    )
                end do
            end do
        end do
        !$omp end parallel do
    end subroutine fdm_laplacian_openmp_cpu_f

    subroutine fdm_laplacian_openmp_gpu_f(u, lu, nx, ny, nz, dx) bind(C, name="fdm_laplacian_openmp_gpu_f")
        integer, intent(in), value :: nx, ny, nz
        real(dp), intent(in) :: u(nx, ny, nz)
        real(dp), intent(out) :: lu(nx, ny, nz)
        real(dp), intent(in), value :: dx
        integer :: i, j, k
        real(dp) :: inv_dx2

        inv_dx2 = 1.0d0 / (dx * dx)

        !$omp target teams distribute parallel do collapse(3) &
        !$omp map(to: u) map(from: lu)
        do k = 2, nz-1
            do j = 2, ny-1
                do i = 2, nx-1
                    lu(i, j, k) = inv_dx2 * ( &
                        u(i-1, j, k) + u(i+1, j, k) + &
                        u(i, j-1, k) + u(i, j+1, k) + &
                        u(i, j, k-1) + u(i, j, k+1) - &
                        6.0d0 * u(i, j, k) &
                    )
                end do
            end do
        end do
        !$omp end target teams distribute parallel do
    end subroutine fdm_laplacian_openmp_gpu_f

end module fdm_laplacian_openmp_m
