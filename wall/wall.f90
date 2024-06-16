module wall_mod
    use iso_fortran_env, only: dp => real64
    implicit none


    type wall_zplane_t
        real(dp) :: z  ! real(8) :: z
    end type wall_zplane_t

    contains

    subroutine intersect_with_wall(wall_zplane, x)

        type(wall_zplane_t), intent(in) :: wall_zplane

    end subroutine intersect_with_wall
end module wall_mod
