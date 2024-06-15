module magfie3d_abstract
    use, intrinsic :: iso_fortran_env, only: dp => real64

    implicit none

    type, abstract :: magfie3d_t
        contains
        procedure(magfie3d_afield), deferred :: afield
    end type magfie3d_t

    interface
        subroutine magfie3d_afield(self, x, A)
            import :: magfie3d_t, dp
            class (magfie3d_t), intent(inout) :: self
            real(dp), intent(in) :: x(:, :)
            real(dp), intent(inout) :: A(:, :)
        end subroutine magfie3d_afield
    end interface
end module magfie3d_abstract
