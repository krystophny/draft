module field3d_abstract
    use, intrinsic :: iso_fortran_env, only: dp => real64

    implicit none

    type, abstract :: field3d_t
        contains
        procedure(field3d_afield), deferred :: afield
    end type field3d_t

    interface
        subroutine field3d_afield(self, x, A)
            import :: field3d_t, dp
            class (field3d_t), intent(inout) :: self
            real(dp), intent(in) :: x(:, :)
            real(dp), intent(inout) :: A(:, :)
        end subroutine field3d_afield
    end interface
end module field3d_abstract
