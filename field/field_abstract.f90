module field_abstract
    use, intrinsic :: iso_fortran_env, only: dp => real64

    implicit none

    type, abstract :: scalar_field_t
        contains
        procedure(scalar_field_evaluate), deferred :: evaluate
    end type scalar_field_t

    interface
        subroutine scalar_field_evaluate(self, x, Phi)
            import :: scalar_field_t, dp
            class (scalar_field_t), intent(inout) :: self
            real(dp), intent(in) :: x(:, :)
            real(dp), intent(inout) :: Phi(:, :)
        end subroutine scalar_field_evaluate
    end interface
end module field_abstract
