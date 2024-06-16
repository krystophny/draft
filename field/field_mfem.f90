module field_mfem
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use field_abstract

    implicit none

    type, extends(scalar_field_t) :: scalar_field_mfem_t
        contains
        procedure :: init => scalar_field_mfem_init
        procedure :: evaluate => scalar_field_mfem_evaluate
    end type scalar_field_mfem_t

    contains

    subroutine scalar_field_mfem_init(self, x, u)
        class(scalar_field_mfem_t), intent(out) :: self
        real(dp), intent(in) :: x(:,:)
        real(dp), intent(in) :: u(:)
    end subroutine scalar_field_mfem_init

    subroutine scalar_field_mfem_evaluate(self, x, u)
        class(scalar_field_mfem_t), intent(inout) :: self
        real(dp), intent(in) :: x(:,:)
        real(dp), intent(inout) :: u(:)
    end subroutine scalar_field_mfem_evaluate

end module field_mfem
