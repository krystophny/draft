module field_base

use, intrinsic :: iso_fortran_env, only: dp => real64
implicit none

type :: field_t
    contains
    procedure :: compute_afield
end type field_t

contains

subroutine compute_afield(self, x, A)
    class (field_t), intent(in) :: self
    real(dp), intent(in) :: x(3)
    real(dp), intent(out) :: A(3)

    print *, "not implemented for base class"
    error stop
end subroutine compute_afield

end module field_base
