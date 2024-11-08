module field_impl1

use, intrinsic :: iso_fortran_env, only: dp => real64
use field_base, only: field_t
implicit none

type, extends(field_t) :: field_impl1_t
    contains
        procedure :: compute_afield
end type field_impl1_t

contains

subroutine compute_afield(self, x, A)
    class(field_impl1_t), intent(in) :: self
    real(dp), intent(in) :: x(3)
    real(dp), intent(out) :: A(3)

    A = 1d0
end subroutine compute_afield

end module field_impl1
