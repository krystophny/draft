module field_base

use, intrinsic :: iso_fortran_env, only: dp => real64
implicit none

type, abstract :: field_t
    contains
    procedure(compute_afield), deferred :: compute_afield
end type field_t

interface
    subroutine compute_afield(self, x, A)
        import :: field_t, dp
        class (field_t), intent(in) :: self
        real(dp), intent(in) :: x(3)
        real(dp), intent(out) :: A(3)
    end subroutine
end interface

end module field_base
