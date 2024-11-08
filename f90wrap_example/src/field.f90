module field

use, intrinsic :: iso_fortran_env, only: dp => real64

implicit none

contains

function field_from_name(field_name)
    use field_base, only: field_t
    use field_impl1, only: field_impl1_t

    class(field_t), allocatable :: field_from_name
    character(*), intent(in) :: field_name

    select case(field_name)
    case('field_impl1')
        allocate(field_impl1_t :: field_from_name)
    case default
        print *, 'field_from_name: Unknown field name ', field_name
        error stop
    end select

end function field_from_name

end module field
