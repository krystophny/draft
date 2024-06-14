module field3d
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use field3d_abstract
    use field3d_analytical

    implicit none

    contains

    function field3d_of_type(type_name) result(field)
        character(*), intent(in) :: type_name
        class(field3d_t), allocatable :: field

        select case(type_name)
        case("wires")
            field = field3d_wires_t()
        case default
            error stop "create_field3d: Unknown type name " // type_name
        end select
    end function field3d_of_type

    function field3d_from_file(filename) result(field)
        character(*), intent(in) :: filename
        class(field3d_t), allocatable :: field

        ! TODO: dispatch from file extension what kind of field to generate
    end function field3d_from_file

end module field3d
