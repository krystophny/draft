module magfie3d
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use magfie3d_abstract
    use magfie3d_analytical

    implicit none

    contains

    function magfie3d_of_type(type_name) result(field)
        character(*), intent(in) :: type_name
        class(magfie3d_t), allocatable :: field

        select case(type_name)
        case("wires")
            field = magfie3d_wires_t()
        case default
            error stop "create_magfie3d: Unknown type name " // type_name
        end select
    end function magfie3d_of_type

    function magfie3d_from_file(filename) result(field)
        character(*), intent(in) :: filename
        class(magfie3d_t), allocatable :: field

        ! TODO: dispatch from file extension what kind of field to generate
    end function magfie3d_from_file

end module magfie3d
