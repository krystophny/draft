module unittest
    implicit none

    ! Define ANSI color codes
    character(len=*), parameter :: reset_code = char(27) // "[0m"
    character(len=*), parameter :: red_code = char(27) // "[31m"
    character(len=*), parameter :: green_code = char(27) // "[32m"
    character(len=*), parameter :: yellow_code = char(27) // "[33m"
    character(len=*), parameter :: blue_code = char(27) // "[34m"
    character(len=*), parameter :: magenta_code = char(27) // "[35m"
    character(len=*), parameter :: cyan_code = char(27) // "[36m"

    contains

    subroutine pass(test_name)
        character(len=*), intent(in) :: test_name
        print *, green_code, "PASSED: ", reset_code, test_name
    end subroutine pass

    subroutine fail(test_name)
        character(len=*), intent(in) :: test_name
        print *, red_code, "FAILED: ", reset_code, test_name
        error stop
    end subroutine fail
end module unittest
