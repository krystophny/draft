module util_for_test
    implicit none

    integer, parameter :: OK = 0, FAIL = 1

    integer :: status = OK

contains

    subroutine print_test(test_name)
        character(*) :: test_name
        print *, "==> ", test_name
    end subroutine print_test

    subroutine pass_test
        call print_ok
    end subroutine pass_test

    subroutine fail_test
        status = FAIL
        call print_fail
    end subroutine fail_test

    subroutine skip_test
        call print_skip
    end subroutine skip_test

    subroutine print_ok
        print *, "    .......................................   OK"
    end subroutine print_ok

    subroutine print_fail
        print *, "    .......................................   FAIL"
    end subroutine print_fail

    subroutine print_skip
        print *, "    .......................................   SKIP"
    end subroutine print_skip

end module util_for_test
