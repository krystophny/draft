program main

implicit none

call test_routine

contains

subroutine test_routine
  integer :: i

  i = 1
  print *, i
end subroutine test_routine

end program main

