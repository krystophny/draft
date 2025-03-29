module testfpm
  implicit none
  private

  public :: say_hello, add
contains
  
  subroutine say_hello
    ! Says hello
    print *, "Hello, testfpm!"
  end subroutine say_hello


  subroutine add(x, y, res)
    ! Adds two numbers
    real, intent(in) :: x  ! First number
    real, intent(inout) :: y  ! Second number
    real, intent(out) :: res  ! Result of the addition    

    res = x + y
  end subroutine add
end module testfpm
