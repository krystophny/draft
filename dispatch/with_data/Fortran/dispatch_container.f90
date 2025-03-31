module dynamic_dispatch
    implicit none
    private

    ! Value container type with direct pointers
    type :: Value
        private
        integer, pointer       :: int_ptr => null()
        real, pointer          :: float_ptr => null()
        real(8), pointer       :: double_ptr => null()
    end type Value

    ! Public interfaces
    public :: Value, create_value, read_config, process_value, print_value

contains

    ! Processing implementations
    subroutine process_int(val)
        integer, intent(inout) :: val
        val = val + 10
    end subroutine process_int

    subroutine process_float(val)
        real, intent(inout) :: val
        val = val * 2.0
    end subroutine process_float

    subroutine process_double(val)
        real(8), intent(inout) :: val
        val = val / 2.0
    end subroutine process_double

    ! Dynamic dispatch processor
    subroutine process_value(this)
        type(Value), intent(inout) :: this
        if (associated(this%int_ptr)) then
            call process_int(this%int_ptr)
        elseif (associated(this%float_ptr)) then
            call process_float(this%float_ptr)
        elseif (associated(this%double_ptr)) then
            call process_double(this%double_ptr)
        endif
    end subroutine process_value

    ! Print implementations
    subroutine print_value(this)
        type(Value), intent(in) :: this
        if (associated(this%int_ptr)) then
            print *, "Processed int: ", this%int_ptr
        elseif (associated(this%float_ptr)) then
            print *, "Processed float: ", this%float_ptr
        elseif (associated(this%double_ptr)) then
            print *, "Processed double: ", this%double_ptr
        endif
    end subroutine print_value

    ! Factory function
    function create_value(dtype) result(val)
        character(len=*), intent(in) :: dtype
        type(Value) :: val
        
        select case(trim(dtype))
        case("int")
            allocate(val%int_ptr)
            val%int_ptr = 42
        case("float")
            allocate(val%float_ptr)
            val%float_ptr = 3.14
        case("double")
            allocate(val%double_ptr)
            val%double_ptr = 2.718281828459045d0
        case default
           print *, "Unsupported value type"
           stop 1
        end select
    end function create_value

    ! Config reader
    function read_config(filename) result(dtype)
        character(len=*), intent(in) :: filename
        character(len=20) :: dtype
        integer :: iostat
        
        open(unit=10, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) stop 1
        read(10, *) dtype
        close(10)
    end function read_config

end module dynamic_dispatch

program main
    use dynamic_dispatch
    implicit none
    type(Value) :: val
    character(len=20) :: dtype
    integer :: i
    integer, parameter :: iterations = 1000000000
    real :: start, finish
    
    ! Get config and create value
    dtype = read_config("config.txt")
    val = create_value(dtype)
    
    ! Process loop
    call cpu_time(start)
    do i = 1, iterations
        call process_value(val)
    end do
    call cpu_time(finish)
    
    ! Print results
    call print_value(val)
    print *, "Time: ", finish-start, " seconds"
end program main
