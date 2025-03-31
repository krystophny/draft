module dispatch_switch_module
    implicit none

    ! Define an enum to represent the type of data stored
    enum, bind(c)
        enumerator :: TYPE_INT = 1, TYPE_FLOAT, TYPE_DOUBLE, TYPE_UNKNOWN
    end enum

    ! Define a type to hold different types of data
    type :: Data
        real(8) :: double_value
    end type Data

    ! Define a struct that holds the data and the type of data it contains
    type :: Value
        integer :: type
        type(Data) :: dat
    end type Value

contains

    ! Function to perform arithmetic on an int
    function process_int(val) result(result_value)
        real(8), intent(in) :: val
        real(8) :: result_value
        result_value = val + 10.0
    end function process_int

    ! Function to perform arithmetic on a float
    function process_float(val) result(result_value)
        real(8), intent(in) :: val
        real(8) :: result_value
        result_value = val * 2.0
    end function process_float

    ! Function to perform arithmetic on a double
    function process_double(val) result(result_value)
        real(8), intent(in) :: val
        real(8) :: result_value
        result_value = val / 2.0
    end function process_double

    ! Function to process the value based on its type
    subroutine process_value(val)
        type(Value), intent(inout) :: val
        select case (val%type)
        case (TYPE_INT)
            val%dat%double_value = process_int(val%dat%double_value)
        case (TYPE_FLOAT)
            val%dat%double_value = process_float(val%dat%double_value)
        case (TYPE_DOUBLE)
            val%dat%double_value = process_double(val%dat%double_value)
        case default
            print *, "Unsupported type"
        end select
    end subroutine process_value

    ! Function to read the data type from the config file
    function read_config(filename) result(type)
        character(len=*), intent(in) :: filename
        integer :: type
        character(len=16) :: type_str
        integer :: iostat

        open(unit=10, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, "Failed to open config file"
            type = TYPE_UNKNOWN
            return
        end if

        read(10, *, iostat=iostat) type_str
        if (iostat /= 0) then
            print *, "Failed to read type from config file"
            type = TYPE_UNKNOWN
            close(10)
            return
        end if

        close(10)

        select case (trim(type_str))
        case ("int")
            type = TYPE_INT
        case ("float")
            type = TYPE_FLOAT
        case ("double")
            type = TYPE_DOUBLE
        case default
            type = TYPE_UNKNOWN
        end select
    end function read_config

    subroutine main_loop(val, iterations)
        type(Value), intent(inout) :: val
        integer, intent(in) :: iterations

        integer :: i

        do i = 1, iterations
            call process_value(val)
        end do
    end subroutine main_loop

end module dispatch_switch_module

program main
    use dispatch_switch_module
    implicit none
    type(Value) :: val
    integer :: type
    integer, parameter :: iterations = 1000000000
    real :: start_time, end_time, elapsed_time

    ! Read the data type from the config file
    type = read_config("config.txt")

    if (type == TYPE_UNKNOWN) then
        print *, "Invalid or unsupported data type in config file"
        stop
    end if

    ! Initialize the value based on the type
    val%type = type
    select case (type)
    case (TYPE_INT)
        val%dat%double_value = 42.0d0
    case (TYPE_FLOAT)
        val%dat%double_value = 3.14d0
    case (TYPE_DOUBLE)
        val%dat%double_value = 2.718281828459045d0
    end select

    ! Start the clock
    call cpu_time(start_time)

    ! Process the value 1,000,000,000 times
    call main_loop(val, iterations)

    ! Stop the clock
    call cpu_time(end_time)

    ! Calculate the elapsed time in seconds
    elapsed_time = end_time - start_time

    ! Print the result and the time taken
    select case (type)
    case (TYPE_INT)
        print *, "Processed int: ", val%dat%double_value
    case (TYPE_FLOAT)
        print *, "Processed float: ", val%dat%double_value
    case (TYPE_DOUBLE)
        print *, "Processed double: ", val%dat%double_value
    end select
    print *, "Time taken for ", iterations, " iterations: ", elapsed_time, " seconds"

end program main
