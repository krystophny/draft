
program main
    use dispatch_switch_module
    implicit none
    type(Value) :: val
    integer :: type
    integer :: i
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
