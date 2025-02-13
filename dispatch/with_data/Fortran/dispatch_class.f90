program main
    use dispatch_class_module
    implicit none
    type(ValueFactory) :: factory
    type(Config) :: conf
    class(value), allocatable :: val
    character(len=20) :: type
    integer :: i
    integer, parameter :: iterations = 1000000000
    real :: start_time, end_time

    try: block
        ! Read configuration
        type = conf%readType("config.txt")

        ! Create value object based on configuration
        val = factory%createValue(type)

        ! Start timing
        call cpu_time(start_time)

        ! Process the value multiple times
        do i = 1, iterations
            call val%process()
        end do

        ! Stop timing
        call cpu_time(end_time)

        ! Print results
        call val%print()
        print *, "Time taken for ", iterations, " iterations: ", end_time - start_time, " seconds"

    end block try

end program main
