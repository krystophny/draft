module dispatch_class_module
    implicit none
    private
    public :: Value, IntValue, FloatValue, DoubleValue, ValueFactory, Config

    ! Abstract base type for vals
    type, abstract :: Value
    contains
        procedure(process_interface), deferred :: process
        procedure(print_interface), deferred :: print
        procedure(clone_interface), deferred :: clone
    end type Value

    abstract interface
        subroutine process_interface(this)
            import :: Value
            class(Value), intent(inout) :: this
        end subroutine process_interface

        subroutine print_interface(this)
            import :: Value
            class(Value), intent(in) :: this
        end subroutine print_interface

        function clone_interface(this) result(clone)
            import :: Value
            class(Value), intent(in) :: this
            class(Value), allocatable :: clone
        end function clone_interface
    end interface

    ! Concrete implementation for int vals
    type, extends(Value) :: IntValue
        integer :: val
    contains
        procedure :: process => process_int
        procedure :: print => print_int
        procedure :: clone => clone_int
    end type IntValue

    ! Concrete implementation for float vals
    type, extends(Value) :: FloatValue
        real :: val
    contains
        procedure :: process => process_float
        procedure :: print => print_float
        procedure :: clone => clone_float
    end type FloatValue

    ! Concrete implementation for double vals
    type, extends(Value) :: DoubleValue
        real(8) :: val
    contains
        procedure :: process => process_double
        procedure :: print => print_double
        procedure :: clone => clone_double
    end type DoubleValue

    ! Factory type to create appropriate val objects
    type :: ValueFactory
    contains
        procedure, nopass :: createValue => create_val
    end type ValueFactory

    ! Configuration reader type
    type :: Config
    contains
        procedure, nopass :: readType => read_type
    end type Config

contains

    ! IntValue procedures
    subroutine process_int(this)
        class(IntValue), intent(inout) :: this
        this%val = this%val + 10
    end subroutine process_int

    subroutine print_int(this)
        class(IntValue), intent(in) :: this
        print *, "Processed int: ", this%val
    end subroutine print_int

    function clone_int(this) result(clone)
        class(IntValue), intent(in) :: this
        class(Value), allocatable :: clone
        allocate(IntValue :: clone)
        select type(clone)
        type is (IntValue)
            clone%val = this%val
        end select
    end function clone_int

    ! FloatValue procedures
    subroutine process_float(this)
        class(FloatValue), intent(inout) :: this
        this%val = this%val * 2.0
    end subroutine process_float

    subroutine print_float(this)
        class(FloatValue), intent(in) :: this
        print *, "Processed float: ", this%val
    end subroutine print_float

    function clone_float(this) result(clone)
        class(FloatValue), intent(in) :: this
        class(Value), allocatable :: clone
        allocate(FloatValue :: clone)
        select type(clone)
        type is (FloatValue)
            clone%val = this%val
        end select
    end function clone_float

    ! DoubleValue procedures
    subroutine process_double(this)
        class(DoubleValue), intent(inout) :: this
        this%val = this%val / 2.0
    end subroutine process_double

    subroutine print_double(this)
        class(DoubleValue), intent(in) :: this
        print *, "Processed double: ", this%val
    end subroutine print_double

    function clone_double(this) result(clone)
        class(DoubleValue), intent(in) :: this
        class(Value), allocatable :: clone
        allocate(DoubleValue :: clone)
        select type(clone)
        type is (DoubleValue)
            clone%val = this%val
        end select
    end function clone_double

    ! Factory procedure
    function create_val(type) result(val)
        character(len=*), intent(in) :: type
        class(Value), allocatable :: val
        select case (type)
        case ("int")
            allocate(IntValue :: val)
        case ("float")
            allocate(FloatValue :: val)
        case ("double")
            allocate(DoubleValue :: val)
        case default
            error stop "Unsupported type"
        end select
    end function create_val

    ! Configuration reader procedure
    function read_type(filename) result(type)
        character(len=*), intent(in) :: filename
        character(len=20) :: type
        integer :: iostat
        open(unit=10, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error stop "Failed to open config file: " // filename
        end if
        read(10, *, iostat=iostat) type
        if (iostat /= 0) then
            error stop "Failed to read type from config file"
        end if
        close(10)
    end function read_type

end module dispatch_class_module
