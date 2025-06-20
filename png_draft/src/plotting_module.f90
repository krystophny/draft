module plotting_module
    implicit none
    
    private
    public :: plot_context, setup_canvas, draw_sine_wave, draw_coordinate_axes, save_plot
    
    type, abstract :: plot_context
        integer :: width, height
        real :: x_min, x_max, y_min, y_max
    contains
        procedure(line_interface), deferred :: line
        procedure(color_interface), deferred :: color
        procedure(save_interface), deferred :: save
    end type plot_context
    
    abstract interface
        subroutine line_interface(this, x1, y1, x2, y2)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            real, intent(in) :: x1, y1, x2, y2
        end subroutine line_interface
        
        subroutine color_interface(this, r, g, b)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            real, intent(in) :: r, g, b
        end subroutine color_interface
        
        subroutine save_interface(this, filename)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            character(len=*), intent(in) :: filename
        end subroutine save_interface
    end interface
    
contains

    subroutine setup_canvas(ctx, width, height)
        class(plot_context), intent(inout) :: ctx
        integer, intent(in) :: width, height
        
        ctx%width = width
        ctx%height = height
        ctx%x_min = 0.0
        ctx%x_max = 1.0
        ctx%y_min = 0.0
        ctx%y_max = 1.0
    end subroutine setup_canvas
    
    subroutine draw_sine_wave(ctx, amplitude, cycles)
        class(plot_context), intent(inout) :: ctx
        real, intent(in) :: amplitude
        integer, intent(in) :: cycles
        
        call ctx%color(0.0, 0.0, 1.0)
        call render_parametric_curve(ctx, amplitude, cycles)
    end subroutine draw_sine_wave
    
    subroutine draw_coordinate_axes(ctx)
        class(plot_context), intent(inout) :: ctx
        
        call ctx%color(0.25, 0.25, 0.25)
        call ctx%line(0.0, 0.5, 1.0, 0.5)
        call ctx%line(0.5, 0.0, 0.5, 1.0)
    end subroutine draw_coordinate_axes
    
    subroutine save_plot(ctx, filename)
        class(plot_context), intent(inout) :: ctx
        character(len=*), intent(in) :: filename
        
        call ctx%save(filename)
    end subroutine save_plot

    subroutine render_parametric_curve(ctx, amplitude, cycles)
        class(plot_context), intent(inout) :: ctx
        real, intent(in) :: amplitude
        integer, intent(in) :: cycles
        real :: x_prev, y_prev, x_curr, y_curr
        real :: t, dt, pi, omega
        integer :: i, steps
        
        pi = 4.0 * atan(1.0)
        omega = real(cycles) * 2.0 * pi
        steps = ctx%width * 2
        dt = omega / real(steps - 1)
        
        call calculate_first_point(0.0, amplitude, omega, x_prev, y_prev)
        
        do i = 2, steps
            t = real(i - 1) * dt
            call calculate_curve_point(t, amplitude, omega, x_curr, y_curr)
            call ctx%line(x_prev, y_prev, x_curr, y_curr)
            x_prev = x_curr
            y_prev = y_curr
        end do
    end subroutine render_parametric_curve

    subroutine calculate_first_point(t, amplitude, omega, x, y)
        real, intent(in) :: t, amplitude, omega
        real, intent(out) :: x, y
        
        x = t / omega
        y = 0.5 + amplitude * sin(t)
    end subroutine calculate_first_point

    subroutine calculate_curve_point(t, amplitude, omega, x, y)
        real, intent(in) :: t, amplitude, omega
        real, intent(out) :: x, y
        
        x = t / omega
        y = 0.5 + amplitude * sin(t)
    end subroutine calculate_curve_point

end module plotting_module