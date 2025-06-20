module plotting_module
    implicit none
    
    private
    public :: plot_context, create_plot, add_sine_wave, add_axes, finalize_plot
    
    ! Abstract plotting context
    type, abstract :: plot_context
        integer :: width, height
        real :: x_min, x_max, y_min, y_max
    contains
        procedure(draw_line_interface), deferred :: draw_line
        procedure(set_color_interface), deferred :: set_color
        procedure(finalize_interface), deferred :: finalize
    end type plot_context
    
    ! Abstract interfaces
    abstract interface
        subroutine draw_line_interface(this, x1, y1, x2, y2)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            real, intent(in) :: x1, y1, x2, y2
        end subroutine draw_line_interface
        
        subroutine set_color_interface(this, r, g, b)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            real, intent(in) :: r, g, b
        end subroutine set_color_interface
        
        subroutine finalize_interface(this, filename)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            character(len=*), intent(in) :: filename
        end subroutine finalize_interface
    end interface
    
contains

    subroutine create_plot(ctx, width, height, x_min, x_max, y_min, y_max)
        class(plot_context), intent(inout) :: ctx
        integer, intent(in) :: width, height
        real, intent(in) :: x_min, x_max, y_min, y_max
        
        ctx%width = width
        ctx%height = height
        ctx%x_min = x_min
        ctx%x_max = x_max
        ctx%y_min = y_min
        ctx%y_max = y_max
    end subroutine create_plot
    
    subroutine add_sine_wave(ctx, amplitude, frequency, phase, n_cycles)
        class(plot_context), intent(inout) :: ctx
        real, intent(in) :: amplitude, frequency, phase
        integer, intent(in) :: n_cycles
        real :: x_prev, y_prev, x_curr, y_curr
        real :: t, dt, pi
        integer :: i, n_steps
        
        pi = 4.0 * atan(1.0)
        
        ! Use fine steps for smooth curves
        n_steps = ctx%width * 2
        dt = real(n_cycles) * 2.0 * pi / real(n_steps - 1)
        
        ! Calculate first point
        t = 0.0
        x_prev = t / (real(n_cycles) * 2.0 * pi)  ! Normalize to [0,1]
        y_prev = 0.5 + amplitude * sin(frequency * t + phase)
        
        ! Draw curve
        do i = 2, n_steps
            t = real(i - 1) * dt
            x_curr = t / (real(n_cycles) * 2.0 * pi)
            y_curr = 0.5 + amplitude * sin(frequency * t + phase)
            
            call ctx%draw_line(x_prev, y_prev, x_curr, y_curr)
            
            x_prev = x_curr
            y_prev = y_curr
        end do
    end subroutine add_sine_wave
    
    subroutine add_axes(ctx)
        class(plot_context), intent(inout) :: ctx
        
        ! Set axis color (gray)
        call ctx%set_color(0.25, 0.25, 0.25)
        
        ! Draw horizontal axis at center
        call ctx%draw_line(0.0, 0.5, 1.0, 0.5)
        
        ! Draw vertical axis at center  
        call ctx%draw_line(0.5, 0.0, 0.5, 1.0)
    end subroutine add_axes
    
    subroutine finalize_plot(ctx, filename)
        class(plot_context), intent(inout) :: ctx
        character(len=*), intent(in) :: filename
        
        call ctx%finalize(filename)
    end subroutine finalize_plot

end module plotting_module