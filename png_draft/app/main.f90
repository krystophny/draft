program plot_generator
    use plotting_module
    use png_context_module
    use pdf_module
    implicit none
    
    ! Image parameters (4:3 aspect ratio)
    integer, parameter :: width = 400
    integer, parameter :: height = 300
    
    type(png_context) :: png_ctx
    type(pdf_context) :: pdf_ctx
    
    ! Generate PNG plot
    png_ctx = create_png_plot(width, height)
    call png_ctx%set_color(0.0, 0.0, 1.0)  ! Blue
    call add_sine_wave(png_ctx, 0.4, 1.0, 0.0, 4)  ! amplitude, frequency, phase, cycles
    call add_axes(png_ctx)
    call finalize_plot(png_ctx, 'output.png')
    
    ! Generate PDF plot
    pdf_ctx = create_pdf_plot(width, height)
    call pdf_ctx%set_color(0.0, 0.0, 1.0)  ! Blue
    call add_sine_wave(pdf_ctx, 0.4, 1.0, 0.0, 4)  ! amplitude, frequency, phase, cycles
    call add_axes(pdf_ctx)
    call finalize_plot(pdf_ctx, 'output.pdf')
    
end program plot_generator
