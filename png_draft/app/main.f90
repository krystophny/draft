program png_generator
    use png_module
    implicit none
    
    ! Image parameters (4:3 aspect ratio)
    integer, parameter :: width = 400
    integer, parameter :: height = 300
    
    ! Generate PNG with plot
    call generate_png_with_plot('output.png', width, height)
end program png_generator
