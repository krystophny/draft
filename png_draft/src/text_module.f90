module text_module
    use iso_c_binding
    implicit none
    
    private
    public :: init_text_system, cleanup_text_system, render_text_to_image
    
    ! FreeType C interfaces
    interface
        function ft_init_freetype(library) bind(C, name="FT_Init_FreeType")
            import :: c_ptr, c_int
            type(c_ptr), intent(out) :: library
            integer(c_int) :: ft_init_freetype
        end function ft_init_freetype
        
        function ft_new_face(library, filepath, face_index, face) bind(C, name="FT_New_Face")
            import :: c_ptr, c_char, c_long, c_int
            type(c_ptr), value :: library
            character(kind=c_char), intent(in) :: filepath(*)
            integer(c_long), value :: face_index
            type(c_ptr), intent(out) :: face
            integer(c_int) :: ft_new_face
        end function ft_new_face
        
        function ft_set_pixel_sizes(face, pixel_width, pixel_height) bind(C, name="FT_Set_Pixel_Sizes")
            import :: c_ptr, c_int
            type(c_ptr), value :: face
            integer(c_int), value :: pixel_width, pixel_height
            integer(c_int) :: ft_set_pixel_sizes
        end function ft_set_pixel_sizes
        
        function ft_load_char(face, char_code, load_flags) bind(C, name="FT_Load_Char")
            import :: c_ptr, c_long, c_int32_t, c_int
            type(c_ptr), value :: face
            integer(c_long), value :: char_code
            integer(c_int32_t), value :: load_flags
            integer(c_int) :: ft_load_char
        end function ft_load_char
        
        subroutine ft_done_face(face) bind(C, name="FT_Done_Face")
            import :: c_ptr
            type(c_ptr), value :: face
        end subroutine ft_done_face
        
        subroutine ft_done_freetype(library) bind(C, name="FT_Done_FreeType")
            import :: c_ptr
            type(c_ptr), value :: library
        end subroutine ft_done_freetype
    end interface
    
    ! FreeType constants
    integer(c_int32_t), parameter :: FT_LOAD_RENDER = int(z'4', c_int32_t)
    
    ! Module variables
    type(c_ptr) :: ft_library = c_null_ptr
    type(c_ptr) :: ft_face = c_null_ptr
    logical :: text_system_initialized = .false.
    
contains

    function init_text_system() result(success)
        logical :: success
        integer(c_int) :: error
        character(len=256, kind=c_char) :: font_path
        character(len=:), allocatable :: font_paths(:)
        integer :: i
        
        success = .false.
        
        if (text_system_initialized) then
            success = .true.
            return
        end if
        
        ! Initialize FreeType library
        error = ft_init_freetype(ft_library)
        if (error /= 0) then
            print *, "Error: Could not initialize FreeType library"
            return
        end if
        
        ! Try different system font paths
        allocate(character(len=64) :: font_paths(4))
        font_paths(1) = "/System/Library/Fonts/Helvetica.ttc" // c_null_char  ! macOS
        font_paths(2) = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" // c_null_char  ! Linux
        font_paths(3) = "/Windows/Fonts/arial.ttf" // c_null_char  ! Windows
        font_paths(4) = "/usr/share/fonts/TTF/arial.ttf" // c_null_char  ! Arch Linux
        
        do i = 1, size(font_paths)
            font_path = font_paths(i)
            error = ft_new_face(ft_library, font_path, 0_c_long, ft_face)
            if (error == 0) exit
        end do
        
        if (error /= 0) then
            print *, "Error: Could not load any system font"
            call ft_done_freetype(ft_library)
            return
        end if
        
        ! Set font size (12 pixels)
        error = ft_set_pixel_sizes(ft_face, 0_c_int, 12_c_int)
        if (error /= 0) then
            print *, "Error: Could not set font size"
            call ft_done_face(ft_face)
            call ft_done_freetype(ft_library)
            return
        end if
        
        text_system_initialized = .true.
        success = .true.
    end function init_text_system

    subroutine cleanup_text_system()
        if (text_system_initialized) then
            call ft_done_face(ft_face)
            call ft_done_freetype(ft_library)
            text_system_initialized = .false.
        end if
    end subroutine cleanup_text_system

    subroutine render_text_to_image(image_data, width, height, x, y, text, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        integer :: pen_x, pen_y, i, char_code
        integer(c_int) :: error
        
        if (.not. text_system_initialized) then
            if (.not. init_text_system()) return
        end if
        
        pen_x = x
        pen_y = y
        
        do i = 1, len_trim(text)
            char_code = iachar(text(i:i))
            
            error = ft_load_char(ft_face, int(char_code, c_long), FT_LOAD_RENDER)
            if (error /= 0) cycle  ! Skip character if it can't be loaded
            
            call render_glyph_bitmap(image_data, width, height, pen_x, pen_y, r, g, b)
            call advance_pen_position(pen_x)
        end do
    end subroutine render_text_to_image

    subroutine render_glyph_bitmap(image_data, width, height, pen_x, pen_y, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, pen_x, pen_y
        integer(1), intent(in) :: r, g, b
        
        ! For now, render a simple placeholder - actual glyph rendering would need
        ! access to FreeType's glyph slot structure which is complex to bind
        call render_simple_placeholder(image_data, width, height, pen_x, pen_y, r, g, b)
    end subroutine render_glyph_bitmap

    subroutine render_simple_placeholder(image_data, width, height, x, y, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        integer(1), intent(in) :: r, g, b
        integer :: pixel_idx, img_x, img_y, max_idx
        
        max_idx = height * (1 + width * 3)
        
        ! Render a simple 5x7 pixel rectangle as placeholder
        do img_y = y, min(y + 6, height - 1)
            do img_x = x, min(x + 4, width - 1)
                if (img_x >= 0 .and. img_y >= 0) then
                    pixel_idx = img_y * (1 + width * 3) + 1 + img_x * 3 + 1
                    if (pixel_idx > 0 .and. pixel_idx <= max_idx - 2) then
                        image_data(pixel_idx) = r
                        image_data(pixel_idx + 1) = g
                        image_data(pixel_idx + 2) = b
                    end if
                end if
            end do
        end do
    end subroutine render_simple_placeholder

    subroutine advance_pen_position(pen_x)
        integer, intent(inout) :: pen_x
        pen_x = pen_x + 7  ! Simple advance for placeholder
    end subroutine advance_pen_position

end module text_module