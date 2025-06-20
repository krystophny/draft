module text_module
    use iso_c_binding
    implicit none
    
    private
    public :: init_text_system, cleanup_text_system, render_text_to_image
    
    ! FreeType structures
    type, bind(C) :: ft_bitmap
        integer(c_int) :: rows
        integer(c_int) :: width
        integer(c_int) :: pitch
        type(c_ptr) :: buffer
        integer(c_short) :: num_grays
        integer(c_char) :: pixel_mode
        integer(c_char) :: palette_mode
        type(c_ptr) :: palette
    end type ft_bitmap
    
    type, bind(C) :: ft_glyph_slot_rec
        type(c_ptr) :: library
        type(c_ptr) :: face
        type(c_ptr) :: next
        integer(c_int) :: reserved
        type(c_ptr) :: generic_data
        type(c_ptr) :: generic_finalizer
        integer(c_long) :: advance_x
        integer(c_long) :: advance_y
        integer(c_int) :: format
        type(ft_bitmap) :: bitmap
        integer(c_int) :: bitmap_left
        integer(c_int) :: bitmap_top
        type(c_ptr) :: outline
        integer(c_int) :: num_subglyphs
        type(c_ptr) :: subglyphs
        type(c_ptr) :: control_data
        integer(c_long) :: control_len
        integer(c_long) :: lsb_delta
        integer(c_long) :: rsb_delta
        type(c_ptr) :: other
        type(c_ptr) :: internal
    end type ft_glyph_slot_rec

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
        
        print *, "PNG: Rendering text '", trim(text), "' at pixel position:", x, y
        
        if (.not. text_system_initialized) then
            if (.not. init_text_system()) then
                print *, "PNG: Failed to initialize text system, using placeholder"
                call render_simple_placeholder(image_data, width, height, x, y, r, g, b)
                return
            else
                print *, "PNG: Text system initialized successfully"
            end if
        end if
        
        pen_x = x
        pen_y = y
        
        do i = 1, len_trim(text)
            char_code = iachar(text(i:i))
            
            error = ft_load_char(ft_face, int(char_code, c_long), FT_LOAD_RENDER)
            if (error /= 0) then
                print *, "PNG: Failed to load character:", text(i:i), "error:", error
                cycle  ! Skip character if it can't be loaded
            end if
            
            print *, "PNG: Rendering character:", text(i:i), "at pen position:", pen_x, pen_y
            call render_glyph_bitmap(image_data, width, height, pen_x, pen_y, r, g, b)
            call advance_pen_position(pen_x)
        end do
    end subroutine render_text_to_image

    subroutine render_glyph_bitmap(image_data, width, height, pen_x, pen_y, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, pen_x, pen_y
        integer(1), intent(in) :: r, g, b
        
        ! Try to get proper FreeType glyph rendering
        call render_freetype_glyph_direct(image_data, width, height, pen_x, pen_y, r, g, b)
    end subroutine render_glyph_bitmap
    
    subroutine render_freetype_glyph_direct(image_data, width, height, pen_x, pen_y, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, pen_x, pen_y
        integer(1), intent(in) :: r, g, b
        integer(c_intptr_t) :: face_ptr
        integer(c_intptr_t) :: glyph_ptr
        type(ft_glyph_slot_rec), pointer :: glyph_slot
        integer(1), pointer :: bitmap_buffer(:)
        integer :: glyph_x, glyph_y, img_x, img_y, row, col, pixel_idx
        integer :: bitmap_width, bitmap_height, bitmap_left, bitmap_top
        integer(1) :: alpha
        real :: alpha_f, inv_alpha
        
        ! Get face pointer as integer
        face_ptr = transfer(ft_face, face_ptr)
        
        ! In FT_FaceRec, glyph is at offset 24 bytes from start
        glyph_ptr = face_ptr + 24_c_intptr_t
        
        ! Convert back to pointer and dereference to get actual glyph slot pointer  
        call c_f_pointer(transfer(glyph_ptr, c_null_ptr), glyph_slot)
        
        bitmap_width = glyph_slot%bitmap%width
        bitmap_height = glyph_slot%bitmap%rows
        bitmap_left = glyph_slot%bitmap_left
        bitmap_top = glyph_slot%bitmap_top
        
        if (bitmap_width <= 0 .or. bitmap_height <= 0) then
            ! Fallback to simple block if no bitmap
            call render_simple_character_block(image_data, width, height, pen_x, pen_y, r, g, b)
            return
        end if
        
        ! Convert buffer pointer to Fortran array
        call c_f_pointer(glyph_slot%bitmap%buffer, bitmap_buffer, [bitmap_width * bitmap_height])
        
        glyph_x = pen_x + bitmap_left
        glyph_y = pen_y - bitmap_top
        
        ! Render the bitmap onto the image with alpha blending
        do row = 0, bitmap_height - 1
            do col = 0, bitmap_width - 1
                img_x = glyph_x + col
                img_y = glyph_y + row
                
                if (img_x >= 0 .and. img_x < width .and. img_y >= 0 .and. img_y < height) then
                    alpha = bitmap_buffer(row * bitmap_width + col + 1)
                    if (alpha > 0) then
                        pixel_idx = img_y * (1 + width * 3) + 1 + img_x * 3 + 1
                        
                        if (alpha == -1_1) then  ! 255 in signed byte
                            ! Fully opaque - use black for text
                            image_data(pixel_idx) = 0_1
                            image_data(pixel_idx + 1) = 0_1  
                            image_data(pixel_idx + 2) = 0_1
                        else
                            ! Alpha blending with black text
                            alpha_f = real(int(alpha, kind=selected_int_kind(2)) + merge(256, 0, alpha < 0)) / 255.0
                            inv_alpha = 1.0 - alpha_f
                            
                            ! Blend towards black (0,0,0)
                            image_data(pixel_idx) = int(inv_alpha * real(int(image_data(pixel_idx), kind=selected_int_kind(2)) + &
                                                        merge(256, 0, image_data(pixel_idx) < 0)), 1)
                            image_data(pixel_idx + 1) = int(inv_alpha * real(int(image_data(pixel_idx + 1), kind=selected_int_kind(2)) + &
                                                            merge(256, 0, image_data(pixel_idx + 1) < 0)), 1)
                            image_data(pixel_idx + 2) = int(inv_alpha * real(int(image_data(pixel_idx + 2), kind=selected_int_kind(2)) + &
                                                            merge(256, 0, image_data(pixel_idx + 2) < 0)), 1)
                        end if
                    end if
                end if
            end do
        end do
    end subroutine render_freetype_glyph_direct

    subroutine render_simple_character_block(image_data, width, height, x, y, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        integer(1), intent(in) :: r, g, b
        integer :: img_x, img_y, pixel_idx
        integer(1) :: black_r, black_g, black_b
        
        ! Use black color for visibility against white background
        black_r = 0_1
        black_g = 0_1
        black_b = 0_1
        
        ! Render a small 4x6 pixel block to represent the character
        do img_y = y, min(y + 5, height - 1)
            do img_x = x, min(x + 3, width - 1)
                if (img_x >= 0 .and. img_y >= 0) then
                    pixel_idx = img_y * (1 + width * 3) + 1 + img_x * 3 + 1
                    if (pixel_idx > 0 .and. pixel_idx <= height * (1 + width * 3) - 2) then
                        image_data(pixel_idx) = black_r
                        image_data(pixel_idx + 1) = black_g
                        image_data(pixel_idx + 2) = black_b
                    end if
                end if
            end do
        end do
    end subroutine render_simple_character_block

    subroutine render_simple_placeholder(image_data, width, height, x, y, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        integer(1), intent(in) :: r, g, b
        integer :: pixel_idx, img_x, img_y, max_idx
        
        max_idx = height * (1 + width * 3)
        
        print *, "PNG: Drawing placeholder at", x, y, "with color", int(r), int(g), int(b)
        
        ! Render a simple 5x7 pixel rectangle as placeholder
        do img_y = y, min(y + 6, height - 1)
            do img_x = x, min(x + 4, width - 1)
                if (img_x >= 0 .and. img_y >= 0) then
                    pixel_idx = img_y * (1 + width * 3) + 1 + img_x * 3 + 1
                    if (pixel_idx > 0 .and. pixel_idx <= max_idx - 2) then
                        image_data(pixel_idx) = r
                        image_data(pixel_idx + 1) = g
                        image_data(pixel_idx + 2) = b
                        print *, "PNG: Set pixel at", img_x, img_y, "index", pixel_idx
                    else
                        print *, "PNG: Pixel out of bounds at", img_x, img_y, "index", pixel_idx
                    end if
                end if
            end do
        end do
    end subroutine render_simple_placeholder

    subroutine advance_pen_position(pen_x)
        integer, intent(inout) :: pen_x
        integer(c_intptr_t) :: face_ptr, glyph_ptr
        type(ft_glyph_slot_rec), pointer :: glyph_slot
        
        ! Get glyph slot to read advance
        face_ptr = transfer(ft_face, face_ptr)
        glyph_ptr = face_ptr + 24_c_intptr_t
        call c_f_pointer(transfer(glyph_ptr, c_null_ptr), glyph_slot)
        
        ! Advance by the glyph's advance value (convert from 26.6 fixed point)
        pen_x = pen_x + max(int(glyph_slot%advance_x / 64), 6)
    end subroutine advance_pen_position

end module text_module