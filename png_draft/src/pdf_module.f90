module pdf_module
    use iso_c_binding
    use plotting_module
    implicit none
    
    private
    public :: pdf_context, create_pdf_plot
    
    ! C interfaces for zlib
    interface
        function compress(dest, destLen, source, sourceLen) bind(C, name="compress")
            import :: c_ptr, c_long, c_int
            type(c_ptr), value :: dest
            type(c_ptr), value :: destLen
            type(c_ptr), value :: source
            integer(c_long), value :: sourceLen
            integer(c_int) :: compress
        end function compress
    end interface
    
    ! PDF plotting context
    type, extends(plot_context) :: pdf_context
        character(len=:), allocatable :: content
        real :: current_r, current_g, current_b
        integer :: content_length
        integer :: next_obj_id
    contains
        procedure :: draw_line => pdf_draw_line
        procedure :: set_color => pdf_set_color
        procedure :: finalize => pdf_finalize
    end type pdf_context
    
contains

    function create_pdf_plot(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(pdf_context) :: ctx
        
        call create_plot(ctx, width, height, 0.0, 1.0, 0.0, 1.0)
        
        ! Initialize PDF content
        ctx%content = ""
        ctx%current_r = 0.0
        ctx%current_g = 0.0
        ctx%current_b = 1.0  ! Default blue
        ctx%content_length = 0
        ctx%next_obj_id = 1
        
        ! Add initial content stream setup
        call append_content(ctx, "q" // char(10))  ! Save graphics state
        call append_content(ctx, "1 0 0 1 0 0 cm" // char(10))  ! Identity matrix
    end function create_pdf_plot
    
    subroutine pdf_draw_line(this, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: this
        real, intent(in) :: x1, y1, x2, y2
        real :: pdf_x1, pdf_y1, pdf_x2, pdf_y2
        character(len=200) :: line_cmd
        
        ! Convert normalized coordinates [0,1] to PDF coordinates
        pdf_x1 = x1 * real(this%width)
        pdf_y1 = (1.0 - y1) * real(this%height)  ! PDF Y increases upward
        pdf_x2 = x2 * real(this%width)
        pdf_y2 = (1.0 - y2) * real(this%height)
        
        ! Create PDF path commands
        write(line_cmd, '(F8.2, 1X, F8.2, 1X, "m", 1X, F8.2, 1X, F8.2, 1X, "l S", A)') &
            pdf_x1, pdf_y1, pdf_x2, pdf_y2, char(10)
        
        call append_content(this, trim(line_cmd))
    end subroutine pdf_draw_line
    
    subroutine pdf_set_color(this, r, g, b)
        class(pdf_context), intent(inout) :: this
        real, intent(in) :: r, g, b
        character(len=100) :: color_cmd
        
        this%current_r = r
        this%current_g = g  
        this%current_b = b
        
        ! Set stroke color in PDF
        write(color_cmd, '(F4.2, 1X, F4.2, 1X, F4.2, 1X, "RG", A)') r, g, b, char(10)
        call append_content(this, trim(color_cmd))
    end subroutine pdf_set_color
    
    subroutine pdf_finalize(this, filename)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer :: unit
        character(len=:), allocatable :: compressed_content
        integer(c_long) :: compressed_size
        integer :: status
        
        ! Close graphics state
        call append_content(this, "Q" // char(10))
        
        ! Compress content using zlib
        call compress_pdf_content(this%content, compressed_content, compressed_size, status)
        
        if (status /= 0) then
            print *, "PDF compression failed with status:", status
            return
        end if
        
        ! Write PDF file
        open(newunit=unit, file=filename, access='stream', form='unformatted', status='replace')
        call write_pdf_header(unit)
        call write_pdf_objects(unit, this%width, this%height, compressed_content, int(compressed_size))
        close(unit)
        
        print *, "PDF file '", trim(filename), "' created successfully!"
    end subroutine pdf_finalize
    
    subroutine append_content(ctx, text)
        type(pdf_context), intent(inout) :: ctx
        character(len=*), intent(in) :: text
        
        if (allocated(ctx%content)) then
            ctx%content = ctx%content // text
        else
            ctx%content = text
        end if
        ctx%content_length = ctx%content_length + len(text)
    end subroutine append_content
    
    subroutine compress_pdf_content(content, compressed, compressed_size, status)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: compressed
        integer(c_long), intent(out) :: compressed_size
        integer, intent(out) :: status
        integer(1), allocatable, target :: source(:), dest(:)
        integer(c_long), target :: dest_len
        integer :: i
        
        ! Convert string to byte array
        allocate(source(len(content)))
        do i = 1, len(content)
            source(i) = int(iachar(content(i:i)), 1)
        end do
        
        ! Allocate destination buffer
        dest_len = int(len(content) * 1.1 + 12, c_long)
        allocate(dest(dest_len))
        
        ! Compress using zlib
        status = compress(c_loc(dest), c_loc(dest_len), c_loc(source), int(len(content), c_long))
        compressed_size = dest_len
        
        if (status == 0) then
            ! Convert back to string
            allocate(character(len=int(compressed_size)) :: compressed)
            do i = 1, int(compressed_size)
                compressed(i:i) = char(dest(i))
            end do
        end if
        
        deallocate(source, dest)
    end subroutine compress_pdf_content
    
    subroutine write_pdf_header(unit)
        integer, intent(in) :: unit
        character(len=*), parameter :: header = "%PDF-1.4" // char(10)
        call write_pdf_string(unit, header)
    end subroutine write_pdf_header
    
    subroutine write_pdf_objects(unit, width, height, content, content_len)
        integer, intent(in) :: unit, width, height, content_len
        character(len=*), intent(in) :: content
        character(len=1000) :: obj_str
        character(len=100) :: len_str, size_str
        integer :: obj1_pos, obj2_pos, obj3_pos, xref_pos
        
        ! Object 1: Catalog
        obj1_pos = get_file_position(unit)
        obj_str = "1 0 obj" // char(10) // &
                 "<<" // char(10) // &
                 "/Type /Catalog" // char(10) // &
                 "/Pages 2 0 R" // char(10) // &
                 ">>" // char(10) // &
                 "endobj" // char(10)
        call write_pdf_string(unit, obj_str)
        
        ! Object 2: Pages
        obj2_pos = get_file_position(unit)
        write(size_str, '(I0, 1X, I0)') width, height
        obj_str = "2 0 obj" // char(10) // &
                 "<<" // char(10) // &
                 "/Type /Pages" // char(10) // &
                 "/Kids [3 0 R]" // char(10) // &
                 "/Count 1" // char(10) // &
                 ">>" // char(10) // &
                 "endobj" // char(10)
        call write_pdf_string(unit, obj_str)
        
        ! Object 3: Page  
        obj3_pos = get_file_position(unit)
        write(len_str, '(I0)') content_len
        obj_str = "3 0 obj" // char(10) // &
                 "<<" // char(10) // &
                 "/Type /Page" // char(10) // &
                 "/Parent 2 0 R" // char(10) // &
                 "/MediaBox [0 0 " // trim(size_str) // "]" // char(10) // &
                 "/Contents 4 0 R" // char(10) // &
                 ">>" // char(10) // &
                 "endobj" // char(10)
        call write_pdf_string(unit, obj_str)
        
        ! Object 4: Content stream
        obj_str = "4 0 obj" // char(10) // &
                 "<<" // char(10) // &
                 "/Length " // trim(len_str) // char(10) // &
                 "/Filter /FlateDecode" // char(10) // &
                 ">>" // char(10) // &
                 "stream" // char(10)
        call write_pdf_string(unit, obj_str)
        
        ! Write compressed content
        call write_pdf_binary(unit, content, content_len)
        
        obj_str = char(10) // "endstream" // char(10) // "endobj" // char(10)
        call write_pdf_string(unit, obj_str)
        
        ! Write xref table and trailer
        xref_pos = get_file_position(unit)
        call write_xref_table(unit, obj1_pos, obj2_pos, obj3_pos)
        call write_trailer(unit, xref_pos)
    end subroutine write_pdf_objects
    
    integer function get_file_position(unit)
        integer, intent(in) :: unit
        inquire(unit=unit, pos=get_file_position)
        get_file_position = get_file_position - 1  ! Convert to 0-based
    end function get_file_position
    
    subroutine write_pdf_string(unit, str)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: str
        integer :: i
        integer(1), allocatable :: bytes(:)
        
        allocate(bytes(len(str)))
        do i = 1, len(str)
            bytes(i) = int(iachar(str(i:i)), 1)
        end do
        write(unit) bytes
        deallocate(bytes)
    end subroutine write_pdf_string
    
    subroutine write_pdf_binary(unit, content, content_len)
        integer, intent(in) :: unit, content_len
        character(len=*), intent(in) :: content
        integer :: i
        integer(1), allocatable :: bytes(:)
        
        allocate(bytes(content_len))
        do i = 1, content_len
            bytes(i) = int(iachar(content(i:i)), 1)
        end do
        write(unit) bytes
        deallocate(bytes)
    end subroutine write_pdf_binary
    
    subroutine write_xref_table(unit, obj1_pos, obj2_pos, obj3_pos)
        integer, intent(in) :: unit, obj1_pos, obj2_pos, obj3_pos
        character(len=200) :: xref_str
        
        write(xref_str, '("xref", A, "0 5", A, "0000000000 65535 f ", A, &
                         &I10.10, " 00000 n ", A, I10.10, " 00000 n ", A, &
                         &I10.10, " 00000 n ", A, "0000000000 65535 f ", A)') &
               char(10), char(10), char(10), obj1_pos, char(10), obj2_pos, char(10), &
               obj3_pos, char(10), char(10)
        call write_pdf_string(unit, trim(xref_str))
    end subroutine write_xref_table
    
    subroutine write_trailer(unit, xref_pos)
        integer, intent(in) :: unit, xref_pos
        character(len=200) :: trailer_str
        
        write(trailer_str, '("trailer", A, "<<", A, "/Size 5", A, "/Root 1 0 R", A, &
                            &">>", A, "startxref", A, I0, A, "%%EOF", A)') &
               char(10), char(10), char(10), char(10), char(10), char(10), xref_pos, char(10)
        call write_pdf_string(unit, trim(trailer_str))
    end subroutine write_trailer

end module pdf_module