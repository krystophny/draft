module png_module
    use iso_c_binding
    implicit none
    
    private
    public :: generate_png_with_plot
    
contains

    subroutine generate_png_with_plot(filename, width, height)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: width, height
        
        ! PNG signature as signed integers (handling unsigned bytes)
        integer(1) :: png_signature(8) = &
            [int(-119,1), int(80,1), int(78,1), int(71,1), int(13,1), int(10,1), int(26,1), int(10,1)]
        
        ! PNG parameters
        integer, parameter :: bit_depth = 8
        integer, parameter :: color_type = 2  ! RGB
        
        ! File unit
        integer :: png_unit = 10
        
        ! Variables
        integer(1), allocatable, target :: image_data(:)
        integer(1), allocatable, target :: compressed_data(:)
        integer(c_long), target :: compressed_size
        integer :: status
        
        ! Open output file
        open(unit=png_unit, file=filename, access='stream', form='unformatted', status='replace')
        
        ! Write PNG signature
        write(png_unit) png_signature
        
        ! Write IHDR chunk
        call write_ihdr_chunk(png_unit, width, height, bit_depth, color_type)
        
        ! Create image data with plotted line
        allocate(image_data(height * (1 + width * 3)))
        call create_line_plot(image_data, width, height)
        
        ! Compress image data
        compressed_size = int(size(image_data) * 1.1 + 12, c_long)
        allocate(compressed_data(compressed_size))
        
        status = compress_data(image_data, size(image_data), compressed_data, compressed_size)
        if (status /= 0) then
            print *, "Compression failed with status:", status
            stop
        end if
        
        ! Write IDAT chunk
        call write_idat_chunk(png_unit, compressed_data, int(compressed_size))
        
        ! Write IEND chunk
        call write_iend_chunk(png_unit)
        
        ! Close file
        close(png_unit)
        
        ! Cleanup
        deallocate(image_data)
        deallocate(compressed_data)
        
        print *, "PNG file '", trim(filename), "' created successfully!"
    end subroutine generate_png_with_plot

    subroutine create_line_plot(image_data, w, h)
        integer(1), intent(out) :: image_data(*)
        integer, intent(in) :: w, h
        integer :: i, j, k, y_plot, x_center, y_center
        real :: x_norm, y_norm, pi
        
        pi = 4.0 * atan(1.0)
        x_center = w / 2
        y_center = h / 2
        
        ! Initialize image with white background
        k = 1
        do i = 1, h
            image_data(k) = 0_1  ! Filter type (none)
            k = k + 1
            do j = 1, w
                image_data(k) = -1_1     ! White background (R=255)
                image_data(k+1) = -1_1   ! White background (G=255)
                image_data(k+2) = -1_1   ! White background (B=255)
                k = k + 3
            end do
        end do
        
        ! Plot sine wave
        do j = 1, w
            x_norm = real(j - 1) / real(w - 1)  ! Normalize x to [0,1]
            y_norm = 0.5 * sin(4.0 * pi * x_norm) + 0.5  ! Sine wave, normalized to [0,1]
            y_plot = int(y_norm * real(h - 1)) + 1  ! Convert to pixel coordinates
            
            ! Clamp y_plot to valid range
            if (y_plot < 1) y_plot = 1
            if (y_plot > h) y_plot = h
            
            ! Calculate pixel position in image_data array
            k = (y_plot - 1) * (1 + w * 3) + 1 + (j - 1) * 3 + 1
            
            ! Set pixel to blue for the line
            image_data(k) = 0_1        ! Red = 0
            image_data(k+1) = 0_1      ! Green = 0  
            image_data(k+2) = -1_1     ! Blue = 255
            
            ! Add some thickness to the line
            if (y_plot > 1) then
                k = (y_plot - 2) * (1 + w * 3) + 1 + (j - 1) * 3 + 1
                image_data(k) = 0_1        ! Red = 0
                image_data(k+1) = 0_1      ! Green = 0  
                image_data(k+2) = -1_1     ! Blue = 255
            end if
            if (y_plot < h) then
                k = y_plot * (1 + w * 3) + 1 + (j - 1) * 3 + 1
                image_data(k) = 0_1        ! Red = 0
                image_data(k+1) = 0_1      ! Green = 0  
                image_data(k+2) = -1_1     ! Blue = 255
            end if
        end do
        
        ! Add axes
        ! X-axis (horizontal line at center)
        do j = 1, w
            k = (y_center - 1) * (1 + w * 3) + 1 + (j - 1) * 3 + 1
            image_data(k) = 64_1       ! Dark gray (R=64)
            image_data(k+1) = 64_1     ! Dark gray (G=64)
            image_data(k+2) = 64_1     ! Dark gray (B=64)
        end do
        
        ! Y-axis (vertical line at center)
        do i = 1, h
            k = (i - 1) * (1 + w * 3) + 1 + (x_center - 1) * 3 + 1
            image_data(k) = 64_1       ! Dark gray (R=64)
            image_data(k+1) = 64_1     ! Dark gray (G=64)
            image_data(k+2) = 64_1     ! Dark gray (B=64)
        end do
    end subroutine create_line_plot

    subroutine write_chunk(unit, chunk_type, chunk_data, data_size)
        integer, intent(in) :: unit
        character(len=4), intent(in) :: chunk_type
        integer(1), intent(in) :: chunk_data(*)
        integer, intent(in) :: data_size
        
        integer :: length_be
        integer(1) :: type_bytes(4)
        integer(1), allocatable, target :: full_data(:)
        integer(c_int32_t) :: crc_value
        integer :: crc_be
        integer :: i
        
        ! Convert length to big-endian
        length_be = to_big_endian(data_size)
        
        ! Convert chunk type to bytes
        do i = 1, 4
            type_bytes(i) = int(iachar(chunk_type(i:i)), 1)
        end do
        
        ! Prepare data for CRC calculation
        allocate(full_data(4 + data_size))
        full_data(1:4) = type_bytes
        if (data_size > 0) then
            full_data(5:4+data_size) = chunk_data(1:data_size)
        end if
        
        ! Calculate CRC
        crc_value = calculate_crc32(full_data, 4 + data_size)
        crc_be = to_big_endian(int(crc_value))
        
        ! Write chunk
        write(unit) length_be
        write(unit) type_bytes
        if (data_size > 0) then
            write(unit) chunk_data(1:data_size)
        end if
        write(unit) crc_be
        
        deallocate(full_data)
    end subroutine write_chunk
    
    subroutine write_ihdr_chunk(unit, w, h, bd, ct)
        integer, intent(in) :: unit, w, h, bd, ct
        integer(1) :: ihdr_data(13)
        integer :: w_be, h_be
        
        w_be = to_big_endian(w)
        h_be = to_big_endian(h)
        
        ! Pack IHDR data
        ihdr_data(1:4) = transfer(w_be, ihdr_data(1:4))
        ihdr_data(5:8) = transfer(h_be, ihdr_data(5:8))
        ihdr_data(9) = int(bd, 1)
        ihdr_data(10) = int(ct, 1)
        ihdr_data(11) = 0_1  ! Compression method
        ihdr_data(12) = 0_1  ! Filter method
        ihdr_data(13) = 0_1  ! Interlace method
        
        call write_chunk(unit, "IHDR", ihdr_data, 13)
    end subroutine write_ihdr_chunk
    
    subroutine write_idat_chunk(unit, data, size)
        integer, intent(in) :: unit, size
        integer(1), intent(in) :: data(size)
        
        call write_chunk(unit, "IDAT", data, size)
    end subroutine write_idat_chunk
    
    subroutine write_iend_chunk(unit)
        integer, intent(in) :: unit
        integer(1) :: dummy(1)
        
        call write_chunk(unit, "IEND", dummy, 0)
    end subroutine write_iend_chunk
    
    function to_big_endian(value) result(be_value)
        integer, intent(in) :: value
        integer :: be_value
        integer(1) :: bytes(4)
        
        bytes(1) = int(ibits(value, 24, 8), 1)
        bytes(2) = int(ibits(value, 16, 8), 1)
        bytes(3) = int(ibits(value, 8, 8), 1)
        bytes(4) = int(ibits(value, 0, 8), 1)
        
        be_value = transfer(bytes, be_value)
    end function to_big_endian
    
    ! Interface to zlib compress function
    function compress_data(source, source_len, dest, dest_len) result(status)
        integer(1), target, intent(in) :: source(*)
        integer, intent(in) :: source_len
        integer(1), target, intent(out) :: dest(*)
        integer(c_long), target, intent(inout) :: dest_len
        integer :: status
        
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
        
        status = compress(c_loc(dest), c_loc(dest_len), c_loc(source), int(source_len, c_long))
    end function compress_data
    
    ! Interface to zlib crc32 function
    function calculate_crc32(data, len) result(crc)
        integer(1), target, intent(in) :: data(*)
        integer, intent(in) :: len
        integer(c_int32_t) :: crc
        
        interface
            function crc32(crc, buf, len) bind(C, name="crc32")
                import :: c_int32_t, c_ptr, c_int
                integer(c_int32_t), value :: crc
                type(c_ptr), value :: buf
                integer(c_int), value :: len
                integer(c_int32_t) :: crc32
            end function crc32
        end interface
        
        crc = crc32(0_c_int32_t, c_loc(data), int(len, c_int))
    end function calculate_crc32
    
    ! Helper function to convert unsigned byte to signed integer(1)
    function uint8_to_int1(uint_val) result(int_val)
        integer, intent(in) :: uint_val
        integer(1) :: int_val
        
        if (uint_val > 127) then
            int_val = int(uint_val - 256, 1)
        else
            int_val = int(uint_val, 1)
        end if
    end function uint8_to_int1

end module png_module