module png_module
    use iso_c_binding
    implicit none
    
    private
    public :: generate_png_with_plot, plot_line
    
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
        call create_plot_with_axes(image_data, width, height)
        
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

    subroutine create_plot_with_axes(image_data, w, h)
        integer(1), intent(out) :: image_data(*)
        integer, intent(in) :: w, h
        real :: pi
        
        pi = 4.0 * atan(1.0)
        
        ! Initialize image with white background
        call initialize_white_background(image_data, w, h)
        
        ! Plot continuous sine wave
        call draw_continuous_sine_wave(image_data, w, h, pi)
        
        ! Add coordinate axes
        call draw_axes(image_data, w, h)
    end subroutine create_plot_with_axes

    subroutine initialize_white_background(image_data, w, h)
        integer(1), intent(out) :: image_data(*)
        integer, intent(in) :: w, h
        integer :: i, j, k
        
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
    end subroutine initialize_white_background

    subroutine draw_axes(image_data, w, h)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: w, h
        integer :: i, j, k, x_center, y_center
        
        x_center = w / 2
        y_center = h / 2
        
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
    end subroutine draw_axes

    subroutine draw_continuous_sine_wave(image_data, w, h, pi)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: w, h
        real, intent(in) :: pi
        real, allocatable :: x_values(:), y_values(:)
        integer :: i
        real :: x_norm, y_norm
        
        ! Generate sine wave data points
        allocate(x_values(w), y_values(w))
        
        do i = 1, w
            x_norm = real(i - 1) / real(w - 1)  ! Normalize x to [0,1]
            y_norm = 0.5 * sin(4.0 * pi * x_norm) + 0.5  ! Sine wave, normalized to [0,1]
            
            x_values(i) = x_norm
            y_values(i) = y_norm
        end do
        
        ! Plot the line using generic function
        call plot_line(image_data, w, h, x_values, y_values, w, 0_1, 0_1, -1_1)
        
        deallocate(x_values, y_values)
    end subroutine draw_continuous_sine_wave

    subroutine plot_line(image_data, img_w, img_h, x_data, y_data, n_points, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, n_points
        real, intent(in) :: x_data(n_points), y_data(n_points)
        integer(1), intent(in) :: r, g, b
        integer :: i, x_prev, y_prev, x_curr, y_curr
        integer :: x_min, x_max, y_min, y_max, x, y
        
        if (n_points < 1) return
        
        ! Convert first point to pixel coordinates
        x_prev = int(x_data(1) * real(img_w - 1)) + 1
        y_prev = int(y_data(1) * real(img_h - 1)) + 1
        call clamp_coordinates(x_prev, y_prev, img_w, img_h)
        
        ! Draw first point
        call set_pixel(image_data, x_prev, y_prev, img_w, img_h, r, g, b)
        
        ! Draw line segments connecting consecutive points
        do i = 2, n_points
            ! Convert current point to pixel coordinates
            x_curr = int(x_data(i) * real(img_w - 1)) + 1
            y_curr = int(y_data(i) * real(img_h - 1)) + 1
            call clamp_coordinates(x_curr, y_curr, img_w, img_h)
            
            ! Draw line segment using Bresenham-like algorithm
            call draw_line_segment(image_data, img_w, img_h, x_prev, y_prev, x_curr, y_curr, r, g, b)
            
            x_prev = x_curr
            y_prev = y_curr
        end do
    end subroutine plot_line

    subroutine draw_line_segment(image_data, img_w, img_h, x1, y1, x2, y2, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: img_w, img_h, x1, y1, x2, y2
        integer(1), intent(in) :: r, g, b
        integer :: dx, dy, steps, i
        real :: x_step, y_step, x, y
        
        dx = x2 - x1
        dy = y2 - y1
        steps = max(abs(dx), abs(dy))
        
        if (steps == 0) then
            call set_pixel(image_data, x1, y1, img_w, img_h, r, g, b)
            return
        end if
        
        x_step = real(dx) / real(steps)
        y_step = real(dy) / real(steps)
        
        x = real(x1)
        y = real(y1)
        
        do i = 0, steps
            call set_pixel(image_data, nint(x), nint(y), img_w, img_h, r, g, b)
            x = x + x_step
            y = y + y_step
        end do
    end subroutine draw_line_segment

    subroutine clamp_coordinates(x, y, max_w, max_h)
        integer, intent(inout) :: x, y
        integer, intent(in) :: max_w, max_h
        
        if (x < 1) x = 1
        if (x > max_w) x = max_w
        if (y < 1) y = 1
        if (y > max_h) y = max_h
    end subroutine clamp_coordinates

    subroutine set_pixel(image_data, x, y, w, h, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: x, y, w, h
        integer(1), intent(in) :: r, g, b
        integer :: k
        
        ! Check bounds
        if (x < 1 .or. x > w .or. y < 1 .or. y > h) return
        
        ! Calculate pixel position in image_data array
        k = (y - 1) * (1 + w * 3) + 1 + (x - 1) * 3 + 1
        
        ! Set pixel color
        image_data(k) = r
        image_data(k+1) = g
        image_data(k+2) = b
    end subroutine set_pixel

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