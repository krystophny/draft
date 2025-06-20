program png_generator
    use iso_c_binding
    implicit none
    
    ! PNG signature as signed integers (handling unsigned bytes)
    integer(1) :: png_signature(8) = &
        [int(-119,1), int(80,1), int(78,1), int(71,1), int(13,1), int(10,1), int(26,1), int(10,1)]
    
    ! Image parameters
    integer, parameter :: width = 100
    integer, parameter :: height = 100
    integer, parameter :: bit_depth = 8
    integer, parameter :: color_type = 2  ! RGB
    
    ! File unit
    integer :: png_unit = 10
    
    ! Variables
    integer :: i, j, k
    integer(1), allocatable, target :: image_data(:)
    integer(1), allocatable, target :: compressed_data(:)
    integer(c_long), target :: compressed_size
    integer :: status
    
    ! Open output file
    open(unit=png_unit, file='output.png', access='stream', form='unformatted', status='replace')
    
    ! Write PNG signature
    write(png_unit) png_signature
    
    ! Write IHDR chunk
    call write_ihdr_chunk(png_unit, width, height, bit_depth, color_type)
    
    ! Create image data (red square)
    allocate(image_data(height * (1 + width * 3)))
    k = 1
    do i = 1, height
        image_data(k) = 0_1  ! Filter type (none)
        k = k + 1
        do j = 1, width
            image_data(k) = -1_1            ! Red (255 as signed byte)
            image_data(k+1) = 0_1           ! Green
            image_data(k+2) = 0_1           ! Blue
            k = k + 3
        end do
    end do
    
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
    
    print *, "PNG file 'output.png' created successfully!"
    
contains

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

end program png_generator
