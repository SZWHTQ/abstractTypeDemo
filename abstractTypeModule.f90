module abstract_type_m
    implicit none
    private :: new_vertice_t, new_line_t, new_rectangle_t, new_circle_t

    type, abstract, public :: geometry_t
        real(8) :: x, y
    contains
        procedure(write), deferred :: write
        generic, public :: write(formatted) => write
    end type

    abstract interface
        subroutine write(self, unit, io_type, v_list, io_stat, io_msg)
            import geometry_t
            class(geometry_t), intent(in) :: self
            integer, intent(in) :: unit
            character(len=*), intent(in) :: io_type
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: io_stat
            character(len=*), intent(inout) :: io_msg
        end subroutine write
    end interface

    type, extends(geometry_t), public :: vertice_t

    contains
        procedure :: write => write_vertice
    end type

    type, extends(geometry_t), public :: line_t
        real(8) :: length
    contains
        procedure :: write => write_line
    end type

    type, extends(geometry_t), public :: rectangle_t
        real(8) :: length
        real(8) :: width
    contains
        procedure :: write => write_rectangle
    end type

    type, extends(geometry_t), public :: circle_t
        real(8) :: radius
    contains
        procedure :: write => write_circle
    end type

    interface line_t
        procedure :: new_line_t
    end interface line_t

    interface vertice_t
        procedure :: new_vertice_t
    end interface vertice_t

    interface rectangle_t
        procedure :: new_rectangle_t
    end interface rectangle_t

    interface circle_t
        procedure :: new_circle_t
    end interface circle_t

    type geometry_holder
        class(geometry_t), pointer :: geometry
    end type geometry_holder

    enum, bind(c)
        enumerator :: vertice = 0
        enumerator :: line = 1
        enumerator :: rectangle = 2
        enumerator :: circle = 3
    end enum

contains
    type(line_t) pure function new_line_t(x, y, length) result(line)
        real(8), intent(in) :: x, y, length

        line%x = x
        line%y = y
        line%length = length
    end function new_line_t

    type(vertice_t) pure function new_vertice_t(x, y) result(vertice)
        real(8), intent(in) :: x, y

        vertice%x = x
        vertice%y = y
    end function new_vertice_t

    type(rectangle_t) pure function new_rectangle_t(x, y, length, width) result(rectangle)
        real(8), intent(in) :: x, y, length, width

        rectangle%x = x
        rectangle%y = y
        rectangle%length = length
        rectangle%width = width
    end function new_rectangle_t

    type(circle_t) pure function new_circle_t(x, y, radius) result(circle)
        real(8), intent(in) :: x, y, radius

        circle%x = x
        circle%y = y
        circle%radius = radius
    end function new_circle_t

    impure subroutine write_line(self, unit, io_type, v_list, io_stat, io_msg)
        class(line_t), intent(in) :: self
        integer, intent(in) :: unit
        character(len=*), intent(in) :: io_type
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: io_stat
        character(len=*), intent(inout) :: io_msg
        integer :: temp
        character :: buffer

        temp = unit
        buffer = io_type
        temp = size(v_list)
        io_stat = 0
        io_msg = "Success"

        write(*, "(A, *(A, G0, 1X))") "line_t: ", "x = ", self%x, "y = ", self%y, "l = ", self%length

    end subroutine write_line

    impure subroutine write_vertice(self, unit, io_type, v_list, io_stat, io_msg)
        class(vertice_t), intent(in) :: self
        integer, intent(in) :: unit
        character(len=*), intent(in) :: io_type
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: io_stat
        character(len=*), intent(inout) :: io_msg
        integer :: temp
        character :: buffer

        temp = unit
        buffer = io_type
        temp = size(v_list)
        io_stat = 0
        io_msg = "Success"

        write(*, "(A, *(A, G0, 1X))") "vertice_t: ", "x = ", self%x, "y = ", self%y

    end subroutine write_vertice

    impure subroutine write_rectangle(self, unit, io_type, v_list, io_stat, io_msg)
        class(rectangle_t), intent(in) :: self
        integer, intent(in) :: unit
        character(len=*), intent(in) :: io_type
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: io_stat
        character(len=*), intent(inout) :: io_msg
        integer :: temp
        character :: buffer

        temp = unit
        buffer = io_type
        temp = size(v_list)
        io_stat = 0
        io_msg = "Success"

        write(*, "(A, *(A, G0, 1X))") "rectangle_t: ", "x = ", self%x, "y = ", self%y, "l = ", self%length, "w = ", self%width

    end subroutine write_rectangle

    impure subroutine write_circle(self, unit, io_type, v_list, io_stat, io_msg)
        class(circle_t), intent(in) :: self
        integer, intent(in) :: unit
        character(len=*), intent(in) :: io_type
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: io_stat
        character(len=*), intent(inout) :: io_msg
        integer :: temp
        character :: buffer

        temp = unit
        buffer = io_type
        temp = size(v_list)
        io_stat = 0
        io_msg = "Success"

        write(*, "(A, *(A, G0, 1X))") "circle_t: ", "x = ", self%x, "y = ", self%y, "r = ", self%radius

    end subroutine write_circle

    pure subroutine destory_geometry_list(list)
        type(geometry_holder), allocatable, intent(inout) :: list(:)
        integer :: i
    
        do i = 1, size(list)
            nullify(list(i)%geometry)
        end do
        deallocate(list)
        
    end subroutine destory_geometry_list

end module abstract_type_m
