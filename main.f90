program main
    use abstract_type_m
    implicit none
    type(geometry_holder), allocatable :: geometry_list(:)
    integer :: n

    read(*,*) n

    allocate(geometry_list(n))
    call initialize(n, geometry_list)
    call output(n, geometry_list)

    call destory_geometry_list(geometry_list)

end program main

subroutine output(n, geometry_list)
    use abstract_type_m
    integer, intent(in) :: n
    type(geometry_holder), intent(in) :: geometry_list(n)
    integer :: i

    do i = 1, n
        write(*,"(A, I0, A)") "Geometry ", i, ": "
        write(*,"(DT)") geometry_list(i)%geometry
    end do

end subroutine output

subroutine initialize(n, geometry_list)
    use abstract_type_m
    implicit none
    integer, intent(in) :: n
    type(geometry_holder), intent(inout) :: geometry_list(n)
    integer :: i

    do i = 1, n
        if ( mod(i, 3) + 1 == line ) then
            block
                type(line_t) :: temp
                read(*,*) temp%x, temp%y, temp%length
                allocate(geometry_list(i)%geometry, source=temp)
            end block
            ! g%length = 7
        else if ( mod(i, 3) + 1 == rectangle ) then
            block
                type(rectangle_t ):: temp
                read(*,*) temp%x, temp%y, temp%length, temp%width
                allocate(geometry_list(i)%geometry, source=temp)
            end block
        else if ( mod(i, 3) + 1 == circle ) then
            block
                type(circle_t) :: temp
                read(*,*) temp%x, temp%y, temp%radius
                allocate(geometry_list(i)%geometry, source=temp)
            end block
        end if
    end do

end subroutine initialize

! subroutine output_allocate(n, geometry_list)
!     use abstract_type_m
!     integer, intent(in) :: n
!     type(geometry_holder), intent(inout) :: geometry_list(n)
!     integer :: i

! #ifdef __GNUC__
!     do i = 1, 3
!         associate(g => geometry_list(i)%geometry)
!             call g%write()
!         end associate
!     end do
! #elif __INTEL_COMPILER
!     do i = 1, 3
!         call geometry_list(i)%geometry%write()
!     end do
! #elif
!     write(*,"(A)") "Compiler not supported"
! #endif

! end subroutine output_allocate

! subroutine initialize_allocate(n, geometry_list)
!     use abstract_type_m
!     implicit none
!     integer, intent(in) :: n
!     type(geometry_holder), intent(inout) :: geometry_list(n)
!     integer :: i

! #ifdef __GNUC__
!     do i = 1, 3
!         associate(g => geometry_list(i)%geometry)
!             if ( i == 1 ) then
!                 allocate(line_t :: g)
!                 g = line_t(2,4,8)
!                 ! g%length = 7
!             else if ( i == 2 ) then
!                 allocate(rectangle_t :: g)
!                 g = rectangle_t(3,6,9,12)
!             else if ( i == 3 ) then
!                 allocate(circle_t :: g)
!                 g = circle_t(5,10,11)
!             end if
!         end associate
!     end do
! #elif __INTEL_COMPILER
!     do i = 1, 3
!         if ( i == 1 ) then
!             block
!                 allocate(line_t :: geometry_list(i)%geometry)
!                 geometry_list(i)%geometry = line_t(2,4,8)
!             end block
!             ! g%length = 7
!         else if ( i == 2 ) then
!             block
!                 allocate(rectangle_t :: geometry_list(i)%geometry)
!                 geometry_list(i)%geometry = rectangle_t(3,6,9,12)
!             end block
!         else if ( i == 3 ) then
!             block
!                 allocate(circle_t :: geometry_list(i)%geometry)
!                 geometry_list(i)%geometry = circle_t(5,10,11)
!             end block
!         end if
!     end do
! #elif
!     write(*,"(A)") "Compiler not supported"
! #endif

! end subroutine initialize_allocate
