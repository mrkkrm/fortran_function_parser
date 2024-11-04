module readwrite_mod

implicit none
private

public :: read_pointer
public :: read_allocatable
public :: write_pointer
public :: write_allocatable


interface read_pointer
    module procedure :: read_pointer__r8_0
    module procedure :: read_pointer__r8_1
    module procedure :: read_pointer__r8_2
    module procedure :: read_pointer__r8_3
end interface

interface read_allocatable
    module procedure :: read_allocatable__r8_0
    module procedure :: read_allocatable__r8_1
    module procedure :: read_allocatable__r8_2
    module procedure :: read_allocatable__r8_3
end interface

interface write_pointer
    module procedure :: write_pointer__r8_0
    module procedure :: write_pointer__r8_1
    module procedure :: write_pointer__r8_2
    module procedure :: write_pointer__r8_3
end interface

interface write_allocatable
    module procedure :: write_allocatable__r8_0
    module procedure :: write_allocatable__r8_1
    module procedure :: write_allocatable__r8_2
    module procedure :: write_allocatable__r8_3
end interface

contains

!********************************************

subroutine write_pointer__r8_0 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(in), pointer :: x
    logical(1) :: test
    test = associated(x)
    write(fid) test
    if (test) then
        write(fid) x
    end if
end subroutine write_pointer__r8_0

subroutine write_pointer__r8_1 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(in), pointer :: x(:)
    logical(1) :: test
    test = associated(x)
    write(fid) test
    if (test) then
        write(fid) size(x)
        write(fid) x
    end if
end subroutine write_pointer__r8_1

subroutine write_pointer__r8_2 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(in), pointer :: x(:,:)
    logical(1) :: test
    test = associated(x)
    write(fid) test
    if (test) then
        write(fid) shape(x)
        write(fid) x
    end if
end subroutine write_pointer__r8_2

subroutine write_pointer__r8_3 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(in), pointer :: x(:,:,:)
    logical(1) :: test
    test = associated(x)
    write(fid) test
    if (test) then
        write(fid) shape(x)
        write(fid) x
    end if
end subroutine write_pointer__r8_3

!********************************************

subroutine read_pointer__r8_0 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(inout), pointer :: x
    logical(1) :: test
    read(fid) test
    if (test) then
        if (.not.associated(x)) allocate(x)
        read(fid) x
    end if
end subroutine read_pointer__r8_0

subroutine read_pointer__r8_1 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(inout), pointer :: x(:)
    logical(1) :: test
    integer(4) :: n
    read(fid) test
    if (test) then
        read(fid) n
        if (.not.associated(x)) then
            allocate(x(n))
        elseif (size(x)/=n) then
            deallocate(x)
            allocate(x(n))
        endif
        read(fid) x
    end if
end subroutine read_pointer__r8_1

subroutine read_pointer__r8_2 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(inout), pointer :: x(:,:)
    logical(1) :: test
    integer(4) :: n(2)
    read(fid) test
    if (test) then
        read(fid) n
        if (.not.associated(x)) then
            allocate(x(n(1),n(2)))
        elseif (all(shape(x)/=n)) then
            deallocate(x)
            allocate(x(n(1),n(2)))
        endif
        read(fid) x
    end if
end subroutine read_pointer__r8_2

subroutine read_pointer__r8_3 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(inout), pointer :: x(:,:,:)
    logical(1) :: test
    integer(4) :: n(3)
    read(fid) test
    if (test) then
        read(fid) n
        if (.not.associated(x)) then
            allocate(x(n(1),n(2),n(3)))
        elseif (all(shape(x)/=n)) then
            deallocate(x)
            allocate(x(n(1),n(2),n(3)))
        endif
        read(fid) x
    end if
end subroutine read_pointer__r8_3

!********************************************

!********************************************
!********************************************
!********************************************


!********************************************

subroutine write_allocatable__r8_0 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(in), allocatable :: x
    logical(1) :: test
    test = allocated(x)
    write(fid) test
    if (test) then
        write(fid) x
    end if
end subroutine write_allocatable__r8_0

subroutine write_allocatable__r8_1 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(in), allocatable :: x(:)
    logical(1) :: test
    test = allocated(x)
    write(fid) test
    if (test) then
        write(fid) size(x)
        write(fid) x
    end if
end subroutine write_allocatable__r8_1

subroutine write_allocatable__r8_2 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(in), allocatable :: x(:,:)
    logical(1) :: test
    test = allocated(x)
    write(fid) test
    if (test) then
        write(fid) shape(x)
        write(fid) x
    end if
end subroutine write_allocatable__r8_2

subroutine write_allocatable__r8_3 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(in), allocatable :: x(:,:,:)
    logical(1) :: test
    test = allocated(x)
    write(fid) test
    if (test) then
        write(fid) shape(x)
        write(fid) x
    end if
end subroutine write_allocatable__r8_3

!********************************************

subroutine read_allocatable__r8_0 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(inout), allocatable :: x
    logical(1) :: test
    read(fid) test
    if (test) then
        if (.not.allocated(x)) allocate(x)
        read(fid) x
    end if
end subroutine read_allocatable__r8_0

subroutine read_allocatable__r8_1 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(inout), allocatable :: x(:)
    logical(1) :: test
    integer(4) :: n
    read(fid) test
    if (test) then
        read(fid) n
        if (.not.allocated(x)) then
            allocate(x(n))
        elseif (size(x)/=n) then
            deallocate(x)
            allocate(x(n))
        endif
        read(fid) x
    end if
end subroutine read_allocatable__r8_1

subroutine read_allocatable__r8_2 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(inout), allocatable :: x(:,:)
    logical(1) :: test
    integer(4) :: n(2)
    read(fid) test
    if (test) then
        read(fid) n
        if (.not.allocated(x)) then
            allocate(x(n(1),n(2)))
        elseif (all(shape(x)/=n)) then
            deallocate(x)
            allocate(x(n(1),n(2)))
        endif
        read(fid) x
    end if
end subroutine read_allocatable__r8_2

subroutine read_allocatable__r8_3 (fid, x)
    integer, intent(in) :: fid
    real(8), intent(inout), allocatable :: x(:,:,:)
    logical(1) :: test
    integer(4) :: n(3)
    read(fid) test
    if (test) then
        read(fid) n
        if (.not.allocated(x)) then
            allocate(x(n(1),n(2),n(3)))
        elseif (all(shape(x)/=n)) then
            deallocate(x)
            allocate(x(n(1),n(2),n(3)))
        endif
        read(fid) x
    end if
end subroutine read_allocatable__r8_3

!********************************************



end module readwrite_mod


program main

    use readwrite_mod

    implicit none

    integer :: fid = 555

    real(8), pointer ::  w,  x(:),  y(:,:),  z(:,:,:)
    real(8), pointer :: ww, xx(:), yy(:,:), zz(:,:,:)

    real(8), allocatable ::  a, b(:), c(:,:), d(:,:,:)

    allocate(w, x(2), y(2,2), z(2,2,2))
    allocate(a, b(2), c(2,2), d(2,2,2))

    call random_seed()
    call random_number(w)
    call random_number(x)
    call random_number(y)
    call random_number(z)

    call random_number(a)
    call random_number(b)
    call random_number(c)
    call random_number(d)

    write(*,*)
    write(*,'("w = ",*(2x,f7.5))') w
    write(*,'("x = ",*(2x,f7.5))') x
    write(*,'("y = ",*(2x,f7.5))') y
    write(*,'("z = ",*(2x,f7.5))') z
    write(*,*)

    open(unit=fid, file='rw.pointer', form='unformatted')
    call write_pointer(fid,w)
    call write_pointer(fid,x)
    call write_pointer(fid,y)
    call write_pointer(fid,z)
    close(fid)

    open(unit=fid, file='rw.pointer', form='unformatted')
    call read_pointer(fid,ww)
    call read_pointer(fid,xx)
    call read_pointer(fid,yy)
    call read_pointer(fid,zz)
    close(fid)

    write(*,*)
    write(*,'("ww = ",*(2x,f7.5))') ww
    write(*,'("xx = ",*(2x,f7.5))') xx
    write(*,'("yy = ",*(2x,f7.5))') yy
    write(*,'("zz = ",*(2x,f7.5))') zz
    write(*,*)


    open(unit=fid, file='rw.allo', form='unformatted')
    call write_allocatable(fid,a)
    call write_allocatable(fid,b)
    call write_allocatable(fid,c)
    call write_allocatable(fid,d)
    close(fid)
    write(*,*)
    write(*,'("a = ",*(2x,f7.5))') a
    write(*,'("b = ",*(2x,f7.5))') b
    write(*,'("c = ",*(2x,f7.5))') c
    write(*,'("d = ",*(2x,f7.5))') d
    write(*,*)

    deallocate(a,b,c,d)

    open(unit=fid, file='rw.allo', form='unformatted')
    call read_allocatable(fid,a)
    call read_allocatable(fid,b)
    call read_allocatable(fid,c)
    call read_allocatable(fid,d)
    close(fid)
    write(*,*)
    write(*,'("a = ",*(2x,f7.5))') a
    write(*,'("b = ",*(2x,f7.5))') b
    write(*,'("c = ",*(2x,f7.5))') c
    write(*,'("d = ",*(2x,f7.5))') d
    write(*,*)



end program main