program tests_mrk

    use function_parser, rk => fparser_rk
    use selection_mod

    implicit none

    character(len=*), dimension(*), parameter :: var = ['A   ', 'B   ', 'C   ', 'D   ', 'acos']
    real(rk),         dimension(*), parameter :: val = [ 3.14_rk, 0.0_rk, -23.0_rk, 0.1_rk, 0.45_rk]

    character(len=*), parameter :: fun1 = "    ~( ~A & ~B & ~C & ~D ) "
    character(len=*), parameter :: fun2 = "  ( ~( ~A & ~B & ~C & ~D ))"
    character(len=*), parameter :: fun3 = " ~( ~( ~A & ~B & ~C & ~D ))"

    real(rk) :: ans

    type(fparser) :: parser, p1, p2

    logical, parameter :: PRINT_STACK = .TRUE.

    FFP_ERROR_NAN     = .TRUE.
    FFP_CHECK_SYNTAX  = .TRUE.
    FFP_VERBOSE_PARSE = .TRUE.

    !call test('~1')
    !call test('~1 & ~1')
    !call test('~1 & ~1 & ~1')
    !call test('~(~1 & ~1 & ~1 & ~1)')

    !call test('~2 >= ~1')

    call test('(-3**2) == (0-3**2)')
    write(*,*)

    !call test('-3 + -4')

    !!call test('0+-3+-4')

    !!call test('-(3+-4)')

    !!call test('(-3)+(-4)')

    call test('(!3**0) == (!(3**0))')
    call test('(!3**0) /= ((!3)**0)')
    write(*,*)

    call test('(-3**0) == (-(3**0))')
    call test('(-3**0) /= ((-3)**0)')
    write(*,*)

    call test('(!3*0) == ((!3)*0)')
    call test('(!3*0) /= (!(3*0))')
    write(*,*)

    call test('(-2*3) == (-(2*3))')
    call test('(-2*3) == ((-2)*3)')
    write(*,*)

    call test('(!0/4) == ((!0)/4)')
    call test('(!0/4) /= (!(0/4))')
    write(*,*)

    call test('(-2/4) == (-(2/4))')
    call test('(-2/4) == ((-2)/4)')
    write(*,*)

    call test( '(-2|1) == ((-2)|1)')
    call test( '(-2|1) /= (-(2|1))')
    write(*,*)

    call test( '(-2&1) == ((-2)&1)')
    call test( '(-2&1) /= (-(2&1))')
    write(*,*)

    call test( '(!2|1) == ((!2)|1)')
    call test( '(!2|1) /= (!(2|1))')
    write(*,*)

    call test( '(!2&0) == ((!2)&0)')
    call test( '(!2&0) /= (!(2&0))')
    write(*,*)

    call test( '(-1<3) == ((-1)<3)')
    call test( '(-1<3) /= (-(1<3))')
    write(*,*)

    call test( '(!1<3) == ((!1)<3)')
    call test( '(!1<3) /= (!(1<3))')
    write(*,*)

    call test( '(!-1<3) == ((!-1)<3)')
    call test( '(!-1<3) /= (!(-1<3))')
    write(*,*)

    call test( '3==2+4==0')
    call test( '(3==(2+4))==0')
    call test( '3==((2+4)==0)')
    call test( '3==(2+(4==0))')
    call test( '((3==2)+4)==0')
    write(*,*)

    call p1%parse('A+AA',['A ','AA'])
    call p2%parse('AA+A',['AA','A '])

    associate(b1=>p1%bytecode, &
              b2=>p2%bytecode  )
      !write(*,*) isequal(b1,b2)
    end associate

    call test('(-1<3)')
    call test('(!1<3)')
    call test('4+++-+-+--+3')
    call test('3-!!+!-0')
    call test('-!0')
    call test('!-0')
    call test('(!-1<3)')
    call test('(0 + !-1<3)')
    call test('(!-1<3)**1')
    call test('-!0 + 3')
    call test('!-0 + 3')
    write(*,*)

    call test('-4**2**2')
    write(*,*)

    call test('acos*1')     ! variable acos
    call test('acos(0.1)')  ! function acos()
    call test('acos(1.5)')  ! function acos() [value error]
    call test('pi()')       ! function pi()
    call test('pi')         ! syntax error
    write(*,*)

    call test('A.eqv. B')
    call test('A.eqv. A')
    call test('A.neqv.B')
    call test('A.neqv.A')
    write(*,*)

    call test(' 2 >  2  == 1')
    call test('(2 >  2) == 1')
    call test(' 2 > (2  == 1)')
    write(*,*)

    call test(' 0 ==  0  > 1')
    call test('(0 ==  0) > 1')   ! This if equal precedence
    call test(' 0 == (0  > 1)')  ! This if ">" has higher precedence
    write(*,*)


    ! unrelated topic (kth order statistic):
    k_order: &
    block
        integer, parameter :: N = 10
        real :: x(N)
        logical :: mask(N)
        integer :: ii, idx, sz
        integer, allocatable :: seed(:)
        call random_seed(size=sz)
        allocate(seed(sz))
        call random_seed(put=[04211979,1])
        call random_seed(get=seed); write(*,*) "seed = ", seed
        call random_number(x)
        call random_seed(get=seed); write(*,*) "seed = ", seed
        call random_number(x)
        call random_seed(get=seed); write(*,*) "seed = ", seed
        call random_number(x)
        call random_seed(get=seed); write(*,*) "seed = ", seed
        call random_number(x)

        write(*,*)
        write(*,*) "  2**3**2  = ",  2**3**2
        write(*,*) "(2**3)**2  = ", (2**3)**2, "Matlab"   !! Matlab answer
        write(*,*) " 2**(3**2) = ", 2**(3**2), "Fortran"  !! Fortran answer
        write(*,*)

        call random_seed()
        call random_number(x)
        write(*,*) x
        mask = .true.
        write(*,*) "min value: ", minval(x);
        write(*,*) "max value: ", maxval(x);
        do ii = 1,N
            idx = minloc(x, dim=1, mask=mask)
            write(*,*) ii, idx, x(idx)
            mask(idx) = .false.
        end do
    end block &
    k_order

    BLOCK
        integer, parameter :: N=100, REP=15, k=10
        real(8) :: x(N), val1, val2
        integer :: ii
        call random_seed()
        write(*,*) "***********************************"
        do ii = 1, REP
            call random_number(x)
            x = 100*x
            val1 = crude_select(x,k)
            val2 = quick_select(x,k)
            write(*,'(2x,i2,1x,2(f9.2,2x),2(i3,2x))') &
              ii, val1, val2, count(x<=val1), count(x<=val2)
        end do
        write(*,*) "***********************************"
        do ii = 1, REP
            call random_number(x)
            x = 100*x
            val1 = crude_select(x,-k)
            val2 = quick_select(x,-k)
            write(*,'(2x,i2,1x,2(f9.2,2x),2(i3,2x))') &
              ii, val1, val2, count(x>=val1), count(x>=val2)
        end do
        write(*,*) "***********************************"
    END BLOCK


    contains

    subroutine test(fun)
        character(len=*) :: fun
        call parser%parse(fun, var, case_sensitive=.true.)
        if (parser%error()) then
            !call parser%print_errors(6)
            write(*,'(a30," = ",a)') fun, "SYNTAX ERROR"
        else
            call parser%evaluate(val, ans)
            write(*,'(a30," = ",g0)') fun, ans
            if (PRINT_STACK) call parser%print_stack
        endif
    end subroutine test

    logical function isequal(v1,v2)
        integer, dimension(:), intent(in) :: v1, v2
        isequal = (size(v1)==size(v2))
        if (isequal) isequal = all(v1==v2)
    end function isequal

end program tests_mrk
