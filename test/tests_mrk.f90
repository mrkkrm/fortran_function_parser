program tests_mrk

    use function_parser, rk => fparser_rk

    implicit none

    character(len=*), dimension(*), parameter :: var = ['A   ', 'B   ', 'C   ', 'D   ', 'acos']
    real(rk),         dimension(*), parameter :: val = [ 3.14_rk, 0.0_rk, -23.0_rk, 0.1_rk, 0.45_rk]

    character(len=*), parameter :: fun1 = "    ~( ~A & ~B & ~C & ~D ) "
    character(len=*), parameter :: fun2 = "  ( ~( ~A & ~B & ~C & ~D ))"
    character(len=*), parameter :: fun3 = " ~( ~( ~A & ~B & ~C & ~D ))"

    real(rk) :: ans

    type(fparser) :: parser, p1, p2

    FFP_ERROR_NAN     = .TRUE.
    FFP_CHECK_SYNTAX  = .TRUE.
    FFP_VERBOSE_PARSE = .FALSE.

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
            !call parser%print_stack
        endif
    end subroutine test

    logical function isequal(v1,v2)
        integer, dimension(:), intent(in) :: v1, v2
        isequal = (size(v1)==size(v2))
        if (isequal) isequal = all(v1==v2)
    end function isequal

end program tests_mrk
