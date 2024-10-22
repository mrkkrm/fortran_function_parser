program tests_mrk

    use function_parser, rk => fparser_rk

    implicit none

    character(len=*), dimension(*), parameter :: var = ['A', 'B', 'C', 'D']
    real(rk),         dimension(*), parameter :: val = [ 3.14_rk, 0.0_rk, -23.0_rk, 0.1_rk]

    character(len=*), parameter :: fun1 = "    ~( ~A & ~B & ~C & ~D ) "
    character(len=*), parameter :: fun2 = "  ( ~( ~A & ~B & ~C & ~D ))"
    character(len=*), parameter :: fun3 = " ~( ~( ~A & ~B & ~C & ~D ))"

    character(len=:), allocatable :: fun

    real(rk) :: ans

    type(fparser) :: parser, p1, p2

    !call test('~1')
    !call test('~1 & ~1')
    !call test('~1 & ~1 & ~1')
    !call test('~(~1 & ~1 & ~1 & ~1)')

    !call test('~2 >= ~1')

    !call test('0-3**2')
    !call test(' -3**2')

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

    call test('(!3*0) == (!(3*0))')
    call test('(!3*0) /= ((!3)*0)')
    write(*,*)

    call test('(-2*3) == (-(2*3))')
    call test('(-2*3) == ((-2)*3)')
    write(*,*)

    call test( '(-2|1) == ((-2)&1)')
    call test( '(-2|1) /= (-(2&1))')
    write(*,*)

    call test('-!3')
    call test('!-3')
    write(*,*)

    call p1%parse('A+AA',['A ','AA'])
    call p2%parse('AA+A',['AA','A '])
    
    associate(b1=>p1%bytecode, &
              b2=>p2%bytecode  )
      write(*,*) b1
      write(*,*) b2
      write(*,*) all(b1.eq.b2)
      write(*,*) size(b1).eq.size(b2) .and. all(b1.eq.b2)
    end associate

    contains

    subroutine test(fun)
        character(len=*) :: fun
        call parser%parse(fun, var, case_sensitive=.true.)
        if (parser%error()) then
            !call parser%print_errors(6)
            write(*,'(a30," = ",a)') fun, "ERROR"
        else
            call parser%evaluate(val, ans)
            write(*,'(a30," = ",g0)') fun, ans
            call parser%print_stack
        endif
    end subroutine test

end program tests_mrk
