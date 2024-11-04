module selection_mod

implicit none
private

public :: crude_select
public :: quick_select

interface crude_select
    !> Crude selection routine
    !> Select the `k`-th order statistic from `array`
    !> Brute force algorithm using minloc/maxloc
    module procedure :: crude_select__i4
    module procedure :: crude_select__r4
    module procedure :: crude_select__i8
    module procedure :: crude_select__r8
end interface

interface quick_select
    !> Quickselect selection routine
    !> Select the `k`-th order statistic from `array`
    !> Quickselect algorithm from: https://en.wikipedia.org/wiki/Quickselect
    module procedure :: quick_select__i4
    module procedure :: quick_select__r4
    module procedure :: quick_select__i8
    module procedure :: quick_select__r8
end interface

contains

!****************************************************
!** CRUDE_SELECT PROCEDURES *************************
!****************************************************

!****************************************************
function crude_select__r8(array,k) result(val)
    integer,  parameter     :: ak = 8
    integer,  intent(in)    :: k
    real(ak), intent(inout) :: array(:)
    real(ak)                :: val
    call select()
    contains
#include "selection_mod__crude_select"
end function crude_select__r8
!****************************************************

!****************************************************
function crude_select__r4(array,k) result(val)
    integer,  parameter     :: ak = 4
    integer,  intent(in)    :: k
    real(ak), intent(inout) :: array(:)
    real(ak)                :: val
    call select()
    contains
#include "selection_mod__crude_select"
end function crude_select__r4
!****************************************************

!****************************************************
function crude_select__i8(array,k) result(val)
    integer,     parameter     :: ak = 8
    integer,     intent(in)    :: k
    integer(ak), intent(inout) :: array(:)
    integer(ak)                :: val
    call select()
    contains
#include "selection_mod__crude_select"
end function crude_select__i8
!****************************************************

!****************************************************
function crude_select__i4(array,k) result(val)
    integer,     parameter     :: ak = 4
    integer,     intent(in)    :: k
    integer(ak), intent(inout) :: array(:)
    integer(ak)                :: val
    call select()
    contains
#include "selection_mod__crude_select"
end function crude_select__i4
!****************************************************


!****************************************************
!** QUICK_SELECT PROCEDURES *************************
!****************************************************


!****************************************************
function quick_select__r8(array,k) result(val)
    integer,  parameter     :: ak = 8
    integer,  intent(in)    :: k
    real(ak), intent(inout) :: array(:)
    real(ak)                :: val
    real(ak)                :: tempValue, pivotValue
    call select()
    contains
#include "selection_mod__quick_select"
end function quick_select__r8
!****************************************************

!****************************************************
function quick_select__r4(array,k) result(val)
    integer,  parameter     :: ak = 4
    integer,  intent(in)    :: k
    real(ak), intent(inout) :: array(:)
    real(ak)                :: val
    real(ak)                :: tempValue, pivotValue
    call select()
    contains
#include "selection_mod__quick_select"
end function quick_select__r4
!****************************************************

!****************************************************
function quick_select__i8(array,k) result(val)
    integer,     parameter     :: ak = 8
    integer,     intent(in)    :: k
    integer(ak), intent(inout) :: array(:)
    integer(ak)                :: val
    integer(ak)                :: tempValue, pivotValue
    call select()
    contains
#include "selection_mod__quick_select"
end function quick_select__i8
!****************************************************

!****************************************************
function quick_select__i4(array,k) result(val)
    integer,     parameter     :: ak = 4
    integer,     intent(in)    :: k
    integer(ak), intent(inout) :: array(:)
    integer(ak)                :: val
    integer(ak)                :: tempValue, pivotValue
    call select()
    contains
#include "selection_mod__quick_select"
end function quick_select__i4
!****************************************************

end module selection_mod
