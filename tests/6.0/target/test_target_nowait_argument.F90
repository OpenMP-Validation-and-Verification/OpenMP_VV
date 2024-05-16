
!===--- test_target_nowait_argument.F90      -------------------------------===//
!
! OpenMP API Version 6.0 
!
! This test checks if the argument passed to the nowait clause 
! on the target directive is honoured.
! 
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"
  
program test_omp_target_nowait
  use iso_fortran_env
  use ompvv_lib
  use omp_lib
  implicit none
  integer :: errors, x, y,iter
  logical :: is_deferred
  real :: rand_no

  OMPVV_TEST_OFFLOADING

  ! Determine if computation is deferred
  do iter = 0,1  
    x = 2
    y = 3
    errors = 0
    if(iter == 0) then
      is_deferred = .false.
    else
     is_deferred =  .true.
    endif
   
  
    ! Offload with nowait argument
    !$omp target map(tofrom: x) nowait(is_deferred)
      call update(x)
    !$omp end target
    
    if (is_deferred) then
      call update(y)
    else
      call update(x)
    endif
  
    ! Check for errors
    if (is_deferred) then
      if (y /= 9) then
        errors = errors + 1
      endif
      !$omp taskwait  ! Ensure completion of deferred tasks
      if (x /= 6) then
        errors = errors + 1
      endif
    else
      if (x /= 18) then
        errors = errors + 1
      endif
    endif

  end do

  OMPVV_ERROR_IF(errors /= 0, "The argument to nowait was not honoured")
  OMPVV_REPORT_AND_RETURN()

contains

subroutine update(num)
  integer, intent(inout) :: num
  !$omp declare target
  num = num * 3
end subroutine update

end program test_omp_target_nowait