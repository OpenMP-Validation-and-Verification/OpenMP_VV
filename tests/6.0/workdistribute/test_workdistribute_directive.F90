!===----test_workdistribute_directive.F90-----------------------------------===//
!
! OpenMP API Version 6.0 
!
! This test checks if the workdistribute passed to the nowait clause
! 
!===------------------------------------------------------------------------===//

#include "ompvv.F90"

module axpy_mod
    implicit none
contains
    subroutine axpy_workdistribute(a, x, y, n)
        implicit none
        integer :: n
        real :: a
        real, dimension(n) :: x
        real, dimension(n) :: y
        !$omp target teams workdistribute map(to:x) map(tofrom:y)
            y = a * x + y
        !$omp end target teams workdistribute
    end subroutine axpy_workdistribute
end module axpy_mod

program test_omp_workdistribute
    use iso_fortran_env
    use ompvv_lib
    use omp_lib
    use axpy_mod, only: axpy_workdistribute
    implicit none

    !OMPVV_TEST_OFFLOADING

    integer :: errors = 0
    integer, parameter :: N = 1024 * 1024
    real :: a
    real :: x0
    real :: y0
    real, dimension(N) :: x
    real, dimension(N) :: y
    a = 2.0
    x = 2.0  ! initialize arrays
    y = 1.0
    x0 = 2.0 ! initialize scalars for validation
    y0 = 1.0
    
    call axpy_workdistribute(a, x, y, N)
    IF (sum(y) / N .NE. a * x0 + y0) THEN
        errors = errors + 1
    END IF 
    !write (*,'(A,F4.2,A,F4.2)') 'sum ', sum(y) / N, ' expected ', a * x0 + y0

    OMPVV_ERROR_IF(errors /= 0, "The workdistribute directive did not perform as expected.")
    OMPVV_REPORT_AND_RETURN()
end program test_omp_workdistribute
